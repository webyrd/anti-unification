;; functional anti-unification algorithm
;;
;; adapted from figure 2 of 'A functional reconstruction of anti-unification'
;; by Bjarte M. Østvold
;; Norwegian Computing Center
;; DART/04/04
;; 2004


(load "pmatch.scm")
(load "test-check.scm")

(define var (lambda (x) (vector x)))
(define var? (lambda (x) (vector? x)))

(define genny (lambda (c n) (string->symbol (list->string (list c #\_ (integer->char (+ n (char->integer #\0))))))))

(define invert-subst (lambda (subst) (map (lambda (p) (cons (cdr p) (car p))) subst)))

(define (pre-process term*)
  (letrec ([pre-process (lambda (term* subst n)
                          (pmatch term*
                            [(,a . ,d)
                             (let-values ([(a subst n) (pre-process a subst n)])
                               (let-values ([(d subst n) (pre-process d subst n)])
                                 (values `(,a . ,d) subst n)))]
                            [,v (guard (var? v))
                             (cond
                               [(assq v subst) => (lambda (p)
                                                    (let ((c (cdr p)))
                                                      (values c subst n)))]
                               [else
                                (let ((c (genny #\c n)))
                                  (let ((subst (cons `(,v . ,c) subst)))
                                    (let ((n (add1 n)))
                                      (values c subst n))))])]
                            [,t (values t subst n)]))])
    (let-values ([(term* subst n) (pre-process term* '() 0)])
      (values term* subst))))

(define (post-process term subst)
  (pmatch term
    [(,a . ,d) (cons (post-process a subst) (post-process d subst))]
    [,c (guard (symbol? c))
     (cond
       [(assq c subst) => cdr]
       [else c])]
    [,t t]))

(define (au term*)
  (unless (> (length term*) 0) (error 'au "|term*| must be > 0"))
  (let-values ([(term* subst) (pre-process term*)])
    (let-values ([(s theta* n) (au-theta* term* '() 0)]) ; rule (6)
      (let ((inv-subst (invert-subst subst)))
        (let ((s (post-process s inv-subst)))
          s)))))

(define (au-theta* term* theta* n)
  (unless (> (length term*) 0) (error 'au-theta* "|term*| must be > 0"))
  (pmatch term*
    [(,t . ,t*) (guard (andmap (lambda (t^) (equal? t^ t)) t*)) ; rule (7)
     (values t theta* n)]
    [((,t1 . ,t1*) . ,t*) (guard (andmap pair? t*)) ; rule (8)
     (let-values ([(s theta^* n) (au-theta* (map car term*) theta* n)])
       (let-values ([(s* theta^^* n) (au-theta* (map cdr term*) theta^* n)])
         (values `(,s . ,s*) theta^^* n)))]
    [,t* (guard (assoc t* theta*)) ; rule (9)
     (let ((x (cdr (assoc t* theta*))))
       (values x theta* n))]
    [,t* ; rule (10)
     (let ((z (var (genny #\z n))))
       (let ((theta* `((,t* . ,z) . ,theta*)))
         (let ((n (add1 n)))
           (values z theta* n))))]))

(test "pre-process-1"
  (let-values ([(t subst) (pre-process '(5 5))])
    (list t subst))
  '((5 5) ()))

(test "pre-process-2"
  (let-values ([(t subst) (pre-process `(,(var 'x) 5))])
    t)
  '(c_0 5))

(test "pre-process-3"
  (let ((x (var 'x)) (y (var 'y)))
    (let ((t* `(,x 5 ,y (,x ,y))))
      (let-values ([(t subst) (pre-process t*)])
        t)))
  '(c_0 5 c_1 (c_0 c_1)))

(let ((x (var 'x)) (y (var 'y)))
  (let ((t* `(,x 5 ,y (,x ,y))))
    (test "post-process-1"
      (let-values ([(t subst) (pre-process t*)])
        (let ((inv-subst (invert-subst subst)))
          (post-process '(c_0 5 c_1 (c_0 c_1)) inv-subst)))
      t*)))

(test "au-1"
  (au '(5 5))
  '5)

(test "au-2"
  (au '(5 6))
  (var (genny #\z 0)))

(test "au-3"
  (au '(5 6 7))
  (var (genny #\z 0)))

(let ((x (var 'x))
      (y (var 'y)))
  (test "au-4"
    (let ((t1 `(c ,x (c ,x)))
          (t2 `(d ,x (d ,y))))
      (au (list t1 t2)))
    (let ((z_0 (var (genny #\z 0)))
          (z_1 (var (genny #\z 1))))
      `(,z_0 ,x (,z_0 ,z_1)))))

(let ((x (var 'x))
      (y (var 'y)))
  (test "au-5"
    (let ((t1 `(c ,x (c ,x)))
          (t2 `(d ,x (d ,y)))
          (t3 `(d ,x (d ,y))))
      (au (list t1 t2 t3)))
    (let ((z_0 (var (genny #\z 0)))
          (z_1 (var (genny #\z 1))))
      `(,z_0 ,x (,z_0 ,z_1)))))
