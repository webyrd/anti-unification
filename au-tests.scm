(load "au.scm")
(load "test-check.scm")


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
  (test "au-4b"
    (let ((t1 `(c ,y (c ,x)))
          (t2 `(d ,x (d ,y))))
      (au (list t1 t2)))
    (let ((z_0 (var (genny #\z 0)))
          (z_1 (var (genny #\z 1)))
          (z_2 (var (genny #\z 2))))
      `(,z_0 ,z_1 (,z_0 ,z_2)))))

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

(test "au-6"
  (let ((t1 `(lambda (5) 5))
        (t2 `(lambda (#t) #t)))
    (au (list t1 t2)))
  (let ((z_0 (var (genny #\z 0))))
    `(lambda (,z_0) ,z_0)))

(test "au-7"
  (let ((t1 `(lambda (5) 5))
        (t2 `(lambda (#t) #t))
        (t3 `(lambda (5) 6)))
    (au (list t1 t2 t3)))
  (let ((z_0 (var (genny #\z 0)))
        (z_1 (var (genny #\z 1))))
    `(lambda (,z_0) ,z_1)))

(test "au-8"  
  (let ((t1 `(lambda (5) 5))
        (t2 `(lambda (#t) #t)))
    (let ((x (var 'x)))
      (let ((t-rhs (au (list t1 t2)))
            (t-lhs `(lambda (,x) ,x)))
        (au (list t-lhs t-rhs)))))
  (let ((z_0 (var (genny #\z 0))))
    `(lambda (,z_0) ,z_0)))

(test "au-9"
  (let ((t1 `(lambda (5) 5))
        (t2 `(lambda (#t) #t))
        (t3 `(lambda (5) 6)))
    (let ((x (var 'x)))
      (let ((t-rhs (au (list t1 t2 t3)))
            (t-lhs `(lambda (,x) ,x)))
        (au (list t-lhs t-rhs)))))
  (let ((z_0 (var (genny #\z 0)))
        (z_1 (var (genny #\z 1))))
    `(lambda (,z_0) ,z_1)))

