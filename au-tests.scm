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
  (test "au-5"
    (let ((t1 `(c ,x (c ,x)))
          (t2 `(d ,x (d ,y)))
          (t3 `(d ,x (d ,y))))
      (au (list t1 t2 t3)))
    (let ((z_0 (var (genny #\z 0)))
          (z_1 (var (genny #\z 1))))
      `(,z_0 ,x (,z_0 ,z_1)))))
