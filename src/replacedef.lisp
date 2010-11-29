(defmacro with-stub (fdef &rest body)
  (let ((oldf (gensym))
        (result (gensym))
        (name (car fdef))
        (args (cadr fdef))
        (rbody (cddr fdef)))
    `(let ((,oldf (symbol-function ',name)))
       (setf (symbol-function ',name) (lambda ,args ,@rbody))
       (let ((,result (progn ,@body)))
         (setf (symbol-function ',name) ,oldf)
         ,result))))

;; Specifications

(defun f () 5)
(defun make-double (x) (* 2 x))
(defun f-plus-one () (1+ (f)))
(defun double-plus-one (x) (1+ (make-double x)))

(specification-for with-stub
                   (it "replaces parameter less function definition"
                       (with-stub (f () 2)
                         (equal 3 (f-plus-one))))
                   (it "replaces function definition with parameter"
                       (with-stub (make-double (x) 4)
                         (equal 5 (double-plus-one 2)))))
