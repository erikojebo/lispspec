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

(defmacro show (x)
  `(format t "~a --> ~a~%"
           ',x ,x))
