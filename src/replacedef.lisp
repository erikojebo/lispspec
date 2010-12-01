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
(defun funcall-f-plus-one () (1+ (f)))
(defun double-plus-one (x) (1+ (make-double x)))

(specification-for with-stub
                   (it "replaces parameter less function definition"
                       (with-stub (f () 2)
                                  (equal 3 (f-plus-one))))
                   (it "replaces function definition with parameter"
                       (with-stub (make-double (x) 4)
                                  (equal 5 (double-plus-one 2))))
                   (it "replaces function value"
                       (with-stub (f () 2)
                                  (let (function-to-call #'f)
                                    (equal 3 (funcall function-to-call))))))
                                  


;; Match argumet list: (with-stub (foo (1 2 3) 7) do-something)
;; Ignore arguments: (with-stub (foo () 7 :ignore-arguments t) do-something)
;; Ignore some: (with-stub (foo (1 2 3) 7 :constraints (#'eq #'ignore #'ignore)) do-something)
;; Return sequence: (with-stub (foo (1 2 3) nil :return-sequence (7 8 9)) do-something)

;; Expect with given argument list: (with-expectation (foo (1 2 3) 7) do-something)
;; Expect call ignoring arguments: (with-expectation (foo (1 2 3) 7 :ignore-arguments t) do-something)
;; Ignore some: (with-expectation (foo (1 2 3) 7 :constraints (#'eq #'ignore #'ignore)) do-something)
;; Return sequence: (with-expectation (foo (1 2 3) nil :return-sequence (7 8 9)) do-something)