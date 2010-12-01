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

;; (with-arg-stub (f (1 2) 3) (f)) =>
;;   (f (&rest args) (if-tree-equal expected args) 3 (funcall #'old-f))

(defmacro with-arg-stub (fdef &rest body)
  (let ((name (car fdef))
        (expected-args (cadr fdef))
        (rbody (cddr fdef)))
    `(let ((oldf (symbol-function ',name)))
       (setf (symbol-function ',name)
             (lambda (&rest actual-args)
               (if (equal ',expected-args actual-args)
                   ,@rbody
               (apply oldf actual-args))))
       (let ((result (progn ,@body)))
         (setf (symbol-function ',name) oldf)
         result))))


;; Specifications

(defun f () 5)
(defun f-plus-one () (1+ (f)))
(defun funcall-f () (funcall #'f))
(defun twice (x) (* 2 x))

(specification-for 
 with-stub
 (it "replaces parameter less function definition called from body"
     (with-stub (f () 2)
       (equal 2 (f))))
 (it "replaces parameter less function definition called from other function"
     (with-stub (f () 2)
       (equal 3 (f-plus-one))))
 (it "replaces parameter less function definition called with funcall from body"
     (with-stub (f () 2)
       (equal 2 (funcall #'f))))
 (it "replaces parameter less function definition called with funcall from other function"
     (with-stub (f () 2)
       (equal 2 (funcall-f))))
 (it "replaces function definition with parameter called from body"
     (with-stub (twice (x) (declare (ignore x)) 4)
       (equal 5 (1+ (twice 2)))))
 (it "replaces function definition if parameters match"
     (with-arg-stub (twice (2) 1)
       (equal 1 (twice 2))))
 (it "does not replace function definition if parameters do not match"
     (with-arg-stub (twice (2) 1)
       (equal 6 (twice 3))))
 )
;;                   (it "replaces function
;;                       (with-stub (f () 2)
;;                                  (let (function-to-call #'f)
;;                                    (equal 3 (funcall function-to-call))))))
                                  


;; Match argumet list: (with-stub (foo (1 2 3) 7) do-something)
;; Ignore arguments: (with-stub (foo () 7 :ignore-arguments t) do-something)
;; Ignore some: (with-stub (foo (1 2 3) 7 :constraints (#'eq #'ignore #'ignore)) do-something)
;; Return sequence: (with-stub (foo (1 2 3) nil :return-sequence (7 8 9)) do-something)

;; Expect with given argument list: (with-expectation (foo (1 2 3) 7) do-something)
;; Expect call ignoring arguments: (with-expectation (foo (1 2 3) 7 :ignore-arguments t) do-something)
;; Ignore some: (with-expectation (foo (1 2 3) 7 :constraints (#'eq #'ignore #'ignore)) do-something)
;; Return sequence: (with-expectation (foo (1 2 3) nil :return-sequence (7 8 9)) do-something)