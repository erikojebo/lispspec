;(defun add (&rest numbers) (...))
;(add) => 0
;(add 1) => 1
;(add 1 2) => 3

;(specification-for add
;		   (it "returns 0 for no arguments"
;		       (equal 0 (add)))
;		   (it "returns same for single number"
;		       (equal 1 (add 1))
;		       (equal 2 (add 2))))

;(setf *lspec-specs* 
;      '(("add" (("returns zero for empty string"
;		 ((equal 0 (add ""))))
;		("returns same for one single digit number"
;		 ((equal 1 (add "1"))
;		  (equal 2 (add "2"))))))'
;	("split-string" (("does not split empty string"
;			 ((equal '("") (split-string "" ","))
;			  (equal '("") (split-string "" ""))))))))

(defparameter *lspec-specs* nil)

(defun exec-specs ()
  (loop for spec in *lspec-specs*
	do (progn
	     (print-function-name spec)
	     (exec-it-statements (cadr spec)))))

(defun print-function-name (spec)
  (format t "~a~%" (car spec)))

(defun exec-it-statements (statements)
  (loop for its on statements
       	do (let ((description (caar its))
		 (asserts (second (car its)))) 
	     (exec-asserts asserts description))))
  
(defun exec-asserts (asserts description)
  (loop for assert-statement in asserts
	with has-printed-statement-header = nil
	do (if (eval assert-statement)
	       (progn 
		 (if (not has-printed-statement-header)
		     (format t " - ~a~%" description))
		 (setf has-printed-statement-header t))
	       (progn
		 (if (not has-printed-statement-header)
		     (format t " - FAIL: ~a~%" description))
		 (format t "    => ~a~%" assert-statement)
		 (setf has-printed-statement-header t)))))

(defmacro it (description &rest asserts)
  (cons description (list asserts)))

(defmacro specification-for (function-name &rest it-statements)
  `(setf *lspec-specs*
	 (cons 
	  (quote ,(list function-name
			(mapcar #'macroexpand-1 it-statements)))
	  ;; remove any previous declaration for the given spec
	  (remove-if
	   #'(lambda (spec) (equal (car spec) (quote ,function-name)))
	   *lspec-specs*))))


(defun clear-specs () (setf *lspec-specs* nil))


