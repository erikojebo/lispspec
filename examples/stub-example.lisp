(defun y () 1)
(defun x () (1+ (y)))


(specification-for 
 x
 (it "adds one to result of y"
     (with-stub (y () 5)
	    (equal 6 (x)))))

