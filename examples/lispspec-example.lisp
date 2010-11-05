(specification-for 
 myadd
 (it "returns zero for no parameters"
     (equal 0 (myadd)))
 (it "returns parameter for single parameters"
     (equal 1 (myadd 1))
     (equal 2 (myadd 2)))
 (it "returns sum for two parameters"
     (equal 1 (myadd 1 0))
     (equal 3 (myadd 1 2))
     (equal 4 (myadd 2 2))))

(specification-for 
 mysubtract
 (it "returns zero for no parameters"
     (equal 0 (mysubtract)))
 (it "returns negated parameter for single parameters"
     (equal -1 (mysubtract 1))
     (equal -2 (mysubtract 2)))
 (it "returns difference for two parameters"
     (equal 1 (mysubtract 1 0))
     (equal -1 (mysubtract 1 2))
     (equal 0 (mysubtract 2 2))))

(defun myadd (&rest values) 
  (reduce #'+ values))

(defun mysubtract (&rest values)
  (let ((value-count (length values)))
    (cond ((> value-count 0)
	   (apply #'- values))
	  (t 0))))