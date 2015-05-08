
(defun func (n) 
    (labels ((%func-1 (n) 
                (if (= n 1) 
                    2
                    (let ((prev (%func-1 (1- n)))) 
                        (- (let ((sin-res (sin prev))) (* sin-res sin-res))
                           (let ((cos-res (cos prev))) (* cos-res cos-res))))))
             (%func-2 (n)
                (if (= n 9) 
                    1 
                    (+ (sqrt (%func-2 (1- n))) (/ (exp n) 100)))))

        (cond 
            ((and (> n 0) (< n 9)) (%func-1 n))
            ((and (> n 8) (< n 15)) (%func-2 n))
            (t (error "~a invalid argument. Should be in the interval [1,14]!" n)))))