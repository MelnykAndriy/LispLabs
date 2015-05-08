

(defun prime-factors-mult (n)
    (labels ((%prime-p (number) 
                (when (> number 1) 
                    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))
                    
            (%next-prime (number) 
                (loop for n from number when (%prime-p n) return n)))
                
        (macrolet ((%do-primes ((var start end) &body body) 
                        `(do ((,var (%next-prime ,start) (%next-prime (1+ ,var))))
                            ((> ,var ,end))
                            ,@body)))
            (let ((primes nil))
                (%do-primes (p 0 n) (push p primes))
                (mapcar (lambda (p) 
                            (list p (do ((ntimes-p p (* ntimes-p p)) 
                                         (ntimes 0 (1+ ntimes)))
                                            ((> (mod n ntimes-p) 0) ntimes))))
                        (reverse (remove-if-not (lambda (p) (zerop (mod n p))) primes)))))))


