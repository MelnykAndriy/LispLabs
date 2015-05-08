
(let ((element-counter 0) )

(defun del-every-third (lst) 
    (declare (list lst))
    (if lst 
        (if (/= 0 (second (multiple-value-list (ffloor (incf element-counter) 3))))    
            (cons (car lst) (del-every-third (cdr lst)))
            (del-every-third (cdr lst)))
        (progn 
            (setq element-counter 0) 
            nil))))