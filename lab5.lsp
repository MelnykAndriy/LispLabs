


(defun get-inmost-list (lst)
    (declare (list lst))
    (labels ((%find-inmost-list (lst deep)
                (if (notany #'listp lst) 
                    (list lst deep) 
                    (car 
                        (sort (loop for sub-lst in lst 
                                when (listp sub-lst)
                                collect (%find-inmost-list sub-lst (1+ deep))) #'> :key #'second)))))
               
        (car (%find-inmost-list lst 0))))