
(defclass Graph () 
    ((representation :accessor representation 
                     :initarg :representation
                     :initform nil)))

(defclass Edge () 
    ((vertex-1 :accessor vertex-1
               :initarg :vertex-1
               :initform (error "Must supply an edge vertex-1."))\
     (vertex-2 :accessor vertex-2
               :initarg :vertex-2
               :initform (error "Must supply an edge vertex-2."))
     (weight   :accessor weight 
               :initarg :weight 
               :initform (error "Must supply an edge weight."))))
               
(defgeneric is-loop (edge))
(defgeneric is-standalone-edge-rep (edge)) 
(defgeneric does-edge-represent-this-standalone-node (edge node))

(defgeneric add-node (grh))
(defgeneric depth-first (grh start finish))
(defgeneric breadth-first (grh start finish))
(defgeneric degree (grh node))
(defgeneric nodes-list (grh))

            
(defgeneric nodes (grh))    
(defgeneric edges (grh))

(defmethod is-loop ((edge Edge))
    (and (eq (vertex-1 edge) (vertex-2 edge))
         (not (zerop (weight edge)))))
    
(defmethod is-standalone-edge-rep ((edge Edge))
    (and (eq (vertex-1 edge) (vertex-2 edge))
         (zerop (weight edge))))
         
(defmethod does-edge-represent-this-standalone-node ((edge Edge) node)
    (and (eq (vertex-1 edge) (vertex-2 edge))
         (zerop (weight edge))
         (eq (vertex-1 edge) node)))

(defmethod add-edge ((grh Graph) (edge Edge))
    (pushnew edge (representation grh)))

(defmethod add-standalone-node ((grh Graph) node) 
    (push (make-instance 'Edge :vertex-1 node :vertex-2 node :weight 0) 
          (representation grh)))

(defmethod depth-first ((grh Graph) start finish)
    (let ((ways nil))
        (labels ((%get-way (current-node way-length &optional visited-nodes)
                    (if (eq current-node finish)
                        (push (list :length way-length :way (reverse (list* current-node visited-nodes))) ways)
                        (let ((possible-moves (remove-if-not (lambda (edge) 
                                                                (and (eq (vertex-1 edge) current-node)
                                                                     (not (member (vertex-2 edge) visited-nodes))))
                                                             (representation grh))))
                            (mapc (lambda (edge) 
                                    (%get-way (vertex-2 edge) (+ way-length (weight edge)) (cons current-node visited-nodes)))
                                   possible-moves)))))
            (when (member finish (member start (nodes grh)))
                (%get-way start 0)
                (car (sort ways #'< :key (lambda (way) (getf way :length))))))))

(defmethod breadth-first ((grh Graph) start finish)
    (let ((ways nil))
        (when (member finish (member start (nodes grh)))
            (labels ((%get-way (current-node way-length &optional visited-nodes)
                        (let ((possible-moves (remove-if-not (lambda (edge) 
                                                                (and (eq (vertex-1 edge) current-node)
                                                                     (not (member (vertex-2 edge) visited-nodes))))
                                                             (representation grh))))
                            
                            (mapc (lambda (edge) 
                                    (%get-way (vertex-2 edge) (+ way-length (weight edge)) (cons current-node visited-nodes)))
                                (mapc (lambda (edge)  
                                        (when (eq (vertex-2 edge) finish)
                                            (push (list :length (+ way-length (weight edge)) 
                                                        :way (reverse (list* current-node (vertex-2 edge) visited-nodes))) ways)))
                                       possible-moves)))))
                (%get-way start 0)
                (car (sort ways #'< :key (lambda (way) (getf way :length))))))))
                                   
(defmethod degree ((grh Graph) node)
    (when (member node (nodes grh))
        (reduce (lambda (degree edge)
                    (+ degree 
                       (if (and (or (eq (vertex-1 edge) node)
                                    (eq (vertex-2 edge) node))
                                (not (does-edge-represent-this-standalone-node edge node)))
                           (if (is-loop edge) 2 1)
                           0)))
                (representation grh) 
                :initial-value 0)))

(defmethod nodes ((grh Graph)) 
    (remove-duplicates 
        (loop for edge in (representation grh) append
            (list (vertex-1 edge) (vertex-2 edge)))))
            
(defmethod edges ((grh Graph))
    (mapcar (lambda (edge) 
                (list (vertex-1 edge) (vertex-2 edge) (weight edge)))
            (representation grh)))

(defmethod nodes-list ((grh Graph))
    (mapcar #'car 
        (sort (mapcar (lambda (node)
                            (list node (degree grh node))) 
                        (nodes grh)) 
              #'> :key #'second)))
            