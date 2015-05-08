
 (eval-when (:execute)
    (ql:quickload :cl-ppcre))

;; core 
    
(let ((unique-id 0))
(defclass PhoneRecord () 
    ((id 
      :reader id 
      :initform (incf unique-id))
     (name 
      :accessor name 
      :initarg :name 
      :initform (error "Must supply a record name."))
     (surname 
      :accessor surname 
      :initarg :surname 
      :initform "")
     (patronymic 
      :accessor patronymic 
      :initarg :patronymic 
      :initform "")
     (phone-number 
      :accessor phone-number 
      :initarg :phone-number
      :initform (error "Must supply a record name"))
     (address 
      :accessor address 
      :initarg :address 
      :initform ""))))
     
(defmethod print-object ((object PhoneRecord) stream)
    (with-slots (id name surname patronymic phone-number address) object
        (format stream "~%id : ~a~%NSP : ~a ~a ~a~%Number : ~a~%Address : ~a~%~%" 
                id name surname patronymic phone-number address)))

(defgeneric add-new-contact (contact))
                
(let ((database nil))

(defun get-all-contacts () database)

(defun get-contacts-by-field-mask (mask field-accessor) 
    (declare (string mask))
    (let ((regex (concatenate 'string "^" (cl-ppcre:regex-replace-all "[*]" mask "\\w*") "$")))
        (remove-if-not (lambda (contact) 
                          (second (multiple-value-list 
                                    (cl-ppcre:regex-replace regex (funcall field-accessor contact) ""))))
                        database)))

(defun find-by-field (field-value field-accessor &key (test #'eq))
    (find field-value database :key field-accessor :test test))

(defun delete-contact (contact-id-to-remove) 
    (format t "~%in delete-contact :~a ~%" contact-id-to-remove)
    (setq database (remove-if (lambda (contact) 
                                (eq contact-id-to-remove (id contact)))
                              database)))

(defmethod add-new-contact ((contact PhoneRecord)) 
    (push contact database))    
    
(defun save-to-file (file) 
    (with-open-file (fstream file :direction :output :if-exists :supersede :if-does-not-exist :create)
        (mapc (lambda (record) 
                (with-slots (name surname patronymic phone-number address) record 
                    (format fstream "~a;~a;~a;~a;~a~%" name surname patronymic phone-number address)))
               database)))

(defun load-from-file (file) 
    (with-open-file (fstream file :direction :input :if-does-not-exist :create)
        (do ((line (read-line fstream nil) (read-line fstream nil)))
                ((null line))
            (let ((splited-line (cl-ppcre:split ";" line)))
                (add-new-contact
                    (list
                        :name (first splited-line)
                        :surname (second splited-line)
                        :patronymic (third splited-line)
                        :phone-number (fourth splited-line)
                        :address (fifth splited-line)))))))       

(defmethod add-new-contact ((contact list)) 
    (add-new-contact (make-instance 'PhoneRecord 
                                    :name (getf contact :name)
                                    :surname (getf contact :surname)
                                    :patronymic (getf contact :patronymic)
                                    :phone-number (getf contact :phone-number)
                                    :address (getf contact :address))))

(defun get-contacts-by-name-mask (name-mask) 
    (get-contacts-by-field-mask name-mask #'name))

                        
(defun find-by-name (name) 
    (find-by-field name #'name :test #'string-equal))
 
(defun find-by-phone (phone)
    (find-by-field phone #'phone-number :test #'string-equal)) 

(defun clean-db () (setq database nil))
    
)

;; view 

(defun list-contacts (contacts &key (stream *standard-output*))
    (mapc (lambda (contact) (format stream "~a" contact)) contacts))

(defun prompt-read (prompt) 
    (format *query-io* "~a: " prompt)
    (force-output *query-io*)
    (read-line *query-io*))
    
(defun menu () 
    (flet ((%show-menu () 
            (format t "1.List all~%")
            (format t "2.Add new contact~%")
            (format t "3.Delete contact~%")
            (format t "4.List contacts by name mask~%")
            (format t "5.Find by name~%")
            (format t "6.Find by phone~%")
            (format t "7.Exit~%")))
        (let ((db-file "db.csv"))    
            (load-from-file db-file)
            (do ((exit-flag nil)
                 (input nil))
                    (exit-flag)
                (%show-menu) 
                (setq input (read-line))
                (cond 
                    ((or (string-equal "1" input)
                         (string-equal "List all" input)) 
                       (list-contacts (get-all-contacts))
                       (read-line))
                    ((or (string-equal "2" input)
                         (string-equal "Add new contact" input))
                        (add-new-contact 
                          (list :name (prompt-read "Name")
                                :surname (prompt-read "Surname")
                                :patronymic (prompt-read "Patronymic")
                                :phone-number (prompt-read "Phone Number")
                                :address (prompt-read "Address"))))
                    ((or (string-equal "3" input) 
                         (string-equal "Delete contact" input)) 
                        (delete-contact (parse-integer (prompt-read "id") :junk-allowed t)))
                    ((or (string-equal "4" input) 
                         (string-equal "List contacts by name mask" input))
                        (list-contacts (get-contacts-by-name-mask (prompt-read "Mask")))
                        (read-line)) 
                    ((or (string-equal "5" input) 
                         (string-equal "Find by name" input))
                        (list-contacts (list (find-by-name (prompt-read "Name"))))
                        (read-line))
                    ((or (string-equal "6" input) 
                         (string-equal "Find by phone" input))
                        (list-contacts (list (find-by-phone (prompt-read "Phone"))))
                        (read-line))
                    ((or (string-equal "7" input) 
                         (string-equal "Exit" input))
                       (setq exit-flag t))
                    (t (format t "invalid input! try again!~%"))))
            
            (save-to-file db-file)
            nil))) 

