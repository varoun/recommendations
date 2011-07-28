;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :groklogs-preprocessor)

;;; The stage 1 preprocessor looks to map customer provided IDs for users and items to internal
;;; IDS used in later processing stages. We start numbering IDs from 0.

;;; Mapping user/item IDs to the forms used internally.
(defun make-updater (db-spec table-name element-type)
  (let ((count 0))
    #'(lambda (element)
	(with-database (db-object db-spec :if-exists :new)
	  (let ((curr-val 
		 (first 
		  (query 
		   (format nil
			   "select ~a from ~a where ~a = ~a"
			   (concatenate 'string element-type "_new")
			   table-name
			   (concatenate 'string element-type "_old")
			   element)
		   :flatp t
		   :database db-object))))
	    (if curr-val
		curr-val
		(prog1
		    count
		  (execute-command (format nil
					   "insert into ~a values (~a, ~a)"
					   table-name element count)
				   :database db-object)
		  (incf count))))))))

(defun update-normalised-links (uid iid table-name db-spec)
  (with-database (db-object db-spec :if-exists :new)
    (execute-command (format nil "insert into ~a values (~a, ~a)"
				   table-name uid iid)
		     :database db-object)))

(defun map-ids (&optional (db *database-spec*)
		(primary-table *primary-links-table*)
		(normalised-table *normalised-links-table*)
		(user-map *uid-map-table*)
		(item-map *iid-map-table*))
  #.(locally-enable-sql-reader-syntax)
  (let ((update-uids (make-updater db user-map "userid"))
	(update-iids (make-updater db item-map "itemid")))
    (with-database (*default-database* *database-spec* :if-exists :new)
      (do-query ((uid iid)
		 [select [*] :from primary-table])
	(let ((new-uid (funcall update-uids uid))
	      (new-iid (funcall update-iids iid)))
	  (update-normalised-links new-uid new-iid normalised-table db)))))
  #.(disable-sql-reader-syntax))
