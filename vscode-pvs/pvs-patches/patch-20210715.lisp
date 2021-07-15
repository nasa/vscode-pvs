;;; utils
(defmethod safe-make-specpath ((name string) &optional (ext "pvs"))
  (if (and (> (length name) (1+ (length ext)))
	   (string= (subseq name (- (length name) (1+ (length ext))))
		    (format nil ".~a" ext)))
      ;; Ends in, e.g., ".pvs", just return it as a path.
      (pathname name)
      (let* ((path (parse-namestring name))
	     (pname (pathname-name path))
	     (dir (pathname-directory path)))
	(if dir
	    (let* ((pdir (namestring (make-pathname :directory dir)))
		   (lib-path (get-library-path pdir)))
	      (if lib-path
		  (make-pathname :directory (pathname-directory lib-path)
				 :name pname :type ext)
		(pvs-output "Library reference error - Could not find lib-path for ~a" pdir)))
	    (make-pathname :defaults *default-pathname-defaults* :name name :type ext)))))

;;;
(defun restore-context ()
  (assert *workspace-session*)
  #+pvsdebug
  (assert (and (file-equal *default-pathname-defaults*
			   (working-directory))
	       (file-equal *default-pathname-defaults*
			   (current-context-path))))
  (unless (current-pvs-context)
    (let ((ctx-file (merge-pathnames *context-name*)))
      (if (file-exists-p ctx-file)
	  (handler-case
	      (let ((context
		     (if (with-open-file (in ctx-file)
			   (and (char= (read-char in) #\()
				(char= (read-char in) #\")))
			 (with-open-file (in ctx-file) (read in))
			 (fetch-object-from-file ctx-file))))
		(setf (cdddr context)
		      (remove-duplicates (cdddr context) :key #'ce-file :test #'equal
					 :from-end t))
		(setf (cdddr context)
		      (delete-if-not #'(lambda (ce)
					 (file-exists-p (safe-make-specpath (ce-file ce))))
			(cdddr context)))
		(dolist (ce (cdddr context))
		  (let ((ndeps (remove-if-not
				#'(lambda (dep)
				    (let ((spec-path (safe-make-specpath dep)))
				      (and spec-path (file-exists-p spec-path))))
				 (ce-dependencies ce))))
		    (unless (equal ndeps (ce-dependencies ce))
		      (pvs-message "PVS context has bad deps: ~a"
			(remove-if #'file-exists-p (ce-dependencies ce)))
		      (setf (ce-dependencies ce) nil)
		      (setf (ce-object-date ce) nil)
		      (setf (ce-theories ce) nil))))
		(cond ((duplicate-theory-entries?)
		       (pvs-message "PVS context has duplicate entries - resetting")
		       (setf (pvs-context *workspace-session*) (list *pvs-version*))
		       (write-context))
		      (t ;;(same-major-version-number (car context) *pvs-version*)
		       ;; Hopefully we are backward compatible between versions
		       ;; 3 and 4.
		       (assert (every #'(lambda (ce)
					  (file-exists-p (safe-make-specpath (ce-file ce))))
				      (pvs-context-entries context)))
		       (setf (pvs-context *workspace-session*) context)
		       (assert (not (duplicates? (pvs-context-entries) :key #'ce-file)))
		       (setf (cadr (current-pvs-context))
			     (delete "PVSio/"
				     (delete "Manip/"
					     (delete "Field/" (cadr (current-pvs-context))
						     :test #'string=)
					     :test #'string=)
				     :test #'string=))
		       (cond ((and (listp (cadr context))
				   (listp (caddr context))
				   (every #'context-entry-p (cdddr context)))
			      (load-prelude-libraries (cadr context))
			      (setq *default-decision-procedure*
				    (or (when (listp (caddr context))
					  (getf (caddr context)
						:default-decision-procedure))
					'shostak))
			      (dolist (ce (cdddr context))
				(unless (listp (ce-object-date ce))
				  (setf (ce-object-date ce) nil))))
			     ((every #'context-entry-p (cdr context))
			      (setf (pvs-context *workspace-session*)
				    (cons (car (current-pvs-context))
					  (cons nil
						(cons nil (cdr (current-pvs-context)))))))
			     (t (pvs-message "PVS context is not quite right ~
                                      - resetting")
				(setf (pvs-context *workspace-session*)
				      (list *pvs-version*))))))
		(assert (not (duplicates? (pvs-context-entries) :key #'ce-file))))
	    (file-error (err)
	      (pvs-message "PVS context problem - resetting")
	      (pvs-log "  ~a" err)
	      (setf (pvs-context *workspace-session*) (list *pvs-version*))
	      (write-context)))
	  (setf (pvs-context *workspace-session*) (list *pvs-version*))))
    nil))
