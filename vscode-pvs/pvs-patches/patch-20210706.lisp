(in-package :pvs)

(defmethod check-assignment-arg-types* (args-list values ex expr (expected recordtype))
  (assert (or *in-checker* *in-evaluator* (null ex) (place ex)))
  (assert (typep expr '(or record-expr update-expr))) ;; these are the only terms with assignments;
  (with-slots (fields) expected
    (if (every #'null args-list)
	(call-next-method)
 	(progn
	  (mapc #'(lambda (a v)
		    (unless a (check-for-tccs* v expected)))
		args-list values)
	  (multiple-value-bind (cargs-list cvalues)
	      (complete-assignments args-list values ex expr expected)
	    (check-assignment-rec-arg-types cargs-list cvalues ex fields expr))))))

(defmethod complete-assignments (args-list values ex expr (rtype recordtype))
  (assert (or  *in-checker* *in-evaluator* (null ex) (place ex)))
  ;; Used to check for dependent?, but we really need all assignments if
  ;; we're going to generate correct TCCs.
  ;; Since field-decls don't point to their associated recordtypes
  ;; (they can't, since they may appear in several different ones due
  ;; to copying and dependent substitutions), we create the
  ;; association list in *field-records*.
  (if (dependent? rtype)
      (let ((*field-records* (mapcar #'(lambda (fld) (cons fld rtype))
			       (fields rtype))))
	(complete-rec-assignments args-list values (fields rtype) ex expr nil nil))
      (values args-list values)))

(defun complete-rec-assignments (args-list values fields ex expr cargs cvalues)
  (assert (or *in-checker* *in-evaluator* (null ex) (place ex)))
  (if (null fields)
      (values (append args-list (nreverse cargs))
              (append values (nreverse cvalues)))
      (let* ((pos (position (car fields) args-list
			    :test #'same-id :key #'caar))
	     (args (if pos
		       cargs
		       (let ((fldass (make-instance 'field-assign
				       :id (id (car fields)))))
			 (set-extended-place fldass expr
					     "completing field assignment for ~a"
					     (id (car fields)))
			 (cons (list (list fldass)) cargs))))
	     (vals (if pos
		       cvalues
		       (let ((fappl (make!-field-application (car fields) ex)))
			 (assert (place ex))
			 (set-extended-place fappl ex
					     "completing field application for ~a"
					     fappl)
			 (cons fappl cvalues)))))
        (complete-rec-assignments args-list values (cdr fields) ex expr args vals))))
