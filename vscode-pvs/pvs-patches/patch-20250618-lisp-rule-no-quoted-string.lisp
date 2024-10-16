(format t "~&~%Loading VSCode-PVS patch '~a'...~%~%" #.(or *compile-file-truename* *load-truename*))

(in-package :pvs)
(defun lisp-rule (lexp)
  #'(lambda (ps)
      (declare (ignore ps))
      (let ((*suppress-printing* nil))
	(let ((result (eval lexp)))
	  (format-if (if (stringp result) "~%~a~%" "~%~s~%") result)))
      (values 'X nil nil)))

(format t "~&~%VSCode-PVS patch '~a' LOADED~%~%" #.(or *compile-file-truename* *load-truename*))
