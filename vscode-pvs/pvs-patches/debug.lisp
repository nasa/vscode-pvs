;; BEGIN debug

;; (defmethod print-object ((ps proofstate) stream)
;;   (let* ((*ps* ps)
;; 	 (*print-ancestor* (if *print-ancestor*
;; 			       *print-ancestor*
;; 			       (parent-proofstate *ps*)))
;; 	 (*pp-print-parens* *show-parens-in-proof*))
;;     (if *debugging-print-object*
;; 	(call-next-method)
;; 	(if (comment ps)
;; 	    (format stream "~%~a (~a) : ~%~a~%~a"
;; 	      (label ps) (unique-ps-id ps)
;; 	      (comment ps)
;; 	      (current-goal ps))
;; 	    (format stream "~%~a (~a) :  ~%~a"  (label ps) (unique-ps-id ps)
;; 		    (current-goal ps))))))

;; (in-package :pvs-jsonrpc)
;; (defun pvs2json-response (prv-result)
;;   "prv-result is an alist of the form
;;  ((\"proofstate\" ...) (\"commentary\" ...) (\"id\" ...) (\"status\" ...))"
;;   (let* ((id (cdr (assoc "id" prv-result :test #'string-equal)))
;; 	 (ps (cdr (assoc "proofstate" prv-result :test #'string-equal)))
;; 	 (err (cdr (assoc "error" prv-result :test #'string-equal)))
;; 	 (errstr (when err (concatenate 'string "Error: " (strim-all err))))
;; 	 (com (cdr (assoc "commentary" prv-result :test #'string-equal)))
;; 	 (commentary (if err (cons errstr com) com))
;; 	 (status (cdr (assoc "status" prv-result :test #'string-equal)))
;; 	 (ps-json (pvs2json-ps ps commentary id status)))
;;     (format t "~&{pvs2json-response} ps-json = ~s~%" ps-json)
;;     ps-json))

;; END debug
