(in-package :pvs-xml-rpc)

(defun xmlrpc-pvs-request (json-request client-url)
  ;;(format t "~%pvs.request: ~s~%  from ~s~%" json-request client-url)
  (handler-case
      (let* ((json:*json-identifier-name-to-lisp* #'identity)
	     (req-str (bytestring-to-string json-request))
	     (request (json:decode-json-from-string req-str))
	     (id (cdr (assoc :id request :test #'string-equal)))
	     (method (cdr (assoc :method request :test #'string-equal)))
	     (params (cdr (assoc :params request :test #'string-equal)))
	     (*print-pretty* nil))
	;; (setq *last-request* (list method params id client-url))
	;; (apply #'pvs-json:process-json-request pvs-xml-rpc::*last-request*)
	;; (format t "~%xmlrpc-pvs-request: current-process = ~a~%   request = ~a~%"
	;;   mp:*current-process* request)
	(let* ((result (pvs-json:process-json-request method params id client-url))
	       (jresult (xmlrpc-result result id)))
	  ;; (format t "~%xmlrpc-pvs-request end: current-process = ~a~%   result = ~s~%"
	  ;;   mp:*current-process* jresult)
	  jresult))
    ;; Note: id will not be available if this error is hit
    (error (c) (xmlrpc-error (format nil "~a" c)))))

(defun bytestring-to-string (str)
  (let ((octets (map '(simple-array (unsigned-byte 8)) #'char-code str)))
    (excl:octets-to-string octets)))
