(in-package :pvs)

;; Modification to ensure each log message starts in a new line and print also date of timestamp
(defun pvs-log (ctl &rest args)
  (multiple-value-bind (sec min hour date month year)
      (decode-universal-time (get-universal-time))
    ;; First time will set the log file and open it for writing
    (unless *pvs-log-stream*
      (let ((fname (format nil "~apvs-~4,'0d-~2,'0d-~2,'0d.log"
			   *pvs-log-directory* year month date)))
	(ensure-directories-exist *pvs-log-directory*)
	(setq *pvs-log-stream*
	      (open (uiop:native-namestring fname) :direction :output
		    :if-exists :append :if-does-not-exist :create))))
    (format *pvs-log-stream* "~&[~2,'0d/~2,'0d/~2,'0d ~2,'0d:~2,'0d:~2,'0d] ~?" year month date hour min sec ctl args)
    (finish-output *pvs-log-stream*)))

(pvs-log "vscode-pvs patch ~a loaded" #.(or *compile-file-truename* *load-truename*))
