;; fix for context.lisp
(defun read-proof-file-stream (input &optional proofs)
  (let ((nproof (read-proof input 'eof)))
    (if (eq nproof 'eof)
	(nreverse proofs)
      (if (not (listp nproof))
	  (error "Corrupt prf file")
	(let ((cproof (convert-proof-if-needed nproof)))
	  (read-proof-file-stream
	   input
	   (if (member cproof proofs :test #'equal)
	       proofs
	     (cons cproof proofs))))))))
