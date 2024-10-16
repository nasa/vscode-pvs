(format t "~&~%Loading VSCode-PVS patch '~a'...~%~%" #.(or *compile-file-truename* *load-truename*))

(in-package :pvs)

(defun strat-eval (strat)
  ;; (commentary "~&--> ~a~%" strat)
  (handler-case
      (cond ((typep strat 'strategy) strat)
	((null strat) (get-rule '(skip) *ps*))
	((not (consp strat))
	 (error-format-if "~%Ill-formed rule/strategy, substituting (skip):~%  ~a" strat)
	 (get-rule '(skip) *ps*))
	((quote? strat)(strat-eval (cadr strat)))
	((if-form? strat) ;;(break "strat-eval: if")
	 (if (expr-eval (cadr strat))
	     (strat-eval (caddr strat))
	     (strat-eval (cadddr strat))))
	((try-form? strat)
	 (make-instance 'strategy
	   :topstep (strat-eval (cond-expr strat))
	   :subgoal-strategy (then-expr strat)
	   :failure-strategy (else-expr strat)))
	((let-form? strat)
	 (let ((let-value (let-eval (let-bindings strat))))
	   (strat-eval (subst-stratexpr
			(let-body strat)
			let-value
			(reverse let-value)))))
	((rule-definition (car strat))
	 (let* ((def (rule-definition (car strat)))
		(subalist (let ((*suppress-printing* nil))
			    (pair-formals-args (formals def)
					       (cdr strat)
					       (car strat))))
		(args (loop for x in (formals def)
			    when (not (memq x '(&optional &rest)))
			    collect
			    (if (consp x)
				;;was ignoring args, otherwise.
				(cdr (assoc (car x) subalist))
				(cdr (assoc x subalist)))))
		(def-expr  (subst-stratexpr
			    (defn def)
			    subalist
			    (reverse subalist)))
		(new-def-expr ;;2/91:so that rules are APPLYed.
		 `(apply ,def-expr))
		(result (strat-eval new-def-expr)))
	   (setq *rule-args-alist*
		 (collect-prover-input-strings args *rule-args-alist*))
	   (setf (rule-input result)
		 strat
		 (rule-format result)
		 (when (format-string (rule-definition (car strat)))
		   (cons (format-string (rule-definition (car strat))) args)
		   )) 
	   result))
	((step-definition (car strat))
	 (let* ((def (step-definition (car strat)))
		(alist (let ((*suppress-printing* nil))
			 (pair-formals-args (formals def)
					    (cdr strat)
					    (car strat))))
		(def-expr  (subst-stratexpr
			    (defn def)
			    alist
			    (reverse alist)
			    )))
	   (strat-eval def-expr)))
	((primitive-rule (car strat))
	 (get-rule strat *ps*))
	(t (error-format-if "~%Ill-formed rule or strategy, substituting (skip):~%  ~a" strat)
	   (get-rule '(skip) *ps*)))
    (error (c)
	   ;; (break)
	   (let ((subst-rule (get-rule '(query*) *ps*)))
	     (commentary (format nil "~%Ill-formed rule or strategy (error: '~a'), applying ~a instead~%" (substitute #\Space #\Newline (format nil "~a" c)) (rule-input subst-rule)))
	     subst-rule))))

(format t "~&~%VSCode-PVS patch '~a' LOADED~%~%" #.(or *compile-file-truename* *load-truename*))
