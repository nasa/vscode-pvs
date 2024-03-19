
(in-package :pvs-websocket)

(defun ws-output-proofstate (ps)
  (format t "~&[ws-output-proofstate] outputting ps: id ~a rule ~a~%" (pvs::ps-display-id ps) (pvs:wish-current-rule ps)) ;; debug
  (let ((sess (pvs::current-session)))
    (if sess
	(when (not pvs::*in-apply*)
	  (pvs::session-output ps))
      (format t "~&[ws-output-proofstate] Warning: No current-session to report ps to~%"))))

(defun websocket-pvs-server (env)
  (pushnew #'pvs-jsonrpc:pvs-message-hook pvs:*pvs-message-hook*)
  ;; (pushnew #'pvs-websocket::ws-output-proofstate pvs:*proofstate-hooks*)
  (pushnew #'pvs-websocket::ws-output-proofstate pvs:*success-proofstate-hooks*)
  (let ((ws (wsd:make-server env))
	(pvs:*pvs-message-hook* #'pvs-jsonrpc:pvs-message-hook))
    (wsd:on :open ws (lambda () (ws-open ws)))
    (wsd:on :message ws (lambda (msg) (ws-message ws msg)))
    (wsd:on :close ws (lambda (&key code reason) (ws-close ws code reason)))
    (wsd:on :error ws (lambda (errmsg) (ws-error ws errmsg)))
    ;;(set-pvs-hooks)
    (setq *ws* ws)
    (lambda (responder)
      (declare (ignore responder))
      (wsd:start-connection ws))))

(in-package :pvs-jsonrpc)

(defun filter-intermediate-proof-states-from-commentary (commentary)
  (when commentary
      (let ((current-proof-state (when (pvs::proofstate? (car commentary)) (car commentary))))
	(if (and current-proof-state
		 (or (pvs::wish-current-rule current-proof-state)
		     (pvs::current-rule current-proof-state)))
	    (let ((result (filter-intermediate-proof-states-from-commentary (append (pvs::x-subgoals current-proof-state) (cdr commentary)))))
	      (pushnew
	       current-proof-state
	       result
	       :key #'pvs::unique-ps-id
	       :test 'string=))
	  (filter-intermediate-proof-states-from-commentary (cdr commentary))))))

(defun read-intermediate-proof-states (commentary id)
  (let ((comntry (mapcar #'(lambda (e)
				(pvs:strim (cond
					    ((stringp e) e)
					    ((pvs::proofstate? e)
					     (format nil "!~a ?~a ~a" (pvs::unique-ps-id e) (pvs::ps-display-id e) e))
					    (t (format nil "~a" e)))))
			 commentary)))
    (let ((intermediate-alists (loop for ps in (filter-intermediate-proof-states-from-commentary commentary)
				     append (pvs2alist-ps ps nil id (pvs::status-flag ps)))))
      (when (and comntry intermediate-alists)
	(setf (car (last intermediate-alists))
	      (acons "commentary" comntry (car (last intermediate-alists)))))
      intermediate-alists)))

;; (defun read-intermediate-proof-states (commentary id)
;;   (when commentary
;;       (let ((current-proof-state (when (pvs::proofstate? (car commentary)) (car commentary))))
;; 	(if current-proof-state
;; 	    (append
;; 	     (pvs2alist-ps current-proof-state nil id (pvs::status-flag current-proof-state))
;; 	     (read-intermediate-proof-states (cdr commentary) id))
;; 	  (read-intermediate-proof-states (cdr commentary) id)))))

;; (defun read-intermediate-proof-states (commentary id &optional prev-proof-state)
;;   (if commentary
;;       (let ((current-proof-state (when (pvs::proofstate? (car commentary)) (car commentary))))
;; 	(if current-proof-state
;; 	    (let ((prev-proof-state-id (when prev-proof-state (pvs::ps-display-id prev-proof-state)))
;; 		  (curr-ps-id (pvs::ps-display-id current-proof-state)))
;; 	      (cond
;; 	       ;; either, the display-id is different
;; 	       ((not (string= curr-ps-id prev-proof-state-id))
;; 		(if prev-proof-state
;; 		    (append (pvs2alist-ps prev-proof-state nil id (pvs::status-flag prev-proof-state))
;; 			    (read-intermediate-proof-states (cdr commentary) id current-proof-state))
;; 		  (read-intermediate-proof-states (cdr commentary) id current-proof-state)))
;; 	       ;; or it has additional information
;; 	       (t
;; 		;; I assume the current ps has more information than the previous one
;; 		;; @M3 do I need to keep the "no action" proof states?
;; 		(read-intermediate-proof-states (cdr commentary) id prev-proof-state))))
;; 	  (read-intermediate-proof-states (cdr commentary) id prev-proof-state)))
;;     (when prev-proof-state
;;       (pvs2alist-ps prev-proof-state nil id (pvs::status-flag prev-proof-state)))))


;; (defun read-intermediate-proof-states (commmentary)
;;   (let ((intermediate-alists
;; 	 (let (prev-proof-state-id)
;; 	   (loop for e in commentary
;; 		 for step = 1 then (1+ step)
;; 		 for has-to-be-added
;; 		 = (and (pvs::proofstate? e)
;; 			;; there could be repeated proof states,if
;; 			;; some of the branches were closed during
;; 			;; the proof command application.
;; 			(let*((curr-ps-id (pvs::ps-display-id e)))
;; 			  (or
;; 			   ;; either, the display-id is different
;; 			   (and (not (string= curr-ps-id prev-proof-state-id))
;; 				(setq prev-proof-state-id curr-ps-id))
;; 			   ;; or it has additional information
;; 			   (let ((e-action
;; 				  (let ((pout (pvs:format-printout e t)))
;; 				    (when pout (pvs:strim pout)))))
;; 			     (and
;; 			      (pvs::wish-current-rule e)
;; 			      (not (pvs::prefix? "No change on:" e-action)))))))
;; 		 do (format t "~&~%{pvs2alist-ps} Step# ~a~%" step)
;; 		 do (if (pvs::proofstate? e)
;; 			(format t "~&{pvs2alist-ps} => ?~a vs. ?~a ~%" (pvs::ps-display-id e) prev-proof-state-id)
;; 		      (format t "~&{pvs2alist-ps} => No ps ~%"))
;; 		 do (when (pvs::proofstate? e)
;; 		      (format t "~&{pvs2alist-ps} Reviewing !~a ?~a ~%" (pvs::unique-ps-id e) (pvs::ps-display-id e))) ;;debug
;; 		 do (format t "~&{pvs2alist-ps} has-to-be-added: ~a ~%" has-to-be-added) ;;debug
;; 		 when has-to-be-added
;; 		 append (pvs2alist-ps e nil id (pvs::status-flag e))
;; 		 else do (format t "~&{pvs2alist-ps} omitting: ~a ~%" (if (pvs::proofstate? e) (pvs::ps-display-id e) e))
;; 		 ;; when (and (pvs::proofstate? e)
;; 		 ;; 	     ;; there could be repeated proof states,if
;; 		 ;; 	     ;; some of the branches were closed during
;; 		 ;; 	     ;; the proof command application.
;; 		 ;; 	     (let((curr-ps-id (pvs::ps-display-id e)))
;; 		 ;; 	       (and (not (string= curr-ps-id prev-proof-state-id))
;; 		 ;; 		    (setq prev-proof-state-id curr-ps-id))))
;; 		 ;; append (pvs2alist-ps e nil id (pvs::status-flag e))
;; 		 ))))
;;     (when (and comntry intermediate-alists)
;;       (setf (car (last intermediate-alists))
;; 	    (acons "commentary" comntry (car (last intermediate-alists)))))
;;     intermediate-alists))

(defun pvs2alist-ps  (ps commentary id status)
  (with-slots (pvs:label pvs:comment pvs:current-goal (pps pvs:parent-proofstate)) ps
    (let* ((action (pvs:strim (pvs:format-printout ps t)))
	   (num-subgoals (pvs:proofstate-num-subgoals ps))
	   (final-sequent (pvs2json-seq pvs:current-goal pps))
	   (curr-cmd (let ((curr-rule (pvs::wish-current-rule ps)))
		       (when curr-rule (format nil "~s" curr-rule))))
	   (prev-cmd (let ((parent-ps (pvs::wish-parent-proofstate ps)))
	   	       (when parent-ps
	   	     	 (let ((parent-rule (pvs:wish-current-rule parent-ps)))
	   	     	   (when parent-rule (format nil "~s" parent-rule))))))
	   (comntry (mapcar #'(lambda (e)
				(pvs:strim (cond
					    ((stringp e) e)
					    ((pvs::proofstate? e)
					     (format nil "{|~a|} ~a" (pvs::ps-display-id e) e))
					    (t (format nil "~a" e)))))
			    commentary))
	   (debug (format t "~&{pvs2alist-ps} |commentary| = ~a~%" (length commentary)))
	   (intermediate-ps (read-intermediate-proof-states commentary id)))
      (or intermediate-ps
	  (list
	   (let((final-ps 
		 `(("id" . ,(string id))
		   ("unique-ps-id" . ,(pvs::unique-ps-id ps))
		   ("display-id" . ,(pvs::ps-display-id ps))
		   ("parent-display-id" . ,(let ((parent-proofstate (pvs::wish-parent-proofstate ps)))
					     (if parent-proofstate
						 (pvs::ps-display-id parent-proofstate)
					       "None")))
	      ("status" . ,(string status))
	      ,@(when comntry `(("commentary" . ,comntry)))
	      ,@(when action `(("action" . ,action)))
	      ,@(when num-subgoals `(("num-subgoals" . ,num-subgoals)))
	      ("label" . ,pvs:label)
	      ,@(when prev-cmd `(("prev-cmd" . ,prev-cmd)))
	      ,@(when curr-cmd `(("curr-cmd" . ,curr-cmd)))
	      ,@(when pvs:comment `(("comment" . ,pvs:comment)))
	      ("path" . ,(format nil "~{~a~^.~}" (pvs:path-from-top ps)))
	      ("sequent" . ,final-sequent)
	      ("children" . ,(loop for child in
				   ;; (append (pvs::done-subgoals ps)
				   ;; 	(pvs::pending-subgoals ps)
				   ;; 	(pvs::remaining-subgoals ps))
				   (pvs::x-subgoals ps)
				   ;; append (pvs2alist-ps child nil id (pvs::status-flag child))
				   collect (pvs::ps-display-id child))))))
		  final-ps))))))

(defun pvs2json-ps (ps commentary id status)
  (arr (pvs2alist-ps ps commentary id status)))

;; @M3 this version returns the whole internal proof tree, but it's too verbose
;;     (compare with the wish proof-tree, for example)
;; BEGIN pvs2json-ps too verbose
;;
;; (defun pvs2json-ps (ps commentary id status)
;;   (arr (proof-state-tree-to-alist ps commentary id status)))
;;
;; (defun proof-state-tree-root (ps)
;;   (let ((parent-ps (pvs:parent-proofstate ps)))
;;     (if parent-ps (proof-state-tree-root parent-ps) ps)))
;;
;; (defun proof-state-tree-preorder-top-down (ps)
;;   (cons ps (loop for child in (pvs::children ps)
;; 		 append (proof-state-tree-preorder-top-down child))))
;;
;; (defun proof-state-tree-to-alist (ps commentary id status)
;;   (let*((comntry (mapcar #'(lambda (e)
;; 			     (pvs:strim (cond
;; 					 ((stringp e) e)
;; 					 ((pvs::proofstate? e)
;; 					  (format nil "{|~a|} ~a" (pvs::unique-ps-id e) e))
;; 					 (t (format nil "~a" e)))))
;; 			 commentary))
;; 	(ps-tree-preorder (proof-state-tree-preorder-top-down (proof-state-tree-root ps)))
;; 	(ps-tree-alist (loop for ps in ps-tree-preorder
;; 			     append (pvs2alist-ps ps nil id (pvs::status-flag ps)))))
;;     (when (and comntry ps-tree-alist)
;;       (setf (car (last ps-tree-alist))
;; 	    (acons "commentary" comntry (car (last ps-tree-alist)))))
;;     ps-tree-alist))
;;
;; (defun pvs2alist-ps  (ps commentary id status)
;;   (with-slots (pvs:label pvs:comment pvs:current-goal (pps pvs:parent-proofstate)) ps
;;     (let* ((action (when pps (pvs:strim (pvs:format-printout pps t))))
;; 	   (num-subgoals (pvs:proofstate-num-subgoals ps))
;; 	   (final-sequent (pvs2json-seq pvs:current-goal pps))
;; 	   (curr-cmd (let ((curr-rule (pvs:wish-current-rule ps)))
;; 		       (when curr-rule (format nil "~s" curr-rule))))
;; 	   (prev-cmd (let ((parent-ps (pvs:parent-proofstate ps)))
;; 		       (when parent-ps
;; 			 (let ((parent-rule (pvs:current-rule parent-ps)))
;; 			   (when parent-rule (format nil "~s" parent-rule))))))
;; 	   (comntry (mapcar #'(lambda (e)
;; 				(pvs:strim (cond
;; 					    ((stringp e) e)
;; 					    ((pvs::proofstate? e)
;; 					     (format nil "{|~a|} ~a" (pvs::unique-ps-id e) e))
;; 					    (t (format nil "~a" e)))))
;; 			    commentary)))
;;       (list (let ((final-ps 
;; 	    `(("id" . ,(string id))
;; 	      ("ps-id" . ,(pvs::unique-ps-id ps))
;; 	      ("parent" . ,(let ((parent-proofstate (pvs:parent-proofstate ps)))
;; 			     (if parent-proofstate
;; 			       (pvs::unique-ps-id parent-proofstate)
;; 			       ;; (pvs2alist-ps parent-proofstate nil id (pvs::status-flag parent-proofstate))
;; 			     "None")))
;; 	      ("status" . ,(string status))
;; 	      ,@(when comntry `(("commentary" . ,comntry)))
;; 	      ,@(when action `(("action" . ,action)))
;; 	      ,@(when num-subgoals `(("num-subgoals" . ,num-subgoals)))
;; 	      ("label" . ,pvs:label)
;; 	      ,@(when prev-cmd `(("prev-cmd" . ,prev-cmd)))
;; 	      ,@(when curr-cmd `(("curr-cmd" . ,curr-cmd)))
;; 	      ,@(when pvs:comment `(("comment" . ,pvs:comment)))
;; 	      ("path" . ,(format nil "~{~a~^.~}" (pvs:path-from-top ps)))
;; 	      ("sequent" . ,final-sequent)
;; 	      ("children" . ,(mapcar #'pvs::unique-ps-id (pvs::children ps)))
;; 	      )))
;; 	      final-ps)))))
;;
;; END pvs2json-ps too verbose

(in-package :pvs)

;; (defun display-proofstate (proofstate)
;;   (format t "~&[display-proofstate] proofstate ~a ~%" (show proofstate)) ;; debug
;;   (when (and (not *in-apply*) *displaying-proof*)
;;     (let ((changed (and *current-displayed*
;; 			(not (eq *current-displayed* proofstate)))))
;;       (when changed
;; 	(if *flush-displayed*
;; 	    (progn
;; 	      (display-proof-from *flush-displayed*)
;; 	      (setf *flush-displayed* nil))
;; 	    (display-proof-from *current-displayed*))
;; 	(display-current proofstate)))))

(defmethod proofstepper ((proofstate proofstate))

;;The key part of the proofstate for the stepper is the strategy field.
;;It indicates which rule to apply to the current goal, how to proceed
;;with the subgoals generated, and how to deal with failures.
;;We have to be careful to ensure that the strategies do not meddle with
;;logical things, i.e., they merely indicate which rules are applied
;;where.  So, it might be better if the strategy merely indicated the
;;top-level rule and the subgoal strategies.
;;a rule-application yields a signal to pop with failure, pop with
;;success, no change (if the rule wasn't applicable), or in the most
;;usual case, a list of subgoals.  To achieve some measure of type
;;correctness, we have a class of rules, and rule application is a
;;method for this class.
;;The defn. below is tentative.  It needs to be cleaned up later.

;;(NSH:4-10-91) I want to use strategies in two ways.  One is as a
;;strategy for applying patterns of rules, and the other is as a rule
;;itself.  The first one is invoked as change-strategy and the second
;;one as apply-strategy.  The second one generates lifts all the pending
;;subgoals back to the current proofstate and thus behaves as a derived
;;rule.  It also won't print out any output.  In replaying a proof, the
;;apply-strategies will be replayed but the change-strategies will not.
;;
  (cond
    ((fresh? proofstate)   ;;new state
     (let ((post-proofstate ;;check if current goal is prop-axiom.
	    (cond ((eq (check-prop-axiom (s-forms (current-goal proofstate)))
		       '!) ;;set flag to proved! and update fields.
		   (setf (status-flag proofstate) '!      
			 (current-rule proofstate) '(propax)
			 (printout proofstate)
			 (format nil "~%which is trivially true.")
			 (justification proofstate)
			 (make-instance 'justification
			   :label (label-suffix (label proofstate))
			   :rule '(propax)))
		   proofstate)	    ;;else display goal, 
		  ;;eval strategy, invoke rule-apply
		  (t (catch-restore ;;in case of restore/enable interrupts
		      (progn
			(when ;;(not (strat-proofstate? proofstate))
			    ;;NSH(8.19.95): display-proofstate called only
			    ;;when current state is root, or has rule/input.
			    ;;in-apply taken care of in display-proofstate. 
			    (and (not (strat-proofstate? proofstate))
				 (or (null (parent-proofstate proofstate))
				     (current-rule (parent-proofstate proofstate))
				     (current-input (parent-proofstate proofstate))))
			  (apply-proofstate-hooks proofstate))
			(let* ((*rule-args-alist* nil)
			       (strategy
				(strat-eval*
				 (if (strategy proofstate)
				     (strategy proofstate)
				     '(postpone t)) ;;if no strat, move on.
				 proofstate))
			       (*proof-strategy-stack*
				(cons strategy *proof-strategy-stack*)))
			  (setf (strategy proofstate) strategy)
			  ;;(run-prover-output-hooks ps)
			  (if (current-session)
			      (multiple-value-bind (result err)
				  (ignore-errors (rule-apply strategy proofstate))
				(if (and err (null result))
				    (progn
				      (session-output err)
				      (throw 'abort nil))
				    result))
			      (rule-apply strategy proofstate)))))))))
       ;;rule-apply returns a revised proofstate.
       (cond ((null post-proofstate) ;;hence aborted
	      (let ((nps	     ;;NSH(7.18.94) for proper restore
		     (nonstrat-parent-proofstate
		      proofstate)))
		(setf (status-flag nps) nil
		      (remaining-subgoals nps) nil
		      (current-subgoal nps) nil
		      (pending-subgoals nps) nil
		      (done-subgoals nps) nil
		      (strategy nps)
		      (query*-step)) ;;unevaluated
		;;(query*-step)
		;;is okay here.
		nps))
	     ((eq (status-flag post-proofstate) '?)	 ;;subgoals created
	      (format-printout post-proofstate)		 ;;print commentary
	      (cond ((> (length (remaining-subgoals post-proofstate)) 1)
		     (when (and *rerunning-proof*
				(integerp *rerunning-proof-message-time*)
				(> (realtime-since *rerunning-proof-message-time*)
				   3000)) ;;print mini-buffer msg
		       (setq *rerunning-proof* (format nil "~a." *rerunning-proof*))
		       (setq *rerunning-proof-message-time*
			     (get-internal-real-time))
		       (pvs-message *rerunning-proof*))
		     (format-if "~%this yields  ~a subgoals: "
				(length (remaining-subgoals post-proofstate))))
		    ((not (typep (car (remaining-subgoals post-proofstate))
				 'strat-proofstate))
		     (format-if "~%this simplifies to: ")))
	      post-proofstate)
	     ((eq (status-flag post-proofstate) '!) ;;rule-apply proved
	      ;; @M3 inform listeners about the closing of the branch [2024]
	      (dolist (hook *success-proofstate-hooks*)
	      	(funcall hook proofstate)) 
	      (format-printout post-proofstate)
	      (wish-done-proof post-proofstate)
	      (dpi-end post-proofstate)
					;		       (when (printout post-proofstate)
					;			 (format-if (printout post-proofstate)))
	      post-proofstate)
	     (t  post-proofstate))))
    ;;if incoming goal has subgoals
    ((eq (status-flag proofstate) '?) ;;working on subgoals
     (cond ((null (remaining-subgoals proofstate))
	    (cond ((null (pending-subgoals proofstate))
		   (success-step proofstate)) ;;no more subgoals,declare success
		  (t			      ;;pending subgoals
		   (post-processing-step proofstate))))
	   (t ;;subgoals remain
	    (let ((newps (pop (remaining-subgoals proofstate))))
	      (setf ;;(parent-proofstate newps) proofstate
	       (current-subgoal proofstate) newps)
	      ;; (substitution newps)
	      ;; (if (null (out-substitution proofstate))
	      ;;     (substitution proofstate)
	      ;;     (out-substitution proofstate))
	      ;; (context newps)
	      ;; (if (null (out-context proofstate))
	      ;;     (context proofstate)
	      ;;     (out-context proofstate))
	      
	      ;; (when (eq (status-flag newps) '*)
	      ;;   (if (null (remaining-subgoals newps));;leaf node
	      ;;       (setf (status-flag newps) nil;;make it fresh
	      ;;             (strategy newps)
	      ;;             (strategy proofstate))
	      ;;             (post-subgoal-strat (strategy proofstate))
	      ;;             nil
	      ;;       (setf (status-flag newps) '?
	      ;;       (strategy newps)
	      ;;       (strategy proofstate))
	      ;;       (post-subgoal-strat (strategy proofstate))
	      ;;       nil
	      ;;   )
	      ;;   (setf (strategy proofstate);;zero out strategy
	      ;;         (if (null (remaining-subgoals proofstate))
	      ;;             nil
	      ;;             (strategy proofstate))))
	      newps))))
   ((eq (status-flag proofstate) '*)  ;;pending goals, but proceeding
    (next-proofstate proofstate))
   ((memq (status-flag proofstate) '(X XX))  ;;failure
    (format-if "~%~%Attempted proof of ~a failed." (label proofstate))
    (next-proofstate proofstate))
   ((eq (status-flag proofstate) '!)  ;;success
    ;;(format t "~%~%Proved ~a." (label proofstate))
    (next-proofstate proofstate))
   (t (next-proofstate proofstate)))) ;;just in case

;; This function provides an unique id on the proof states that
;; should be shown on a graphical display of the proof tree (like
;; the whish printer or the vscode-pvs GUI).
(defun ps-display-id (ps &optional (label (label ps)) (num 0))
  (let ((par-ps (wish-parent-proofstate ps)))
    (if (or (null par-ps) (not (string= (label par-ps) label)))
	(format nil "~a-~d" label num)
	(ps-display-id par-ps label (1+ num)))))

;; debug
(defmethod print-object ((ps proofstate) stream)
  (let* ((*ps* ps)
	 (*print-ancestor* (if *print-ancestor*
			       *print-ancestor*
			       (parent-proofstate *ps*)))
	 (*pp-print-parens* *show-parens-in-proof*))
    (if *debugging-print-object*
	(call-next-method)
	(if (comment ps)
	    (format stream "~%~a (~a) : ~%~a~%~a"
	      (label ps) (unique-ps-id ps)
	      (comment ps)
	      (current-goal ps))
	    (format stream "~%~a (~a) :  ~%~a"  (label ps) (unique-ps-id ps)
		    (current-goal ps))))))
