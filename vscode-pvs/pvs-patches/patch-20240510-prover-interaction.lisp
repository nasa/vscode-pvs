(in-package :pvs-websocket)

(defun ws-output-proofstate (ps)
  ;; (format t "~&[ws-output-proofstate] outputting ps: id ~a rule ~a~%" (pvs::ps-display-id ps) (pvs:wish-current-rule ps)) ;; debug
  (let ((sess (pvs::current-session)))
    (if sess
	(when (not pvs::*in-apply*)
	  (pvs::session-output ps))
      (format t "~&[ws-output-proofstate] Warning: No current-session to report ps to~%"))))

(defun websocket-pvs-server (env)
  (pushnew #'pvs-jsonrpc:pvs-message-hook pvs:*pvs-message-hook*)
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
		     (pvs::current-rule current-proof-state)
		     (null (cdr commentary))))
	    (let ((result (filter-intermediate-proof-states-from-commentary
			   (append (pvs::x-subgoals current-proof-state) (cdr commentary)))))
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
					     (format nil "~a" e))
					    (t (format nil "~a" e)))))
			 commentary)))
    (let*((intermediate-ps (filter-intermediate-proof-states-from-commentary commentary))
	  ;; need to add the parent of the first reported proof state (the ps on
	  ;; which the first proof command has been applied) when not reporting
	  ;; 'done' and first proof state has no children @M3
	  (intermediate-alists (when intermediate-ps
				 (loop for ps in (let*((first-ps (car intermediate-ps))
						       (parent-of-first-ps
							(unless (and (string= (pvs::status-flag first-ps) "!")
								     (null (pvs::x-subgoals first-ps)))
							  (pvs:parent-proofstate first-ps))))
						   (if parent-of-first-ps
						       (cons parent-of-first-ps intermediate-ps)
						     intermediate-ps))
				     append (pvs2alist-ps ps nil id (pvs::status-flag ps))))))
      (when (and comntry intermediate-alists)
	(setf (car (last intermediate-alists))
	      (acons "commentary" comntry (car (last intermediate-alists)))))
      intermediate-alists)))

(defun pvs2alist-ps  (ps commentary id status)
  (with-slots (pvs:label pvs:comment pvs:current-goal (pps pvs:parent-proofstate)) ps
    (let* ((action (pvs:strim (pvs:format-printout ps t)))
	   (num-subgoals (pvs:proofstate-num-subgoals ps))
	   (final-sequent (pvs2json-seq pvs:current-goal pps))
	   (curr-cmd (let ((curr-rule (pvs::wish-current-rule ps)))
		       (when curr-rule (remove #\Newline (format nil "~s" curr-rule)))))
	   (prev-cmd (let ((parent-ps (pvs::wish-parent-proofstate ps)))
	   	       (when parent-ps
	   	     	 (let ((parent-rule (pvs:wish-current-rule parent-ps)))
	   	     	   (when parent-rule (remove #\Newline (format nil "~s" parent-rule)))))))
	   (comntry (mapcar #'(lambda (e)
	   			(pvs:strim (cond
	   				    ((stringp e) e)
	   				    (t (format nil "~a" e)))))
	   		    commentary))
	   ;; (debug (format t "{pvs2alist-ps} all comntry (~a)~%" (length comntry))) ;; debug
	   ;; (debug (format t "~{~&{pvs2alist-ps} ~a ~}~%" comntry)) ;; debug
	   (comntry (let*((prev-com)
			  (length-commentary-1 (1- (length commentary))))
		      (loop for com in commentary
			    for i from 0 to length-commentary-1
			    if (stringp com)
			    collect com
			    else if (or (not (and (= i length-commentary-1) (pvs::proofstate? com)))
				     (not
				      (and
				       (pvs::proofstate? prev-com)
				       (pvs::proofstate? com)
				       (string= (pvs::ps-display-id prev-com) (pvs::ps-display-id com)))))
			    collect com
			    do (setq prev-com com))))
	   ;; (debug (format t "{pvs2alist-ps} filtered comntry (~a)~%" (length comntry))) ;; debug
	   ;; (debug (format t "~{~&{pvs2alist-ps} ~a ~}~%" comntry)) ;; debug	   
	   (intermediate-ps (when commentary
			      (read-intermediate-proof-states
			       (if (string= status "proved")
				   ;; when status is 'proved', pvs returns the top-proofstate, adding it
				   ;; to commentary would provoke to send all the proofstates in the proof
				   ;; to the client.
				   commentary
				 (append commentary (list ps)))
			       id)))
	   ;; (debug (format t "{pvs2alist-ps} filtered comntry 2 (~a)~%" (length comntry))) ;; debug
	   ;; (debug (format t "~{~&{pvs2alist-ps} ~a ~}~%" comntry)) ;; debug	   
	   )
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
				   (pvs::x-subgoals ps)
				   collect (pvs::ps-display-id child))))))
		  final-ps))))))

(defun pvs2json-ps (ps commentary id status)
  (arr (pvs2alist-ps ps commentary id status)))

(in-package :pvs)
;; The only difference w.r.t. the current version of proofstepper is tagged [2024]
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


(in-package :pvs)
;; CHANGE: Add message on undoing proof command @M3
(defun undo-proof (info ps)
  (if (eq info 'undo)
      (cond ((and *record-undone-proofstate*
		  (eq ps (car *record-undone-proofstate*)))
	     (let ((oldps (cadr *record-undone-proofstate*))
		   (newps (caddr *record-undone-proofstate*)))
	       (commentary "~%Restoring the proof to state prior to UNDO, ")
	       (setf (justification ps)
		     (justification oldps)
		     (status-flag ps) (status-flag oldps)
		     (remaining-subgoals ps) (remaining-subgoals oldps)
		     (pending-subgoals ps) (pending-subgoals oldps)
		     (done-subgoals ps) (done-subgoals oldps)
		     (current-subgoal ps) (current-subgoal oldps)
		     (current-rule ps) (current-rule oldps)
		     (current-xrule ps) (current-xrule oldps)
		     (printout ps) (printout oldps)
		     (strategy ps) (strategy oldps))
	       (setf (strategy newps)(query*-step))
	       newps))
	    (t (if *record-undone-proofstate*
		   (commentary "~%Undo operations must be immediately undone.")
		   (commentary "~%No undo to undo."))
	       (setf (strategy ps) (query*-step));;(NSH:5/8/99)
	       ps))
      (let ((newps (findps info ps)))
	(cond ((null newps)
	       (commentary "~%Sorry. Couldn't find such a proof state.")
	       (setf (strategy ps) (query*-step))
	       ps)
	      ((eq ps newps)
	       (commentary "~%No change.")
	       (setf (strategy ps)(query*-step))
	       ps)
	      (t (let ((response
			(or (eq *multiple-proof-default-behavior* :noquestions)
			    (pvs-y-or-n-p "~%This will undo the proof to: ~a~%Sure? "
					  newps))))
		   (cond (response
			  (commentary "~&Undoing last proof command application,~%this simplifies to:") ;; [2024] @M3
			  (when *displaying-proof*
			    (setf *flush-displayed* newps))
			  (setq *record-undone-proofstate*
				(list newps (copy newps) ps))
			  (setf (justification newps)
				(collect-justification newps)
				(status-flag newps) nil
				(remaining-subgoals newps) nil
				(pending-subgoals newps) nil
				(done-subgoals newps) nil
				(current-subgoal newps) nil
				(dependent-decls newps) nil;;NSH(12.14.94)
				;;d-d was commented, now uncommented(4.9.99)
				(current-rule newps) nil
				(current-xrule newps) nil
				(printout newps) nil
				(strategy newps) (query*-step))
			  newps)
			 (t (setf (strategy ps) (query*-step))
			    ps))))))))
