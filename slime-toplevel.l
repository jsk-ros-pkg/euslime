;;;;;;;;;;;;;;;;;;;;;;
;; Toplevel Overwrites
;;;;;;;;;;;;;;;;;;;;;;

(in-package "LISP")

(eval-when (compile)
  (unless (find-package "SLIME") (make-package "SLIME")))

(eval-when (load eval)

;; Recursive load tags
(defun load (fname &rest args)
  (when slime::*load-hooks*
    (dolist (fn slime::*load-hooks*)
      (funcall fn fname)))
  (apply slime::load-org-for-slime fname args))

;; Handle unix sub processes
(defun unix:system (&optional command)
  (slime::socket-request slime::*slime-stream* "read-mode" t)
  (funcall slime::*old-unix-system-fn* command))

;; Remap old *standard-input*
;; don't compile to override the reploop reference when exec command is null
(setf (symbol-function 'piped-fork)
      '(lambda-closure piped-fork 0 0 (&optional (exec) &rest args)
        (flet ((reploop (&rest reploop-args)
                 (apply slime::*old-reploop-fn* reploop-args)))
          (let ((*standard-input* (send slime::*old-terminal-io* :instream))
                (*use-top-selector* nil))
            (apply slime::*old-piped-fork-fn* exec args)))))


;; Recompile with new unix:system
(defun evaluate-stream (input)
  (let* ((eof (cons nil nil))
	 (command (read input nil eof))
	 (arglist) (arg) result)
    (cond	((eq command eof) )
                ((symbolp command)
                 ;;	 (if *history* (add-history (input . buffer)))
                 (cond ((fboundp command)
                        (setq arglist nil)
                        (while (not (eq (setq arg (read input nil eof)) eof))
                          (push arg arglist))
                        (setq - (cons command (nreverse arglist)))
                        (setq result (eval -)))
                       ((and (boundp command)
                             (eq (read input nil eof) eof))
                        (setq - command)
                        (setq result (eval command)))
                       ((find-package (string command)) (in-package (string command)))
                       (*try-unix*
                        (setq - (list 'unix:system (input . buffer)))
                        (setq result (unix:system (input . buffer)) ) )
                       (t (warn "?~%")) ))
                (t
                 ;;	 (if *history* (add-history (input . buffer)))
                 (setq - command)
                 (setq result (eval command)) ))
    result))

;; Recompile with new evaluate-stream
(defun rep1 (repstream eof local-bindings &optional (ttyp t))
  (let ((input (read-list-from-line repstream eof)) result)
    (if (eq input eof) (return-from rep1 eof))
    (when (and input (or (not (streamp input))
                         (> (length (send input :buffer)) 0)))
      (when *history*
        (add-history 
         (cond ((consp input) (format nil "~s" input))
               ((streamp input) (send input :buffer))
               (t (string input)))) )
      ;; if something is going to be put in the history buffer,
      ;; it certainly has some value to be processed by the hook.
      (if *toplevel-hook* (funcall *toplevel-hook*))
      )
    (cond
      ((null input) nil)
      ((symbolp input)
       ;;	 (if *history*	 (add-history (string input)))
       (setq - input
             result
             (cond
               ((> *replevel* 0)
                (eval-dynamic input local-bindings))
               ((boundp input) (eval input))
               (t '*unbound*)))
       ;;	 (if ttyp (print result repstream))
       (print result repstream)
       )
      ((or (null (streamp input)) (listp input))
       ;;	 (if *history* (add-history (format nil "~s" input)))
       (setq - input)
       (setq result (eval input))
       ;;(if ttyp (print result  repstream)))
       (print result  repstream))
      ((streamp input)
       (setq result (evaluate-stream input) )
       ;;(if ttyp  (print result repstream ))
       (print result repstream ))
      (t (print "?" repstream)))
    (setq +++ ++ ++ + + -)
    (setq *** ** ** * * result)))

(defun toplevel-prompt (strm)
  ;; Do not print the history number and the '$' sign in the end
  (if (> *replevel* 0)
      (format strm "~A~D-" *reptype* *replevel*))
  (if (not (eql *package* *user-package*))
      (format strm "~A:" (package-name *package*)))
  (princ *prompt-string* strm))

(defun repsel (repstream eof ttyp local-bindings)
  ;; Do not print the evaluation result to *standard-output*
  ;; Instead, redirect it to *slime-stream*
  (let* ((out (send repstream :outstream))
         (repstream (make-two-way-stream
                     (send repstream :instream)
                     (make-string-output-stream)))
         (result (rep1 repstream eof local-bindings ttyp)))
    (if (eql result eof) (throw :reploop-select nil))
    (slime::slime-clear-stream repstream)
    (slime::slime-finish-output out)
    (slime::socket-request slime::*slime-stream* "result" result)))

(defun reploop (prompt &optional (repstream slime::*old-terminal-io*) (ttyp nil))
  (let ((*prompt* prompt))
    (slime::slime-clear-stream repstream)
    (slime::slime-finish-output repstream)
    (slime::socket-request slime::*slime-stream* "abort" nil)
    (send *top-selector* :add-port slime::*slime-stream* #'slime::socket-eval slime::*slime-stream*)
    (reploop-select repstream ttyp)))
)

;;;;;;;;;;;;;
;; Setup REPL
;;;;;;;;;;;;;

(eval-when (load eval)

  ;; Set signal-handler and *history* for non-tty streams
  (unless (unix:isatty *standard-input*)
    (unix:signal unix::sigint 'slime::slime-sigint-handler 2)
    (when (fboundp 'unix:tcgets)
      (setq *tc* (unix:tcgets *standard-input*))
      (new-history *history-max*)))

  ;; Connect to socket
  (flet ((make-slime-socket (num)
           (let ((port (find (format nil "--port~A=" num) *eustop-argument*
                             :test #'(lambda (a b) (string= a b :end2 8)))))
             (when port
               (setq port (read-from-string (subseq port 8)))
               (assert (numberp port))
               (slime::slime-connect-socket port)))))
    (defconstant slime::*slime-stream* (make-slime-socket 1))
    (defconstant slime::*slime-internal-stream* (make-slime-socket 2)))
)
