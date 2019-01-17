;;;;
;;;;euslisp toplevel loop and error/signal handler
;;;;
;;;;Copyright 1987,1991, Toshihiro MATSUI, Electrotechnical Laboratory
;;;1987-Oct
;;;1988execute unix command
;;;1989reset alarm signal
;;;1991-AugHistory
;;;1991-Novprompt
;;;1996-May*eustop-hook* to intercept control at start-up.

(in-package "LISP")

;;
;; read-list-from-line returns eof if eof hit
;; if a list is entered, its list is returned
;; otherwise, a list of all input items in a line is returned
;;

(eval-when (load eval)

  ;;
  ;; Read-Eval-Print-1
  ;;
  (defun rep1 (repstream eof local-bindings &optional (ttyp t))
    (let* ((input (read-list-from-line repstream eof))
           (slime-func (and (consp input)
                            (string= (send (symbol-package (car input)) :name) "SLIME")))
           result)
      (if (eq input eof) (return-from rep1 eof))
      (when (and input (or (not (streamp input))
                           (> (length (send input :buffer)) 0)))
        (when (and *history* (not slime-func))
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
         ;; (if *history* (add-history (string input)))
         (setq - input
               result
               (cond
                 ((> *replevel* 0)
                  (eval-dynamic input local-bindings))
                 ((boundp input) (eval input))
                 (t '*unbound*)))
         ;; (if ttyp (print result repstream))
         (print result repstream)
         )
        ((or (null (streamp input)) (listp input))
         ;; (if *history* (add-history (format nil "~s" input)))
         (setq - input)
         (setq result (eval input))
         ;;(if ttyp (print result  repstream)))
         (print result  repstream))
        ((streamp input)
         (setq result (evaluate-stream input) )
         ;;(if ttyp  (print result repstream ))
         (print result repstream ))
        (t (print "?" repstream)))
      (unless slime-func
        (setq +++ ++ ++ + + -)
        (setq *** ** ** * * result))))


  (defun reploop-non-select (&optional (repstream *terminal-io*)
                               (ttyp (unix:isatty repstream)))
                                        ;read-eval-print loop
    (let* ((*error-handler* 'euserror)
           (eof (gensym))
           (input) (local-bindings) (special-bindings))
      (if (> *replevel* 0)
          (setq  local-bindings (sys:list-all-bindings)
                                        ;special-bindings (sys:list-all-special-bindings)
                 ))
      (while t
        (catch :reploop
          (if ttyp (prompt repstream))
          (if (eql (rep1 repstream eof local-bindings ttyp) eof)
              (return-from reploop-non-select nil))
          ))))

  (defun repsel (repstream eof ttyp local-bindings)
    (if (eql (rep1 repstream eof local-bindings ttyp) eof)
        (throw :reploop-select nil))
    (if ttyp (prompt repstream) ))
  )

