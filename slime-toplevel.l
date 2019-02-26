(unless (find-package "SLIME") (make-package "SLIME"))
(in-package "SLIME")

;;;;;;;;;;;;;;;;;;;;
;; Socket Connection
;;;;;;;;;;;;;;;;;;;;

(export '(*slime-stream* slime-connect-socket socket-eval
          slime-error slime-finish-output slimetop
          print-callstack))

(defvar *slime-stream*)

(eval-when (load eval)

(defun slime-connect-socket (port)
  (do ((strm (connect-server "0.0.0.0" port) (connect-server "0.0.0.0" port)))
      ((streamp strm)
       (defconstant *slime-stream* strm)
       strm)
    (unix:usleep 100000)))

(defun socket-request (command value)
  (assert (streamp *slime-stream*))
  (flet ((send-request (str)
           (let ((len (substitute #\0 #\space (format nil "~6,x" (length str)))))
             (format *slime-stream* len)
             (format *slime-stream* str)
             (finish-output *slime-stream*))))
    (send-request command)
    (send-request (format nil "~s" value))))

(defun socket-eval (strm)
  (socket-request "result"
   (evaluate-stream strm)))

;; Slime Toplevel

(defun slime-error (code msg1 form &optional (msg2))
  (if (and msg2 (zerop (length msg1))) (setq msg1 msg2 msg2 nil))
  (socket-request "error"
   (with-output-to-string (s)
     (format s "~a" msg1)
     (if msg2 (format s " ~a" msg2))
     (if form (format s " in ~s" form))))
  (let ((*replevel* (1+ *replevel*))
        (*reptype* "E"))
    (while (catch *replevel* (reploop #'toplevel-prompt))))
  (throw *replevel* t))

(defun slime-finish-output (strm)
  (when (derivedp *slime-stream* socket-stream)
    (format strm "~Ceuslime-token-~A" 29 ;; group separator
            (send (lisp::socket-stream-address *slime-stream*) :port))
    (finish-output strm)))

(defun slimetop ()
  (lisp::install-error-handler 'slime:slime-error)
  (setq lisp::*max-callstack-depth* 0)
  (catch :eusexit
    (while t
      (catch 0
        (let ((*replevel* 0) (*reptype* ""))
          (reploop #'toplevel-prompt))
        (throw :eusexit nil))
      ;; (slime:slime-finish-output *terminal-io*)
      ))
  (throw :eusexit nil))

(defun print-callstack (n)
  (let ((lisp::*max-callstack-depth* n))
    (error "print-callstack")))
)

;;;;;;;;;;;;;;;;;;;;;;
;; Toplevel Overwrites
;;;;;;;;;;;;;;;;;;;;;;

(in-package "LISP")

(defun toplevel-prompt (strm)
  (if (> *replevel* 0)
      (format strm "~A~D-" *reptype* *replevel*))
  (if (not (eql *package* *user-package*))
      (format strm "~A:" (package-name *package*)))
  (format strm "~a" *prompt-string*))

(defun repsel (repstream eof ttyp local-bindings)
  ;; Do not print the evaluation result to *standard-output*
  ;; Instead, redirect it to *slime-stream*
  (let* ((out (send repstream :outstream))
         (repstream (make-two-way-stream
                     (send repstream :instream)
                     (make-string-output-stream)))
         (result (rep1 repstream eof local-bindings ttyp)))
    (if (eql result eof) (throw :reploop-select nil))
    (slime:slime-finish-output out)
    (slime::socket-request "result" result)))

(defun reploop (prompt &optional (repstream *terminal-io*) (ttyp (unix:isatty repstream)))
  (let ((*prompt* prompt))
    (slime:slime-finish-output repstream)
    (slime::socket-request "abort" nil)
    (send *top-selector* :add-port slime:*slime-stream* #'slime:socket-eval slime:*slime-stream*)
    (reploop-select repstream ttyp)))


;;;;;;;;;;;;;
;; Setup REPL
;;;;;;;;;;;;;

(eval-when (load eval)

  ;; Set signal-handler and *history* for non-tty streams
  (unless (unix:isatty *standard-input*)
    (unix:signal unix::sigint 'sigint-handler 2)
    (when (fboundp 'unix:tcgets)
      (setq *tc* (unix:tcgets *standard-input*))
      (new-history *history-max*)))

  ;; Connect to socket
  (let ((port (find "--port-" *eustop-argument* :test #'(lambda (a b) (string= a b :end2 7)))))
    (when port
      (setq port (read-from-string (subseq port 7)))
      (assert (numberp port))
      (slime:slime-connect-socket port)))
)
