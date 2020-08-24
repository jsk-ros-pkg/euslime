(unless (find-package "SLIME") (make-package "SLIME"))
(in-package "SLIME")

;;;;;;;;;;;;;;;;;;;;
;; Socket Connection
;;;;;;;;;;;;;;;;;;;;

(defmacro ensure-print (&rest body)
  `(let ((*print-case* :downcase)
         *print-length* *print-level*)
     ,@body))

(eval-when (load eval)

(export '(*slime-stream* *slime-internal-stream* slime-connect-socket socket-eval socket-request
          slime-error slime-finish-output slimetop print-callstack))

(defvar *slime-stream*)
(defvar *slime-internal-stream*)

(defun slime-connect-socket (port)
  (flet ((try-connect (&optional (timeout 5))
           (make-client-socket-stream
            ;; `service' must be null when working in docker environments
            ;; because (unix:getservbyname "echo" nil) return error
            (make-socket-address :host "0.0.0.0" :port port :service nil)
            timeout)))
    (do ((strm (try-connect) (try-connect)))
        ((streamp strm)
         (unix:usleep 100000)
         strm))))

(defun socket-request (strm command value)
  (assert (streamp strm) "Cannot connect to socket stream!")
  (flet ((send-request (str)
           (ensure-print
            (let ((len (substitute #\0 #\space (format nil "~6,x" (length str)))))
              (princ len strm)
              (princ str strm)
              (finish-output strm)))))
    (send-request command)
    (send-request (prin1-to-string value))))

(defun socket-eval (strm)
  (ensure-print
   (socket-request strm "result" (evaluate-stream strm))))

;; Slime Toplevel

(defun clean-eustop ()
  (dolist (num (list 1 2))
    (setq lisp::*eustop-argument*
          (delete-if #'(lambda (val) (string= val (format nil "--port~A=" num) :end1 8))
                     lisp::*eustop-argument* :count 1))))

(defun slime-error (code msg1 form &optional (msg2))
  (if (and msg2 (zerop (length msg1))) (setq msg1 msg2 msg2 nil))
  (socket-request *slime-stream* "error"
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
    (ensure-print
     (format strm "~Ceuslime-token-~A" 29 ;; group separator
             (send (lisp::socket-stream-address *slime-stream*) :port))
     (finish-output strm))))

(defun slimetop ()
  (clean-eustop)
  (lisp::install-error-handler 'slime::slime-error)
  (setq lisp::*max-callstack-depth* 0)
  (sys::make-thread 1)
  (sys::thread 'eval `(while t (slime::socket-eval slime::*slime-internal-stream*)))
  (catch :eusexit
    (while t
      (catch 0
        (let ((*replevel* 0) (*reptype* ""))
          (reploop #'toplevel-prompt))
        (throw :eusexit nil))
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

(eval-when (load eval)

(defun toplevel-prompt (strm)
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
    (slime::slime-finish-output out)
    (slime::socket-request slime::*slime-stream* "result" result)))

(defun reploop (prompt &optional (repstream *terminal-io*) (ttyp (unix:isatty repstream)))
  (let ((*prompt* prompt))
    (slime::slime-finish-output repstream)
    (slime::socket-request slime::*slime-stream* "abort" nil)
    (reploop-select repstream ttyp)))
)

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
