(unless (find-package "SLIME") (make-package "SLIME"))
(in-package "SLIME")


;;;;;;;;;;;;;;;;;;;;;;
;; Recursive Load Tags
;;;;;;;;;;;;;;;;;;;;;;

(defvar *load-hooks* nil)
(defvar load-org-for-slime (symbol-function 'load))


;;;;;;;;;;;;;;;;;;;;
;; Socket Connection
;;;;;;;;;;;;;;;;;;;;

(defmacro ensure-print (&rest body)
  `(let ((*print-case* :downcase)
         *print-length* *print-level*)
     ,@body))

(defclass slime-input-stream :super stream :slots (fd read-flag))

(eval-when (load eval)

(export '(*slime-stream* *slime-internal-stream* slime-connect-socket socket-request socket-eval
          slime-input-stream slime-sigint-handler slime-error slimetop print-callstack))

(defvar *slime-stream*)
(defvar *slime-internal-stream*)
(defvar *slime-input-stream*)
(defvar *old-terminal-io* *terminal-io*)
(defvar *old-unix-system-fn* #'unix::system)
(defvar *old-piped-fork-fn* #'piped-fork)
(defvar *old-reploop-fn* #'reploop)

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


;; Slime Input Stream

;; creates a substitute for `*standard-input*' that notifies slime at every read
(defmethod slime-input-stream
  (:init ()
    (send-super :init :input 128)
    (setq fd 0)
    (setq tail 0)
    self)
  (:fill ()
    (when read-flag
      ;; `read-flag' is set by the slime process when the message ends exactly at the buffer end
      ;; in such cases, we reset tail to ensure that a new request is send
      (setq tail 0)
      (setq read-flag nil))
    (unless (= tail (length buffer))
      ;; only send the request once when reading a message longer than the buffer
      (slime::socket-request slime::*slime-stream* "read" nil))
    (let ((uread-length (unix:uread 0 buffer)))
      (setq tail (max uread-length 0))
      ;; return nil for EOF
      (plusp uread-length)))
  (:set-flag () (setq read-flag t))
  ;; methods to resemble file-stream interface
  (:fd () fd)
  (:instream () self)
  (:outstream () nil)
  (:infd () fd)
  (:outfd () nil)
  (:fname () nil)
  (:flag () (unix:fcntl fd F_GETFL 0)))

;; Slime Toplevel

(defun clean-eustop ()
  (dolist (num (list 1 2))
    (setq lisp::*eustop-argument*
          (delete-if #'(lambda (val) (string= val (format nil "--port~A=" num) :end1 8))
                     lisp::*eustop-argument* :count 1))))

(defun slime-sigint-handler (sig code)
  (if (fboundp 'unix:ualarm)
      (unix:ualarm 0 0)
      (unix:alarm 0))
  (slime::socket-request slime::*slime-stream* "abort" "Keyboard Interrupt")
  (let* ((*replevel* (1+ *replevel*))
         (*reptype* "B"))
    (catch *replevel* (reploop #'toplevel-prompt))))

(defun format-error-message (msg1 form &optional msg2)
  (if (and msg2 (zerop (length msg1))) (setq msg1 msg2 msg2 nil))
  (with-output-to-string (s)
    (format s "~a" msg1)
    (if msg2 (format s " ~a" msg2))
    (if form (format s " in ~s" form))))

(defun slime-error (code msg1 form &optional (msg2))
  (socket-request *slime-stream* "error" (format-error-message msg1 form msg2))
  (let ((*replevel* (1+ *replevel*))
        (*reptype* "E"))
    (while (catch *replevel* (reploop #'toplevel-prompt))))
  (throw *replevel* t))

(defun slime-internal-error (code msg1 form &optional (msg2))
  (socket-request *slime-internal-stream* "error" (format-error-message msg1 form msg2))
  (throw 0 nil))

(defun slime-finish-output (strm)
  (when (derivedp *slime-stream* socket-stream)
    (ensure-print
     (format strm "~Ceuslime-token-~A" 29 ;; group separator
             (send (lisp::socket-stream-address *slime-stream*) :port))
     (finish-output strm))))

(defun slime-clear-stream (strm)
  (let ((in (send strm :instream)))
    (send in :set-val "COUNT" 0)
    (send in :set-val "TAIL" 0))
  t)

(defun slimetop ()
  (clean-eustop)
  (lisp::install-error-handler 'slime::slime-error)
  (setq lisp::*max-callstack-depth* 0)
  (sys::make-thread 1)
  (sys::thread 'eval
    `(progn
       (lisp::install-error-handler 'slime::slime-internal-error)
       (while t
         (catch 0 (slime::socket-eval slime::*slime-internal-stream*)))))
  (catch :eusexit
    (let* ((*standard-input* (setq slime::*slime-input-stream*
                                   (instance slime::slime-input-stream :init)))
           (*terminal-io* (make-two-way-stream
                           *standard-input*
                           *standard-output*)))
      (while t
        (catch 0
          (let ((*replevel* 0) (*reptype* ""))
            (reploop #'toplevel-prompt))
          (throw :eusexit nil))
        )))
  (throw :eusexit nil))

(defun print-callstack (n)
  (let ((lisp::*max-callstack-depth* n))
    (error "print-callstack")))
)
