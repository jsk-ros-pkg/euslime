(require 'slime)
(require 'slime-repl)
(require 'ansi-color)

;; AUXILIARY FUNCTIONS
(defun remove-asdf-system-shortcuts ()
  (cl-flet ((sys? (shortcut)
              (string-match-p "system" (car (slime-repl-shortcut.names shortcut)))))
    (cl-remove-if #'sys? slime-repl-shortcut-table)))

(defun slime-apropos-symbol-package (prefix package)
  "Show apropos listing for symbols in PACKAGE including PREFIX."
  (interactive (list (read-string "SLIME Apropos: ")
                     (let ((pkg (slime-read-package-name "Package: ")))
                       (if (string= pkg "") (slime-current-package) pkg))))
  (slime-apropos prefix t package))

;; Override to inherit the buffer-local `slime-repl-shortcut-table'
(defun slime-list-repl-short-cuts ()
  (interactive)
  (let ((mode (if slime-euslisp-mode 'slime-euslisp-mode)))
    (slime-with-popup-buffer ((slime-buffer-name :repl-help) :mode mode)
      (let ((table (cl-sort (cl-copy-list slime-repl-shortcut-table) #'string<
                            :key (lambda (x)
                                   (car (slime-repl-shortcut.names x))))))
        (save-excursion
          (dolist (shortcut table)
            (let ((names (slime-repl-shortcut.names shortcut)))
              (insert (pop names)) ;; first print the "full" name
              (when names
                ;; we also have aliases
                (insert " (aka ")
                (while (cdr names)
                  (insert (pop names) ", "))
                (insert (car names) ")"))
              (when (slime-repl-shortcut.one-liner shortcut)
                (insert "\n     " (slime-repl-shortcut.one-liner shortcut)))
              (insert "\n"))))))))

;; Override to use LISP package when in EusLisp mode
(defslime-repl-shortcut slime-repl-defparameter ("defparameter" "!")
  (:handler (lambda (name value)
              (interactive (list (slime-read-symbol-name "Name (symbol): " t)
                                 (slime-read-from-minibuffer "Value: " "*")))
              (let ((prefix (if slime-euslisp-mode "lisp" "cl")))
                (insert "(" prefix ":" "defparameter " name " " value
                        " \"REPL generated global variable.\")"))
              (slime-repl-send-input t)))
  (:one-liner "Define a new global, special, variable."))

;; Override to abort operation instead of reinitializing (only have hard restarts)
(defun slime-maybe-start-lisp (program program-args env directory buffer)
  "Return a new or existing inferior lisp process."
  (cond ((not (comint-check-proc buffer))
         (slime-start-lisp program program-args env directory buffer))
        ((slime-reinitialize-inferior-lisp-p program program-args env buffer)
         (when (string= program "euslime")
           (switch-to-buffer (slime-output-buffer))
           (keyboard-quit))
         (let ((conn (cl-find (get-buffer-process buffer)
                              slime-net-processes
                              :key #'slime-inferior-process)))
           (when conn
             (slime-net-close conn)))
         (get-buffer-process buffer))
        (t (slime-start-lisp program program-args env directory
                             (generate-new-buffer-name buffer)))))

;; Override to avoid trailing newlines upon consecutive execution
;; The same could be attained by setting `slime-repl-history-trim-whitespaces',
;; but this causes color change in the prompt-string in Euslisp mode
(defslime-repl-shortcut slime-repl-resend ("resend-form")
  (:handler (lambda ()
              (interactive)
              (insert (car slime-repl-input-history))
              (slime-repl-send-input t)))
  (:one-liner "Resend the last form."))

;; Override to avoid printing the warning message
(defun slime-repl-abort-read (thread tag)
  (with-current-buffer (slime-output-buffer)
    (pop slime-read-string-threads)
    (pop slime-read-string-tags)
    (slime-repl-read-mode -1)
    (unless slime-euslisp-mode
      (message "Read aborted"))))

;; Support ansi-colors in popup buffers
(add-hook 'slime-popup-buffer-mode-hook
          (lambda () (ansi-color-apply-on-region (point-min) (point-max))))

;; COMPLETION FUNCTIONS
(defun slime-maybe-complete-as-filename ()
   "If point is at a string starting with \", complete it as filename.
 Return nil if point is not at filename."
   (when (save-excursion (re-search-backward "\"[^ \t\n]+\\="
                                            (max (point-min)
                                                 (- (point) 1000)) t))
     ;; TODO complete ros packages "package://..."
     ;; ROSEMACS (comint-dynamic-complete-ros-package)
     (let ((comint-completion-addsuffix '("/" . "\"")))
       ;; (comint-replace-by-expanded-filename)
       (setq default-directory (slime-eval `(swank:default-directory)))
       (comint-dynamic-complete-filename)
      t)))

(defun slime-set-minibuffer-completion ()
  (let ((buf (other-buffer (current-buffer) t)))
    (if (local-variable-if-set-p 'slime-complete-symbol-function buf)
        (setq-local slime-complete-symbol-function
          (buffer-local-value 'slime-complete-symbol-function buf)))))

;; DEFINE MINOR MODE
(defun slime-euslisp--doc-map-prefix ()
  (concat
   (car (rassoc '(slime-prefix-map) slime-parent-bindings))
   (car (rassoc '(slime-doc-map) slime-prefix-bindings))))

(define-minor-mode slime-euslisp-mode
  "Toggle Euslisp SLIME mode."
  :lighter " eus"
  :keymap (let ((prefix (slime-euslisp--doc-map-prefix)))
            `((,(concat prefix (kbd "C-p")) . slime-apropos-symbol-package)
              (,(concat prefix "p") . slime-apropos-symbol-package)
              (,(kbd "M-,") . tags-loop-continue)
              (,(kbd "M-.") . find-tag)))
  ;; Use simple-completions rather than fuzzy-completions
  (setq-local slime-complete-symbol-function 'slime-complete-symbol*)
  (add-hook 'minibuffer-setup-hook 'slime-set-minibuffer-completion)
  (setq-local slime-complete-symbol*-fancy nil)
  ;; Remove unsupported ASDF commands
  (setq-local slime-repl-shortcut-table (remove-asdf-system-shortcuts))
  ;; Keep history record in a different file
  (setq-local slime-repl-history-file "~/.euslime-history.eld")
  (slime-repl-safe-load-history)
  ;; Start Message
  (when (called-interactively-p 'interactive)
    (message "Euslisp SLIME mode %s."
             (if slime-euslisp-mode "enabled" "disabled"))))

(provide 'slime-euslisp)
