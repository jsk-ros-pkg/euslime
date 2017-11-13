;;; slime-euswank.el --- 

;; Copyright (C) 2017  Yuki Furuta

;; Author: Yuki Furuta <furushchev@jsk.imi.i.u-tokyo.ac.jp>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(defgroup slime-euswank nil "Slime Extension for Euslisp"
  :group 'slime-euswank)

(defcustom slime-euswank-command "run.py"
  "Command for running the euswank server"
  :type 'string
  :group 'slime-euswank)

(defcustom slime-euswank-args '()
  "Command arguments for running euswank server."
  :type '(repeat (string :tag "Arg"))
  :group 'slime-euswank)

(define-slime-contrib slime-euswank
  "Support for Euswank on emacs-side"
  (:authors "Yuki Furuta")
  (:license "BSD")
  (:slime-dependencies slime-repl)
  (:on-load (add-hook 'slime-event-hooks 'slime-euswank-event-hook-function))
  (:on-unload (add-hook 'slime-event-hooks 'slime-euswank-event-hook-function)))

(defun slime-euswank-repl-update-package ()
  (let ((name (slime-curent-package)))
    (with-current-buffer (slime-output-buffer)
      (let ((previous-point (- (point) slime-repl-input-start-mark)))
        (setf (slime-lisp-package) name
              (slime-lisp-package-prompt-string) name
              slime-buffer-package name)
        (slime-repl-insert-prompt)
        (when (plusp previous-point)
          (goto-char (+ previous-point slime-repl-input-start-mark)))))))

(defvar slime-euswank-buffer 'nil)

(defun slime-euswank-run (&optional port-file)
  "Runs swank server"
  (interactive)
  (setq slime-euswank-buffer
        (apply #'make-comint
               "euswank"
               (expand-file-name slime-euswank-command)
               nil port-file slime-euswank-args)))

(defun slime-euswank-init (file encoding)
  (slime-euswank-run file)
  "")

(setq slime-lisp-implementations
      '((euswank ("euswank") :init slime-euswank-init :coding-system utf-8-unix)))

(defun euswank ()
  (interactive)
  (slime 'euswank))

(defun slime-euswank-event-hook-function (event)
  (when (equal "irteusgl" (slime-lisp-implementation-type))
    (message "event: %s" event)
    (destructure-case event
     ((:new-package package prompt)
      (let ((buffer (slime-connection-output-buffer)))
        (setf (slime-lisp-package) package)
        (setf (slime-lisp-package-prompt-string) prompt)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (setq slime-buffer-package package)
            (slime-euswank-repl-update-package)
            (save-excursion
              (goto-char (marker-position slime-repl-prompt-start-mark))
              (slime-mark-output-start))))
        t))
     (t nil))))

(defun slime-euswank-eval (str &optional cont)
  (slime-eval-async `(swank:interactive-eval ,str) cont))

(defun slime-euswank-send-defun ()
  (interactive)
  (save-excursion
    (lexical-let ((start (beginning-of-defun))
                  (end (end-of-defun)))
      (slime-flash-region start end)
      (slime-euswank-eval
       (buffer-substring-no-properties start end)
       #'(lambda (v)
           (save-excursion
             (goto-char start)
             (let ((sent-func "<...>"))
               (when (looking-at "[ \t]*defun \\([^ )]\\)")
                 (setf sent-func (match-string 1)))
               (message "Sent: %s" sent-func))))))))

(defun slime-euswank-send-region (start end)
  (interactive "r")
  (save-excursion
    (slime-flash-region start end)
    (slime-euswank-eval
     (buffer-substring-no-properties start end))
    (message "Sent region")))

(defun slime-euswank-send-buffer ()
  (interactive)
  (save-excursion
    (let ((start (point-min))
          (end (point-max)))
      (slime-flash-region start end)
      (slime-euswank-eval
       (buffer-substring-no-properties start end))
      (message "Sent buffer"))))

(define-minor-mode slime-euswank-minor-mode
  "Toggle minor mode for Euswank"
  nil
  " Euswank"
  '(("\C-c\C-c" . slime-euswank-send-defun)
    ("\C-c\C-r" . slime-euswank-send-region)
    ("\C-c\C-k" . slime-euswank-send-buffer)
    ("\C-c\C-z" . slime-switch-to-output-buffer)))


(provide 'slime-euswank)
;;; slime-euswank.el ends here
