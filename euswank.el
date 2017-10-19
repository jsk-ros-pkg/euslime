;;; euswank.el --- 

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

(defgroup euswank nil "Slime Extension for Euslisp"
  :group 'euswank)

(defcustom slime-euswank-command "euswank"
  "Command for running the euswank server"
  :type 'string
  :group 'euswank)

(defcustom slime-euswank-args '()
  "Command arguments for running euswank server."
  :type '(repeat (string :tag "Arg"))
  :group 'euswank)

(define-slime-contrib euswank
  "Support for Euswank on emacs-side"
  (:authors "Yuki Furuta")
  (:license "BSD")
  (:slime-dependencies slime-repl)
  (:on-load (add-hook 'slime-event-hooks 'euswank-event-hook-function))
  (:on-unload (add-hook 'slime-event-hooks 'euswank-event-hook-function)))

(defvar slime-euswank-buffer 'nil)

(defun slime-euswank-run ()
  "Runs swank server"
  (interactive)
  (setq slime-euswank-buffer
        (apply #'make-comint
               "euswank"
               (expand-file-name slime-euswank-command)
               nil slime-euswank-args)))

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

(defun slime-euswank-event-hook-function (event)
  (when (equal "EUS" (slime-lisp-implementation-type))
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


(defun slime-euswank-send-defun ()
  )

(defun slime-euswank-send-region (start end)
  )

(defun slime-euswank-send-buffer ()
  )

(define-minor-mode slime-euswank-minor-mode
  "Toggle minor mode for Euswank"
  nil
  " Euswank"
  '(("\C-c\C-c" . slime-euswank-send-defun)
    ("\C-c\C-r" . slime-euswank-send-region)
    ("\C-c\C-k" . slime-euswank-send-buffer)
    ("\C-c\C-z" . slime-switch-to-output-buffer)))


(provide 'euswank)
;;; euswank.el ends here
