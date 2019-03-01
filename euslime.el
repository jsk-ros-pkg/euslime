;;; euslime.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2018  furushchev

;; Author: furushchev <furushchev@mochi>
;; Keywords: lisp

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

;;; Commentary:

;;; Code:

(require 'slime-euslisp)

(defvar euslime-executable "euslime"
  "The euslime executable for Euslisp SLIME.")

(defvar euslime-path nil
  "Directory containing the Euslisp SLIME package.")
(setq euslime-path (file-name-directory load-file-name))

(defvar euslime-compile-path nil
  "Path to Euslisp SLIME compiled files.")
(setq euslime-compile-path (expand-file-name "~/.euslime/"))

(defvar slime-lisp-implementations)
(unless slime-lisp-implementations
  (setq slime-lisp-implementations
        '((sbcl ("sbcl") :coding-system utf-8-unix))))

(nconc slime-lisp-implementations
       (list `(euslisp (,euslime-executable "--emacs-mode")
                       :init euslime-init
                       :coding-system utf-8-unix)))

(defvar euslime-port 0 ;; Let the OS pick an available port
  "Port number to use for communicating to the swank server.")

;; Start EusLisp mode
(add-hook 'slime-repl-mode-hook
          (lambda ()
            (when (string= "euslisp"
                   (ignore-errors (slime-connection-name (slime-current-connection))))
              (slime-euslisp-mode 1))))

(defun euslime-prepare-files ()
  (cl-flet
      ((needs-compile (name)
         (let ((src-file (expand-file-name (concat name ".l") euslime-path))
               (dst-file (expand-file-name (concat name ".so") euslime-compile-path)))
           (if (file-newer-than-file-p src-file dst-file)
               ;; Returns list for mapcan'able result
               (list src-file))))
       (write-loader (file)
         (write-region
          (concat
           (format "(unless (find-package %S) (make-package %S))\n\n"
                   "SLIME" "SLIME")
           (format "(load %S :package %S)\n"
                   (expand-file-name "slime-util" euslime-compile-path)
                   "SLIME")
           (format "(load %S :package %S)\n"
                   (expand-file-name "slime-toplevel" euslime-compile-path)
                   "LISP"))
          nil file)))

    (let* ((loader (expand-file-name "slime-loader.l" euslime-compile-path))
           (files (cl-mapcan #'needs-compile (list "slime-util" "slime-toplevel")))
           (res (apply #'euslime-compile-files files)))
      (cond
       ((null res) ;; FILES UP-TO-DATE
        (unless (file-exists-p loader)
          (write-loader loader)))
       ((zerop res) ;; SUCCESS
        (write-loader loader))
       (t ;; ERROR
        (error "Compile Failed!"))))))

(defun euslime-compile-files (&rest files)
  (when files
    (let (cmd-lst)
      ;; Create directory
      (unless (file-exists-p euslime-compile-path)
        (make-directory euslime-compile-path))
      ;; Probe files
      (dolist (file files)
        (cl-assert (file-exists-p file))
        (push (format " (compiler:compile-file %S :o %S) " file euslime-compile-path)
              cmd-lst))
      ;; Compile files
      (let ((cmd-str (concat "eus '(unwind-protect t"
                             (apply #'concat (nreverse cmd-lst))
                             "(exit))'")))
        (print (format "Executing shell command: %s" cmd-str))
        (message "Compiling files...")
        (shell-command cmd-str)))))

(defun euslime-prepare-tags ()
  (let ((eusdir (getenv "EUSDIR"))
        (eustag (format "%s/EUSTAGS" euslime-compile-path))
        (irttag (format "%s/IRTEUSTAGS" euslime-compile-path)))
    (euslime-maybe-generate-tag
     (format "%s/lisp" eusdir) eustag
     (format "etags %s/lisp/l/*.l -l none --regex='/pointer [A-Z_0-9]+[ ]*(/' --no-globals %s/lisp/c/*.c -o %s" eusdir eusdir eustag))
    (euslime-maybe-generate-tag
     (format "%s/irteus" eusdir) irttag
     (format "etags %s/irteus/*.l -o %s" eusdir irttag))
    ;; (visit-tags-table eustag)
    (setq tags-table-list (list eustag irttag))))

(defun euslime-maybe-generate-tag (src-dir tag-file cmd)
  (when (file-newer-than-file-p src-dir tag-file)
    (message (format "Generating %s file..." tag-file))
    (shell-command cmd)))

(defun euslime-init (file _)
  (setq slime-protocol-version 'ignore)
  (format "--port %s --port-filename %s %s\n" euslime-port file
          (if (member 'slime-repl-ansi-color slime-contribs)
              "--color" "")))

(defun euslime ()
  "euslime"
  (interactive)
  (euslime-prepare-files)
  (euslime-prepare-tags)
  (slime 'euslisp))

(provide 'euslime)
;;; euslime.el ends here
