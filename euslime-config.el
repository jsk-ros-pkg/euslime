;;; euslime-config.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2018  furushchev

;; Authors:
;;   Yuki Furuta <furushchev@jsk.imi.i.u-tokyo.ac.jp>
;;   Guilherme de Campos Affonso <affonso@jsk.imi.i.u-tokyo.ac.jp>
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

(require 'euslime)

(defvar euslime-path nil
  "Directory containing the Euslisp SLIME package.")
(setq euslime-path (file-name-directory load-file-name))

(defvar euslime-session-tag-path nil
  "Stores the name of the generated session-specific tag files")

(defcustom euslime-compile-path (expand-file-name "~/.euslime/")
  "Path to Euslisp SLIME compiled files."
  :type 'string)

(defcustom euslime-match-function 'tag-implicit-name-match-p
  "Match function passed to `euslime-find-definitions' for finding a tag."
  :type 'symbol)

(defcustom inferior-euslisp-program "roseus"
  "Backend program invoked by Euslisp SLIME."
  :type 'string)

(defcustom slime-use-slime-clear-screen t
  "Clear screen instead of recentering on slime repl"
  :group 'slime-mode
  :type 'boolean)

(defvar slime-lisp-implementations)
(unless slime-lisp-implementations
  (setq slime-lisp-implementations
        '((sbcl ("sbcl") :coding-system utf-8-unix))))

(nconc slime-lisp-implementations
       (list '(euslisp ("rosrun" "euslime" "euslime" "--emacs-mode")
                       :init euslime-init
                       :coding-system utf-8-unix)))

(defvar euslime-port 0 ;; Let the OS pick an available port
  "Port number to use for communicating to the swank server.")

;; Start EusLisp mode
(add-hook 'slime-repl-mode-hook
          (lambda ()
            (when (string= (ignore-errors (slime-connection-name)) "euslisp")
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
                   (expand-file-name "slime-connection" euslime-compile-path)
                   "SLIME")
           (format "#+:ros (load %S :package %S)\n"
                   (expand-file-name "slime-roseus" euslime-compile-path)
                   "SLIME")
           (format "(load %S :package %S)\n"
                   (expand-file-name "slime-toplevel" euslime-compile-path)
                   "LISP"))
          nil file)))

    (let* ((loader (expand-file-name "slime-loader.l" euslime-compile-path))
           (files (cl-mapcan #'needs-compile
                    (list "slime-util" "slime-connection" "slime-roseus" "slime-toplevel")))
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
      (let ((cmd-str (concat "roseus '(unwind-protect t"
                             (apply #'concat (nreverse cmd-lst))
                             "(exit))'")))
        (print (format "Executing shell command: %s" cmd-str))
        (message "Compiling files...")
        (shell-command cmd-str)))))

(defun euslime-prepare-tags ()
  (let ((eusdir (getenv "EUSDIR"))
        (eustag (format "%s/EUSTAGS" euslime-compile-path))
        (euscomptag (format "%s/EUSCOMPTAGS" euslime-compile-path))
        (eusgeotag (format "%s/EUSGEOTAGS" euslime-compile-path))
        (irttag (format "%s/IRTEUSTAGS" euslime-compile-path))
        (rostag (format "%s/ROSEUSTAGS" euslime-compile-path)))
    (euslime-maybe-generate-tag
     eustag "eus"
     (format "%s/lisp/l/*.l" eusdir)
     (format "%s/lisp/c/*.c" eusdir))
    (euslime-maybe-generate-tag
     euscomptag "eus"
     (format "%s/lisp/comp/*.l" eusdir))
    (euslime-maybe-generate-tag
     eusgeotag "\\(g\\|x\\|comp\\|roseus\\)"  ;; eusstart.l:146
     (format "%s/lisp/geo/*.l" eusdir))
    (euslime-maybe-generate-tag
     irttag "\\(irteus\\|roseus\\)"
     (format "%s/irteus/*.l" eusdir))
    ;; TODO: Use rosemacs to probe/find roseus package
    (let ((rosdir (if (= (shell-command "rospack find roseus") 0)
                      (replace-regexp-in-string "\n$" ""
                        (shell-command-to-string "rospack find roseus")))))
      (when rosdir
        (euslime-maybe-generate-tag
         rostag "roseus"
         (format "%s/euslisp/*.l" rosdir)
         (format "%s/*.cpp" rosdir))))))

(defun euslime-maybe-generate-tag (tag-file match-str lfiles &optional cfiles)
  (when (string-match-p match-str inferior-euslisp-program)
    (when (or (file-newer-than-file-p (file-name-directory lfiles) tag-file)
              (and cfiles (file-newer-than-file-p (file-name-directory cfiles) tag-file)))
      (message (format "Generating %s file..." tag-file))
      (let ((inhibit-message t))
        (shell-command
         ;; Include `(:methods' in l files
         (format "etags --regex='/[ \\t]*(:[^ \\t\\$\\(]*/' %s %s -o %s"
                 (expand-file-name lfiles)
                 ;; Include `pointer FUNCTIONS' in c files
                 (if cfiles
                     ;; TODO: ignore .old.c files
                     (format "-l none -R --regex='/pointer [A-Z_0-9]+[ ]*(/' --no-globals %s"
                             (expand-file-name cfiles))
                   "")
                 tag-file))))
    (add-to-list 'tags-table-list tag-file)))

(defun euslime-load-tags (filename)
  ;; make a separate directory for each session
  (unless euslime-session-tag-path
    (setq-local euslime-session-tag-path (make-temp-file "euslime-session-tag" t)))
  ;; hash filenames so that they can be safely overwritten on repeated loads
  (let ((tag-file
         (format "%s/%s-%s"
                 euslime-session-tag-path
                 (file-name-base filename)
                 (sxhash filename))))
    (euslime-maybe-generate-tag tag-file inferior-euslisp-program filename)))

(defun etags-class-of-tag ()
  (save-excursion
    (re-search-backward "\n\\(\(defmethod [^\n\t ]*\\)[ \t]*\177[0-9]*,[0-9]*\n")
    (buffer-substring (match-beginning 1) (match-end 1))))

(defun euslime-find-definitions (name &rest other-names)
  ;; e.g. (euslime-find-definitions "simple-action-server" "ros::simple-action-server")
  (let ((first-table t)
        result)
    (flet ((etag>xref (file tag-info)
             `(,(concat
                 (if (string-match-p "(:" (car tag-info)) ;; class method
                     ;; ignore-errors ?
                     (save-excursion (forward-line 1) (etags-class-of-tag)))
                 (car tag-info))
               (:location
                (:file ,file)
                (:position ,(cddr tag-info))
                (:snippet ,(car tag-info))))))
      (visit-tags-table-buffer (car tags-table-list))
      (while (or first-table (visit-tags-table-buffer t))
        (if first-table (setq first-table nil))
        (goto-char (point-min))
        (while (search-forward name nil t)
          (when (or (funcall euslime-match-function name)
                    (some #'(lambda (nm)
                              (and (looking-back nm)
                                   (funcall euslime-match-function nm)))
                          other-names))
            (beginning-of-line)
            (push
             (etag>xref
              (expand-file-name (save-excursion (forward-line 1) (file-of-tag)))
              (funcall snarf-tag-function))
             result))))
      result)))

;; Override to abort operation instead of reinitializing (only have hard restarts)
(defun slime-maybe-start-lisp (program program-args env directory buffer)
  "Return a new or existing inferior lisp process."
  (cond ((not (comint-check-proc buffer))
         (slime-start-lisp program program-args env directory buffer))
        ((slime-reinitialize-inferior-lisp-p program program-args env buffer)
         (when (and (car program-args) (string= (car program-args) "euslime"))
           (pop-to-buffer (slime-output-buffer))
           (keyboard-quit))
         (let ((conn (cl-find (get-buffer-process buffer)
                              slime-net-processes
                              :key #'slime-inferior-process)))
           (when conn
             (slime-net-close conn)))
         (get-buffer-process buffer))
        (t (slime-start-lisp program program-args env directory
                             (generate-new-buffer-name buffer)))))

(defun euslime-init (file _)
  (setq slime-protocol-version 'ignore)
  (format "--euslisp-program %s --init-file %s --port %s --port-filename %s %s\n"
          inferior-euslisp-program
          (expand-file-name "slime-loader.l" euslime-compile-path)
          euslime-port
          file
          (if (member 'slime-repl-ansi-color slime-contribs) "--color" "")))

(defun euslime ()
  "euslime"
  (interactive)
  (euslime-prepare-files)
  (euslime-prepare-tags)
  (slime 'euslisp))

(provide 'euslime-config)
;;; euslime-config.el ends here
