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

(defvar slime-euslime-path "euslime"
  "Path to euslime executable for with Euslisp SLIME.")

(defvar slime-lisp-implementations
  '((sbcl ("sbcl") :coding-system utf-8-unix)))
(nconc slime-lisp-implementations
       (list `(euslisp (,slime-euslime-path)
                       :init euslime-init
                       :coding-system utf-8-unix)))

(defvar slime-euslime-port 4005
  "Port number to use for communicating to the swank server.")

(defun euslime-init (file _)
  (setq slime-protocol-version 'ignore)
  (format "%S\n"
          `(begin '(require-extension slime)
                  (swank-server-start ,slime-euslime-port ,file))))

(defun euslime ()
  "euslime"
  (interactive)
  (slime 'euslisp))

(provide 'euslime)
;;; euslime.el ends here
