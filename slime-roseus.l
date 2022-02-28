(unless (find-package "SLIME") (make-package "SLIME"))
(in-package "SLIME")

;; Recursive load tags
(defvar roseus-add-files-org (symbol-function 'ros::roseus-add-files))

(defun ros::roseus-add-files (pkg type)
  ;; inhibit load-hooks when adding ros messages
  (let (*load-hooks*)
    (funcall roseus-add-files-org pkg type)))

;; Recompile with new ros::roseus-add-files
(defun load-file-and-tags (filename)
  (let* ((acc nil)
         (hook-fn #'(lambda (name) (aif (true-filename name) (push it acc))))
         (*load-hooks* (cons hook-fn *load-hooks*)))
    (load filename)
    (nreverse acc)))
