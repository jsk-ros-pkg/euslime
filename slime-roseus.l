(unless (find-package "SLIME") (make-package "SLIME"))
(in-package "SLIME")

;; Recursive load tags
(defvar roseus-add-files-org (symbol-function 'ros::roseus-add-files))

(defun ros::roseus-add-files (pkg type)
  ;; inhibit load-hooks when adding ros messages
  (let (load-hooks)
    (funcall roseus-add-files-org pkg type)))

(defun get-recursive-load-notes (filename)
  (let* ((acc nil)
         (hook-fn #'(lambda (name)
                      (when (substringp "package://" name)
                        (setq name (ros::resolve-ros-path name)))
                      (push name acc)))
         (load-hooks (cons hook-fn load-hooks)))
    (load filename)
    acc))
