(unless (find-package "SLIME") (make-package "SLIME"))
(in-package "SLIME")


;; Anaphoric macros
(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro aand (&rest args)
  (cond ((null args) t)
	((null (cdr args)) (car args))
	(t `(aif ,(car args) (aand ,@(cdr args))))))


;; Utility
(defun position-from-end (item seq &rest key-args)
  (aand
   (apply #'position item (reverse seq) key-args)
   (- (length seq) it 1)))

(defmacro do-callable-symbols (var &rest forms)
  ;; var: (variable-name &optional result)
  (let ((apackage (gensym "DOALLSYM")))
    `(progn
       (do-symbols (,(car var) *package*) ,@forms)
       (dolist (,apackage (*package* . use) ,(cadr var))
	 (do-external-symbols (,(car var) ,apackage) ,@forms)))))

(defmacro slime-do-symbols (var &rest body)
  ;; var: (variable-name &optional package result)
  `(if ,(cadr var)
       (do-symbols ,var ,@body)
       (do-callable-symbols (,(car var) ,(caddr var)) ,@body)))

(defun find-callable-symbol (name &optional package)
  (if package
      (find-symbol name package)
      (let ((packs (cons *package* (*package* . use))))
        (dolist (p packs)
          (aif (find-symbol name p)
               (return-from find-callable-symbol it))))))

;; Symbol completion
(defun slime-find-symbol (start &optional prefix)
  (let* ((pack (aand (position #\: start) (string-upcase (subseq start 0 it))))
         (str (if pack (subseq start (1+ (position-from-end #\: start))) start))
         (len (length str))
         str-lst)
    (slime-do-symbols (sym pack)
      (let ((sym-str (if (eql *print-case* :downcase)
                         (string-downcase (send sym :pname))
                         (send sym :pname))))
        (when (string-equal str sym-str :start1 0 :end1 len :start2 0 :end2 len)
          (push (concatenate string prefix start (subseq sym-str len)) str-lst))))
    (cond
      ((cdr str-lst)
       (list str-lst (subseq (car str-lst) 0 (get-common str-lst len))))
      (str-lst
       (list str-lst (car str-lst)))
      (t nil))))

(defun get-common (lst &optional (start 0))
  (do* ((min (reduce #'min (mapcar #'length lst)))
	(i start (1+ i))
	(c nil))
       ((>= i min) i)
    (setq c (elt (car lst) i))
    (dolist (s lst)
      (unless (eq (elt s i) c) (return-from get-common i)))))


;; Lambda list completion
(help '+ nil nil) ;; create *help-hash*

(defun autodoc (name cursor &optional item)
  (let* ((arglist (lambda-list name))
         (len (length arglist)))
    (when arglist
      (dotimes (i (min len cursor))
        (case (nth i arglist)
          (&rest
           (return-from autodoc (set-cursor arglist (1+ i))))
          (&optional
           (incf cursor))
          (&key
           (return-from autodoc (set-keyword arglist (1+ i) item)))))
      (if (> cursor len)
          arglist
          (set-cursor arglist (1- cursor))))))

(defun lambda-list (name)
  (let* ((pack (aand (position #\: name) (string-upcase (subseq name 0 it))))
         (func (if pack (subseq name (1+ (position-from-end #\: name))) name))
         (arglist
          (aif (gethash (string-downcase name) help::*help-hash*)
               (format nil "(~a)" (car (send it :read-help)))
               (get (find-callable-symbol (string-upcase func) pack)
                    :function-documentation))))
    (if (stringp arglist)
        (read-from-string arglist)
        (copy-list arglist))))

(defun keyword-list (arglist &optional start)
  (let ((pos (or start (aand (position '&key arglist) (1+ it)))))
    (if pos
        (mapcar #'(lambda (val) (format nil ":~a" (if (consp val) (car val) val)))
                (nthcdr pos arglist)))))

(defun set-cursor (lst pos)
  ;; TODO: use string not symbols
  (list-insert (intern "<===" *package*) (1+ pos) lst)
  (list-insert (intern "===>" *package*) pos lst))

(defun set-keyword (arglist start item)
  (let* ((keys (keyword-list arglist start))
         (pos (position item keys :test #'string-equal)))
    (if pos
        (set-cursor arglist (+ start pos))
        arglist)))


;; Keyword completion
(defun slime-find-keyword (start &optional form)
  (cond
    ((null form)
     (let ((*package* *keyword-package*))
       (slime-find-symbol (subseq start 1) ":")))
    ((string-equal (car form) "send")
     ;; TODO: method completion
     nil)
    (t
     (let ((keys (keyword-list (lambda-list (car form))))
           (len (length start)))
       (aif
        (remove-if-not
         #'(lambda (str) (string-equal start str :start1 1 :end1 len :start2 1 :end2 len))
         keys)
        (list it (subseq (car it) 0 (get-common it len))))))))
