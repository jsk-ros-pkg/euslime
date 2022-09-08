(unless (find-package "SLIME") (make-package "SLIME"))
(in-package "SLIME")

;; Generate  *help-hash*
;; (setq help::*eus-tex-dir* "/path/to/latex/")
(help '+ nil nil)

;; UTILITY FUNCTIONS
(defun position-from-end (item seq &rest key-args)
  (aand
   (apply #'position item (reverse seq) key-args)
   (- (length seq) it 1)))

(defun read-copy (obj name)
  (cond
    ((stringp obj)
     (if (and (> (length obj) 0) (eq (elt obj 0) #\())
         (cons name (read-from-string obj))))
    ((consp obj)
     (cons name (copy-list obj)))))

(defun maybe-downcase (str)
  (if (eql *print-case* :downcase)
      (string-downcase str)
      str))

;; Anaphoric macros
(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro aand (&rest args)
  (cond ((null args) t)
	((null (cdr args)) (car args))
	(t `(aif ,(car args) (aand ,@(cdr args))))))

;; File related
(defun true-filename (fname)
  (labels ((check-file (name)
             (if (eql (probe-file name) :file) name))
           (check-name (name)
             (or (check-file name)
                 (check-file (namestring (merge-pathnames ".l" name))))))
    (cond
      ((and (stringp fname) (substringp "package://" fname))
       ;; avoid using package notation to be safe for other eus executables
       (unless (find-package "ROS") (error "no such package ROS"))
       (check-name (funcall (find-symbol "RESOLVE-ROS-PATH" "ROS") fname)))
      ((aand (pathname-directory (pathname fname))
             (eql (car it) :root))
       (check-name (namestring fname)))
      (t
       (dolist (p (union *loader-current-directory* *load-path*
                         :test #'string=))
         (aif (check-name (concatenate-pathnames p fname))
              (return-from true-filename (namestring it))))))))

;; Symbol related
(defun split-package (name)
  ;; TODO: read |escaped\|forms|
  (let* ((pos (position-from-end #\: name))
         (external (and pos (plusp pos) (not (= #\: (elt name (1- pos)))))))
    (case pos
      (nil (values nil name external))
      (0 (values "KEYWORD" (subseq name (1+ pos)) external))
      (t (values (string-upcase (subseq name 0 (if external pos (1- pos))))
                 (subseq name (1+ pos))
                 external)))))

(defmacro slime-do-symbols (var &rest body)
  ;; var: (variable-name &optional package external-p)
  `(if ,(second var)
       (if (find-package ,(second var))
           (if ,(third var)
               (do-external-symbols (,(car var) ,(cadr var)) ,@body)
               (do-symbols (,(car var) ,(cadr var)) ,@body)))
       (do-callable-symbols (,(car var)) ,@body)))

(defmacro do-callable-symbols (var &rest forms)
  ;; var: (variable-name &optional result)
  (let ((apackage (gensym "DOALLSYM")))
    `(progn
       (do-symbols (,(car var) *package*) ,@forms)
       (dolist (,apackage (*package* . use) ,(cadr var))
	 (do-external-symbols (,(car var) ,apackage) ,@forms)))))

(defun find-external-symbol (name package)
  (do-external-symbols (sym package)
    (if (string= (symbol-pname sym) name)
        (return-from find-external-symbol sym))))

(defun find-callable-symbol (name)
  (multiple-value-bind (pack name external-p) (split-package name)
    (setq name (string-upcase name))
    (if pack
        (and (find-package pack)
             (if external-p
                 (find-external-symbol name pack)
                 (find-symbol name pack)))
        (or (find-symbol name) 
            (let ((packs (package-use *package*)))
              (dolist (p packs)
                (aif (find-external-symbol name p)
                     (return-from find-callable-symbol it))))))))

(defmacro with-callable-symbol-value (var-lst &rest body)
  ;; var-lst is in form (val-var symbol-name &optional result) OR
  ;; ((sym-var val-var) symbol-name &optional result)
  (let ((val-var (if (consp (car var-lst))
                     (cadar var-lst)
                     (car var-lst)))
        (sym-var (if (consp (car var-lst))
                     (caar var-lst)
                     (gensym))))
    `(let ((,sym-var (find-callable-symbol ,(cadr var-lst))))
       (if (and ,sym-var (boundp ,sym-var))
           (let ((,val-var (if (< (symbol-vtype ,sym-var) 3)
                               (symbol-value ,sym-var)
                               (elt (system::thread-specials) (symbol-vtype ,sym-var)))))
             ,@body
             ,@(cddr var-lst))))))

(defun list-possible-symbol-names (sym)
  (let ((name (symbol-pname sym))
        (pkg (symbol-homepkg sym)))
    (mapcar #'string-downcase
            (remove-duplicates
             (append
              (if (eq pkg *keyword-package*)
                  (list (format nil ":~A" name))
                  (list name))
              (mapcar #'(lambda (val) (format nil "~A::~A" val name))
                      (send pkg :nicknames))
              (if (find-external-symbol name pkg)
                  (mapcar #'(lambda (val) (format nil "~A:~A" val name))
                          (send pkg :nicknames))))
             :test #'string-equal))))

;; for optimization
(defmacro is-send/instance-symbol (name)
  ;; return "send", "instance" or nil
  `(cdr (assoc ,name
               ',(mapcan #'list
                         (mapcar #'(lambda (val) (cons val "send"))
                                 (list-possible-symbol-names 'send))
                         (mapcar #'(lambda (val) (cons val "instance"))
                                 (list-possible-symbol-names 'instance)))
               :test #'string-equal)))


;; Prefix related
(defun append-common (match-lst len)
  (cond
    ((cdr match-lst)
     (list match-lst (subseq (car match-lst) 0 (get-common match-lst len))))
    (match-lst ;; one element list
     (list match-lst (car match-lst)))
    (t nil)))

(defun get-common (lst &optional (start 0))
  (do* ((min (reduce #'min (mapcar #'length lst)))
	(i start (1+ i))
	(c nil))
       ((>= i min) i)
    (setq c (elt (car lst) i))
    (dolist (s lst)
      (unless (eq (elt s i) c) (return-from get-common i)))))

(defun find-prefix (prefix lst &optional (start 0))
  (let ((len (length prefix)))
    (append-common
     (remove-if-not
      #'(lambda (str) (string-equal prefix str :start1 start :end1 len :start2 start :end2 len))
      lst)
     len)))

;; Help search
(defun get-help (name &optional class)
  (flet ((read-item (item)
           (case (help::help-item-type item)
             ((1 2 3 6) ;; method, function, macro or special form
              (read-from-string (format nil "(~a ~a)" name (car (send item :read-help))))))))
    (let ((name (string-downcase name))
          (class (and class (string-downcase (send class :name)))))
      (if class
          (aand (gethash class help::*help-hash*)
                (gethash name (help::help-item-mhash it))
                (read-item it))
          (aand (gethash name help::*help-hash*)
                (read-item it))))))

(defun get-help-method-list (name)
  (let (acc)
    (dotimes (i (length help::*method-list*))
      (aif (gethash name (nth i help::*method-list*))
          (push (help::help-item-clas it) acc)))
    acc))

;; Properties list
(defun lambda-list (name)
  (let ((sym (find-callable-symbol name)))
    (if sym
        (or (some #'get-help (list-possible-symbol-names sym))
            (if (and (fboundp sym) (consp (symbol-function sym)))
                (cons sym (copy-list (cadr (symbol-function sym)))))
            (aif (assoc :function-documentation (send sym :plist))
                 (read-copy (cdr it) sym))))))

(defun method-lambda-list (obj name &optional instance-completion-p)
  (if (zerop (length name)) (return-from method-lambda-list nil))
  (let* ((sym (or (find-symbol (string-upcase (subseq name 1)) *keyword-package*)
                  (return-from method-lambda-list nil)))
         (method (or (if instance-completion-p
                         (aand (classp obj) (send obj :method sym) (list obj it))
                         (find-method obj sym))
                     (return-from method-lambda-list nil)))
         (class (car method))
         (def (second (second method)))
         (arglist (or
                   (get-help name class)
                   (if (listp def) (cons sym (copy-list def)))
                   (aif (assoc class (get sym :method-documentation))
                        (read-copy (cdr it) sym)))))
    (if (and (eq (second arglist) 'self) (eq (third arglist) 'class))
        (cons (car arglist) (cdddr arglist))
        arglist)))

(defun keyword-list (arglist &optional start)
  (let ((pos (or start (aand (position '&key arglist) (1+ it)))))
    (if pos
        (mapcar #'(lambda (val)
                    (let ((key (if (consp val) (car val) val)))
                      (if (consp key) ;; &key ((:keyword kwd) 10)
                          (format nil "~a" (car key))
                          (format nil ":~a" key))))
                (nthcdr pos arglist)))))

(defun method-list (obj &optional instance-completion-p)
  (let* ((class (or (if instance-completion-p
                        (and (classp obj) obj)
                        (class obj))
                    (return-from method-list nil)))
         (meths (mapcar #'car (send class :all-methods))))
    (mapcar #'(lambda (val) (format nil "~a" val))
            (remove-duplicates meths))))


;; CONNECTION-INFO
(defun implementation-version ()
  ;; Remove color from the console,
  ;; usually trailing from roseus init message
  (format t "~c[0m" #\Esc)
  (lisp-implementation-version))


;; REPL-PROMPT
(defvar last-prompt)
(defun slime-prompt ()
  (let ((prompt
         (string-right-trim (list #\space #\$ #\:)
          (with-output-to-string (s) (lisp::prompt s)))))
    (unless (equal prompt last-prompt)
      (setq last-prompt prompt)
      (list (package-name *package*) prompt))))


;; SWANK-SIMPLE-COMPLETIONS
(defun slime-find-symbol (start &optional package prefix)
  (let ((*package* (or (find-package package) *package*)))
    (multiple-value-bind (pack str external-p) (split-package start)
      (let ((len (length str))
            str-lst)
        (flet ((fill-str-lst (ext-p)
                 (slime-do-symbols (sym pack ext-p)
                   (let ((sym-str (maybe-downcase (send sym :pname))))
                     (when (string-equal str sym-str :start1 0 :end1 len :start2 0 :end2 len)
                       (push
                        (if (and external-p (not ext-p))
                            (concatenate string prefix (subseq start 0 (length pack)) "::" sym-str)
                            (concatenate string prefix (subseq start 0 (- (length start) len)) sym-str))
                        str-lst))))))

          ;; add possible package names
          (unless (or pack prefix)
            (mapc #'(lambda (p) (let ((name (package-name p)))
                                  (if (and (string-equal str name :end2 len)
                                           (> (p . intsymcount) 0))
                                      (push (format nil "~a:" (maybe-downcase name))
                                            str-lst))))
                  (list-all-packages)))

          ;; fill completions
          (fill-str-lst external-p)

          ;; if no external symbols are found search for internal ones
          (if (and (null str-lst) external-p)
              (fill-str-lst nil))

          ;; sort results and append common substring in the end
          (append-common (sort str-lst #'string<) (length start)))))))


;; SWANK-AUTODOC
(defun autodoc (name &optional cursor form)
  (let ((last (car (last form))))
    (cond
      ((not (stringp last))
       (function-autodoc name (1- cursor) form))
      ((string= last "")
       (function-autodoc name (1- cursor) (butlast form)))
      ((and (> cursor 1)
            (with-callable-symbol-value ((sym value) last)
              (unless (eq (symbol-package sym) *keyword-package*)
                (format nil "~a => ~s" last value)))))
      (t
       (function-autodoc name (1- cursor) form)))))

(defun function-autodoc (name &optional cursor form)
  (let* ((arglist
          (aif (method-lambda-list-from-form form)
               (prog1 it
                 (if cursor (decf cursor 2)))
               (lambda-list name)))
         (len (length arglist)))
    (when arglist
      (unless cursor (return-from function-autodoc arglist))
      (do ((i 0 (1+ i)))
          ((>= i (min len (1+ cursor))))  ;; evaluate every iteration
        (case (nth i arglist)
          (&rest
           (aif (position '&key arglist :start (1+ i))
                (return-from function-autodoc (set-keyword arglist it (car (last form))))
                (return-from function-autodoc (set-cursor arglist (1+ i)))))
          (&optional
           (incf cursor))
          (&key
           (return-from function-autodoc (set-keyword arglist i (car (last form)))))))
      (if (or (zerop cursor) (>= cursor len))
          arglist
          (set-cursor arglist cursor)))))

(defun method-lambda-list-from-form (form &optional send/instance)
  ;; form: '("send" "obj" ":selector" ...)
  (if (and (cddr form)
           (setq send/instance (or send/instance (is-send/instance-symbol (first form))))
           (stringp (second form))
           (stringp (third form)))
      (with-callable-symbol-value (value (cadr form))
        (method-lambda-list value (third form) (string= send/instance "instance")))))

(defun set-cursor (lst pos)
  ;; TODO: use string not symbols
  (list-insert (intern "<===" *package*) (1+ pos) lst)
  (list-insert (intern "===>" *package*) pos lst))

(defun set-keyword (arglist start item)
  (let* ((keys (keyword-list arglist start))
         (pos (and (stringp item)
                   (position item keys :test #'string-equal))))
    (if pos
        (set-cursor arglist (+ start pos))
        arglist)))


;; SWANK-COMPLETIONS-FOR-KEYWORD
(defun slime-find-keyword (start &optional form package)
  (if (zerop (length start)) (return-from slime-find-keyword nil))
  (let ((*package* (or (find-package package) *package*)))
    (cond
      ((null form)
       (slime-find-symbol (subseq start 1) *keyword-package* ":"))
      ((setq send/instance (is-send/instance-symbol (car form)))
       (if (stringp (cadr form))
           ;; Always return nil if a matching callable symbol is not found
           (with-callable-symbol-value (value (cadr form))
             (if (cddr form)
                 (let ((keys (keyword-list (method-lambda-list-from-form form send/instance))))
                   (find-prefix start keys 1))
                 (find-prefix start (method-list value (string= send/instance "instance")) 1)))
           ;; Complete as global symbol if object is an s-expression
           (slime-find-symbol (subseq start 1) *keyword-package* ":")))
      (t
       (let ((keys (keyword-list (lambda-list (car form)))))
         (find-prefix start keys 1))))))


;; SWANK-COMPLETIONS-FOR-CHARACTER
(defun slime-find-character (start)
  (find-prefix start *char-lst*))

(defconstant *char-lst*
  '("Space" "Newline" "Linefeed"
    "Backspace" "Delete" "Rubout"
    "Return" "Page" "Formfeed"
    "Esc" "Escape" "Tab"
    "Left-Paren" "Right-Paren" "Lparen" "Rparen"
    "Bell"  "Null" "SOH" "STX" "ETX"))


;; SWANK-DESCRIBE-SYMBOL
(defun slime-describe-symbol (name &optional package)
  (let* ((*package* (or (find-package package) *package*))
         (sym (find-callable-symbol name))
         (help-name-lst (list-possible-symbol-names sym)))
    (unless sym
      (error "symbol not found"))
    (with-output-to-string (s)
      (dolist (help-name help-name-lst)
        (when (gethash help-name help::*help-hash*)
          (help help-name nil s)
          (terpri s)
          (return))
        (dolist (mthd (get-help-method-list help-name))
          (when (aand (gethash mthd help::*help-hash*)
                      (gethash help-name (help::help-item-mhash it)))
            (format s "-- ~A --~%~%" (string-upcase mthd))
            (help help-name mthd s)
            (terpri s))))
      (format s "PROPERTIES~%~%")
      (describe sym s))))


;; SWANK-EXPAND-1
(defun slime-macroexpand (form &optional package)
  (let ((*package* (or (find-package package) *package*)))
    (with-output-to-string (s)
      (pprint (macroexpand2 (read-from-string form)) s))))


;; SWANK-LIST-ALL-PACKAGE-NAMES
(defun slime-all-packages (&optional nicknames)
  (if nicknames
      (apply #'append (mapcar #'(lambda (p) (package-names p)) (list-all-packages)))
      (mapcar #'package-name (list-all-packages))))


;; SWANK-APROPOS-LIST
(defun slime-apropos-list (key &optional package)
  ;; (:designator "SB-UNICODE:CHAR-BLOCK" :function "Returns the Unicode block in which CHARACTER resides as a keyword.")
  ;; (:designator "SB-UNIX:EWOULDBLOCK" :variable :not-documented)
  (labels ((get-props (sym)
             (cond
               ((fboundp sym)
                (list
                 (cond
                   ((special-form-p sym) :special-operator)
                   ((macro-function sym) :macro)
                   (t :function))
                 (aif (get sym :function-documentation)
                      (if (stringp it)
                          it
                          (format nil "~s" it))
                      :not-documented)))
               ((and (boundp sym) (not (keywordp sym)))
                (list
                 (if (classp sym)
                     :class
                     :variable)
                 (aif (get sym :variable-documentation)
                      (if (stringp it)
                          it
                          (format nil "~s" it))
                      :not-documented)))))
           (list-props (sym)
             (aif (get-props sym)
                  (list (list*
                         :designator
                         (string-upcase (format nil "~a" sym))
                         it)))))
    (sort
     (mapcan #'list-props (apropos-list key package))
     #'string< #'cadr)))


;; SWANK-FIND-TAG-NAME-FOR-EMACS
(defun find-tag-name-for-emacs (name &optional package)
  (let ((*package* (or (find-package package) *package*)))
    (let* ((sym (or (find-callable-symbol name)
                    (return-from find-tag-name-for-emacs nil)))
           (cname (get sym 'compiler::builtin-function-entry)))
      (if cname
          (list cname)
          (list-possible-symbol-names sym)))))

;; SWANK-LOAD-FILE-AND-TAGS
(defun load-file-and-tags (filename)
  (let* ((acc nil)
         (hook-fn #'(lambda (name) (aif (true-filename name) (push it acc))))
         (*load-hooks* (cons hook-fn *load-hooks*)))
    (load filename)
    (nreverse acc)))

;; SWANK-SET-PACKAGE
(defun set-package (name)
  (cond
    ((null-string-p name))
    ((find-package name)
     (in-package name))
    (t
     (error "No such package: ~S" name)))
  (or (slime-prompt)
      (list (package-name *package*) last-prompt)))


;; SWANK-CLEAR-REPL-VARIABLES
(defun clear-repl-variables ()
  (let ((variables '(*** ** * +++ ++ +)))
    (dolist (variable variables)
      (setf (symbol-value variable) nil))))


;; ROS SHORTCUTS
(defun rossetmaster (host port)
  (unix::putenv (format nil "ROS_MASTER_URI=http://~A:~D" host port))
  t)

(defun rossetip (hostname)
  (unix::putenv (format nil "ROS_IP=~A" hostname))
  (unix::putenv (format nil "ROS_HOSTNAME=~A" hostname))
  t)

