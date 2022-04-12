from eus import eus

class irteusgl(eus):
    EUSLISP_PROGRAM = 'irteusgl'
    EUSLISP_PROGRAM_NAME = 'irteusgl'

    def test_completions_irt_1(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(progn (setq c (make-coords :name \\"test\\")) (send c :name))\n") "USER" :repl-thread 31)',
            '(:write-string "\\"test\\"" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 31)')
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":pos" (quote ("send" "c" "" swank::%cursor-marker%))) "USER" :repl-thread 35)',
            '(:return (:ok ((":pos") ":pos")) 35)')
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":" (quote ("send" "c" "" swank::%cursor-marker%))) "USER" :repl-thread 37)',
            '(:return (:ok ((":dimension" ":rot" ":pos" ":x-axis" ":y-axis" ":z-axis" ":newcoords" ":replace-rot" ":replace-pos" ":replace-coords" ":copy-rot" ":copy-pos" ":copy-coords" ":coords" ":worldrot" ":worldpos" ":worldcoords" ":copy-worldcoords" ":parentcoords" ":changed" ":reset-coords" ":move-to" ":rotate-vector" ":transform-vector" ":inverse-transform-vector" ":inverse-transformation" ":transformation" ":transform" ":rotate-with-matrix" ":rotate" ":orient-with-matrix" ":orient" ":parent-vector" ":parent-orientation" ":translate" ":locate" ":scale" ":euler" ":euler-angle" ":rpy" ":rpy-angle" ":rotation-angle" ":4x4" ":create" ":init" ":axis" ":difference-position" ":difference-rotation" ":move-coords" ":inverse-rotate-vector" ":vertices" ":draw-on" ":plist" ":get" ":put" ":name" ":remprop" ":prin1" ":warning" ":error" ":slots" ":methods" ":super" ":get-val" ":set-val") ":")) 37)')
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":world" (quote ("send" "c" "" swank::%cursor-marker%))) "USER" :repl-thread 39)',
            '(:return (:ok ((":worldrot" ":worldpos" ":worldcoords") ":world")) 39)')
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(makunbound \'c)\n") "USER" :repl-thread 45)',
            '(:write-string "t" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 45)')

    def test_completions_irt_2(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(progn (make-irtviewer) (send *irtviewer* :name))\n") "USER" :repl-thread 78)',
            '(:write-string "\\"IRT viewer\\"" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 78)')
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":draw" \'("send" "*irtviewer*" "" swank::%cursor-marker%)) "USER" :repl-thread 10)',
            '(:return (:ok ((":draw-event" ":draw-objects" ":draw-origin" ":draw-floor" ":drawable" ":draw-point" ":draw-string" ":draw-image-string" ":draw-rectangle" ":draw-fill-rectangle" ":draw-arc" ":draw-fill-arc" ":draw-lines" ":draw-polygon" ":drawline-primitive" ":draw-line") ":draw")) 10)')
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":view" \'("send" "*irtviewer*" "" swank::%cursor-marker%)) "USER" :repl-thread 14)',
            '(:return (:ok ((":viewer" ":viewtarget" ":viewpoint") ":view")) 14)')
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(send *irtviewer* :quit )\n") "USER" :repl-thread 95)',
            '(:write-string ":destroyed" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 95)')

    def test_autodoc_irt_1(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(progn (make-irtviewer) (send *irtviewer* :name))\n") "USER" :repl-thread 78)',
            '(:write-string "\\"IRT viewer\\"" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 78)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc (quote ("send" "*irtviewer*" ":redraw" swank::%cursor-marker%)) :print-right-margin 100) "USER" :repl-thread 85)',
            '(:return (:ok ("(:redraw)" t)) 85)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc (quote ("send" "*irtviewer*" ":viewtarget" "" swank::%cursor-marker%)) :print-right-margin 100) "USER" :repl-thread 87)',
            '(:return (:ok ("(:viewtarget &optional ===> p <===)" t)) 87)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc (quote ("send" "*irtviewer*" ":quit" swank::%cursor-marker%)) :print-right-margin 100) "USER" :repl-thread 93)',
            '(:return (:ok ("(:quit &rest args)" t)) 93)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc (quote ("send" "*irtviewer*" ":quit" "" swank::%cursor-marker%)) :print-right-margin 100) "USER" :repl-thread 94)',
            '(:return (:ok ("(:quit &rest ===> args <===)" t)) 94)')
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(send *irtviewer* :quit )\n") "USER" :repl-thread 95)',
            '(:write-string ":destroyed" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 95)')

    def test_read_irt_1(self):
        self.assertAsyncRequest(
            ['(:emacs-rex (swank-repl:listener-eval "(do-until-key)\n") "USER" :repl-thread 47)',
             '(:emacs-return-string 0 1 "\n")'],
            ['(:read-string 0 1)',
             '(:write-string "nil" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 47)'],
            rate_send=0.1)
        self.assertAsyncRequest(
            ['(:emacs-rex (swank-repl:listener-eval "(do-until-key)\n") "USER" :repl-thread 47)',
             '(:emacs-return-string 0 1 "a\n")'],
            ['(:read-string 0 1)',
             '(:write-string "nil" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 47)'],
            rate_send=0.1)

    def test_emacs_interrupt_irt_1(self):
        self.with_unwind_protect(
            (self.assertAsyncRequest,
             ['(:emacs-rex (swank-repl:listener-eval "(do-until-key)\n") "USER" :repl-thread 24)',
              '(:emacs-interrupt 0)'],
             ['(:read-string 0 1)',
              '(:read-aborted 0 1)',
              '(:new-package "USER" "B1-{}")'.format(self.EUSLISP_PROGRAM_NAME),
              '(:return (:abort "\'Keyboard Interrupt\'") 24)'],
             0.1),
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "(reset)\n") "USER" :repl-thread 6)',
             '(:new-package "USER" "{}")'.format(self.EUSLISP_PROGRAM_NAME),
             '(:return (:ok nil) 6)'))

    def test_sldb_irt_1(self):
        self.with_unwind_protect(
            (self.assertSocketIgnoreAddress,
             '(:emacs-rex (swank-repl:listener-eval "(let ((i 0)) (do-until-key (if (> (incf i) 10) (+ i nil))))\n") "USER" :repl-thread 24)',
             '(:read-string 0 1)',
             '(:read-aborted 0 1)',
             '(:debug 0 1 ("Number expected in (+ i nil)" "" nil) (("QUIT" "Quit to the SLIME top level") ("CONTINUE" "Ignore the error and continue in the same stack level") ("RESTART" "Restart euslisp process")) ((0 "(+ i nil)" (:restartable nil)) (1 "(if (> (incf i) 10) (+ i nil))" (:restartable nil)) (2 "(while (and (null (funcall slime::*old-select-stream* (list *standard-input*) 0.001)) (eval t)) (if (> (incf i) 10) (+ i nil)))" (:restartable nil)) (3 "(let ((#:prog1431 (while (and (null (funcall slime::*old-select-stream* (list *standard-input*) 0.001)) (eval t)) (if (> (incf i) 10) (+ i nil))))) (progn (let ((lisp::strm (car (funcall slime::*old-select-stream* (list *standard-input*) 0.1)))) (if lisp::strm (read-line (send slime::*old-terminal-io* :instream) nil nil)))) #:prog1431)" (:restartable nil)) (4 "(prog1 (while (and (null (funcall slime::*old-select-stream* (list *standard-input*) 0.001)) (eval t)) (if (> (incf i) 10) (+ i nil))) (let ((lisp::strm (car (funcall slime::*old-select-stream* (list *standard-input*) 0.1)))) (if lisp::strm (read-line (send slime::*old-terminal-io* :instream) nil nil))))" (:restartable nil)) (5 "(progn (slime:socket-request slime:*slime-stream* \\"read\\" nil) (prog1 (while (and (null (funcall slime::*old-select-stream* (list *standard-input*) 0.001)) (eval t)) (if (> (incf i) 10) (+ i nil))) (let ((lisp::strm (car (funcall slime::*old-select-stream* (list *standard-input*) 0.1)))) (if lisp::strm (read-line (send slime::*old-terminal-io* :instream) nil nil)))))" (:restartable nil)) (6 "(do-until-key-with-check t (if (> (incf i) 10) (+ i nil)))" (:restartable nil)) (7 "(do-until-key (if (> (incf i) 10) (+ i nil)))" (:restartable nil)) (8 "(let ((i 0)) (do-until-key (if (> (incf i) 10) (+ i nil))))" (:restartable nil)) (9 "(slime:slimetop)" (:restartable nil))) (nil))'),
            (self.assertSocket,
             '(:emacs-rex (swank:invoke-nth-restart-for-emacs 1 0) "USER" 0 25)',
             '(:return (:abort nil) 25)',
             '(:debug-return 0 1 nil)',
             '(:return (:abort "\'Number expected\'") 24)'))
