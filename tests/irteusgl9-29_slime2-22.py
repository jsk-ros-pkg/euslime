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
