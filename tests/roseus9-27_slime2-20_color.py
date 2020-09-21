from roseus import roseus

class roseus_color(roseus):
    USE_COLOR = True

    def test_rosout_1(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(ros::ros-debug \\"THIS\\")\n") "USER" :repl-thread 10)',
            '(:write-string "t" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 10)')

    def test_rosout_2(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(ros::ros-info \\"THIS\\")\n") "USER" :repl-thread 12)',
            '(:write-string "\x1b[0m[ INFO]: THIS\x1b[0m\\n")',
            '(:write-string "t" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 12)')


    def test_rosout_3(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(ros::ros-warn \\"THIS\\")\n") "USER" :repl-thread 18)',
            '(:write-string "\x1b[33m[ WARN]: THIS\x1b[0m\\n")',
            '(:write-string "t" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 18)')

    def test_rosout_4(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(ros::ros-error \\"THIS\\")\n") "USER" :repl-thread 21)',
            '(:write-string "\x1b[31m[ERROR]: THIS\x1b[0m\\n")',
            '(:write-string "t" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 21)')

    def test_rosout_5(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(ros::ros-fatal \\"THIS\\")\n") "USER" :repl-thread 24)',
            '(:write-string "\x1b[31m[FATAL]: THIS\x1b[0m\\n")',
            '(:write-string "t" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 24)')
