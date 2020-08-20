from eus import eus

class eus_color(eus):
    USE_COLOR = True

    def test_warning_message_1(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(warning-message 1 \\"Hello\\")\n") "USER" :repl-thread 8)',
            '(:read-string 0 1)',
            '(:write-string "[31mHello[0m")',
            '(:write-string "nil" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:read-aborted 0 1)',
            '(:return (:ok nil) 8)')

    def test_warning_message_2(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(warning-message 2 \\"Hello\\")\n") "USER" :repl-thread 9)',
            '(:read-string 0 1)',
            '(:write-string "[32mHello[0m")',
            '(:write-string "nil" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:read-aborted 0 1)',
            '(:return (:ok nil) 9)')

    def test_warning_message_3(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(warning-message 3 \\"~S~%\\" \\"HELLO\\")\n") "USER" :repl-thread 7)',
            '(:read-string 0 1)',
            '(:write-string "[33m\\"HELLO\\"\\n[0m")',
            '(:write-string "nil" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:read-aborted 0 1)',
            '(:return (:ok nil) 7)')

    def test_warning_message_4(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(warning-message 4 \\"~A~%\\" (1+ 1))\n") "USER" :repl-thread 8)',
            '(:read-string 0 1)',
            '(:write-string "[34m2\\n[0m")',
            '(:write-string "nil" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:read-aborted 0 1)',
            '(:return (:ok nil) 8)')
