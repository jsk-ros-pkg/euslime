from eus import eus

class eus_color(eus):
    USE_COLOR = True

    def test_warning_message_1(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(warning-message 1 \\"Hello\\")\n") "USER" :repl-thread 8)',
            '(:write-string "[31mHello[0m")',
            '(:write-string "nil" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 8)')

    def test_warning_message_2(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(warning-message 2 \\"Hello\\")\n") "USER" :repl-thread 9)',
            '(:write-string "[32mHello[0m")',
            '(:write-string "nil" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 9)')

    def test_warning_message_3(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(warning-message 3 \\"~S~%\\" \\"HELLO\\")\n") "USER" :repl-thread 7)',
            '(:write-string "[33m\\"HELLO\\"\\n[0m")',
            '(:write-string "nil" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 7)')

    def test_warning_message_4(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(warning-message 4 \\"~A~%\\" (1+ 1))\n") "USER" :repl-thread 8)',
            '(:write-string "[34m2\\n[0m")',
            '(:write-string "nil" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 8)')

    def test_sldb_color_1(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(warning-message 1 \\"Hello\\")\n") "USER" :repl-thread 11)',
            '(:write-string "[31mHello[0m")',
            '(:write-string "nil" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 11)')
        self.assertSocketIgnoreAddress(
            '(:emacs-rex (swank-repl:listener-eval "(1+ nil)\n") "USER" :repl-thread 13)',
            '(:debug 0 1 ("Integer expected in (1+ nil)" "" nil) (("QUIT" "Quit to the SLIME top level") ("CONTINUE" "Ignore the error and continue in the same stack level") ("RESTART" "Restart euslisp process")) ((0 "(1+ nil)" (:restartable nil)) (1 "(slime:slimetop)" (:restartable nil)) (2 "(slime:slimetop)" (:restartable nil)) (3 "#<compiled-code #X5a17290>" (:restartable nil))) (nil))')
        self.assertSocket(
            '(:emacs-rex (swank:invoke-nth-restart-for-emacs 1 2) "USER" 0 14)',
            '(:return (:abort nil) 14)',
            '(:new-package "USER" "{}")'.format(self.EUSLISP_PROGRAM_NAME),
            '(:debug-return 0 1 nil)',
            '(:return (:abort "\'Integer expected\'") 13)')
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(warning-message 2 \\"Hello\\")\n") "USER" :repl-thread 15)',
            '(:write-string "[32mHello[0m")',
            '(:write-string "nil" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 15)')
