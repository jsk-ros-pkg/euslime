from irteusgl import irteusgl

class roseus(irteusgl):
    EUSLISP_PROGRAM = 'roseus'
    EUSLISP_PROGRAM_NAME = 'irteusgl'

    # TODO: regexp match to avoid comparing time stamp
    # def test_rosout_1(self):
    #     self.assertSocket(
    #         '(:emacs-rex (swank-repl:listener-eval "(ros::ros-debug \\"HERE\\")\n") "USER" :repl-thread 13)',
    #         '(:read-string 0 1)',
    #         '(:write-string "t" :repl-result)',
    #         '(:write-string "\\n" :repl-result)',
    #         '(:read-aborted 0 1)',
    #         '(:return (:ok nil) 13)')

    # def test_rosout_2(self):
    #     self.assertSocket(
    #         '(:emacs-rex (swank-repl:listener-eval "(ros::ros-info \\"HERE\\")\n") "USER" :repl-thread 6)',
    #         '(:read-string 0 1)',
    #         '(:write-string "[ INFO] [1556256025.220748554]: HERE\\n")',
    #         '(:write-string "t" :repl-result)',
    #         '(:write-string "\\n" :repl-result)',
    #         '(:read-aborted 0 1)',
    #         '(:return (:ok nil) 6)')

    # def test_rosout_3(self):
    #     self.assertSocket(
    #         '(:emacs-rex (swank-repl:listener-eval "(ros::ros-warn \\"HERE\\")\n") "USER" :repl-thread 9)',
    #         '(:read-string 0 1)',
    #         '(:write-string "[ WARN] [1556256034.693622389]: HERE\\n")',
    #         '(:write-string "t" :repl-result)',
    #         '(:write-string "\\n" :repl-result)',
    #         '(:read-aborted 0 1)',
    #         '(:return (:ok nil) 9)')

    # def test_rosout_4(self):
    #     self.assertSocket(
    #         '(:emacs-rex (swank-repl:listener-eval "(ros::ros-error \\"HERE\\")\n") "USER" :repl-thread 12)',
    #         '(:read-string 0 1)',
    #         '(:write-string "[ERROR] [1556256041.151062928]: HERE\\n")',
    #         '(:write-string "t" :repl-result)',
    #         '(:write-string "\\n" :repl-result)',
    #         '(:read-aborted 0 1)',
    #         '(:return (:ok nil) 12)')

    # def test_rosout_5(self):
    #     self.assertSocket(
    #         '(:emacs-rex (swank-repl:listener-eval "(ros::ros-fatal \\"THIS\\")\n") "USER" :repl-thread 6)',
    #         '(:read-string 0 1)',
    #         '(:write-string "[FATAL] [1556256284.796886986]: THIS\\n")',
    #         '(:write-string "t" :repl-result)',
    #         '(:write-string "\\n" :repl-result)',
    #         '(:read-aborted 0 1)',
    #         '(:return (:ok nil) 6)')
