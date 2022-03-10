from euslime_test_case import EuslimeTestCase
import os
import signal
import traceback

class eus(EuslimeTestCase):
    EUSLISP_PROGRAM = 'eus'
    EUSLISP_PROGRAM_NAME = 'eus'

    # LISTENER-EVAL
    def test_eval_1(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(list 1 2 3)\n") "USER" :repl-thread 8)',
            '(:write-string "(1 2 3)" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 8)')

    def test_eval_2(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(find 4 \'((1 . a) (2 . b) (3 . c) (4 . d) (4 . e)) :key #\'car)\n") "USER" :repl-thread 33)',
            '(:write-string "(4 . d)" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 33)')

    def test_eval_3(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "list 1 2 3\n") "USER" :repl-thread 5)',
            '(:write-string "(1 2 3)" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 5)')

    def test_eval_4(self):
        self.with_unwind_protect(
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "(progn (setq *package* *lisp-package*) (send *package* :name))\n") "USER" :repl-thread 30)',
             '(:write-string "\\"LISP\\"" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:new-package "LISP" "LISP:{}")'.format(self.EUSLISP_PROGRAM_NAME),
             '(:return (:ok nil) 30)'),
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "(progn (setq *package* *user-package*) (send *package* :name))\n") "LISP" :repl-thread 31)',
             '(:write-string "\\"USER\\"" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:new-package "USER" "{}")'.format(self.EUSLISP_PROGRAM_NAME),
             '(:return (:ok nil) 31)'))

    def test_eval_5(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(print \\"this\\")\n") "USER" :repl-thread 43)',
            '(:write-string "\\"this\\"\\n")',
            '(:write-string "\\"this\\"" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 43)')

    def test_eval_6(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(format nil \\"~S\\" \\"this\\")\n") "USER" :repl-thread 41)',
            '(:write-string "\\"\\\\\\"this\\\\\\"\\"" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 41)')

    def test_eval_7(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "#f(1 2 3)\n") "USER" :repl-thread 11)',
            '(:write-string "#f(1.0 2.0 3.0)" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 11)')

    def test_eval_8(self):
        self.with_unwind_protect(
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "(setq *print-case* :upcase)\n") "USER" :repl-thread 39)',
             '(:write-string ":UPCASE" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 39)'),
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "(list \'a \'b \'c)\n") "USER" :repl-thread 44)',
             '(:write-string "(A B C)" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 44)'),
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "(setq *print-case* :downcase)\n") "USER" :repl-thread 49)',
             '(:write-string ":downcase" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 49)'))

    def test_eval_9(self):
        self.with_unwind_protect(
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "(setq *print-level* 0)\n") "USER" :repl-thread 60)',
             '(:write-string "#" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 60)'),
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "10\n") "USER" :repl-thread 61)',
             '(:write-string "#" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 61)'),
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "(list 1 2 3)\n") "USER" :repl-thread 62)',
             '(:write-string "#" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 62)'),
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "(setq *print-level* nil)\n") "USER" :repl-thread 63)',
             '(:write-string "nil" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 63)'))

    def test_eval_10(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(reset)\n") "USER" :repl-thread 5)',
            '(:return (:ok nil) 5)')

    def test_eval_11(self):
        self.with_unwind_protect(
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "(assert nil)\n") "USER" :repl-thread 7)',
             '(:new-package "USER" "ass")',
             '(:return (:ok nil) 7)'),
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "(reset)\n") "USER" :repl-thread 8)',
             '(:new-package "USER" "{}")'.format(self.EUSLISP_PROGRAM_NAME),
             '(:return (:ok nil) 8)'))

    def test_eval_12(self):
        self.with_unwind_protect(
            (self.assertSocketIgnoreAddress,
             '(:emacs-rex (swank-repl:listener-eval "(lisp:none 10)\n") "USER" :repl-thread 12)',
             '(:write-string "LISP:NONE ")',
             '(:debug 0 1 ("No such external symbol LISP:NONE in (slime:slimetop)" "" nil) (("QUIT" "Quit to the SLIME top level") ("CONTINUE" "Ignore the error and continue in the same stack level") ("RESTART" "Restart euslisp process")) ((0 "(slime:slimetop)" (:restartable nil)) (1 "(slime:slimetop)" (:restartable nil)) (2 "#<compiled-code #X6147290>" (:restartable nil))) (nil))'),
            (self.assertSocket,
             '(:emacs-rex (swank:invoke-nth-restart-for-emacs 1 0) "USER" 0 13)',
             '(:return (:abort nil) 13)',
             '(:debug-return 0 1 nil)',
             '(:return (:abort "\'No such external symbol LISP:NONE\'") 12)'),
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "\n") "USER" :repl-thread 14)',
             '(:write-string "nil" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 14)'))

    def test_eval_13(self):
        self.assertAsyncRequest(
            ['(:emacs-rex (swank-repl:listener-eval "eus\n") "USER" :repl-thread 5)',
             '(:emacs-return-string 0 1 "(1+ 1)\n")',
             '(:emacs-return-string 0 1 "(quit)\n")'],
            ['(:read-string 0 1)',
             '(:read-string 0 1)',
             '(:write-string "2\\n")',
             '(:read-string 0 1)',
             '(:write-string "0" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:read-aborted 0 1)',
             '(:return (:ok nil) 5)'],
            unordered_output=True)

    # READ
    def test_read_1(self):
        # Both with and without slime-input-stream the behavior of toplevel read is ustable
        self.with_unwind_protect(
            (self.assertAsyncRequest,
             ['(:emacs-rex (swank-repl:listener-eval "(read)\n") "USER" :repl-thread 5)',
              '(:emacs-return-string 0 1 "hello\n")'],
             ['(:read-string 0 1)',
              '(:write-string "hello" :repl-result)',
              '(:write-string "\\n" :repl-result)',
              '(:return (:ok nil) 5)']),
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "10\n") "USER" :repl-thread 6)',
             '(:write-string "10" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 6)'),
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "(read-char)\n") "USER" :repl-thread 45)',
             '(:write-string "10" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 45)'))

    def test_read_2(self):
        self.assertAsyncRequest(
            ['(:emacs-rex (swank-repl:listener-eval "(y-or-n-p)\n") "USER" :repl-thread 21)',
             '(:emacs-return-string 0 1 "a\n")',
             '(:emacs-return-string 0 1 "y\n")'],
            ['(:write-string "(Y or N): ")',
             '(:read-string 0 1)',
             '(:write-string "(Y or N): ")',
             '(:read-string 0 1)',
             '(:write-string "t" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 21)'],
            rate_send=0.1)

    def test_read_3(self):
        self.with_unwind_protect(
            (self.assertAsyncRequest,
             ['(:emacs-rex (swank-repl:listener-eval "(read-char)\n") "USER" :repl-thread 44)',
              '(:emacs-return-string 0 1 "a\n")'],
             ['(:read-string 0 1)',
              '(:write-string "97" :repl-result)',
              '(:write-string "\\n" :repl-result)',
              '(:return (:ok nil) 44)']),
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "(read-char)\n") "USER" :repl-thread 45)',
             '(:write-string "10" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 45)'))

    def test_read_4(self):
        self.assertAsyncRequest(
            ['(:emacs-rex (swank-repl:listener-eval "(read-line)\n") "USER" :repl-thread 47)',
             '(:emacs-return-string 0 1 "hello world\n")'],
            ['(:read-string 0 1)',
             '(:write-string "\\"hello world\\"" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 47)'])

    def test_read_5(self):
        self.assertAsyncRequest(
            ['(:emacs-rex (swank-repl:listener-eval "(read-line t)\n") "USER" :repl-thread 47)',
             '(:emacs-return-string 0 1 "hello world\n")'],
            ['(:read-string 0 1)',
             '(:write-string "\\"hello world\\"" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 47)'])

    def test_read_6(self):
        self.assertAsyncRequest(
            ['(:emacs-rex (swank-repl:listener-eval "(read-line *standard-input*)\n") "USER" :repl-thread 47)',
             '(:emacs-return-string 0 1 "hello world\n")'],
            ['(:read-string 0 1)',
             '(:write-string "\\"hello world\\"" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 47)'])

    def test_read_7(self):
        # test for input which ends exactly at buffer end
        test_string = 'a'*127
        self.with_unwind_protect(
            (self.assertAsyncRequest,
             ['(:emacs-rex (swank-repl:listener-eval "(read-line)\n") "USER" :repl-thread 47)',
              '(:emacs-return-string 0 1 "{}")'.format(test_string + '\n')],
             ['(:read-string 0 1)',
              '(:write-string "\\"{}\\"" :repl-result)'.format(test_string),
              '(:write-string "\\n" :repl-result)',
              '(:return (:ok nil) 47)']),
            (self.assertAsyncRequest,
             ['(:emacs-rex (swank-repl:listener-eval "(read-line)\n") "USER" :repl-thread 47)',
              '(:emacs-return-string 0 1 "hello world\n")'],
             ['(:read-string 0 1)',
              '(:write-string "\\"hello world\\"" :repl-result)',
              '(:write-string "\\n" :repl-result)',
              '(:return (:ok nil) 47)']))

    def test_read_8(self):
        # test for input which exceeds buffer length
        test_string = 'a'*500
        self.assertAsyncRequest(
            ['(:emacs-rex (swank-repl:listener-eval "(read-line)\n") "USER" :repl-thread 47)',
             '(:emacs-return-string 0 1 "{}")'.format(test_string + '\n')],
            ['(:read-string 0 1)',
             '(:write-string "\\"{}\\"" :repl-result)'.format(test_string),
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 47)'])

    def test_unix_system_1(self):
        self.assertSocketPossibleResults(
            '(:emacs-rex (swank-repl:listener-eval "(unix:system \\"pwd\\")\n") "USER" :repl-thread 8)',
            ['(:read-string 0 1)',
             '(:write-string "{}\\n")'.format(os.getcwd()),
             '(:write-string "0" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:read-aborted 0 1)',
             '(:return (:ok nil) 8)'],
            ['(:write-string "{}\\n")'.format(os.getcwd()),
             '(:read-string 0 1)',
             '(:write-string "0" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:read-aborted 0 1)',
             '(:return (:ok nil) 8)'])

    def test_unix_system_2(self):
        self.assertAsyncRequest(
            ['(:emacs-rex (swank-repl:listener-eval "(unix:system \\"eus\\")\n") "USER" :repl-thread 6)',
             '(:emacs-return-string 0 1 "(1+ 1)\n")',
             '(:emacs-return-string 0 1 "(quit)\n")'],
            ['(:read-string 0 1)',
             '(:read-string 0 1)',
             '(:write-string "2\\n")',
             '(:read-string 0 1)',
             '(:write-string "0" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:read-aborted 0 1)',
             '(:return (:ok nil) 6)'],
            rate_send=0.1)

    def test_piped_fork_1(self):
        self.with_unwind_protect(
            (self.assertSocketIgnoreAddress,
             '(:emacs-rex (swank-repl:listener-eval "(setq s (piped-fork \\"pwd\\"))\n") "USER" :repl-thread 27)',
             '(:write-string "#<io-stream #X55db3aadc4a0>" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 27)'),
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "(read s nil)\n") "USER" :repl-thread 30)',
             '(:write-string "{}" :repl-result)'.format(os.getcwd()),
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 30)'),
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "(read s nil)\n") "USER" :repl-thread 31)',
             '(:write-string "nil" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 31)'),
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "(close s)\n") "USER" :repl-thread 33)',
             '(:write-string "t" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 33)'),
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "(makunbound \'s)\n") "USER" :repl-thread 38)',
             '(:write-string "t" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 38)'))

    def test_piped_fork_2(self):
        self.with_unwind_protect(
            (self.assertSocketIgnoreAddress,
             '(:emacs-rex (swank-repl:listener-eval "(setq s (piped-fork))\n") "USER" :repl-thread 27)',
             '(:write-string "#<io-stream #X55db3aadc4a0>" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 27)'),
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "(format s \\"(1+ 1)~%\\")\n") "USER" :repl-thread 30)',
             '(:write-string "nil" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 30)'),
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "(read s nil)~%") "USER" :repl-thread 31)',
             '(:write-string "2" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 31)'),
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "(format s \\"(quit)~%\\")\n") "USER" :repl-thread 32)',
             '(:write-string "nil" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 32)'),
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "(close s)\n") "USER" :repl-thread 33)',
             '(:write-string "t" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 33)'),
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "(makunbound \'s)\n") "USER" :repl-thread 38)',
             '(:write-string "t" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 38)'))

    # SIMULTANEOUS REQUESTS
    def test_async_1(self):
        self.assertAsyncRequest(
            ['(:emacs-rex (swank-repl:listener-eval "(unix:usleep 200000)\n") "USER" :repl-thread 17)',
             '(:emacs-rex (swank:apropos-list-for-emacs ":none" t nil (quote nil)) "USER" :repl-thread 18)'],
            ['(:return (:ok ()) 18)',
             '(:write-string "t" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 17)'])

    def test_async_2(self):
        self.assertAsyncRequest(
            ['(:emacs-rex (swank-repl:listener-eval "(unix:usleep 200000)\\n") "USER" :repl-thread 14)',
             '(:emacs-rex (swank:compile-string-for-emacs "(print \\"hello\\")" "test.l" \'((:position 1) (:line 1 1)) "/tmp/test.l" \'nil) "USER" t 15)'],
            ['(:write-string "t" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 14)',
             '(:write-string "\\"hello\\"\\n")',
             '(:write-string "; Loaded (print \\"hello\\")\\n")',
             '(:return (:ok (:compilation-result () t 0.01 nil nil)) 15)'])

    def test_async_3(self):
        self.assertAsyncRequest(
            ['(:emacs-rex (swank-repl:listener-eval "(unix:usleep 200000)\\n") "USER" :repl-thread 8)',
             '(:emacs-rex (swank:load-file "{}/test_async_3.l") "USER" :repl-thread 9)'.format(os.getcwd())],
            ['(:write-string "t" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 8)',
             '(:write-string "Loading file: {}/test_async_3.l ...\\n")'.format(os.getcwd()),
             '(:write-string "start\\n")',
             '(:write-string "end\\n")',
             '(:write-string "Loaded.\\n")',
             '(:return (:ok ("{}/test_async_3.l")) 9)'.format(os.getcwd())])

    def test_async_4(self):
        self.with_unwind_protect(
            (self.assertAsyncRequest,
             ['(:emacs-rex (swank-repl:listener-eval "(unix:usleep 200000)\n") "USER" :repl-thread 7)',
              '(:emacs-rex (swank:set-package "LISP") "USER" :repl-thread 9)'],
             ['(:write-string "t" :repl-result)',
              '(:write-string "\\n" :repl-result)',
              '(:return (:ok nil) 7)',
              '(:return (:ok ("LISP" "LISP:{}")) 9)'.format(self.EUSLISP_PROGRAM_NAME)]),
            (self.assertSocket,
             '(:emacs-rex (swank:set-package "USER") "LISP" :repl-thread 11)',
             '(:return (:ok ("USER" "{}")) 11)'.format(self.EUSLISP_PROGRAM_NAME)))

    def test_async_5(self):
        self.assertAsyncRequest(
            ['(:emacs-rex (swank-repl:listener-eval "(unix:usleep 200000)\\n") "USER" :repl-thread 5)',
             '(:emacs-rex (swank-repl:clear-repl-variables) "USER" :repl-thread 6)'],
            ['(:write-string "t" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 5)',
             '(:return (:ok ()) 6)'])
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(list * ** ***)\\n") "USER" :repl-thread 10)',
            '(:write-string "(nil nil nil)" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 10)')

    def test_async_6(self):
        self.assertAsyncRequest(
            ['(:emacs-rex (swank-repl:listener-eval "(dotimes (i 5) (print i) (unix:usleep 100000))\n") "USER" :repl-thread 5)',
             '(:emacs-rex (swank-repl:listener-eval "\'this\n") "USER" :repl-thread 6)',
             '(:emacs-rex (swank-repl:listener-eval "\'that\n") "USER" :repl-thread 7)',
             '(:emacs-rex (swank-repl:listener-eval "1\n") "USER" :repl-thread 8)',
             '(:emacs-rex (swank-repl:listener-eval "(1+ 1)\n") "USER" :repl-thread 9)'],
            ['(:write-string "0\\n")',
             '(:write-string "1\\n")',
             '(:write-string "2\\n")',
             '(:write-string "3\\n")',
             '(:write-string "4\\n")',
             '(:write-string "nil" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 5)',
             '(:write-string "this" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 6)',
             '(:write-string "that" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 7)',
             '(:write-string "1" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 8)',
             '(:write-string "2" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 9)'])

    def test_async_7(self):
        self.assertAsyncRequest(
            ['(:emacs-rex (swank-repl:listener-eval "(unix:usleep 200000)\n") "USER" :repl-thread 5)',
             '(:emacs-rex (swank-repl:listener-eval "(print 1)\n") "USER" :repl-thread 6)'],
            ['(:write-string "t" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 5)',
             '(:write-string "1\\n")',
             '(:write-string "1" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 6)'])

    # COMPILE REGION
    def test_compile_region_1(self):
        self.assertSocket(
            '(:emacs-rex (swank:compile-string-for-emacs "(defun foo (a) (1+ a))" "test.l" (quote ((:position 1) (:line 1 1))) "/tmp/test.l" (quote nil)) "USER" t 24)',
            '(:write-string "; Loaded (defun foo ...)\\n")',
            '(:return (:ok (:compilation-result () t 0.01 nil nil)) 24)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc (quote ("foo" "" swank::%cursor-marker%)) :print-right-margin 203) "USER" t 25)',
            '(:return (:ok ("(foo ===> a <===)" t)) 25)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc (quote ("foo" "1" swank::%cursor-marker%)) :print-right-margin 203) "USER" t 26)',
            '(:return (:ok ("(foo ===> a <===)" t)) 26)')
        self.assertSocket(
            '(:emacs-rex (swank:compile-string-for-emacs "(setq c (foo 1))" "test.l" (quote ((:position 25) (:line 3 1))) "/tmp/test.l" (quote nil)) "USER" t 29)',
            '(:write-string "; Loaded (setq c ...)\\n")',
            '(:return (:ok (:compilation-result () t 0.01 nil nil)) 29)')
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "c\n") "USER" :repl-thread 30)',
            '(:write-string "2" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 30)')
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(makunbound \'c)\n") "USER" :repl-thread 32)',
            '(:write-string "t" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 32)')

    def test_compile_region_2(self):
        self.assertSocket(
            '(:emacs-rex (swank:compile-string-for-emacs "(print 1)\n(setq c (list 1 2 3))\n" "test.l" (quote ((:position 1) (:line 1 1))) "/tmp/test.l" (quote nil)) "USER" t 14)',
            '(:write-string "1\\n")',
            '(:write-string "; Loaded (print 1)\\n")',
            '(:write-string "; Loaded (setq c ...)\\n")',
            '(:return (:ok (:compilation-result () t 0.01 nil nil)) 14)')
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "c\n") "USER" :repl-thread 15)',
            '(:write-string "(1 2 3)" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 15)')
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(makunbound \'c)\n") "USER" :repl-thread 17)',
            '(:write-string "t" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 17)')

    # AUTODOC
    def test_autodoc_function_1(self):
        self.assertSocket(
            '(:emacs-rex (swank:autodoc (quote ("list" "" swank::%cursor-marker%)) :print-right-margin 100) "USER" :repl-thread 5)',
            '(:return (:ok ("(list &rest ===> elements <===)" t)) 5)')

        self.assertSocket(
            '(:emacs-rex (swank:autodoc (quote ("list" "1" "" swank::%cursor-marker%)) :print-right-margin 100) "USER" :repl-thread 6)',
            '(:return (:ok ("(list &rest ===> elements <===)" t)) 6)')

        self.assertSocket(
            '(:emacs-rex (swank:autodoc (quote ("list" "1" "2" "" swank::%cursor-marker%)) :print-right-margin 100) "USER" :repl-thread 7)',
            '(:return (:ok ("(list &rest ===> elements <===)" t)) 7)')

    def test_autodoc_function_2(self):
        self.assertSocket(
            '(:emacs-rex (swank:autodoc (quote ("find" "" swank::%cursor-marker%)) :print-right-margin 100) "USER" :repl-thread 5)',
            '(:return (:ok ("(find ===> item <=== seq &key start end test test-not key (count 1))" t)) 5)')

        self.assertSocket(
            '(:emacs-rex (swank:autodoc (quote ("find" "4" "" swank::%cursor-marker%)) :print-right-margin 100) "USER" :repl-thread 6)',
            '(:return (:ok ("(find item ===> seq <=== &key start end test test-not key (count 1))" t)) 6)')

        self.assertSocket(
            '(:emacs-rex (swank:autodoc (quote ("find" "4" (("" swank::%cursor-marker%)))) :print-right-margin 100) "USER" :repl-thread 7)',
            '(:return (:ok (:not-available t)) 7)')

        self.assertSocket(
            '(:emacs-rex (swank:autodoc (quote ("find" "4" (("1" "." "a") ("2" "." "b") ("3" "." "c") ("4" "." "" swank::%cursor-marker%)))) :print-right-margin 100) "USER" :repl-thread 22)',
            '(:return (:ok (:not-available t)) 22)')

        self.assertSocket(
            '(:emacs-rex (swank:autodoc (quote ("find" "4" (("1" "." "a") ("2" "." "b") ("3" "." "c") ("4" "." "d") ("4" "." "e")) "" swank::%cursor-marker%)) :print-right-margin 100) "USER" :repl-thread 28)',
            '(:return (:ok ("(find item seq &key start end test test-not key (count 1))" t)) 28)')

        self.assertSocket(
            '(:emacs-rex (swank:autodoc (quote ("find" "4" (("1" "." "a") ("2" "." "b") ("3" "." "c") ("4" "." "d") ("4" "." "e")) ":key" "" swank::%cursor-marker%)) :print-right-margin 100) "USER" :repl-thread 29)',
            '(:return (:ok ("(find item seq &key start end test test-not ===> key <=== (count 1))" t)) 29)')

    def test_autodoc_function_3(self):
        self.assertSocket(
            '(:emacs-rex (swank:autodoc (quote ("print" "" swank::%cursor-marker%)) :print-right-margin 100) "USER" :repl-thread 30)',
            '(:return (:ok ("(print ===> obj <=== &optional stream)" t)) 30)')

        self.assertSocket(
            '(:emacs-rex (swank:autodoc (quote ("print" "\\"this\\"" "" swank::%cursor-marker%)) :print-right-margin 100) "USER" :repl-thread 31)',
            '(:return (:ok ("(print obj &optional ===> stream <===)" t)) 31)')

        self.assertSocket(
            '(:emacs-rex (swank:autodoc (quote ("print" "\\"this\\"" "nil" swank::%cursor-marker%)) :print-right-margin 100) "USER" :repl-thread 32)',
            '(:return (:ok ("(print obj &optional ===> stream <===)" t)) 32)')

    def test_autodoc_function_4(self):
        # compiled code without help entry
        self.assertSocket(
            '(:emacs-rex (swank:autodoc (quote ("deflocal" "" swank::%cursor-marker%)) :print-right-margin 80) "USER" :repl-thread 9)',
            '(:return (:ok ("(deflocal ===> var <=== &optional (init nil) (doc nil))" t)) 9)')

    def test_autodoc_function_5(self):
        # lisp form
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(defun foo (a b &rest args &key c &allow-other-keys)\n            (list a b c))\n") "USER" :repl-thread 20)',
            '(:write-string "foo" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 20)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc (quote ("foo" "" swank::%cursor-marker%)) :print-right-margin 100) "USER" :repl-thread 21)',
            '(:return (:ok ("(foo ===> a <=== b &rest args &key c &allow-other-keys)" t)) 21)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc (quote ("foo" "1" "" swank::%cursor-marker%)) :print-right-margin 100) "USER" :repl-thread 22)',
            '(:return (:ok ("(foo a ===> b <=== &rest args &key c &allow-other-keys)" t)) 22)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc (quote ("foo" "1" "2" "" swank::%cursor-marker%)) :print-right-margin 100) "USER" :repl-thread 23)',
            '(:return (:ok ("(foo a b &rest args &key c &allow-other-keys)" t)) 23)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc (quote ("foo" "1" "2" ":c" "" swank::%cursor-marker%)) :print-right-margin 100) "USER" :repl-thread 24)',
            '(:return (:ok ("(foo a b &rest args &key ===> c <=== &allow-other-keys)" t)) 24)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc (quote ("foo" "1" "2" ":c" "3" swank::%cursor-marker%)) :print-right-margin 100) "USER" :repl-thread 25)',
            '(:return (:ok ("(foo a b &rest args &key c &allow-other-keys)" t)) 25)')
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(foo 1 2 :c 3 :d 4)\n") "USER" :repl-thread 29)',
            '(:write-string "(1 2 3)" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 29)')

    def test_autodoc_symbol_1(self):
        self.assertSocket(
            '(:emacs-rex (swank:autodoc (quote ("list" "*prompt-string*" swank::%cursor-marker%)) :print-right-margin 100) "USER" :repl-thread 24)',
            '(:return (:ok ("*prompt-string* => \\"{}\\"" nil)) 24)'.format(self.EUSLISP_PROGRAM_NAME))

    def test_autodoc_symbol_2(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "1\n") "USER" :repl-thread 107)',
            '(:write-string "1" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 107)')
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "2\n") "USER" :repl-thread 108)',
            '(:write-string "2" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 108)')
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "3\n") "USER" :repl-thread 109)',
            '(:write-string "3" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 109)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("list" "*" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 111)',
            '(:return (:ok ("* => 3" nil)) 111)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("list" "*" "**" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 113)',
            '(:return (:ok ("** => 2" nil)) 113)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("list" "*" "**" "***" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 115)',
            '(:return (:ok ("*** => 1" nil)) 115)')
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(list * ** ***)\n") "USER" :repl-thread 116)',
            '(:write-string "(3 2 1)" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 116)')

    def test_autodoc_send_1(self):
        # from instance
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("send" "*lisp-package*" ":import" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 59)',
            '(:return (:ok ("(:import sym)" t)) 59)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("send" "*lisp-package*" ":import" "" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 60)',
            '(:return (:ok ("(:import ===> sym <===)" t)) 60)')

    def test_autodoc_send_2(self):
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("send" "*lisp-package*" "" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 127)',
            '(:return (:ok ("(send object ===> selector <=== &rest args)" t)) 127)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("send" "*lisp-package*" ":init" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 128)',
            '(:return (:ok ("(send object ===> selector <=== &rest args)" t)) 128)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("send" "*lisp-package*" ":none" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 119)',
            '(:return (:ok ("(send object ===> selector <=== &rest args)" t)) 119)')

    def test_autodoc_send_3(self):
        # from class
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("send" "symbol" ":method" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 63)',
            '(:return (:ok ("(:method name)" t)) 63)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("send" "symbol" ":method" "" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 64)',
            '(:return (:ok ("(:method ===> name <===)" t)) 64)')

    def test_autodoc_send_4(self):
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("send" "symbol" ":" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 132)',
            '(:return (:ok ("(send object ===> selector <=== &rest args)" t)) 132)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("send" "symbol" ":init" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 133)',
            '(:return (:ok ("(send object ===> selector <=== &rest args)" t)) 133)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("send" "symbol" ":none" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 134)',
            '(:return (:ok ("(send object ===> selector <=== &rest args)" t)) 134)')

    def test_autodoc_send_5(self):
        # with help
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(progn (setq c (make-coords :name \\"test\\")) (send c :name))\n") "USER" :repl-thread 25)',
            '(:write-string "\\"test\\"" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 25)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc (quote ("send" "c" ":pos" swank::%cursor-marker%)) :print-right-margin 100) "USER" :repl-thread 30)',
            '(:return (:ok ("(:pos)" t)) 30)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc (quote ("send" "c" ":replace-pos" "" swank::%cursor-marker%)) :print-right-margin 100) "USER" :repl-thread 37)',
            '(:return (:ok ("(:replace-pos ===> p <===)" t)) 37)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("send" "c" ":compile" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 39)',
            '(:return (:ok ("(send object ===> selector <=== &rest args)" t)) 39)')
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(makunbound \'c)\n") "USER" :repl-thread 42)',
            '(:write-string "t" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 42)')

    def test_autodoc_send_6(self):
        # from lisp form
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(defclass test_send)\n") "USER" :repl-thread 165)',
            '(:write-string "test_send" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 165)')
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(defmethod test_send (:foo (a b c)) (:bar ()))\n") "USER" :repl-thread 181)',
            '(:write-string "test_send" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 181)')
        self.assertSocketIgnoreAddress(
            '(:emacs-rex (swank-repl:listener-eval "(setq c (instance test_send))\n") "USER" :repl-thread 185)',
            '(:write-string "#<test_send #X5c48b48>" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 185)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("send" "c" ":foo" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 188)',
            '(:return (:ok ("(:foo a b c)" t)) 188)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("send" "c" ":bar" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 189)',
            '(:return (:ok ("(:bar)" t)) 189)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("send" "c" ":init" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 190)',
            '(:return (:ok ("(send object ===> selector <=== &rest args)" t)) 190)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("send" "c" ":none" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 193)',
            '(:return (:ok ("(send object ===> selector <=== &rest args)" t)) 193)')
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(makunbound \'c)\n") "USER" :repl-thread 42)',
            '(:write-string "t" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 42)')

    def test_autodoc_send_7(self):
        # from unbound object
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(boundp \'unbound)\n") "USER" :repl-thread 204)',
            '(:write-string "nil" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 204)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("send" "unbound" ":init" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 198)',
            '(:return (:ok ("(send object ===> selector <=== &rest args)" t)) 198)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("send" "unbound" ":name" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 199)',
            '(:return (:ok ("(send object ===> selector <=== &rest args)" t)) 199)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("send" "unbound" ":compile" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 200)',
            '(:return (:ok ("(send object ===> selector <=== &rest args)" t)) 200)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("send" "unbound" ":none" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 201)',
            '(:return (:ok ("(send object ===> selector <=== &rest args)" t)) 201)')

    def test_autodoc_send_8(self):
        # from expression
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("send" ("make-coords" ":name" "\"test\"") ":pos" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 16)',
            '(:return (:ok ("(send object ===> selector <=== &rest args)" t)) 16)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("send" ("make-coords" ":name" "\"test\"") ":replace-pos" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 17)',
            '(:return (:ok ("(send object ===> selector <=== &rest args)" t)) 17)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("send" ("make-coords" ":name" "\"test\"") ":compile" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 23)',
            '(:return (:ok ("(send object ===> selector <=== &rest args)" t)) 23)')

    def test_autodoc_instance_1(self):
        # from class
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("instance" "symbol" ":init" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 26)',
            '(:return (:ok ("(:init pn &optional (vt 1))" t)) 26)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("instance" "symbol" ":init" "" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 27)',
            '(:return (:ok ("(:init ===> pn <=== &optional (vt 1))" t)) 27)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("instance" "geo::coordinates" ":init" "" swank::%cursor-marker%) :print-right-margin 80) "USER" :repl-thread 11)',
            '(:return (:ok ("(:init &key (pos #f(0.0 0.0 0.0)) (rot #2f((1.0 0.0 0.0) (0.0 1.0 0.0) (0.0 0.0 1.0))) rpy euler axis angle 4x4 coords properties name)" t)) 11)')

    def test_autodoc_instance_2(self):
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("instance" "symbol" ":" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 10)',
            '(:return (:ok ("(instance class &rest ===> message <===)" t)) 10)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("instance" "symbol" ":method" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 11)',
            '(:return (:ok ("(instance class &rest ===> message <===)" t)) 11)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("instance" "symbol" ":none" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 12)',
            '(:return (:ok ("(instance class &rest ===> message <===)" t)) 12)')

    def test_autodoc_instance_3(self):
        # from instance
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("instance" "*lisp-package*" ":import" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 7)',
            '(:return (:ok ("(instance class &rest ===> message <===)" t)) 7)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("instance" "*lisp-package*" ":init" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 17)',
            '(:return (:ok ("(instance class &rest ===> message <===)" t)) 17)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("instance" "*lisp-package*" ":none" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 18)',
            '(:return (:ok ("(instance class &rest ===> message <===)" t)) 18)')

    def test_autodoc_instance_4(self):
        # with help
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("instance" "geo::coordinates" ":pos" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 39)',
            '(:return (:ok ("(:pos)" t)) 39)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("instance" "geo::coordinates" ":replace-pos" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 40)',
            '(:return (:ok ("(:replace-pos p)" t)) 40)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("instance" "geo::coordinates" ":compile" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 41)',
            '(:return (:ok ("(instance class &rest ===> message <===)" t)) 41)')

    def test_autodoc_instance_5(self):
        # from lisp form
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(defclass test_send)\n") "USER" :repl-thread 165)',
            '(:write-string "test_send" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 165)')
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(defmethod test_send (:foo (a b c)) (:bar ()))\n") "USER" :repl-thread 181)',
            '(:write-string "test_send" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 181)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("instance" "test_send" ":foo" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 47)',
            '(:return (:ok ("(:foo a b c)" t)) 47)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("instance" "test_send" ":bar" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 48)',
            '(:return (:ok ("(:bar)" t)) 48)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("instance" "test_send" ":init" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 49)',
            '(:return (:ok ("(instance class &rest ===> message <===)" t)) 49)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("instance" "test_send" ":none" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 50)',
            '(:return (:ok ("(instance class &rest ===> message <===)" t)) 50)')

    def test_autodoc_instance_6(self):
        # unbound object
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(boundp \'unbound)\n") "USER" :repl-thread 204)',
            '(:write-string "nil" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 204)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("instance" "unbound" ":init" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 54)',
            '(:return (:ok ("(instance class &rest ===> message <===)" t)) 54)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("instance" "unbound" ":name" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 55)',
            '(:return (:ok ("(instance class &rest ===> message <===)" t)) 55)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("instance" "unbound" ":compile" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 56)',
            '(:return (:ok ("(instance class &rest ===> message <===)" t)) 56)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("instance" "unbound" ":none" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 57)',
            '(:return (:ok ("(instance class &rest ===> message <===)" t)) 57)')

    def test_autodoc_instance_7(self):
        # expression
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("instance" ("class" ("make-coords")) ":pos" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 75)',
            '(:return (:ok ("(instance class &rest ===> message <===)" t)) 75)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("instance" ("class" ("make-coords")) ":replace-pos" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 76)',
            '(:return (:ok ("(instance class &rest ===> message <===)" t)) 76)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("instance" ("class" ("make-coords")) ":compile" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 77)',
            '(:return (:ok ("(instance class &rest ===> message <===)" t)) 77)')

    def test_autodoc_package_1(self):
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("sys:list-all-bindings" "" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 6)',
            '(:return (:ok ("(system:list-all-bindings)" t)) 6)')
        self.assertSocketSameResult(
            '(:emacs-rex (swank:autodoc \'("si:list-all-bindings" "" swank::%cursor-marker%) :print-right-margin 80) "USER" :repl-thread 27)',
            '(:emacs-rex (swank:autodoc \'("sys:list-all-bindings" "" swank::%cursor-marker%) :print-right-margin 80) "USER" :repl-thread 27)',
            '(:emacs-rex (swank:autodoc \'("system:list-all-bindings" "" swank::%cursor-marker%) :print-right-margin 80) "USER" :repl-thread 27)',
            '(:emacs-rex (swank:autodoc \'("si::list-all-bindings" "" swank::%cursor-marker%) :print-right-margin 80) "USER" :repl-thread 27)',
            '(:emacs-rex (swank:autodoc \'("sys::list-all-bindings" "" swank::%cursor-marker%) :print-right-margin 80) "USER" :repl-thread 27)',
            '(:emacs-rex (swank:autodoc \'("system::list-all-bindings" "" swank::%cursor-marker%) :print-right-margin 80) "USER" :repl-thread 27)')

    def test_autodoc_package_2(self):
        self.with_unwind_protect(
            (self.assertSocket,
             '(:emacs-rex (swank:set-package "KEYWORD") "USER" :repl-thread 6)',
             '(:return (:ok ("KEYWORD" "KEYWORD:{}")) 6)'.format(self.EUSLISP_PROGRAM_NAME)),
            (self.assertSocket,
             '(:emacs-rex (swank:autodoc \'("set-matrix-column" "" swank::%cursor-marker%) :print-right-margin 100) "KEYWORD" :repl-thread 11)',
             '(:return (:ok (:not-available t)) 11)'),
            (self.assertSocket,
             '(:emacs-rex (swank:set-package "USER") "KEYWORD" :repl-thread 13)',
             '(:return (:ok ("USER" "{}")) 13)'.format(self.EUSLISP_PROGRAM_NAME)),
            (self.assertSocket,
             '(:emacs-rex (swank:autodoc \'("set-matrix-column" "" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 14)',
             '(:return (:ok ("(set-matrix-column ===> mat <=== col values)" t)) 14)'))

    def test_autodoc_package_3(self):
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("lisp::setf-expand" "" swank::%cursor-marker%) :print-right-margin 80) "USER" :repl-thread 12)',
            '(:return (:ok ("(lisp::setf-expand ===> l <===)" t)) 12)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("setf-expand" "" swank::%cursor-marker%) :print-right-margin 80) "USER" :repl-thread 8)',
            '(:return (:ok (:not-available t)) 8)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc \'("lisp:setf-expand" "" swank::%cursor-marker%) :print-right-margin 80) "USER" :repl-thread 9)',
            '(:return (:ok (:not-available t)) 9)')

    def test_autodoc_package_4(self):
        self.assertSocketSameResult(
            '(:emacs-rex (swank:autodoc \'("send" "*lisp-package*" ":import" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 59)',
            '(:emacs-rex (swank:autodoc \'("lisp:send" "*lisp-package*" ":import" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 59)',
            '(:emacs-rex (swank:autodoc \'("lisp::send" "*lisp-package*" ":import" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 59)')
        self.assertSocketSameResult(
            '(:emacs-rex (swank:autodoc \'("send" "*lisp-package*" ":import" "" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 60)',
            '(:emacs-rex (swank:autodoc \'("lisp:send" "*lisp-package*" ":import" "" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 60)',
            '(:emacs-rex (swank:autodoc \'("lisp::send" "*lisp-package*" ":import" "" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 60)')

    def test_autodoc_package_5(self):
        self.assertSocketSameResult(
            '(:emacs-rex (swank:autodoc \'("instance" "symbol" ":init" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 26)',
            '(:emacs-rex (swank:autodoc \'("lisp:instance" "symbol" ":init" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 26)',
            '(:emacs-rex (swank:autodoc \'("lisp::instance" "symbol" ":init" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 26)')
        self.assertSocketSameResult(
            '(:emacs-rex (swank:autodoc \'("instance" "symbol" ":init" "" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 27)',
            '(:emacs-rex (swank:autodoc \'("lisp:instance" "symbol" ":init" "" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 27)',
            '(:emacs-rex (swank:autodoc \'("lisp::instance" "symbol" ":init" "" swank::%cursor-marker%) :print-right-margin 100) "USER" :repl-thread 27)')

    # COMPLETIONS
    def test_completions_symbol_1(self):
        self.assertSocket(
            '(:emacs-rex (swank:completions "find-if" (quote "USER")) "USER" :repl-thread 11)',
            '(:return (:ok (("find-if" "find-if-not") "find-if")) 11)')

    def test_completions_symbol_2(self):
        self.assertSocket(
            '(:emacs-rex (swank:completions "find-if-no" (quote "USER")) "USER" :repl-thread 13)',
            '(:return (:ok (("find-if-not") "find-if-not")) 13)')

    def test_completions_symbol_3(self):
        self.assertSocket(
            '(:emacs-rex (swank:completions "*er" (quote "USER")) "USER" :repl-thread 5)',
            '(:return (:ok (("*error-handler*" "*error-output*") "*error-")) 5)')

    def test_completions_symbol_4(self):
        self.assertSocket(
            '(:emacs-rex (swank:completions "*prompt" (quote "USER")) "USER" :repl-thread 20)',
            '(:return (:ok (("*prompt*" "*prompt-string*") "*prompt")) 20)')

    def test_completions_symbol_5(self):
        self.assertSocket(
            '(:emacs-rex (swank:completions "*prompt-" (quote "USER")) "USER" :repl-thread 23)',
            '(:return (:ok (("*prompt-string*") "*prompt-string*")) 23)')

    def test_completions_symbol_6(self):
        self.assertSocket(
            '(:emacs-rex (swank:completions ":none" (quote "USER")) "USER" :repl-thread 27)',
            '(:return (:ok ()) 27)')

    def test_completions_keyword_1(self):
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":test" (quote ("find" "1" ("list" "1" "2" "3") "" swank::%cursor-marker%))) "USER" :repl-thread 17)',
            '(:return (:ok ((":test" ":test-not") ":test")) 17)')

    def test_completions_keyword_2(self):
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":test-n" (quote ("find" "1" ("list" "1" "2" "3") "" swank::%cursor-marker%))) "USER" :repl-thread 19)',
            '(:return (:ok ((":test-not") ":test-not")) 19)')

    def test_completions_keyword_3(self):
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":count" (quote ("find" "1" ("list" "1" "2" "3") ":test-not" "" swank::%cursor-marker%))) "USER" :repl-thread 22)',
            '(:return (:ok ((":count") ":count")) 22)')

    def test_completions_keyword_4(self):
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":none" (quote ("find" "1" ("list" "1" "2" "3") ":test-not" ":count" "" swank::%cursor-marker%))) "USER" :repl-thread 26)',
            '(:return (:ok ()) 26)')

    def test_completions_keyword_5(self):
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":test" (quote nil)) "USER" :repl-thread 5)',
            '(:return (:ok ((":test" ":test-not") ":test")) 5)')

    def test_completions_keyword_6(self):
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":test" (quote nil)) "USER" :repl-thread 47)',
            '(:return (:ok ((":test" ":test-not") ":test")) 47)')

    def test_completions_character_1(self):
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-character "Ne") "USER" :repl-thread 19)',
            '(:return (:ok (("Newline") "Newline")) 19)')

    def test_completions_character_2(self):
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-character "") "USER" :repl-thread 20)',
            '(:return (:ok (("Space" "Newline" "Linefeed" "Backspace" "Delete" "Rubout" "Return" "Page" "Formfeed" "Esc" "Escape" "Tab" "Left-Paren" "Right-Paren" "Lparen" "Rparen" "Bell" "Null" "SOH" "STX" "ETX") "")) 20)')

    def test_completions_send_1(self):
        # from instance
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":sl" \'("send" "*lisp-package*" "" swank::%cursor-marker%)) "USER" :repl-thread 32)',
            '(:return (:ok ((":slots") ":slots")) 32)')
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":ini" \'("send" "*lisp-package*" "" swank::%cursor-marker%)) "USER" :repl-thread 33)',
            '(:return (:ok ()) 33)')

    def test_completions_send_2(self):
        # from class
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":all-m" \'("send" "geo::coordinates" "" swank::%cursor-marker%)) "USER" :repl-thread 44)',
            '(:return (:ok ((":all-methods" ":all-method-names") ":all-method")) 44)')
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":ini" \'("send" "geo::coordinates" "" swank::%cursor-marker%)) "USER" :repl-thread 45)',
            '(:return (:ok ()) 45)')

    def test_completions_send_3(self):
        # for bound object
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(progn (setq c (make-coords)) t)\n") "USER" :repl-thread 36)',
            '(:write-string "t" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 36)')
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":worldp" \'("send" "c" "" swank::%cursor-marker%)) "USER" :repl-thread 41)',
            '(:return (:ok ((":worldpos") ":worldpos")) 41)')
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":world" \'("send" "c" "" swank::%cursor-marker%)) "USER" :repl-thread 46)',
            '(:return (:ok ((":worldrot" ":worldpos" ":worldcoords") ":world")) 46)')
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":world-" \'("send" "c" "" swank::%cursor-marker%)) "USER" :repl-thread 53)',
            '(:return (:ok ()) 53)')
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(makunbound \'c)\n") "USER" :repl-thread 56)',
            '(:write-string "t" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 56)')

    def test_completions_send_4(self):
        # from unbound object
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(boundp \'unbound)\n") "USER" :repl-thread 204)',
            '(:write-string "nil" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 204)')
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":worldp" \'("send" "unbound" "" swank::%cursor-marker%)) "USER" :repl-thread 8)',
            '(:return (:ok ()) 8)')
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":world" \'("send" "unbound" "" swank::%cursor-marker%)) "USER" :repl-thread 9)',
            '(:return (:ok ()) 9)')
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":world-" \'("send" "unbound" "" swank::%cursor-marker%)) "USER" :repl-thread 11)',
            '(:return (:ok ()) 11)')

    def test_completions_send_5(self):
        # from expression
        # intern keyword to unify eus, irteus, roseus
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(intern \\"WORLD-CENTROID\\" *keyword-package*)\n") "USER" :repl-thread 10)',
            '(:write-string ":world-centroid" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 10)')
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":worldp" \'("send" ("make-coords") "" swank::%cursor-marker%)) "USER" :repl-thread 61)',
            '(:return (:ok ((":worldpos") ":worldpos")) 61)')
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":world" \'("send" ("make-coords") "" swank::%cursor-marker%)) "USER" :repl-thread 62)',
            '(:return (:ok ((":world" ":world-centroid" ":worldcoords" ":worldpos" ":worldrot") ":world")) 62)')
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":world-" \'("send" ("make-coords") "" swank::%cursor-marker%)) "USER" :repl-thread 63)',
            '(:return (:ok ((":world-centroid") ":world-centroid")) 63)')

    def test_completions_send_6(self):
        # with help
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(progn (setq c (make-coords)) t)\n") "USER" :repl-thread 36)',
            '(:write-string "t" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 36)')
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":po" \'("send" "c" ":init" "" swank::%cursor-marker%)) "USER" :repl-thread 14)',
            '(:return (:ok ((":pos") ":pos")) 14)')
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":r" \'("send" "c" ":init" "" swank::%cursor-marker%)) "USER" :repl-thread 17)',
            '(:return (:ok ((":rot" ":rpy") ":r")) 17)')
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":non" \'("send" "c" ":init" "" swank::%cursor-marker%)) "USER" :repl-thread 23)',
            '(:return (:ok ()) 23)')
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(makunbound \'c)\n") "USER" :repl-thread 56)',
            '(:write-string "t" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 56)')

    def test_completions_send_7(self):
        # from compiled symbol
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(setq p #P\\"/tmp\\")\n") "USER" :repl-thread 24)',
            '(:write-string "#P\\"/tmp\\"" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 24)')
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":hos" \'("send" "p" ":init" "" swank::%cursor-marker%)) "USER" :repl-thread 49)',
            '(:return (:ok ((":host") ":host")) 49)')
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":na" \'("send" "p" ":init" "" swank::%cursor-marker%)) "USER" :repl-thread 52)',
            '(:return (:ok ((":name") ":name")) 52)')
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":non" \'("send" "p" ":init" "" swank::%cursor-marker%)) "USER" :repl-thread 53)',
            '(:return (:ok ()) 53)')
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(makunbound \'p)\n") "USER" :repl-thread 56)',
            '(:write-string "t" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 56)')

    def test_completions_send_8(self):
        # from lisp form
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(defclass test_send)\n") "USER" :repl-thread 71)',
            '(:write-string "test_send" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 71)')
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(defmethod test_send (:foo (a b c)) (:bar ()) (:foobar (&key this that test)))\n") "USER" :repl-thread 72)',
            '(:write-string "test_send" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 72)')
        self.assertSocketIgnoreAddress(
            '(:emacs-rex (swank-repl:listener-eval "(setq c (instance test_send))\n") "USER" :repl-thread 76)',
            '(:write-string "#<test_send #X63e58e8>" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 76)')
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":tes" \'("send" "c" ":foobar" "" swank::%cursor-marker%)) "USER" :repl-thread 83)',
            '(:return (:ok ((":test") ":test")) 83)')
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":t" \'("send" "c" ":foobar" "" swank::%cursor-marker%)) "USER" :repl-thread 84)',
            '(:return (:ok ((":this" ":that" ":test") ":t")) 84)')
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":non" \'("send" "c" ":foobar" "" swank::%cursor-marker%)) "USER" :repl-thread 86)',
            '(:return (:ok ()) 86)')
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(makunbound \'c)\n") "USER" :repl-thread 56)',
            '(:write-string "t" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 56)')

    def test_completions_instance_1(self):
        # from class
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":worldp" \'("instance" "geo::coordinates" "" swank::%cursor-marker%)) "USER" :repl-thread 41)',
            '(:return (:ok ((":worldpos") ":worldpos")) 41)')
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":world" \'("instance" "geo::coordinates" "" swank::%cursor-marker%)) "USER" :repl-thread 46)',
            '(:return (:ok ((":worldrot" ":worldpos" ":worldcoords") ":world")) 46)')
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":world-" \'("instance" "geo::coordinates" "" swank::%cursor-marker%)) "USER" :repl-thread 53)',
            '(:return (:ok ()) 53)')

    def test_completions_instance_2(self):
        # from instance
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":sl" \'("instance" "*lisp-package*" "" swank::%cursor-marker%)) "USER" :repl-thread 32)',
            '(:return (:ok ()) 32)')
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":ini" \'("instance" "*lisp-package*" "" swank::%cursor-marker%)) "USER" :repl-thread 33)',
            '(:return (:ok ()) 33)')

    def test_completions_instance_3(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(boundp \'unbound)\n") "USER" :repl-thread 204)',
            '(:write-string "nil" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 204)')
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":worldp" \'("instance" "unbound" "" swank::%cursor-marker%)) "USER" :repl-thread 8)',
            '(:return (:ok ()) 8)')
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":world" \'("instance" "unbound" "" swank::%cursor-marker%)) "USER" :repl-thread 9)',
            '(:return (:ok ()) 9)')
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":world-" \'("instance" "unbound" "" swank::%cursor-marker%)) "USER" :repl-thread 11)',
            '(:return (:ok ()) 11)')

    def test_completions_instance_4(self):
        # from expression
        # intern keyword to unify eus, irteus, roseus
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(intern \\"WORLD-CENTROID\\" *keyword-package*)\n") "USER" :repl-thread 10)',
            '(:write-string ":world-centroid" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 10)')
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":worldp" \'("instance" ("class" ("make-coords")) "" swank::%cursor-marker%)) "USER" :repl-thread 61)',
            '(:return (:ok ((":worldpos") ":worldpos")) 61)')
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":world" \'("instance" ("class" ("make-coords")) "" swank::%cursor-marker%)) "USER" :repl-thread 62)',
            '(:return (:ok ((":world" ":world-centroid" ":worldcoords" ":worldpos" ":worldrot") ":world")) 62)')
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":world-" \'("instance" ("class" ("make-coords")) "" swank::%cursor-marker%)) "USER" :repl-thread 63)',
            '(:return (:ok ((":world-centroid") ":world-centroid")) 63)')

    def test_completions_instance_5(self):
        # with help
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":po" \'("instance" "geo::coordinates" ":init" "" swank::%cursor-marker%)) "USER" :repl-thread 14)',
            '(:return (:ok ((":pos") ":pos")) 14)')
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":r" \'("instance" "geo::coordinates" ":init" "" swank::%cursor-marker%)) "USER" :repl-thread 17)',
            '(:return (:ok ((":rot" ":rpy") ":r")) 17)')
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":non" \'("instance" "geo::coordinates" ":init" "" swank::%cursor-marker%)) "USER" :repl-thread 23)',
            '(:return (:ok ()) 23)')

    def test_completions_instance_6(self):
        # from compiled symbol
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":hos" \'("instance" "pathname" ":init" "" swank::%cursor-marker%)) "USER" :repl-thread 49)',
            '(:return (:ok ((":host") ":host")) 49)')
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":na" \'("instance" "pathname" ":init" "" swank::%cursor-marker%)) "USER" :repl-thread 52)',
            '(:return (:ok ((":name") ":name")) 52)')
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":non" \'("instance" "pathname" ":init" "" swank::%cursor-marker%)) "USER" :repl-thread 53)',
            '(:return (:ok ()) 53)')

    def test_completions_instance_7(self):
        # from lisp form
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(defclass test_send)\n") "USER" :repl-thread 71)',
            '(:write-string "test_send" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 71)')
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(defmethod test_send (:foo (a b c)) (:bar ()) (:foobar (&key this that test)))\n") "USER" :repl-thread 72)',
            '(:write-string "test_send" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 72)')
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":tes" \'("instance" "test_send" ":foobar" "" swank::%cursor-marker%)) "USER" :repl-thread 83)',
            '(:return (:ok ((":test") ":test")) 83)')
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":t" \'("instance" "test_send" ":foobar" "" swank::%cursor-marker%)) "USER" :repl-thread 84)',
            '(:return (:ok ((":this" ":that" ":test") ":t")) 84)')
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":non" \'("instance" "test_send" ":foobar" "" swank::%cursor-marker%)) "USER" :repl-thread 86)',
            '(:return (:ok ()) 86)')

    def test_completions_package_1(self):
        self.assertSocket(
            '(:emacs-rex (swank:completions "user" \'"USER") "USER" :repl-thread 17)',
            '(:return (:ok (("user:") "user:")) 17)')

    def test_completions_package_2(self):
        self.assertSocket(
            '(:emacs-rex (swank:completions "LISP::*stan" \'"USER") "USER" :repl-thread 6)',
            '(:return (:ok (("LISP::*standard-input*" "LISP::*standard-output*") "LISP::*standard-")) 6)')

    def test_completions_package_3(self):
        self.assertSocket(
            '(:emacs-rex (swank:completions "unix:sigin" \'"USER") "USER" :repl-thread 5)',
            '(:return (:ok (("unix::sigint") "unix::sigint")) 5)')

    def test_completions_package_4(self):
        self.assertSocket(
            '(:emacs-rex (swank:completions "setf-exp" \'"USER") "USER" :repl-thread 5)',
            '(:return (:ok ()) 5)')
        self.assertSocket(
            '(:emacs-rex (swank:completions "lisp::setf-exp" \'"USER") "USER" :repl-thread 7)',
            '(:return (:ok (("lisp::setf-expand" "lisp::setf-expand-1") "lisp::setf-expand")) 7)')
        self.assertSocket(
            '(:emacs-rex (swank:completions "lisp:setf-exp" \'"USER") "USER" :repl-thread 8)',
            '(:return (:ok (("lisp::setf-expand" "lisp::setf-expand-1") "lisp::setf-expand")) 8)')
        self.assertSocket(
            '(:emacs-rex (swank:completions "setf-exp" \'"LISP") "LISP" :repl-thread 8)',
            '(:return (:ok (("setf-expand" "setf-expand-1") "setf-expand")) 8)')

    def test_completions_package_5(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(defun foo (&key test-not))\n") "USER" :repl-thread 17)',
            '(:write-string "foo" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 17)')
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":t" \'("foo" "" swank::%cursor-marker%)) "USER" :repl-thread 19)',
            '(:return (:ok ((":test-not") ":test-not")) 19)')
        self.with_unwind_protect(
            (self.assertSocket,
             '(:emacs-rex (swank:set-package "LISP") "USER" :repl-thread 21)',
             '(:return (:ok ("LISP" "LISP:{}")) 21)'.format(self.EUSLISP_PROGRAM_NAME)),
            (self.assertSocket,
             '(:emacs-rex (swank:completions-for-keyword ":t" \'("foo" "" swank::%cursor-marker%)) "LISP" :repl-thread 22)',
             '(:return (:ok ()) 22)'),
            (self.assertSocket,
             '(:emacs-rex (swank:set-package "USER") "LISP" :repl-thread 27)',
             '(:return (:ok ("USER" "{}")) 27)'.format(self.EUSLISP_PROGRAM_NAME)))

    def test_completions_package_6(self):
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":sl" \'("send" "*user-package*" "" swank::%cursor-marker%)) "USER" :repl-thread 53)',
            '(:return (:ok ((":slots") ":slots")) 53)')
        self.with_unwind_protect(
            (self.assertSocket,
             '(:emacs-rex (swank:set-package "KEYWORD") "USER" :repl-thread 55)',
             '(:return (:ok ("KEYWORD" "KEYWORD:{}")) 55)'.format(self.EUSLISP_PROGRAM_NAME)),
            (self.assertSocket,
             '(:emacs-rex (swank:completions-for-keyword ":sl" \'("send" "*lisp-package*" "" swank::%cursor-marker%)) "KEYWORD" :repl-thread 60)',
             '(:return (:ok ()) 60)'),
            (self.assertSocket,
             '(:emacs-rex (swank:set-package "USER") "KEYWORD" :repl-thread 73)',
             '(:return (:ok ("USER" "{}")) 73)'.format(self.EUSLISP_PROGRAM_NAME)))

    def test_completions_package_7(self):
        self.assertSocketSameResult(
            '(:emacs-rex (swank:completions-for-keyword ":sl" \'("send" "*lisp-package*" "" swank::%cursor-marker%)) "USER" :repl-thread 32)',
            '(:emacs-rex (swank:completions-for-keyword ":sl" \'("lisp:send" "*lisp-package*" "" swank::%cursor-marker%)) "USER" :repl-thread 32)',
            '(:emacs-rex (swank:completions-for-keyword ":sl" \'("lisp::send" "*lisp-package*" "" swank::%cursor-marker%)) "USER" :repl-thread 32)')
        self.assertSocketSameResult(
            '(:emacs-rex (swank:completions-for-keyword ":ini" \'("send" "*lisp-package*" "" swank::%cursor-marker%)) "USER" :repl-thread 33)',
            '(:emacs-rex (swank:completions-for-keyword ":ini" \'("lisp:send" "*lisp-package*" "" swank::%cursor-marker%)) "USER" :repl-thread 33)',
            '(:emacs-rex (swank:completions-for-keyword ":ini" \'("lisp::send" "*lisp-package*" "" swank::%cursor-marker%)) "USER" :repl-thread 33)')

    def test_completions_package_8(self):
        self.assertSocketSameResult(
            '(:emacs-rex (swank:completions-for-keyword ":worldp" \'("instance" "geo::coordinates" "" swank::%cursor-marker%)) "USER" :repl-thread 41)',
            '(:emacs-rex (swank:completions-for-keyword ":worldp" \'("lisp:instance" "geo::coordinates" "" swank::%cursor-marker%)) "USER" :repl-thread 41)',
            '(:emacs-rex (swank:completions-for-keyword ":worldp" \'("lisp::instance" "geo::coordinates" "" swank::%cursor-marker%)) "USER" :repl-thread 41)')
        self.assertSocketSameResult(
            '(:emacs-rex (swank:completions-for-keyword ":world" \'("instance" "geo::coordinates" "" swank::%cursor-marker%)) "USER" :repl-thread 46)',
            '(:emacs-rex (swank:completions-for-keyword ":world" \'("lisp:instance" "geo::coordinates" "" swank::%cursor-marker%)) "USER" :repl-thread 46)',
            '(:emacs-rex (swank:completions-for-keyword ":world" \'("lisp::instance" "geo::coordinates" "" swank::%cursor-marker%)) "USER" :repl-thread 46)')
        self.assertSocketSameResult(
            '(:emacs-rex (swank:completions-for-keyword ":world-" \'("instance" "geo::coordinates" "" swank::%cursor-marker%)) "USER" :repl-thread 53)',
            '(:emacs-rex (swank:completions-for-keyword ":world-" \'("lisp:instance" "geo::coordinates" "" swank::%cursor-marker%)) "USER" :repl-thread 53)',
            '(:emacs-rex (swank:completions-for-keyword ":world-" \'("lisp::instance" "geo::coordinates" "" swank::%cursor-marker%)) "USER" :repl-thread 53)')

    def test_completions_package_9(self):
        self.assertSocket(
            '(:emacs-rex (swank:completions "geo::coordinates-p" \'"USER") "USER" :repl-thread 7)',
            '(:return (:ok (("geo::coordinates-p" "geo::coordinates-plist" "geo::coordinates-pos") "geo::coordinates-p")) 7)')
        self.assertSocket(
            '(:emacs-rex (swank:completions "geo:coordinates-p" \'"USER") "USER" :repl-thread 8)',
            '(:return (:ok (("geo:coordinates-p" "geo:coordinates-pos") "geo:coordinates-p")) 8)')

    def test_completions_fuzzy_1(self):
        self.assertSocket(
            '(:emacs-rex (swank:fuzzy-completions "find-i" "USER" :limit 300 :time-limit-in-msec 1500) "USER" t 5)',
            '(:return (:ok ((("find-if" 0 nil nil) ("find-if-not" 0 nil nil)) nil)) 5)')
        self.assertSocket(
            '(:emacs-rex (swank:fuzzy-completions "find-if-n" "USER" :limit 300 :time-limit-in-msec 1500) "USER" t 6)',
            '(:return (:ok ((("find-if-not" 0 nil nil)) nil)) 6)')
        self.assertSocket(
            '(:emacs-rex (swank:fuzzy-completions "none" "USER" :limit 300 :time-limit-in-msec 1500) "USER" t 7)',
            '(:return (:ok (() nil)) 7)')

    # OUTPUT
    def test_output_1(self):
        self.assertSocketWriteString(
            '(:emacs-rex (swank-repl:listener-eval "(dotimes (i 10) (print i) (unix:usleep 1000))\n") "USER" :repl-thread 17)',
            '(:write-string "0\\n")',
            '(:write-string "1\\n2\\n3\\n4\\n")',
            '(:write-string "5\\n6\\n7\\n8\\n9\\n")',
            '(:write-string "nil" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 17)')

    def test_output_2(self):
        with open('test_output_2.txt') as f:
            lines = [x.rstrip('\n') for x in f.readlines()]
            self.assertSocketWriteString(*lines)

    # DEBUGGER
    def test_sldb_1(self):
        self.with_unwind_protect(
            (self.assertSocketIgnoreAddress,
             '(:emacs-rex (swank-repl:listener-eval "(1+ nil)\n") "USER" :repl-thread 7)',
             '(:debug 0 1 ("Integer expected in (1+ nil)" "" nil) (("QUIT" "Quit to the SLIME top level") ("CONTINUE" "Ignore the error and continue in the same stack level") ("RESTART" "Restart euslisp process")) ((0 "(1+ nil)" (:restartable nil)) (1 "(slime:slimetop)" (:restartable nil)) (2 "(slime:slimetop)" (:restartable nil)) (3 "#<compiled-code #X6102f08>" (:restartable nil))) (nil))'),
            (self.assertSocket,
             '(:emacs-rex (swank:invoke-nth-restart-for-emacs 1 0) "USER" 0 8)',
             '(:return (:abort nil) 8)',
             '(:debug-return 0 1 nil)',
             '(:return (:abort "\'Integer expected\'") 7)'))

    def test_sldb_2(self):
        self.with_unwind_protect(
            (self.assertSocketIgnoreAddress,
             '(:emacs-rex (swank-repl:listener-eval "(let ((a 1)) (error \\"test\\"))\n") "USER" :repl-thread 17)',
             '(:debug 0 1 ("Test in (error \\"test\\")" "" nil) (("QUIT" "Quit to the SLIME top level") ("CONTINUE" "Ignore the error and continue in the same stack level") ("RESTART" "Restart euslisp process")) ((0 "(error \\"test\\")" (:restartable nil)) (1 "(let ((a 1)) (error \\"test\\"))" (:restartable nil)) (2 "(slime:slimetop)" (:restartable nil)) (3 "(slime:slimetop)" (:restartable nil)) (4 "#<compiled-code #X6102f08>" (:restartable nil))) (nil))'),
            (self.assertSocket,
             '(:emacs-rex (swank:invoke-nth-restart-for-emacs 1 1) "USER" 0 18)',
             '(:return (:abort nil) 18)',
             '(:new-package "USER" "E1-{}")'.format(self.EUSLISP_PROGRAM_NAME),
             '(:debug-return 0 1 nil)',
             '(:return (:abort "\'Test\'") 17)'),
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "(eval-dynamic \'a)\n") "USER" :repl-thread 21)',
             '(:write-string "1" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 21)'),
            (self.assertSocketIgnoreAddress,
             '(:emacs-rex (swank-repl:listener-eval "(let ((b 2)) (error \\"test\\"))\n") "USER" :repl-thread 25)',
             '(:debug 0 1 ("Test in (error \\"test\\")" "" nil) (("QUIT" "Quit to the SLIME top level") ("CONTINUE" "Ignore the error and continue in the same stack level") ("RESTART" "Restart euslisp process")) ((0 "(error \\"test\\")" (:restartable nil)) (1 "(let ((b 2)) (error \\"test\\"))" (:restartable nil)) (2 "slime:slime-error" (:restartable nil)) (3 "slime:slime-error" (:restartable nil)) (4 "(error \\"test\\")" (:restartable nil)) (5 "(let ((a 1)) (error \\"test\\"))" (:restartable nil)) (6 "(slime:slimetop)" (:restartable nil)) (7 "(slime:slimetop)" (:restartable nil)) (8 "#<compiled-code #X6102f08>" (:restartable nil))) (nil))'),
            (self.assertSocket,
             '(:emacs-rex (swank:invoke-nth-restart-for-emacs 1 0) "USER" 0 26)',
             '(:return (:abort nil) 26)',
             '(:new-package "USER" "{}")'.format(self.EUSLISP_PROGRAM_NAME),
             '(:debug-return 0 1 nil)',
             '(:return (:abort "\'Test\'") 25)'))
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(eval-dynamic \'a)\n") "USER" :repl-thread 27)',
            '(:write-string "*unbound*" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 27)')
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(eval-dynamic \'b)\n") "USER" :repl-thread 29)',
            '(:write-string "*unbound*" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 29)')

    def test_sldb_3(self):
        self.with_unwind_protect(
            (self.assertSocketIgnoreAddress,
             '(:emacs-rex (swank-repl:listener-eval "(1+ nil)\n") "USER" :repl-thread 31)',
             '(:debug 0 1 ("Integer expected in (1+ nil)" "" nil) (("QUIT" "Quit to the SLIME top level") ("CONTINUE" "Ignore the error and continue in the same stack level") ("RESTART" "Restart euslisp process")) ((0 "(1+ nil)" (:restartable nil)) (1 "(slime:slimetop)" (:restartable nil)) (2 "(slime:slimetop)" (:restartable nil)) (3 "#<compiled-code #X6102f08>" (:restartable nil))) (nil))'),
            (self.assertSocket,
             '(:emacs-rex (swank:throw-to-toplevel) "USER" 0 32)',
             '(:return (:abort nil) 32)',
             '(:debug-return 0 1 nil)',
             '(:return (:abort "\'Integer expected\'") 31)'))

    def test_sldb_4(self):
        self.with_unwind_protect(
            (self.assertSocketIgnoreAddress,
             '(:emacs-rex (swank-repl:listener-eval "(1+ nil)\n") "USER" :repl-thread 33)',
             '(:debug 0 1 ("Integer expected in (1+ nil)" "" nil) (("QUIT" "Quit to the SLIME top level") ("CONTINUE" "Ignore the error and continue in the same stack level") ("RESTART" "Restart euslisp process")) ((0 "(1+ nil)" (:restartable nil)) (1 "(slime:slimetop)" (:restartable nil)) (2 "(slime:slimetop)" (:restartable nil)) (3 "#<compiled-code #X6102f08>" (:restartable nil))) (nil))'),
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "\n") "USER" :repl-thread 34)',
             '(:return (:abort nil) 34)',
             '(:debug-return 0 1 nil)',
             '(:return (:abort "\'Integer expected\'") 33)'))

    def test_sldb_5(self):
        self.with_unwind_protect(
            (self.assertSocketIgnoreAddress,
             '(:emacs-rex (swank-repl:listener-eval "(list (list (list (list (list (list (list (1+ nil))))))))\n") "USER" :repl-thread 18)',
             '(:debug 0 1 ("Integer expected in (1+ nil)" "" nil) (("QUIT" "Quit to the SLIME top level") ("CONTINUE" "Ignore the error and continue in the same stack level") ("RESTART" "Restart euslisp process")) ((0 "(1+ nil)" (:restartable nil)) (1 "(list (1+ nil))" (:restartable nil)) (2 "(list (list (1+ nil)))" (:restartable nil)) (3 "(list (list (list (1+ nil))))" (:restartable nil)) (4 "(list (list (list (list (1+ nil)))))" (:restartable nil)) (5 "(list (list (list (list (list (1+ nil))))))" (:restartable nil)) (6 "(list (list (list (list (list (list (1+ nil)))))))" (:restartable nil)) (7 "(list (list (list (list (list (list (list (1+ nil))))))))" (:restartable nil)) (8 "(slime:slimetop)" (:restartable nil)) (9 "(slime:slimetop)" (:restartable nil))) (nil))'),
            (self.assertSocket,
             '(:emacs-rex (swank:invoke-nth-restart-for-emacs 1 1) "USER" 0 19)',
             '(:return (:abort nil) 19)',
             '(:new-package "USER" "E1-{}")'.format(self.EUSLISP_PROGRAM_NAME),
             '(:debug-return 0 1 nil)',
             '(:return (:abort "\'Integer expected\'") 18)'),
            (self.assertSocketIgnoreAddress,
             '(:emacs-rex (swank-repl:listener-eval "(list (list (list (list (list (list (list (1+ nil))))))))\n") "USER" :repl-thread 20)',
             '(:debug 0 1 ("Integer expected in (1+ nil)" "" nil) (("QUIT" "Quit to the SLIME top level") ("CONTINUE" "Ignore the error and continue in the same stack level") ("RESTART" "Restart euslisp process")) ((0 "(1+ nil)" (:restartable nil)) (1 "(list (1+ nil))" (:restartable nil)) (2 "(list (list (1+ nil)))" (:restartable nil)) (3 "(list (list (list (1+ nil))))" (:restartable nil)) (4 "(list (list (list (list (1+ nil)))))" (:restartable nil)) (5 "(list (list (list (list (list (1+ nil))))))" (:restartable nil)) (6 "(list (list (list (list (list (list (1+ nil)))))))" (:restartable nil)) (7 "(list (list (list (list (list (list (list (1+ nil))))))))" (:restartable nil)) (8 "slime:slime-error" (:restartable nil)) (9 "slime:slime-error" (:restartable nil))) (nil))'),
            (self.assertSocketIgnoreAddress,
             '(:emacs-rex (swank:backtrace 11 51) "USER" 0 21)',
             '(:return (:ok ((11 "(list (1+ nil))" (:restartable nil)) (12 "(list (list (1+ nil)))" (:restartable nil)) (13 "(list (list (list (1+ nil))))" (:restartable nil)) (14 "(list (list (list (list (1+ nil)))))" (:restartable nil)) (15 "(list (list (list (list (list (1+ nil))))))" (:restartable nil)) (16 "(list (list (list (list (list (list (1+ nil)))))))" (:restartable nil)) (17 "(list (list (list (list (list (list (list (1+ nil))))))))" (:restartable nil)) (18 "(slime:slimetop)" (:restartable nil)) (19 "(slime:slimetop)" (:restartable nil)) (20 "#<compiled-code #X55874e8>" (:restartable nil)))) 21)'),
            (self.assertSocket,
             '(:emacs-rex (swank:invoke-nth-restart-for-emacs 1 0) "USER" 0 22)',
             '(:return (:abort nil) 22)',
             '(:new-package "USER" "{}")'.format(self.EUSLISP_PROGRAM_NAME),
             '(:debug-return 0 1 nil)',
             '(:return (:abort "\'Integer expected\'") 20)'))

    def test_sldb_6(self):
        self.with_unwind_protect(
            (self.assertSocketIgnoreAddress,
             '(:emacs-rex (swank:compile-string-for-emacs "(1+ nil)" "test.l" (quote ((:position 1) (:line 1 1))) "/tmp/test.l" (quote nil)) "USER" t 25)',
             '(:debug 0 1 ("Integer expected in (1+ nil)" "" nil) (("QUIT" "Quit to the SLIME top level") ("CONTINUE" "Ignore the error and continue in the same stack level") ("RESTART" "Restart euslisp process")) ((0 "(1+ nil)" (:restartable nil)) (1 "(progn (1+ nil))" (:restartable nil)) (2 "(slime:slimetop)" (:restartable nil)) (3 "(slime:slimetop)" (:restartable nil)) (4 "#<compiled-code #X55874e8>" (:restartable nil))) (nil))'),
            (self.assertSocket,
             '(:emacs-rex (swank:invoke-nth-restart-for-emacs 1 0) "USER" 0 26)',
             '(:return (:abort nil) 26)',
             '(:debug-return 0 1 nil)',
             '(:return (:abort "\'Integer expected\'") 25)'))

    def test_sldb_7(self):
        self.with_unwind_protect(
            (self.assertSocketIgnoreAddress,
             '(:emacs-rex (swank-repl:listener-eval "(let ((a 1)) (error \\"THIS\\"))\n") "USER" :repl-thread 5)',
             '(:debug 0 1 ("THIS in (error \\"THIS\\")" "" nil) (("QUIT" "Quit to the SLIME top level") ("CONTINUE" "Ignore the error and continue in the same stack level") ("RESTART" "Restart euslisp process")) ((0 "(error \\"THIS\\")" (:restartable nil)) (1 "(let ((a 1)) (error \\"THIS\\"))" (:restartable nil)) (2 "(slime:slimetop)" (:restartable nil)) (3 "(slime:slimetop)" (:restartable nil)) (4 "#<compiled-code #X49e4290>" (:restartable nil))) (nil))'),
            (self.assertSocket,
             '(:emacs-rex (swank:invoke-nth-restart-for-emacs 1 1) "USER" 0 6)',
             '(:return (:abort nil) 6)',
             '(:new-package "USER" "E1-{}")'.format(self.EUSLISP_PROGRAM_NAME),
             '(:debug-return 0 1 nil)',
             '(:return (:abort "\'THIS\'") 5)'),
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "(eval-dynamic \'a)\n") "USER" :repl-thread 7)',
             '(:write-string "1" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 7)'),
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "reset\n") "USER" :repl-thread 8)',
             '(:new-package "USER" "{}")'.format(self.EUSLISP_PROGRAM_NAME),
             '(:return (:ok nil) 8)'))

    def test_sldb_8(self):
        self.with_unwind_protect(
            (self.assertSocketIgnoreAddress,
             '(:emacs-rex (swank-repl:listener-eval "(1+ nil)\n") "USER" :repl-thread 7)',
             '(:debug 0 1 ("Integer expected in (1+ nil)" "" nil) (("QUIT" "Quit to the SLIME top level") ("CONTINUE" "Ignore the error and continue in the same stack level") ("RESTART" "Restart euslisp process")) ((0 "(1+ nil)" (:restartable nil)) (1 "(slime:slimetop)" (:restartable nil)) (2 "(slime:slimetop)" (:restartable nil)) (3 "#<compiled-code #X6850290>" (:restartable nil))) (nil))'),
            (self.assertSocket,
             '(:emacs-rex (swank:invoke-nth-restart-for-emacs 1 1) "USER" 0 8)',
             '(:return (:abort nil) 8)',
             '(:new-package "USER" "E1-{}")'.format(self.EUSLISP_PROGRAM_NAME),
             '(:debug-return 0 1 nil)',
             '(:return (:abort "\'Integer expected\'") 7)'),
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "reset\n") "USER" :repl-thread 9)',
             '(:new-package "USER" "{}")'.format(self.EUSLISP_PROGRAM_NAME),
             '(:return (:ok nil) 9)'))

    def test_sldb_9(self):
        self.with_unwind_protect(
            (self.assertSocketIgnoreAddress,
             '(:emacs-rex (swank-repl:listener-eval "(error \\"THIS\\")\n") "USER" :repl-thread 9)',
             '(:debug 0 1 ("THIS in (error \\"THIS\\")" "" nil) (("QUIT" "Quit to the SLIME top level") ("CONTINUE" "Ignore the error and continue in the same stack level") ("RESTART" "Restart euslisp process")) ((0 "(error \\"THIS\\")" (:restartable nil)) (1 "(slime:slimetop)" (:restartable nil)) (2 "(slime:slimetop)" (:restartable nil)) (3 "#<compiled-code #X48fb290>" (:restartable nil))) (nil))'),
            (self.assertSocket,
             '(:emacs-rex (swank:invoke-nth-restart-for-emacs 1 4) "USER" 0 10)',
             '(:return (:ok nil) 10)'),
            (self.assertSocket,
             '(:emacs-rex (swank:invoke-nth-restart-for-emacs 1 0) "USER" 0 11)',
             '(:return (:abort nil) 11)',
             '(:debug-return 0 1 nil)',
             '(:return (:abort "\'THIS\'") 9)'))

    def test_sldb_10(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(defvar test-sldb-11)\n") "USER" :repl-thread 7)',
            '(:write-string "test-sldb-11" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 7)')
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(boundp \'test-sldb-11)\n") "USER" :repl-thread 8)',
            '(:write-string "t" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 8)')
        self.with_unwind_protect(
            (self.assertSocketIgnoreAddress,
             '(:emacs-rex (swank-repl:listener-eval "(error \\"THIS\\")\n") "USER" :repl-thread 9)',
             '(:debug 0 1 ("THIS in (error \\"THIS\\")" "" nil) (("QUIT" "Quit to the SLIME top level") ("CONTINUE" "Ignore the error and continue in the same stack level") ("RESTART" "Restart euslisp process")) ((0 "(error \\"THIS\\")" (:restartable nil)) (1 "(slime:slimetop)" (:restartable nil)) (2 "(slime:slimetop)" (:restartable nil)) (3 "#<compiled-code #X48fb290>" (:restartable nil))) (nil))'),
            (self.assertSocket,
             '(:emacs-rex (swank:invoke-nth-restart-for-emacs 1 2) "USER" 0 10)',
             '(:return (:abort nil) 10)',
             '(:new-package "USER" "{}")'.format(self.EUSLISP_PROGRAM_NAME),
             '(:debug-return 0 1 nil)',
             '(:return (:abort "\'THIS\'") 9)'))
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(boundp \'test-sldb-11)\n") "USER" :repl-thread 11)',
            '(:write-string "nil" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 11)')


    def test_sldb_11(self):
        self.with_unwind_protect(
            (self.assertSocketIgnoreAddress,
             '(:emacs-rex (swank-repl:listener-eval "(elt #f() 1)\n") "USER" :repl-thread 23)',
             '(:debug 0 1 ("Array index out of range in (elt #f() 1)" "" nil) (("QUIT" "Quit to the SLIME top level") ("CONTINUE" "Ignore the error and continue in the same stack level") ("RESTART" "Restart euslisp process")) ((0 "(elt #f() 1)" (:restartable nil)) (1 "(slime:slimetop)" (:restartable nil)) (2 "(slime:slimetop)" (:restartable nil)) (3 "#<compiled-code #X5f9d290>" (:restartable nil))) (nil))'),
            (self.assertSocket,
             '(:emacs-rex (swank:invoke-nth-restart-for-emacs 1 1) "USER" 0 24)',
             '(:return (:abort nil) 24)',
             '(:new-package "USER" "E1-{}")'.format(self.EUSLISP_PROGRAM_NAME),
             '(:debug-return 0 1 nil)',
             '(:return (:abort "\'Array index out of range\'") 23)'),
            (self.assertSocketIgnoreAddress,
             '(:emacs-rex (swank-repl:listener-eval "(elt \\"\\" 1)\n") "USER" :repl-thread 27)',
             '(:debug 0 1 ("Array index out of range in (elt \\"\\" 1)" "" nil) (("QUIT" "Quit to the SLIME top level") ("CONTINUE" "Ignore the error and continue in the same stack level") ("RESTART" "Restart euslisp process")) ((0 "(elt \\"\\" 1)" (:restartable nil)) (1 "slime:slime-error" (:restartable nil)) (2 "slime:slime-error" (:restartable nil)) (3 "(elt #f() 1)" (:restartable nil)) (4 "(slime:slimetop)" (:restartable nil)) (5 "(slime:slimetop)" (:restartable nil)) (6 "#<compiled-code #X5f9d290>" (:restartable nil))) (nil))'),
            (self.assertSocket,
             '(:emacs-rex (swank:invoke-nth-restart-for-emacs 1 2) "USER" 0 28)',
             '(:return (:abort nil) 28)',
             '(:new-package "USER" "{}")'.format(self.EUSLISP_PROGRAM_NAME),
             '(:debug-return 0 1 nil)',
             '(:return (:abort "\'Array index out of range\'") 27)'))

    def test_sldb_12(self):
        self.with_unwind_protect(
            (self.assertSocketPossibleResults,
             '(:emacs-rex (swank-repl:listener-eval "(quit)\n") "USER" :repl-thread 5)',

             # eus irteusgl
             ['(:debug 0 1 ("Process exited with code 0 (SIG_DFL)" "" nil) (("RESTART" "Restart euslisp process")) nil (nil))'],
             ['(:debug 0 1 ("Socket connection closed" "" nil) (("RESTART" "Restart euslisp process")) nil (nil))'],

             # roseus
             ['(:write-string "[ INFO]: cell* ROSEUS_EXIT(context*, int, cell**)\\n")',
              '(:debug 0 1 ("Process exited with code 0 (SIG_DFL)" "" nil) (("RESTART" "Restart euslisp process")) nil (nil))'],
             ['(:write-string "[ INFO]: cell* ROSEUS_EXIT(context*, int, cell**)\\n")',
              '(:debug 0 1 ("Socket connection closed" "" nil) (("RESTART" "Restart euslisp process")) nil (nil))'],

             # roseus_color
             ['(:write-string "\x1b[0m[ INFO]: cell* ROSEUS_EXIT(context*, int, cell**)\x1b[0m\\n")',
              '(:debug 0 1 ("Process exited with code 0 (SIG_DFL)" "" nil) (("RESTART" "Restart euslisp process")) nil (nil))'],
             ['(:write-string "\x1b[0m[ INFO]: cell* ROSEUS_EXIT(context*, int, cell**)\x1b[0m\\n")',
              '(:debug 0 1 ("Socket connection closed" "" nil) (("RESTART" "Restart euslisp process")) nil (nil))']),
            (self.assertSocketPossibleResults,
             '(:emacs-rex (swank:invoke-nth-restart-for-emacs 1 0) "USER" 0 6)',
             ['(:return (:abort nil) 6)',
              '(:new-package "USER" "{}")'.format(self.EUSLISP_PROGRAM_NAME),
              '(:debug-return 0 1 nil)',
              '(:return (:abort "\'Process exited with code 0 (SIG_DFL)\'") 5)'],
             ['(:return (:abort nil) 6)',
              '(:new-package "USER" "{}")'.format(self.EUSLISP_PROGRAM_NAME),
              '(:debug-return 0 1 nil)',
              '(:return (:abort "\'Socket connection closed\'") 5)']))

    def test_sldb_13(self):
        self.with_unwind_protect(
            (self.assertSocketPossibleResults,
             '(:emacs-rex (swank-repl:listener-eval "(exit -1)\n") "USER" :repl-thread 10)',

             # eus irteusgl
             ['(:debug 0 1 ("Process exited with code 255" "" nil) (("RESTART" "Restart euslisp process")) nil (nil))'],
             ['(:debug 0 1 ("Socket connection closed" "" nil) (("RESTART" "Restart euslisp process")) nil (nil))'],

             # roseus
             ['(:write-string "[ INFO]: cell* ROSEUS_EXIT(context*, int, cell**)\\n")',
              '(:debug 0 1 ("Process exited with code 255" "" nil) (("RESTART" "Restart euslisp process")) nil (nil))'],
             ['(:write-string "[ INFO]: cell* ROSEUS_EXIT(context*, int, cell**)\\n")',
              '(:debug 0 1 ("Socket connection closed" "" nil) (("RESTART" "Restart euslisp process")) nil (nil))'],

             # roseus_color
             ['(:write-string "\x1b[0m[ INFO]: cell* ROSEUS_EXIT(context*, int, cell**)\x1b[0m\\n")',
              '(:debug 0 1 ("Process exited with code 255" "" nil) (("RESTART" "Restart euslisp process")) nil (nil))'],
             ['(:write-string "\x1b[0m[ INFO]: cell* ROSEUS_EXIT(context*, int, cell**)\x1b[0m\\n")',
              '(:debug 0 1 ("Socket connection closed" "" nil) (("RESTART" "Restart euslisp process")) nil (nil))']),
            (self.assertSocketPossibleResults,
             '(:emacs-rex (swank:invoke-nth-restart-for-emacs 1 0) "USER" 0 11)',
             ['(:return (:abort nil) 11)',
              '(:new-package "USER" "{}")'.format(self.EUSLISP_PROGRAM_NAME),
              '(:debug-return 0 1 nil)',
              '(:return (:abort "\'Process exited with code 255\'") 10)'],
             ['(:return (:abort nil) 11)',
              '(:new-package "USER" "{}")'.format(self.EUSLISP_PROGRAM_NAME),
              '(:debug-return 0 1 nil)',
              '(:return (:abort "\'Socket connection closed\'") 10)']))

    def test_sldb_14(self):
        self.with_unwind_protect(
            (self.assertSocketIgnoreAddress,
             '(:emacs-rex (swank-repl:listener-eval "(1+ nil)\n") "USER" :repl-thread 44)',
             '(:debug 0 1 ("Integer expected in (1+ nil)" "" nil) (("QUIT" "Quit to the SLIME top level") ("CONTINUE" "Ignore the error and continue in the same stack level") ("RESTART" "Restart euslisp process")) ((0 "(1+ nil)" (:restartable nil)) (1 "(slime:slimetop)" (:restartable nil)) (2 "(slime:slimetop)" (:restartable nil)) (3 "#<compiled-code #X5f21290>" (:restartable nil))) (nil))'),
            (self.assertSocketIgnoreAddress,
             '(:emacs-rex (swank:describe-symbol "none") "USER" 0 45)',
             '(:debug 0 2 ("Symbol not found in (slime::slime-describe-symbol \\"none\\" \\"USER\\")" "" nil) (("CONTINUE" "Ignore the error and continue in the same stack level")) ((0 "(1+ nil)" (:restartable nil)) (1 "(slime:slimetop)" (:restartable nil)) (2 "(slime:slimetop)" (:restartable nil)) (3 "#<compiled-code #X5f21290>" (:restartable nil))) (nil))'),
            (self.assertSocketIgnoreAddress,
             '(:emacs-rex (swank:invoke-nth-restart-for-emacs 2 0) "USER" 0 46)',
             '(:return (:abort nil) 46)',
             '(:debug-return 0 2 nil)',
             '(:return (:abort "\'Symbol not found\'") 45)',
             '(:debug 0 1 ("Integer expected in (1+ nil)" "" nil) (("QUIT" "Quit to the SLIME top level") ("CONTINUE" "Ignore the error and continue in the same stack level") ("RESTART" "Restart euslisp process")) ((0 "(1+ nil)" (:restartable nil)) (1 "(slime:slimetop)" (:restartable nil)) (2 "(slime:slimetop)" (:restartable nil)) (3 "#<compiled-code #X5f21290>" (:restartable nil))) (nil))'),
            (self.assertSocket,
             '(:emacs-rex (swank:invoke-nth-restart-for-emacs 1 0) "USER" 0 47)',
             '(:return (:abort nil) 47)',
             '(:debug-return 0 1 nil)',
             '(:return (:abort "\'Integer expected\'") 44)'))

    def test_sldb_15(self):
        self.with_unwind_protect(
            (self.assertSocketIgnoreAddress,
             '(:emacs-rex (swank-repl:listener-eval "(1+ nil)\n") "USER" :repl-thread 30)',
             '(:debug 0 1 ("Integer expected in (1+ nil)" "" nil) (("QUIT" "Quit to the SLIME top level") ("CONTINUE" "Ignore the error and continue in the same stack level") ("RESTART" "Restart euslisp process")) ((0 "(1+ nil)" (:restartable nil)) (1 "(slime:slimetop)" (:restartable nil)) (2 "(slime:slimetop)" (:restartable nil)) (3 "#<compiled-code #X5f21290>" (:restartable nil))) (nil))'),
            (self.assertSocket,
             '(:emacs-rex (swank:default-directory) "USER" 0 31)',
             '(:return (:ok "{}") 31)'.format(os.getcwd())),
            (self.assertSocketIgnoreAddress,
             '(:emacs-rex (swank:describe-symbol "none") "USER" t 32)',
             '(:debug 0 2 ("Symbol not found in (slime::slime-describe-symbol \\"none\\" \\"USER\\")" "" nil) (("CONTINUE" "Ignore the error and continue in the same stack level")) ((0 "(1+ nil)" (:restartable nil)) (1 "(slime:slimetop)" (:restartable nil)) (2 "(slime:slimetop)" (:restartable nil)) (3 "#<compiled-code #X5f21290>" (:restartable nil))) (nil))'),
            (self.assertSocketIgnoreAddress,
             '(:emacs-rex (swank:compile-string-for-emacs "(format \\"this\\")\n" "test.l" \'((:position 1) (:line 1 1)) "/tmp/test.l" \'nil) "USER" t 33)',
             '(:debug 0 3 ("Mismatch argument in (format \\"this\\")" "" nil) (("QUIT" "Quit to the SLIME top level") ("CONTINUE" "Ignore the error and continue in the same stack level") ("RESTART" "Restart euslisp process")) ((0 "(format \\"this\\")" (:restartable nil)) (1 "(progn (format \\"this\\"))" (:restartable nil)) (2 "slime:slime-error" (:restartable nil)) (3 "slime:slime-error" (:restartable nil)) (4 "(1+ nil)" (:restartable nil)) (5 "(slime:slimetop)" (:restartable nil)) (6 "(slime:slimetop)" (:restartable nil)) (7 "#<compiled-code #X5f21290>" (:restartable nil))) (nil))'),
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "10\n") "USER" :repl-thread 34)',
             '(:return (:abort nil) 34)',
             '(:debug-return 0 3 nil)',
             '(:return (:abort "\'Mismatch argument\'") 33)',
             '(:debug-return 0 2 nil)',
             '(:return (:abort "\'Symbol not found\'") 32)',
             '(:debug-return 0 1 nil)',
             '(:return (:abort "\'Integer expected\'") 30)'))

    # SEGMENTATION FAULT
    def test_segfault_1(self):
        self.with_unwind_protect(
            (self.assertAsyncRequest,
             ['(:emacs-rex (swank-repl:listener-eval "(1+ (and))\n") "USER" :repl-thread 6)',
              '(:emacs-return-string 0 1 "(1+ 1)\n")',
              '(:emacs-return-string 0 1 "(reset)\n")'],
             ['(:write-string ";; Segmentation Fault.\\n;; in ")',
              '(:write-string "(1+ (and))\\n;; You are still in a signal handler.\\n;;Try reset or throw to upper level as soon as possible.\\n;; code=782681776 x=2ea6c580 addr=\\n")',
              '(:read-string 0 1)',
              '(:read-string 0 1)',
              '(:write-string "signal=11 to thread 0, \\n")',
              '(:write-string "2\\n")',
              '(:read-string 0 1)',
              '(:read-aborted 0 1)',
              '(:return (:ok nil) 6)'],
             {'ignore_c_address':True,
              'unordered_output':True}),

            (self.assertSocketPossibleResults,
             '(:emacs-rex (swank-repl:listener-eval "(1+ (and))\n") "USER" :repl-thread 8)',
             ['(:debug 0 1 ("Process exited with code 11 (SIGSEGV)" "" nil) (("RESTART" "Restart euslisp process")) nil (nil))'],
             ['(:debug 0 1 ("Socket connection closed" "" nil) (("RESTART" "Restart euslisp process")) nil (nil))']),

            (self.assertSocketPossibleResults,
             '(:emacs-rex (swank:invoke-nth-restart-for-emacs 1 0) "USER" 0 9)',
             ['(:return (:abort nil) 9)',
              '(:new-package "USER" "{}")'.format(self.EUSLISP_PROGRAM_NAME),
              '(:debug-return 0 1 nil)',
              '(:return (:abort "\'Process exited with code 11 (SIGSEGV)\'") 8)'],
             ['(:return (:abort nil) 9)',
              '(:new-package "USER" "{}")'.format(self.EUSLISP_PROGRAM_NAME),
              '(:debug-return 0 1 nil)',
              '(:return (:abort "\'Socket connection closed\'") 8)']))

    def test_segfault_2(self):
        self.with_unwind_protect(
            (self.assertAsyncRequest,
             ['(:emacs-rex (swank-repl:listener-eval "(1+ (and))\n") "USER" :repl-thread 6)',
              '(:emacs-return-string 0 1 "(list 1 2 3)\n")',
              '(:emacs-return-string 0 1 "reset\n")'],
             ['(:write-string ";; Segmentation Fault.\\n;; in (1+ (and))\\n;; You are still in a signal handler.\\n;;Try reset or throw to upper level as soon as possible.\\n;; code=-107578960 x=f9967880 addr=\\n")',
              '(:read-string 0 1)',
              '(:read-string 0 1)',
              '(:write-string "signal=11 to thread 0, \\n(1 2 3)\\n")',
              '(:read-string 0 1)',
              '(:read-aborted 0 1)',
              '(:debug 0 1 ("Unbound variable reset in (1+ (and))" "" nil) (("QUIT" "Quit to the SLIME top level") ("CONTINUE" "Ignore the error and continue in the same stack level") ("RESTART" "Restart euslisp process")) ((0 "(1+ (and))" (:restartable nil)) (1 "(slime:slimetop)" (:restartable nil)) (2 "(slime:slimetop)" (:restartable nil)) (3 "#<compiled-code #X5df2290>" (:restartable nil))) (nil))'],
             {'ignore_address':True,
              'ignore_c_address':True,
              'unordered_output':True}),

            (self.assertSocket,
             '(:emacs-rex (swank:invoke-nth-restart-for-emacs 1 0) "USER" 0 7)',
             '(:return (:abort nil) 7)',
             '(:debug-return 0 1 nil)',
             '(:return (:abort "\'Unbound variable reset\'") 6)'),

            (self.assertSocketPossibleResults,
             '(:emacs-rex (swank-repl:listener-eval "(1+ (and))\n") "USER" :repl-thread 8)',
             ['(:debug 0 1 ("Process exited with code 11 (SIGSEGV)" "" nil) (("RESTART" "Restart euslisp process")) nil (nil))'],
             ['(:debug 0 1 ("Socket connection closed" "" nil) (("RESTART" "Restart euslisp process")) nil (nil))']),

            (self.assertSocketPossibleResults,
             '(:emacs-rex (swank:invoke-nth-restart-for-emacs 1 0) "USER" 0 9)',
             ['(:return (:abort nil) 9)',
              '(:new-package "USER" "{}")'.format(self.EUSLISP_PROGRAM_NAME),
              '(:debug-return 0 1 nil)',
              '(:return (:abort "\'Process exited with code 11 (SIGSEGV)\'") 8)'],
             ['(:return (:abort nil) 9)',
              '(:new-package "USER" "{}")'.format(self.EUSLISP_PROGRAM_NAME),
              '(:debug-return 0 1 nil)',
              '(:return (:abort "\'Socket connection closed\'") 8)']))

    def test_segfault_3(self):
        fn = signal.signal(signal.SIGINT, signal.SIG_IGN)
        error = None
        try:
            self.assertAsyncRequest(
                ['(:emacs-rex (swank-repl:listener-eval "(1+ (and))\n") "USER" :repl-thread 5)',
                 '(:emacs-interrupt 0)'],
                ['(:write-string ";; Segmentation Fault.\\n;; in ")',
                 '(:write-string "(1+ (and))\\n;; You are still in a signal handler.\\n;;Try reset or throw to upper level as soon as possible.\\n;; code=1614907632 x=604187c0 addr=\\n")',
                 '(:read-string 0 1)',
                 '(:read-aborted 0 1)',
                 '(:read-aborted 0 1)',
                 '(:new-package "USER" "B1-{}")'.format(self.EUSLISP_PROGRAM_NAME),
                 '(:return (:abort "\'Keyboard Interrupt\'") 5)'],
                ignore_c_address=True,
                unordered_output=True)
        except AssertionError as e:
            traceback.print_exception(type(e), e, None)
            error = e
        finally:
            signal.signal(signal.SIGINT, fn)

        self.with_unwind_protect(
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "reset\n") "USER" :repl-thread 6)',
             '(:new-package "USER" "{}")'.format(self.EUSLISP_PROGRAM_NAME),
             '(:return (:ok nil) 6)'),

            (self.assertSocketPossibleResults,
             '(:emacs-rex (swank-repl:listener-eval "(1+ (and))\n") "USER" :repl-thread 8)',
             ['(:debug 0 1 ("Process exited with code 11 (SIGSEGV)" "" nil) (("RESTART" "Restart euslisp process")) nil (nil))'],
             ['(:debug 0 1 ("Socket connection closed" "" nil) (("RESTART" "Restart euslisp process")) nil (nil))']),

            (self.assertSocketPossibleResults,
             '(:emacs-rex (swank:invoke-nth-restart-for-emacs 1 0) "USER" 0 9)',
             ['(:return (:abort nil) 9)',
              '(:new-package "USER" "{}")'.format(self.EUSLISP_PROGRAM_NAME),
              '(:debug-return 0 1 nil)',
              '(:return (:abort "\'Process exited with code 11 (SIGSEGV)\'") 8)'],
             ['(:return (:abort nil) 9)',
              '(:new-package "USER" "{}")'.format(self.EUSLISP_PROGRAM_NAME),
              '(:debug-return 0 1 nil)',
              '(:return (:abort "\'Socket connection closed\'") 8)']))

        if error:
            raise error

    # EMACS INTERRUPT
    def test_emacs_interrupt_1(self):
        self.with_unwind_protect(
            (self.assertAsyncRequest,
             ['(:emacs-rex (swank-repl:listener-eval "(loop)\n") "USER" :repl-thread 5)',
              '(:emacs-interrupt :repl-thread)'],
             ['(:new-package "USER" "B1-{}")'.format(self.EUSLISP_PROGRAM_NAME),
              '(:return (:abort "\'Keyboard Interrupt\'") 5)']),
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "10\n") "USER" :repl-thread 6)',
             '(:write-string "10" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 6)'),
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "(reset)\n") "USER" :repl-thread 7)',
             '(:new-package "USER" "{}")'.format(self.EUSLISP_PROGRAM_NAME),
             '(:return (:ok nil) 7)'))

    def test_emacs_interrupt_2(self):
        self.with_unwind_protect(
            (self.assertAsyncRequest,
             ['(:emacs-rex (swank-repl:listener-eval "(read)\n") "USER" :repl-thread 24)',
              '(:emacs-interrupt 0)'],
             ['(:read-string 0 1)',
              '(:read-aborted 0 1)',
              '(:new-package "USER" "B1-{}")'.format(self.EUSLISP_PROGRAM_NAME),
              '(:return (:abort "\'Keyboard Interrupt\'") 24)']),
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "(reset)\n") "USER" :repl-thread 6)',
             '(:new-package "USER" "{}")'.format(self.EUSLISP_PROGRAM_NAME),
             '(:return (:ok nil) 6)'))

    def test_emacs_interrupt_3(self):
        self.with_unwind_protect(
            (self.assertAsyncRequest,
             ['(:emacs-interrupt :repl-thread)',
              '(:emacs-rex (swank:set-package "") "USER" :repl-thread 5)'],
             ['(:return (:ok ("USER" "B1-{}")) 5)'.format(self.EUSLISP_PROGRAM_NAME)]),
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "(reset)\n") "USER" :repl-thread 6)',
             '(:new-package "USER" "{}")'.format(self.EUSLISP_PROGRAM_NAME),
             '(:return (:ok nil) 6)'))

    def test_emacs_interrupt_4(self):
        self.with_unwind_protect(
            (self.assertAsyncRequest,
             ['(:emacs-rex (swank:compile-string-for-emacs "(progn\n  (print \'start)\n  (unix:sleep 5)\n  (print \'end))\n" "test.l" \'((:position 1) (:line 1 1)) "/tmp/test.l" \'nil) "USER" t 18)',
              '(:emacs-interrupt :repl-thread)',
              '(:emacs-rex (swank:set-package "") "USER" :repl-thread 19)'],
             ['(:write-string "start\\n")',
              '(:write-string "; Evaluation aborted on \'Keyboard Interrupt\'\\n" :repl-result)',
              '(:return (:ok nil) 18)',
              '(:return (:ok ("USER" "B1-{}")) 19)'.format(self.EUSLISP_PROGRAM_NAME)]),
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "(reset)\n") "USER" :repl-thread 6)',
             '(:new-package "USER" "{}")'.format(self.EUSLISP_PROGRAM_NAME),
             '(:return (:ok nil) 6)'))

    def test_emacs_interrupt_5(self):
        self.with_unwind_protect(
            (self.assertAsyncRequest,
             ['(:emacs-rex (swank:load-file "{}/test_emacs_interrupt_5.l") "USER" :repl-thread 5)'.format(os.getcwd()),
              '(:emacs-interrupt :repl-thread)',
              '(:emacs-rex (swank:set-package "") "USER" :repl-thread 6)',],
             ['(:write-string "Loading file: {}/test_emacs_interrupt_5.l ...\\n")'.format(os.getcwd()),
              '(:write-string "start\\n")',
              '(:write-string "; Evaluation aborted on \'Keyboard Interrupt\'\\n" :repl-result)',
              '(:return (:ok nil) 5)',
              '(:return (:ok ("USER" "B1-{}")) 6)'.format(self.EUSLISP_PROGRAM_NAME)]),
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "(reset)\n") "USER" :repl-thread 6)',
             '(:new-package "USER" "{}")'.format(self.EUSLISP_PROGRAM_NAME),
             '(:return (:ok nil) 6)'))

    def test_emacs_interrupt_6(self):
        self.with_unwind_protect(
            (self.assertAsyncRequest,
             ['(:emacs-rex (swank-repl:listener-eval "(let ((bak (unix:signal unix::sigint #\'(lambda (&rest args) (print \\"TEST\\"))))) (unwind-protect (unix:sleep 5) (unix:signal unix::sigint bak)))") "USER" :repl-thread 35)',
              '(:emacs-interrupt :repl-thread)',
              '(:emacs-rex (swank:set-package "") "USER" :repl-thread 36)'],
             ['(:write-string "\\"TEST\\"\\n")',
              '(:write-string "nil" :repl-result)',
              '(:write-string "\\n" :repl-result)',
              '(:return (:ok nil) 35)',
              '(:return (:ok ("USER" "{}")) 36)'.format(self.EUSLISP_PROGRAM_NAME)]),
            (self.assertAsyncRequest,
             ['(:emacs-interrupt :repl-thread)',
              '(:emacs-rex (swank:set-package "") "USER" :repl-thread 37)'],
             ['(:return (:ok ("USER" "B1-{}")) 37)'.format(self.EUSLISP_PROGRAM_NAME)]),
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "(reset)\n") "USER" :repl-thread 38)',
             '(:new-package "USER" "{}")'.format(self.EUSLISP_PROGRAM_NAME),
             '(:return (:ok nil) 38)'))

    def test_emacs_interrupt_7(self):
        fn = signal.signal(signal.SIGINT, signal.SIG_IGN)
        try:
            self.assertAsyncRequest(
                ['(:emacs-rex (swank-repl:listener-eval "eus\n") "USER" :repl-thread 8)',
                 '(:emacs-return-string 0 1 "(list 1 2 3)\n")',
                 '(:emacs-interrupt 0)'],
                ['(:read-string 0 1)',
                 '(:read-string 0 1)',
                 '(:write-string "(1 2 3)\\n")',
                 '(:read-aborted 0 1)',
                 '(:write-string "2" :repl-result)',
                 '(:write-string "\\n" :repl-result)',
                 '(:read-aborted 0 1)',
                 '(:return (:ok nil) 8)'],
                unordered_output=True)
        finally:
            signal.signal(signal.SIGINT, fn)

    # SET PACKAGE
    def test_set_package_1(self):
        self.with_unwind_protect(
            (self.assertSocket,
             '(:emacs-rex (swank:set-package "LISP") "USER" :repl-thread 33)',
             '(:return (:ok ("LISP" "LISP:{}")) 33)'.format(self.EUSLISP_PROGRAM_NAME)),
            (self.assertSocket,
             '(:emacs-rex (swank:set-package "USER") "LISP" :repl-thread 35)',
             '(:return (:ok ("USER" "{}")) 35)'.format(self.EUSLISP_PROGRAM_NAME)))

    # APROPOS
    def test_apropos_1(self):
        self.assertSocket(
            '(:emacs-rex (swank:apropos-list-for-emacs "prompt" t nil (quote nil)) "USER" :repl-thread 17)',
            '(:return (:ok ((:designator "*PROMPT*" :variable :not-documented) (:designator "*PROMPT-STRING*" :variable :not-documented) (:designator "LISP::PROMPT" :function "(strm)") (:designator "SLIME::LAST-PROMPT" :variable :not-documented) (:designator "SLIME::SLIME-PROMPT" :function "nil") (:designator "TOPLEVEL-PROMPT" :function "(strm)"))) 17)')

    def test_apropos_2(self):
        self.assertSocket(
            '(:emacs-rex (swank:apropos-list-for-emacs "find-" t nil (quote "LISP")) "USER" :repl-thread 10)',
            '(:return (:ok ((:designator "FIND-EXECUTABLE" :function "(progname)") (:designator "FIND-IF" :function "(pred seq &key (start 0) (end (length seq)) (key #\'identity))") (:designator "FIND-IF-NOT" :function "(pred seq &key (start 0) (end (length seq)) (key #\'identity))") (:designator "FIND-METHOD" :function :not-documented) (:designator "FIND-PACKAGE" :function :not-documented) (:designator "FIND-SYMBOL" :function :not-documented))) 10)')

    # DESCRIBE
    def test_describe_1(self):
        self.assertSocketIgnoreAddress(
            '(:emacs-rex (swank:describe-definition-for-emacs "*PROMPT-STRING*" :variable) "USER" t 13)',
            '(:return (:ok "NAME\\n     *prompt-string*\\nTYPE\\n     variable\\nDESCRIPTION\\n     prompt string used by \x1b[1meustop\x1b[m. \\n\\nPROPERTIES\\n\\nplist=nil\\nvalue=\\"{}\\"\\nvtype=2\\nfunction=*unbound*\\npname=\\"*PROMPT-STRING*\\"\\nhomepkg=#<package #X5f12ae8 LISP>\\n") 13)'.format(self.EUSLISP_PROGRAM_NAME))

    def test_describe_2(self):
        self.assertSocketIgnoreAddress(
            '(:emacs-rex (swank:describe-symbol "*prompt-string*") "USER" :repl-thread 16)',
            '(:return (:ok "NAME\\n     *prompt-string*\\nTYPE\\n     variable\\nDESCRIPTION\\n     prompt string used by \x1b[1meustop\x1b[m. \\n\\nPROPERTIES\\n\\nplist=nil\\nvalue=\\"{}\\"\\nvtype=2\\nfunction=*unbound*\\npname=\\"*PROMPT-STRING*\\"\\nhomepkg=#<package #X5f12ae8 LISP>\\n") 16)'.format(self.EUSLISP_PROGRAM_NAME))

    def test_describe_3(self):
        self.with_unwind_protect(
            (self.assertSocketIgnoreAddress,
             '(:emacs-rex (swank:describe-symbol "none") "USER" :repl-thread 5)',
             '(:debug 0 1 ("Symbol not found in (slime::slime-describe-symbol \\"none\\" \\"USER\\")" "" nil) (("CONTINUE" "Ignore the error and continue in the same stack level")) ((0 "#<compiled-code #X48fb290>" (:restartable nil))) (nil))'),
            (self.assertSocket,
             '(:emacs-rex (swank:invoke-nth-restart-for-emacs 1 0) "USER" 0 6)',
             '(:return (:abort nil) 6)',
             '(:debug-return 0 1 nil)',
             '(:return (:abort "\'Symbol not found\'") 5)'))

    def test_describe_4(self):
        self.assertSocketIgnoreAddress(
            '(:emacs-rex (swank:describe-symbol "sys:list-all-bindings") "USER" :repl-thread 7)',
            '(:return (:ok "NAME\\n     sys:list-all-bindings\\nTYPE\\n     function\\nSYNOPSIS\\n     sys:list-all-bindings  \\nDESCRIPTION\\n     scans bind stack, and returns a list of all the accessible value bindings. \\n\\nPROPERTIES\\n\\nplist=((compiler::builtin-function-entry . \\"LISTBINDINGS\\"))\\nvalue=*unbound*\\nvtype=1\\nfunction=#<compiled-code #X5884ab8>\\npname=\\"LIST-ALL-BINDINGS\\"\\nhomepkg=#<package #X58a7ce0 SYSTEM>\\n") 7)')
        self.assertSocketSameResult(
            '(:emacs-rex (swank:describe-symbol "si:list-all-bindings") "USER" :repl-thread 28)',
            '(:emacs-rex (swank:describe-symbol "sys:list-all-bindings") "USER" :repl-thread 28)',
            '(:emacs-rex (swank:describe-symbol "system:list-all-bindings") "USER" :repl-thread 28)',
            '(:emacs-rex (swank:describe-symbol "si::list-all-bindings") "USER" :repl-thread 28)',
            '(:emacs-rex (swank:describe-symbol "sys::list-all-bindings") "USER" :repl-thread 28)',
            '(:emacs-rex (swank:describe-symbol "system::list-all-bindings") "USER" :repl-thread 28)')

    def test_describe_5(self):
        self.with_unwind_protect(
            (self.assertSocketIgnoreAddress,
             '(:emacs-rex (swank:describe-symbol "setf-expand-1") "USER" :repl-thread 31)',
             '(:debug 0 1 ("Symbol not found in (slime::slime-describe-symbol \\"setf-expand-1\\" \\"USER\\")" "" nil) (("CONTINUE" "Ignore the error and continue in the same stack level")) ((0 "#<compiled-code #X5136290>" (:restartable nil))) (nil))'),
            (self.assertSocket,
             '(:emacs-rex (swank:invoke-nth-restart-for-emacs 1 0) "USER" 0 32)',
             '(:return (:abort nil) 32)',
             '(:debug-return 0 1 nil)',
             '(:return (:abort "\'Symbol not found\'") 31)'))
        self.with_unwind_protect(
            (self.assertSocket,
             '(:emacs-rex (swank:set-package "LISP") "USER" :repl-thread 34)',
             '(:return (:ok ("LISP" "LISP:{}")) 34)'.format(self.EUSLISP_PROGRAM_NAME)),
            (self.assertSocketIgnoreAddress,
             '(:emacs-rex (swank:describe-symbol "setf-expand-1") "LISP" :repl-thread 35)',
             '(:return (:ok "PROPERTIES\\n\\nplist=((:function-documentation . \\"(place newvalue)\\"))\\nvalue=*unbound*\\nvtype=1\\nfunction=#<compiled-code #X5077558>\\npname=\\"SETF-EXPAND-1\\"\\nhomepkg=#<package #X5072ae8 LISP>\\n") 35)'),
            (self.assertSocket,
             '(:emacs-rex (swank:set-package "USER") "LISP" :repl-thread 37)',
             '(:return (:ok ("USER" "{}")) 37)'.format(self.EUSLISP_PROGRAM_NAME)))

    def test_describe_6(self):
        self.assertSocketIgnoreAddress(
            '(:emacs-rex (swank:describe-symbol "lisp::setf-expand") "USER" :repl-thread 24)',
            '(:return (:ok "PROPERTIES\\n\\nplist=((:function-documentation . \\"(l)\\"))\\nvalue=*unbound*\\nvtype=1\\nfunction=#<compiled-code #X60dc588>\\npname=\\"SETF-EXPAND\\"\\nhomepkg=#<package #X60d7ae8 LISP>\\n") 24)')
        self.with_unwind_protect(
            (self.assertSocketIgnoreAddress,
             '(:emacs-rex (swank:describe-symbol "lisp:setf-expand") "USER" :repl-thread 22)',
             '(:debug 0 1 ("Symbol not found in (slime::slime-describe-symbol \\"lisp:setf-expand\\" \\"USER\\")" "" nil) (("CONTINUE" "Ignore the error and continue in the same stack level")) ((0 "#<compiled-code #X619b290>" (:restartable nil))) (nil))'),
            (self.assertSocket,
             '(:emacs-rex (swank:invoke-nth-restart-for-emacs 1 0) "USER" 0 23)',
             '(:return (:abort nil) 23)',
             '(:debug-return 0 1 nil)',
             '(:return (:abort "\'Symbol not found\'") 22)'))

    def test_describe_7(self):
        self.assertSocketIgnoreAddress(
            '(:emacs-rex (swank:describe-symbol ":slots") "USER" :repl-thread 5)',
            '(:return (:ok "-- OBJECT --\\n\\nNAME\\n     :slots\\nTYPE\\n     method\\nCLASS\\n     object \\nSYNOPSIS\\n     :slots  \\nDESCRIPTION\\n     returns the list of variable-name and value pair of all the slots of the object. You can get the value of a specific slot by applying \x1b[1massoc\x1b[m to this list, although you cannot alter them. \\n\\n-- METACLASS --\\n\\nNAME\\n     :slots\\nTYPE\\n     method\\nCLASS\\n     metaclass \\nSYNOPSIS\\n     :slots  \\nDESCRIPTION\\n     returns the slot-name vector. \\n\\nPROPERTIES\\n\\nplist=((:method-documentation\\n        (#<metaclass metaclass> . \\"(self class)\\")\\n        (#<metaclass object> . \\"(self class)\\"))\\n       (:class #<metaclass metaclass> #<metaclass object>))\\nvalue=:slots\\nvtype=0\\nfunction=*unbound*\\npname=\\"SLOTS\\"\\nhomepkg=#<package KEYWORD>\\n") 5)')

    # LOAD FILE
    def test_load_file_1(self):
        self.with_unwind_protect(
            (self.assertSocket,
             '(:emacs-rex (swank:load-file "{}/test_load_file_1.l") "USER" :repl-thread 6)'.format(os.getcwd()),
             '(:write-string "Loading file: {}/test_load_file_1.l ...\\n")'.format(os.getcwd()),
             '(:write-string "Loaded.\\n")',
             '(:return (:ok ("{}/test_load_file_1.l")) 6)'.format(os.getcwd())),
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "(and (boundp \'test-load-var) test-load-var)\n") "USER" :repl-thread 11)',
             '(:write-string "10" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 11)'),
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "(makunbound \'test-load-var)\n") "USER" :repl-thread 13)',
             '(:write-string "t" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 13)'))

    def test_load_file_2(self):
        self.with_unwind_protect(
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "(progn (with-output-to-string (s) (let ((*standard-output* s) (*error-output* s)) (compiler:compile-file \\"test_load_file_1.l\\" :o \\"/tmp/\\"))) t)\\n") "USER" :repl-thread 5)',
             '(:write-string "t" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 5)'),
            (self.assertSocket,
             '(:emacs-rex (swank:load-file "/tmp/test_load_file_1.so") "USER" :repl-thread 6)',
             '(:write-string "Loading file: /tmp/test_load_file_1.so ...\\n")',
             '(:write-string "Loaded.\\n")',
             '(:return (:ok ("/tmp/test_load_file_1.so")) 6)'),
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "(and (boundp \'test-load-var) test-load-var)\n") "USER" :repl-thread 11)',
             '(:write-string "10" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 11)'),
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "(makunbound \'test-load-var)\n") "USER" :repl-thread 13)',
             '(:write-string "t" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 13)'))

    def test_load_file_3(self):
        if self.EUSLISP_PROGRAM == 'roseus':
            print("Skipping test...")
            return
        file = "{}/none.l".format(os.getcwd())
        self.with_unwind_protect(
            (self.assertSocketIgnoreAddress,
             '(:emacs-rex (swank:load-file "{0}") "USER" :repl-thread 6)'.format(file),
             '(:write-string "Loading file: {0} ...\\n")'.format(file),
             '(:debug 0 1 ("File #P\\"{0}\\" not found in (slime::load-file-and-tags \\"{0}\\")" "" nil) (("QUIT" "Quit to the SLIME top level") ("CONTINUE" "Ignore the error and continue in the same stack level") ("RESTART" "Restart euslisp process")) ((0 "(slime::load-file-and-tags \\"{0}\\")" (:restartable nil)) (1 "(slime::load-file-and-tags \\"{0}\\")" (:restartable nil)) (2 "(slime:slimetop)" (:restartable nil)) (3 "(slime:slimetop)" (:restartable nil)) (4 "#<compiled-code #X48c80f0>" (:restartable nil))) (nil))'.format(file)),
            (self.assertSocket,
             '(:emacs-rex (swank:invoke-nth-restart-for-emacs 1 0) "USER" 0 7)',
             '(:return (:abort nil) 7)',
             '(:debug-return 0 1 nil)',
             '(:return (:abort "\'File #P\\"{0}\\" not found\'") 6)'.format(file)))

    def test_load_file_4(self):
        # test recursive loading
        self.with_unwind_protect(
            (self.assertSocket,
             '(:emacs-rex (swank:load-file "{}/test_load_file_4.l") "USER" :repl-thread 6)'.format(os.getcwd()),
             '(:write-string "Loading file: {}/test_load_file_4.l ...\\n")'.format(os.getcwd()),
             '(:write-string "Loaded.\\n")',
             '(:return (:ok ("{0}/test_load_file_4.l" "{0}/test_load_file_1.l")) 6)'.format(os.getcwd())),
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "(and (boundp \'test-load-var) test-load-var)\n") "USER" :repl-thread 11)',
             '(:write-string "10" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 11)'),
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "(makunbound \'test-load-var)\n") "USER" :repl-thread 13)',
             '(:write-string "t" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 13)'))

    def test_load_file_5(self):
        # test loading without the implicit ".l"
        self.with_unwind_protect(
            (self.assertSocket,
             '(:emacs-rex (swank:load-file "{}/test_load_file_4") "USER" :repl-thread 6)'.format(os.getcwd()),
             '(:write-string "Loading file: {}/test_load_file_4 ...\\n")'.format(os.getcwd()),
             '(:write-string "Loaded.\\n")',
             '(:return (:ok ("{0}/test_load_file_4.l" "{0}/test_load_file_1.l")) 6)'.format(os.getcwd())),
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "(and (boundp \'test-load-var) test-load-var)\n") "USER" :repl-thread 11)',
             '(:write-string "10" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 11)'),
            (self.assertSocket,
             '(:emacs-rex (swank-repl:listener-eval "(makunbound \'test-load-var)\n") "USER" :repl-thread 13)',
             '(:write-string "t" :repl-result)',
             '(:write-string "\\n" :repl-result)',
             '(:return (:ok nil) 13)'))

    # MACRO EXPAND
    def test_macro_expand_1(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(defmacro foo (&rest body) `(progn ,@(reverse body)))\n") "USER" :repl-thread 16)',
            '(:write-string "foo" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 16)')
        self.assertSocket(
            '(:emacs-rex (swank:swank-expand-1 "(foo (print 1) (print 2) (print 3))") "USER" :repl-thread 27)',
            '(:return (:ok "(progn (print 3) (print 2) (print 1))\\n") 27)')

    def test_macro_expand_2(self):
        self.with_unwind_protect(
            (self.assertSocketIgnoreAddress,
             '(:emacs-rex (swank:swank-expand-1 "(none 1 2 3)") "USER" :repl-thread 51)',
             '(:debug 0 1 ("Undefined function none in (slime::slime-macroexpand \\"(none 1 2 3)\\" \\"USER\\")" "" nil) (("CONTINUE" "Ignore the error and continue in the same stack level")) ((0 "#<compiled-code #X547e290>" (:restartable nil))) (nil))'),
            (self.assertSocket,
             '(:emacs-rex (swank:invoke-nth-restart-for-emacs 1 0) "USER" 0 52)',
             '(:return (:abort nil) 52)',
             '(:debug-return 0 1 nil)',
             '(:return (:abort "\'Undefined function none\'") 51)'))

    def test_macro_expand_3(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(defmacro foo (form) (reverse form))\n") "USER" :repl-thread 5)',
            '(:write-string "foo" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 5)')
        self.assertSocket(
            '(:emacs-rex (swank:swank-expand-1 "(foo (3 2 1 list))") "USER" :repl-thread 10)',
            '(:return (:ok "(list 1 2 3)\\n") 10)')
        self.with_unwind_protect(
            (self.assertSocket,
             '(:emacs-rex (swank:set-package "LISP") "USER" :repl-thread 12)',
             '(:return (:ok ("LISP" "LISP:{}")) 12)'.format(self.EUSLISP_PROGRAM_NAME)),
            (self.assertSocketIgnoreAddress,
             '(:emacs-rex (swank:swank-expand-1 "(foo (3 2 1 list))") "LISP" :repl-thread 13)',
             '(:debug 0 1 ("Undefined function foo in (slime::slime-macroexpand \\"(foo (3 2 1 list))\\" \\"LISP\\")" "" nil) (("CONTINUE" "Ignore the error and continue in the same stack level")) ((0 "#<compiled-code #X5853290>" (:restartable nil))) (nil))'),
            (self.assertSocket,
             '(:emacs-rex (swank:invoke-nth-restart-for-emacs 1 0) "LISP" 0 14)',
             '(:return (:abort nil) 14)',
             '(:debug-return 0 1 nil)',
             '(:return (:abort "\'Undefined function foo\'") 13)'),
            (self.assertSocket,
             '(:emacs-rex (swank:set-package "USER") "LISP" :repl-thread 16)',
             '(:return (:ok ("USER" "{}")) 16)'.format(self.EUSLISP_PROGRAM_NAME)))

    def test_macro_expand_4(self):
        self.assertSocket(
            '(:emacs-rex (swank:swank-expand-1 "(unless t\n       (print \\"HERE\\"))") "USER" :repl-thread 10)',
            '(:return (:ok "(when (not t) (print \\"HERE\\"))\\n") 10)')

    # ENCODING
    def test_encoding_1(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(print \\"\xe3\x81\x93\xe3\x82\x93\xe3\x81\xab\xe3\x81\xa1\xe3\x81\xaf\xe3\x80\x82\\")\n") "USER" :repl-thread 5)',
            '(:write-string "\\"\xe3\x81\x93\xe3\x82\x93\xe3\x81\xab\xe3\x81\xa1\xe3\x81\xaf\xe3\x80\x82\\"\\n")',
            '(:write-string "\\"\xe3\x81\x93\xe3\x82\x93\xe3\x81\xab\xe3\x81\xa1\xe3\x81\xaf\xe3\x80\x82\\"" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 5)')

    def test_encoding_2(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(make-string 10)\n") "USER" :repl-thread 8)',
            '(:write-string "\\"{}\\"" :repl-result)'.format(chr(0) * 10),
            '(:write-string "\\n" :repl-result)',
            '(:return (:ok nil) 8)')

    def test_encoding_3(self):
        self.assertSocket(
            '(:emacs-rex (swank:swank-expand-1 "(unless t\n\t(print \\"\xe3\x81\x82\xe3\x81\x82\xe3\x81\x82\\"))") "USER" :repl-thread 8)',
            '(:return (:ok "(when (not t) (print \\"\xe3\x81\x82\xe3\x81\x82\xe3\x81\x82\\"))\\n") 8)')

    # DEFAULT-DIRECTORY
    def test_default_directory_1(self):
        self.assertSocket(
            '(:emacs-rex (swank:default-directory) "USER" :repl-thread 48)',
            '(:return (:ok "{}") 48)'.format(os.getcwd()))
