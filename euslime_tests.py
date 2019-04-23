#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Test suite for EusLisp SLIME

import pprint
import socket
import time
import unittest

from euslime.logger import get_logger
from euslime.server import EuslimeServer
from thread import start_new_thread

HEADER_LENGTH = 6

log = get_logger(__name__)

class EuslimeTestBase(unittest.TestCase):
    @classmethod
    def setUpClass(self):
        self.server = EuslimeServer(('0.0.0.0', 0))
        host, port = self.server.socket.getsockname()
        start_new_thread(self.server.serve_forever, ())
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        # self.socket.settimeout(5)
        self.socket.connect((host, port))

    @classmethod
    def tearDownClass(self):
        log.info("Tearing down...")
        self.socket.shutdown(socket.SHUT_RDWR)
        time.sleep(0.1)
        log.info("...DONE")

    def socket_recv_one(self, *options):
        try:
            len = self.socket.recv(HEADER_LENGTH, *options)
            hex_len = int(len, 16)
            return self.socket.recv(hex_len)
        except socket.error:
            return;

    def socket_recv(self, times):
        result = []
        for i in range(times):
            res = self.socket_recv_one()
            log.info(res)
            if res == None:
                break
            result.append(res)
        return tuple(result) or None

    def socket_clean(self):
        while self.socket_recv_one(socket.MSG_DONTWAIT):
            pass

    def socket_send(self, req):
        header = '{0:06x}'.format(len(req))
        self.socket.send(header + req)

    def assertSocket(self, req, *res):
        log.info('request: \n%s', req)
        log.info('expected response: \n%s', pprint.pformat(res, width=5))
        # self.socket_clean()
        self.socket_send(req)
        response = self.socket_recv(len(res))
        log.info('received response: \n%s', pprint.pformat(response, width=5))
        self.assertEqual(res, response)


    ### TESTS

    # CREATE-REPL
    def test_001_swank_repl_create_repl(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:create-repl nil :coding-system "utf-8-unix") "COMMON-LISP-USER" t 4)',
            '(:return (:ok ("USER" "irteusgl")) 4)')

    # LISTENER-EVAL
    def test_swank_eval_1(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(list 1 2 3)\n") "USER" :repl-thread 8)',
            '(:read-string 0 1)',
            '(:write-string "(1 2 3)" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:read-aborted 0 1)',
            '(:return (:ok nil) 8)')

    def test_swank_eval_2(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(find 4 \'((1 . a) (2 . b) (3 . c) (4 . d) (4 . e)) :key #\'car)\n") "USER" :repl-thread 33)',
            '(:read-string 0 1)',
            '(:write-string "(4 . d)" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:read-aborted 0 1)',
            '(:return (:ok nil) 33)')

    def test_swank_eval_3(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "list 1 2 3\n") "USER" :repl-thread 5)',
            '(:read-string 0 1)',
            '(:write-string "(1 2 3)" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:read-aborted 0 1)',
            '(:return (:ok nil) 5)')

    def test_swank_eval_4(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(progn (setq *package* *lisp-package*) (send *package* :name))\n") "USER" :repl-thread 30)',
            '(:read-string 0 1)',
            '(:write-string "\\"LISP\\"" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:read-aborted 0 1)',
            '(:new-package "LISP" "LISP:irteusgl")',
            '(:return (:ok nil) 30)')
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(progn (setq *package* *user-package*) (send *package* :name))\n") "LISP" :repl-thread 31)',
            '(:read-string 0 1)',
            '(:write-string "\\"USER\\"" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:read-aborted 0 1)',
            '(:new-package "USER" "irteusgl")',
            '(:return (:ok nil) 31)')

    def test_swank_eval_5(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(print \\"this\\")\n") "USER" :repl-thread 43)',
            '(:read-string 0 1)',
            '(:write-string "\\"this\\"\\n")',
            '(:write-string "\\"this\\"" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:read-aborted 0 1)',
            '(:return (:ok nil) 43)')

    def test_swank_eval_6(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(format nil \\"~S\\" \\"this\\")\n") "USER" :repl-thread 41)',
            '(:read-string 0 1)',
            '(:write-string "\\"\\\\\\"this\\\\\\"\\"" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:read-aborted 0 1)',
            '(:return (:ok nil) 41)')

    # AUTODOC
    def test_swank_autodoc_1(self):
        self.assertSocket(
            '(:emacs-rex (swank:autodoc (quote ("list" "" swank::%cursor-marker%)) :print-right-margin 100) "USER" :repl-thread 5)',
            '(:return (:ok ("(list &rest ===> elements <===)" t)) 5)')

    def test_swank_autodoc_2(self):
        self.assertSocket(
            '(:emacs-rex (swank:autodoc (quote ("list" "1" "" swank::%cursor-marker%)) :print-right-margin 100) "USER" :repl-thread 6)',
            '(:return (:ok ("(list &rest ===> elements <===)" t)) 6)')

    def test_swank_autodoc_3(self):
        self.assertSocket(
            '(:emacs-rex (swank:autodoc (quote ("list" "1" "2" "" swank::%cursor-marker%)) :print-right-margin 100) "USER" :repl-thread 7)',
            '(:return (:ok ("(list &rest ===> elements <===)" t)) 7)')

    def test_swank_autodoc_4(self):
        self.assertSocket(
            '(:emacs-rex (swank:autodoc (quote ("find" "" swank::%cursor-marker%)) :print-right-margin 100) "USER" :repl-thread 5)',
            '(:return (:ok ("(find ===> item <=== seq &key start end test test-not key (count 1))" t)) 5)')

    def test_swank_autodoc_5(self):
        self.assertSocket(
            '(:emacs-rex (swank:autodoc (quote ("find" "4" "" swank::%cursor-marker%)) :print-right-margin 100) "USER" :repl-thread 6)',
            '(:return (:ok ("(find item ===> seq <=== &key start end test test-not key (count 1))" t)) 6)')

    def test_swank_autodoc_6(self):
        self.assertSocket(
            '(:emacs-rex (swank:autodoc (quote ("find" "4" (("" swank::%cursor-marker%)))) :print-right-margin 100) "USER" :repl-thread 7)',
            '(:return (:ok (:not-available t)) 7)')

    def test_swank_autodoc_7(self):
        self.assertSocket(
            '(:emacs-rex (swank:autodoc (quote ("find" "4" (("1" "." "a") ("2" "." "b") ("3" "." "c") ("4" "." "" swank::%cursor-marker%)))) :print-right-margin 100) "USER" :repl-thread 22)',
            '(:return (:ok (:not-available t)) 22)')

    def test_swank_autodoc_8(self):
        self.assertSocket(
            '(:emacs-rex (swank:autodoc (quote ("find" "4" (("1" "." "a") ("2" "." "b") ("3" "." "c") ("4" "." "d") ("4" "." "e")) "" swank::%cursor-marker%)) :print-right-margin 100) "USER" :repl-thread 28)',
            '(:return (:ok ("(find item seq &key start end test test-not key (count 1))" t)) 28)')

    def test_swank_autodoc_9(self):
        self.assertSocket(
            '(:emacs-rex (swank:autodoc (quote ("find" "4" (("1" "." "a") ("2" "." "b") ("3" "." "c") ("4" "." "d") ("4" "." "e")) ":key" "" swank::%cursor-marker%)) :print-right-margin 100) "USER" :repl-thread 29)',
            '(:return (:ok ("(find item seq &key start end test test-not ===> key <=== (count 1))" t)) 29)')

    def test_swank_autodoc_10(self):
        self.assertSocket(
            '(:emacs-rex (swank:autodoc (quote ("setq" "" swank::%cursor-marker%)) :print-right-margin 100) "USER" :repl-thread 6)',
            '(:return (:ok ("(setq &rest ===> forms <===)" t)) 6)')

    def test_swank_autodoc_11(self):
        self.assertSocket(
            '(:emacs-rex (swank:autodoc (quote ("list" "*prompt-string*" swank::%cursor-marker%)) :print-right-margin 100) "USER" :repl-thread 24)',
            '(:return (:ok ("*prompt-string* => \\"irteusgl\\"" nil)) 24)')

    def test_swank_autodoc_12(self):
        self.assertSocket(
            '(:emacs-rex (swank:autodoc (quote ("print" "" swank::%cursor-marker%)) :print-right-margin 100) "USER" :repl-thread 30)',
            '(:return (:ok ("(print ===> obj <=== &optional stream)" t)) 30)')

    def test_swank_autodoc_13(self):
        self.assertSocket(
            '(:emacs-rex (swank:autodoc (quote ("print" "\\"this\\"" "" swank::%cursor-marker%)) :print-right-margin 100) "USER" :repl-thread 31)',
            '(:return (:ok ("(print obj &optional ===> stream <===)" t)) 31)')

    def test_swank_autodoc_14(self):
        self.assertSocket(
            '(:emacs-rex (swank:autodoc (quote ("print" "\\"this\\"" "nil" swank::%cursor-marker%)) :print-right-margin 100) "USER" :repl-thread 32)',
            '(:return (:ok ("(print obj &optional ===> stream <===)" t)) 32)')

    def test_swank_autodoc_15(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(defun foo (a b &rest args &key c &allow-other-keys)\n            (list a b c))\n") "USER" :repl-thread 20)',
            '(:read-string 0 1)',
            '(:write-string "foo" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:read-aborted 0 1)',
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
            '(:read-string 0 1)',
            '(:write-string "(1 2 3)" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:read-aborted 0 1)',
            '(:return (:ok nil) 29)')

    # COMPLETIONS
    def test_swank_completions_1(self):
        self.assertSocket(
            '(:emacs-rex (swank:completions "find-if" (quote "USER")) "USER" :repl-thread 11)',
            '(:return (:ok (("find-if" "find-if-not") "find-if")) 11)')

    def test_swank_completions_2(self):
        self.assertSocket(
            '(:emacs-rex (swank:completions "find-if-no" (quote "USER")) "USER" :repl-thread 13)',
            '(:return (:ok (("find-if-not") "find-if-not")) 13)')

    def test_swank_completions_3(self):
        self.assertSocket(
            '(:emacs-rex (swank:completions "*pro" (quote "USER")) "USER" :repl-thread 18)',
            '(:return (:ok (("*program-name*" "*projected-edges*" "*prompt*" "*prompt-string*") "*pro")) 18)')

    def test_swank_completions_4(self):
        self.assertSocket(
            '(:emacs-rex (swank:completions "*prompt" (quote "USER")) "USER" :repl-thread 20)',
            '(:return (:ok (("*prompt*" "*prompt-string*") "*prompt")) 20)')

    def test_swank_completions_5(self):
        self.assertSocket(
            '(:emacs-rex (swank:completions "*prompt-" (quote "USER")) "USER" :repl-thread 23)',
            '(:return (:ok (("*prompt-string*") "*prompt-string*")) 23)')

    # EMACS INTERRUPT
    def test_emacs_interrupt(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(loop)\n") "USER" :repl-thread 5)',
            '(:read-string 0 1)')
        self.assertSocket('(:emacs-interrupt 0)',
                          '(:read-aborted 0 1)',
                          '(:return (:abort "\'Keyboard Interrupt\'") 5)')
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "10\n") "USER" :repl-thread 6)',
            '(:read-string 0 1)',
            '(:write-string "10" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:read-aborted 0 1)',
            '(:return (:ok nil) 6)')

    # SET PACKAGE
    def test_swank_set_package(self):
        self.assertSocket(
            '(:emacs-rex (swank:set-package "LISP") "USER" :repl-thread 33)',
            '(:return (:ok ("LISP" "LISP:irteusgl")) 33)')
        self.assertSocket(
            '(:emacs-rex (swank:set-package "USER") "LISP" :repl-thread 35)',
            '(:return (:ok ("USER" "irteusgl")) 35)')

    # APROPOS
    def test_apropos_1(self):
        self.assertSocket(
            '(:emacs-rex (swank:apropos-list-for-emacs "prompt" t nil (quote nil)) "USER" :repl-thread 17)',
            '(:return (:ok ((:designator "*PROMPT*" :variable :not-documented) (:designator "*PROMPT-STRING*" :variable :not-documented) (:designator "LISP::PROMPT" :function "(strm)") (:designator "SLIME::LAST-PROMPT" :variable :not-documented) (:designator "SLIME::SLIME-PROMPT" :function "nil") (:designator "TOPLEVEL-PROMPT" :function "(strm)"))) 17)')


if __name__ == '__main__':
    unittest.main()
