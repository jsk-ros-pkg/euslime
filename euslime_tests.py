#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Test suite for EusLisp SLIME

import pprint
import re
import socket
import time
import unittest

from euslime.logger import get_logger
from euslime.server import EuslimeServer
from thread import start_new_thread

HEADER_LENGTH = 6
REGEX_ADDR = re.compile(r'#X[0-9a-f]{7} ')

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

    def socket_get_response(self, req, n):
        log.info('request: \n%s', req)
        # self.socket_clean()
        self.socket_send(req)
        return self.socket_recv(n)

    def assertSocket(self, req, *res):
        log.info('expected response: \n%s', pprint.pformat(res, width=5))
        response = self.socket_get_response(req, len(res))
        log.info('received response: \n%s', pprint.pformat(response, width=5))
        self.assertEqual(res, response)

    def assertSocketIgnoreAddress(self, req, *res):
        def substitute_address(lst):
            return [REGEX_ADDR.sub(str(), msg) for msg in lst]
        res = substitute_address(res)
        log.info('expected response: \n%s', pprint.pformat(res, width=5))
        response = self.socket_get_response(req, len(res))
        response = substitute_address(response)
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

    def test_swank_eval_7(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(y-or-n-p)\n") "USER" :repl-thread 21)',
            '(:read-string 0 1)',
            '(:write-string "(Y or N): ")')
        self.assertSocket(
            '(:emacs-return-string 0 1 "y\n")',
            '(:read-string 0 1)',
            '(:write-string "t" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:read-aborted 0 1)',
            '(:return (:ok nil) 21)')

    def test_swank_eval_8(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(setq *print-case* :upcase)\n") "USER" :repl-thread 39)',
            '(:read-string 0 1)',
            '(:write-string ":UPCASE" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:read-aborted 0 1)',
            '(:return (:ok nil) 39)')
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(list \'a \'b \'c)\n") "USER" :repl-thread 44)',
            '(:read-string 0 1)',
            '(:write-string "(A B C)" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:read-aborted 0 1)',
            '(:return (:ok nil) 44)')
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(setq *print-case* :downcase)\n") "USER" :repl-thread 49)',
            '(:read-string 0 1)',
            '(:write-string ":downcase" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:read-aborted 0 1)',
            '(:return (:ok nil) 49)')

    def test_swank_eval_9(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(setq *print-level* 0)\n") "USER" :repl-thread 60)',
            '(:read-string 0 1)',
            '(:write-string "#" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:read-aborted 0 1)',
            '(:return (:ok nil) 60)')
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "10\n") "USER" :repl-thread 61)',
            '(:read-string 0 1)',
            '(:write-string "#" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:read-aborted 0 1)',
            '(:return (:ok nil) 61)')
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(list 1 2 3)\n") "USER" :repl-thread 62)',
            '(:read-string 0 1)',
            '(:write-string "#" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:read-aborted 0 1)',
            '(:return (:ok nil) 62)')
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(setq *print-level* nil)\n") "USER" :repl-thread 63)',
            '(:read-string 0 1)',
            '(:write-string "nil" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:read-aborted 0 1)',
            '(:return (:ok nil) 63)')

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
            '(:read-string 0 1)',
            '(:write-string "2" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:read-aborted 0 1)',
            '(:return (:ok nil) 30)')
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(makunbound \'c)\n") "USER" :repl-thread 32)',
            '(:read-string 0 1)',
            '(:write-string "t" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:read-aborted 0 1)',
            '(:return (:ok nil) 32)')

    def test_compile_region_2(self):
        self.assertSocket(
            '(:emacs-rex (swank:compile-string-for-emacs "(print 1)\n(print 2)\n(print 3)\n(setq c (list 1 2 3))\n" "test.l" (quote ((:position 1) (:line 1 1))) "/tmp/test.l" (quote nil)) "USER" t 14)',
            '(:write-string "; Loaded (print 1)\\n")',
            '(:write-string "; Loaded (print 2)\\n")',
            '(:write-string "; Loaded (print 3)\\n")',
            '(:write-string "; Loaded (setq c ...)\\n")',
            '(:return (:ok (:compilation-result () t 0.01 nil nil)) 14)')
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "c\n") "USER" :repl-thread 15)',
            '(:read-string 0 1)',
            '(:write-string "(1 2 3)" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:read-aborted 0 1)',
            '(:return (:ok nil) 15)')
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(makunbound \'c)\n") "USER" :repl-thread 17)',
            '(:read-string 0 1)',
            '(:write-string "t" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:read-aborted 0 1)',
            '(:return (:ok nil) 17)')

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

    def test_swank_autodoc_16(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(progn (setq c (make-coords :name \\"test\\")) (send c :name))\n") "USER" :repl-thread 25)',
            '(:read-string 0 1)',
            '(:write-string "\\"test\\"" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:read-aborted 0 1)',
            '(:return (:ok nil) 25)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc (quote ("send" "c" ":pos" swank::%cursor-marker%)) :print-right-margin 100) "USER" :repl-thread 30)',
            '(:return (:ok ("(:pos)" t)) 30)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc (quote ("send" "c" ":replace-pos" "" swank::%cursor-marker%)) :print-right-margin 100) "USER" :repl-thread 37)',
            '(:return (:ok ("(:replace-pos ===> p <===)" t)) 37)')
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(makunbound \'c)\n") "USER" :repl-thread 42)',
            '(:read-string 0 1)',
            '(:write-string "t" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:read-aborted 0 1)',
            '(:return (:ok nil) 42)')

    def test_swank_autodoc_17(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(progn (make-irtviewer) (send *irtviewer* :name))\n") "USER" :repl-thread 78)',
            '(:read-string 0 1)',
            '(:write-string "\\"IRT viewer\\"" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:read-aborted 0 1)',
            '(:return (:ok nil) 78)')
        self.assertSocket(
            '(:emacs-rex (swank:autodoc (quote ("send" "*irtviewer*" ":view" swank::%cursor-marker%)) :print-right-margin 100) "USER" :repl-thread 85)',
            '(:return (:ok ("(:view)" t)) 85)')
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
            '(:read-string 0 1)',
            '(:write-string ":destroyed" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:read-aborted 0 1)',
            '(:return (:ok nil) 95)')

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

    def test_completions_6(self):
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":test" (quote ("find" "1" ("list" "1" "2" "3") "" swank::%cursor-marker%))) "USER" :repl-thread 17)',
            '(:return (:ok ((":test" ":test-not") ":test")) 17)')

    def test_completions_7(self):
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":test-n" (quote ("find" "1" ("list" "1" "2" "3") "" swank::%cursor-marker%))) "USER" :repl-thread 19)',
            '(:return (:ok ((":test-not") ":test-not")) 19)')

    def test_completions_8(self):
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":count" (quote ("find" "1" ("list" "1" "2" "3") ":test-not" "" swank::%cursor-marker%))) "USER" :repl-thread 22)',
            '(:return (:ok ((":count") ":count")) 22)')

    def test_completions_9(self):
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":none" (quote ("find" "1" ("list" "1" "2" "3") ":test-not" ":count" "" swank::%cursor-marker%))) "USER" :repl-thread 26)',
            '(:return (:ok ()) 26)')

    def test_completions_10(self):
        self.assertSocket(
            '(:emacs-rex (swank:completions ":none" (quote "USER")) "USER" :repl-thread 27)',
            '(:return (:ok ()) 27)')

    def test_completions_11(self):
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(progn (setq c (make-coords :name \\"test\\")) (send c :name))\n") "USER" :repl-thread 31)',
            '(:read-string 0 1)',
            '(:write-string "\\"test\\"" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:read-aborted 0 1)',
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
            '(:read-string 0 1)',
            '(:write-string "t" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:read-aborted 0 1)',
            '(:return (:ok nil) 45)')

    def test_completions_12(self):
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":position" (quote nil)) "USER" :repl-thread 46)',
            '(:return (:ok ((":position" ":position-list" ":position_covariance" ":position_covariance_type") ":position")) 46)')

    def test_completions_13(self):
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-keyword ":test" (quote nil)) "USER" :repl-thread 47)',
            '(:return (:ok ((":test" ":test-not") ":test")) 47)')

    def test_completions_14(self):
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-character "Ne") "USER" :repl-thread 19)',
            '(:return (:ok (("Newline") "Newline")) 19)')

    def test_completions_15(self):
        self.assertSocket(
            '(:emacs-rex (swank:completions-for-character "") "USER" :repl-thread 20)',
            '(:return (:ok (("Space" "Newline" "Linefeed" "Backspace" "Delete" "Rubout" "Return" "Page" "Formfeed" "Esc" "Escape" "Tab" "Left-Paren" "Right-Paren" "Lparen" "Rparen" "Bell" "Null" "SOH" "STX" "ETX") "")) 20)')

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

    def test_apropos_2(self):
        self.assertSocket(
            '(:emacs-rex (swank:apropos-list-for-emacs "find-" t nil (quote "LISP")) "USER" :repl-thread 10)',
            '(:return (:ok ((:designator "FIND-EXECUTABLE" :function "(progname)") (:designator "FIND-IF" :function "(pred seq &key (start 0) (end (length seq)) (key #\'identity))") (:designator "FIND-IF-NOT" :function "(pred seq &key (start 0) (end (length seq)) (key #\'identity))") (:designator "FIND-METHOD" :function :not-documented) (:designator "FIND-PACKAGE" :function :not-documented) (:designator "FIND-SYMBOL" :function :not-documented))) 10)')

    # DESCRIBE
    def test_describe_1(self):
        self.assertSocketIgnoreAddress(
            '(:emacs-rex (swank:describe-definition-for-emacs "*PROMPT-STRING*" :variable) "USER" t 13)',
            '(:return (:ok "NAME\\n     *prompt-string*\\nTYPE\\n     variable\\nDESCRIPTION\\n     prompt string used by \x1b[1meustop\x1b[m. \\n\\nPROPERTIES\\n\\nplist=nil\\nvalue=\\"irteusgl\\"\\nvtype=2\\nfunction=*unbound*\\npname=\\"*PROMPT-STRING*\\"\\nhomepkg=#<package #X5f12ae8 LISP>\\n") 13)')

    def test_describe_2(self):
        self.assertSocketIgnoreAddress(
            '(:emacs-rex (swank:describe-symbol "*prompt-string*") "USER" :repl-thread 16)',
            '(:return (:ok "NAME\\n     *prompt-string*\\nTYPE\\n     variable\\nDESCRIPTION\\n     prompt string used by \x1b[1meustop\x1b[m. \\n\\nPROPERTIES\\n\\nplist=nil\\nvalue=\\"irteusgl\\"\\nvtype=2\\nfunction=*unbound*\\npname=\\"*PROMPT-STRING*\\"\\nhomepkg=#<package #X5f12ae8 LISP>\\n") 16)')

if __name__ == '__main__':
    unittest.main()
