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

    def socket_recv(self, times):
        def recv_one():
            try:
                len = self.socket.recv(HEADER_LENGTH)
                hex_len = int(len, 16)
                return self.socket.recv(hex_len)
            except socket.error:
                return;
        result = []
        for i in range(times):
            res = recv_one()
            log.info(res)
            if res == None:
                break
            result.append(res)
        return tuple(result) or None

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


if __name__ == '__main__':
    unittest.main()
