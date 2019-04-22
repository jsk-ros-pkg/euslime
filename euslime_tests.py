#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Test suite for EusLisp SLIME

import pprint
import socket
import unittest

from euslime.logger import get_logger

HEADER_LENGTH = 6

log = get_logger(__name__)
host = '0.0.0.0'
port = 5050

class EuslimeTest(unittest.TestCase):
    def setUp(self):
        socket.setdefaulttimeout(5)
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.socket.connect((host, port))

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
        # assert res == response

    def test_1(self):
        log.info('TEST 1')
        self.assertSocket(
            '(:emacs-rex (swank-repl:listener-eval "(+ 1 1)\n") "USER" :repl-thread 7)',
            '(:read-string 0 1)',
            '(:write-string "2" :repl-result)',
            '(:write-string "\\n" :repl-result)',
            '(:read-aborted 0 1)',
            '(:new-package "USER" "irteusgl")',
            '(:return (:ok nil) 7)')


if __name__ == '__main__':
    unittest.main()
