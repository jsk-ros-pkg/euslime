import pprint
import re
import socket
import time
import unittest

from euslime.logger import get_logger
from euslime.server import EuslimeServer
from threading import Thread

HEADER_LENGTH = 6
REGEX_ADDR = re.compile(r' #X[0-9a-f]*')

log = get_logger(__name__)


class EuslimeTestCase(unittest.TestCase):
    # values are defined in the child classes
    EUSLISP_PROGRAM = ''
    EUSLISP_PROGRAM_NAME = ''

    @classmethod
    def setUpClass(self):
        self.maxDiff = None
        self.validation_num = 0
        self.server = EuslimeServer(('0.0.0.0', 0),
                                    program=self.EUSLISP_PROGRAM)
        host, port = self.server.socket.getsockname()
        Thread(target=self.server.serve_forever).start()
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        # self.socket.settimeout(5)
        self.socket.connect((host, port))
        # Wait for process to fully start
        time.sleep(1)
        # Create REPL
        log.info('Creating REPL...')
        req = '(:emacs-rex (swank-repl:create-repl nil :coding-system "utf-8-unix") "COMMON-LISP-USER" t 4)'
        res = ('(:return (:ok ("USER" "{}")) 4)'.format(self.EUSLISP_PROGRAM_NAME),)
        response = self.socket_get_response(req)
        log.debug('expected response: \n%s', pprint.pformat(res, width=5))
        log.debug('received response: \n%s', pprint.pformat(response, width=5))
        if res != response:
            raise Exception('REPL Initialization Failed')

        log.info('Initialization Complete.')
        log.info('Start testing...\n')

    @classmethod
    def tearDownClass(self):
        log.info("Tearing down...")
        self.socket.shutdown(socket.SHUT_RDWR)
        self.socket.close()
        log.info("...DONE")

    def setUp(self):
        log.info('Testing %s', self)

    def __del__(self):
        try:
            self.socket.send('')
            # Close the socket if is still alive
            self.tearDownClass()
        except Exception:
            pass

    @classmethod
    def socket_recv_one(self, *options):
        try:
            len = self.socket.recv(HEADER_LENGTH, *options)
            hex_len = int(len, 16)
            return self.socket.recv(hex_len)
        except socket.error:
            return

    @classmethod
    def socket_send(self, req):
        log.info('request: \n%s', pprint.pformat(req, width=5))
        header = '{0:06x}'.format(len(req))
        self.socket.send(header + req)

    @classmethod
    def socket_get_response(self, req):
        self.validation_num += 1
        val_num = self.validation_num
        req = '(:euslime-test {} {})'.format(val_num, req)
        result = []
        self.socket_send(req)
        while True:
            res = self.socket_recv_one()
            if res is None or res == '(:euslime-test {})'.format(val_num):
                return tuple(result) or None
            result.append(res)

    def socket_get_response_no_wait(self, rate, *commands):
        result = []
        for c in commands:
            self.socket_send(c)
            time.sleep(rate)
            res = self.socket_recv_one(socket.MSG_DONTWAIT)
            while res:
                result.append(res)
                time.sleep(0.01)
                res = self.socket_recv_one(socket.MSG_DONTWAIT)
        return tuple(result) or None

    def socket_clean(self):
        while self.socket_recv_one(socket.MSG_DONTWAIT):
            pass

    def assertSocket(self, req, *res):
        response = self.socket_get_response(req)
        log.info('expected response: \n%s', pprint.pformat(res, width=5))
        log.info('received response: \n%s', pprint.pformat(response, width=5))
        self.assertEqual(res, response)

    def assertSocketNoWait(self, req_list, res_list, rate=0.05):
        res = tuple(res_list)
        response = self.socket_get_response_no_wait(rate, *req_list)
        log.info('expected response: \n%s', pprint.pformat(res, width=5))
        log.info('received response: \n%s', pprint.pformat(response, width=5))
        self.assertEqual(res, response)

    def assertSocketIgnoreAddress(self, req, *res):
        def substitute_address(lst):
            return [REGEX_ADDR.sub(str(), msg) for msg in lst]
        res = substitute_address(res)
        response = self.socket_get_response(req)
        response = substitute_address(response)
        log.info('expected response: \n%s', pprint.pformat(res, width=5))
        log.info('received response: \n%s', pprint.pformat(response, width=5))
        self.assertEqual(res, response)