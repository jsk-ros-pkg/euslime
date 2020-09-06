import os
import pprint
import re
import socket
import time
import unittest

from euslime.logger import get_logger
from euslime.server import EuslimeServer
from sexpdata import dumps
from sexpdata import loads
from threading import Thread

HEADER_LENGTH = 6
REGEX_ADDR = re.compile(r' #X[0-9a-f]*')

log = get_logger(__name__)


class EuslimeTestCase(unittest.TestCase):
    # values are defined in the child classes
    EUSLISP_PROGRAM = ''
    EUSLISP_PROGRAM_NAME = ''
    USE_COLOR = False

    @classmethod
    def setUpClass(self):
        self.maxDiff = None
        self.validation_num = 0
        os.environ.update({'ROSCONSOLE_FORMAT':'[${severity}]: ${message}'})  # for rosout tests
        self.server = EuslimeServer(('0.0.0.0', 0),
                                    program=self.EUSLISP_PROGRAM,
                                    color=self.USE_COLOR)
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

    def socket_get_response_no_wait(self, rate_send, rate_recv, *commands):
        result = []
        for c in commands:
            self.socket_send(c)
            time.sleep(rate_send)
        time.sleep(0.1)
        res = self.socket_recv_one(socket.MSG_DONTWAIT)
        while res:
            result.append(res)
            time.sleep(rate_recv)
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

    def assertSocketWriteString(self, req, *res):
        def join_write_string(first, *more):
            """Merge the string part of several :write-string lines into one statement"""
            if not more:
                return first
            first[1] = first[1] + ''.join([x[1] for x in more])
            return first

        def get_buffer_type(loaded_form):
            if len(loaded_form) < 3:
                return None
            else:
                return loaded_form[2]

        def with_join_write_string(lst):
            output = []
            buffer = []
            buffer_type = None
            for a in lst:
                is_write_string = a[1:14] == ':write-string'
                if is_write_string:
                    ld_a = loads(a)
                    bt = get_buffer_type(ld_a)
                    if buffer and not buffer_type == bt:
                        out = dumps(join_write_string(*buffer))
                        output.append(out)
                        buffer = []
                    buffer_type = bt
                    buffer.append(ld_a)
                else:
                    if buffer:
                        out = dumps(join_write_string(*buffer))
                        output.append(out.encode('utf-8'))
                        buffer = []
                    output.append(a)
            return output

        response = self.socket_get_response(req)
        res = with_join_write_string(res)
        response = with_join_write_string(response)
        log.info('expected response: \n%s', pprint.pformat(res, width=5))
        log.info('received response: \n%s', pprint.pformat(response, width=5))
        self.assertEqual(res, response)

    def assertSocketNoWait(self, req_list, res_list, rate_send=0.05, rate_recv=0.05):
        res = tuple(res_list)
        response = self.socket_get_response_no_wait(rate_send, rate_recv, *req_list)
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
