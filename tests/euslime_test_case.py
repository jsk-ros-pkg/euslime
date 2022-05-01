import os
import pprint
import re
import socket
import time
import traceback
import unittest

from euslime.logger import get_logger
from euslime.server import EuslimeServer
from sexpdata import dumps
from sexpdata import loads
from threading import Thread

HEADER_LENGTH = 6
REGEX_ADDR = re.compile(r' #X[0-9a-f]*')
REGEX_C_ADDR = re.compile(r'-?[0-9a-f]{6,12}')
REGEX_GENSYM = re.compile(r'#:[^ )]*')

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

    def socket_clean(self):
        while self.socket_recv_one(socket.MSG_DONTWAIT):
            pass

    def with_join_write_string(self, lst):
        output = []
        buffer = []
        buffer_type = None

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

        def check_buffer():
            if buffer:
                out = dumps(join_write_string(*buffer))
                output.append(out.encode('utf-8'))
                return True
            return False

        for a in lst:
            is_write_string = a[1:14] == ':write-string'
            if is_write_string:
                ld_a = loads(a)
                bt = get_buffer_type(ld_a)
                if not buffer_type == bt and check_buffer():
                    buffer = []
                buffer_type = bt
                buffer.append(ld_a)
            else:
                if check_buffer():
                    buffer = []
                output.append(a)
        check_buffer()
        return output

    def with_unwind_protect(self, *body):
        error = None
        for form in body:
            fn = form[0]
            if isinstance(form[-1], dict):
                args = form[1:-1]
                kargs = form[-1]
            else:
                args = form[1:]
                kargs = None
            try:
                if kargs:
                    fn(*args, **kargs)
                else:
                    fn(*args)
            except AssertionError as e:
                # print(traceback.format_exc())
                traceback.print_exception(type(e), e, None)
                if error is None:
                    error = e
        if error:
            raise error

    def assertSocket(self, req, *res):
        response = self.socket_get_response(req)
        log.info('expected response: \n%s', pprint.pformat(res, width=5))
        log.info('received response: \n%s', pprint.pformat(response, width=5))
        self.assertEqual(res, response)

    def assertSocketSameResult(self, *req):
        res_list = [self.socket_get_response(x) for x in req]
        for x,y in zip(res_list, res_list[1:]):
            self.assertEqual(x,y)

    def assertSocketPossibleResults(self, req, *res_lsts):
        response = list(self.socket_get_response(req))
        recv_response_msg = pprint.pformat(response, width=5)
        log.info('received response: \n%s', recv_response_msg)
        self.assertTrue(response in res_lsts,
                        'Response not found in list of possible results: \n\n{}'.
                        format(recv_response_msg))

    def assertSocketWriteString(self, req, *res):
        response = self.socket_get_response(req)
        res = self.with_join_write_string(res)
        response = self.with_join_write_string(response)
        log.info('expected response: \n%s', pprint.pformat(res, width=5))
        log.info('received response: \n%s', pprint.pformat(response, width=5))
        self.assertEqual(res, response)

    def assertSocketIgnoreAddress(self, req, *res):
        def substitute_address(lst):
            lst = [REGEX_ADDR.sub(str(), msg) for msg in lst]
            lst = [REGEX_GENSYM.sub('#:g0', msg) for msg in lst]
            return lst
        res = substitute_address(res)
        response = self.socket_get_response(req)
        response = substitute_address(response)
        log.info('expected response: \n%s', pprint.pformat(res, width=5))
        log.info('received response: \n%s', pprint.pformat(response, width=5))
        self.assertEqual(res, response)

    def assertAsyncRequest(self, req_list, res_list, rate_send=0.05,
                           ignore_address=False, ignore_c_address=False,
                           ignore_gensym=False,
                           unordered_output=False):
        handshake_list = []
        response = []
        for req in req_list:
            self.validation_num += 1
            val_num = self.validation_num
            handshake_list.append('(:euslime-test {})'.format(val_num))
            req = '(:euslime-test {} {})'.format(val_num, req)
            self.socket_send(req)
            time.sleep(rate_send)
        while handshake_list:
            res = self.socket_recv_one()
            if res in handshake_list:
                handshake_list.remove(res)
                continue
            response.append(res)
        if ignore_address:
            res_list = [REGEX_ADDR.sub(str(), msg) for msg in res_list]
            response = [REGEX_ADDR.sub(str(), msg) for msg in response]
        if ignore_c_address:
            res_list = [REGEX_C_ADDR.sub(str(), msg) for msg in res_list]
            response = [REGEX_C_ADDR.sub(str(), msg) for msg in response]
        if ignore_gensym:
            res_list = [REGEX_GENSYM.sub('#:g0', msg) for msg in res_list]
            response = [REGEX_GENSYM.sub('#:g0', msg) for msg in response]
        if unordered_output:
            def split_out(lst):
                out = []
                other = []
                for l in lst:
                    if l[1:14] == ':write-string':
                        out.append(l)
                    else:
                        other.append(l)
                return other, self.with_join_write_string(out)
            res_list, res_out_list = split_out(res_list)
            response, response_out = split_out(response)
            log.info('expected output: \n%s', pprint.pformat(res_out_list, width=5))
            log.info('received output: \n%s', pprint.pformat(response_out, width=5))
            self.assertEqual(res_out_list, response_out)
        log.info('expected response: \n%s', pprint.pformat(res_list, width=5))
        log.info('received response: \n%s', pprint.pformat(response, width=5))
        self.assertEqual(res_list, response)
