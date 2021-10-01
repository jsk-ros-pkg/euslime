from __future__ import print_function

import os
import re
import signal
import socket
import subprocess
import sys
import time
import traceback
from threading import Event, Lock, Thread
from sexpdata import loads, Symbol
from euslime.logger import get_logger

log = get_logger(__name__)
IS_POSIX = 'posix' in sys.builtin_module_names
HEADER_LENGTH = 6
BUFSIZE = 1
BUFLENGTH = 7000
EXEC_RATE = 0.005
DELIM = os.linesep
REGEX_ANSI = re.compile(r'\x1b[^m]*m')


def get_signal(signum):
    match = [v for v, k in signal.__dict__.iteritems() if k == signum]
    if match:
        return match[0]


def no_color(msg):
    return REGEX_ANSI.sub(str(), msg)


def gen_to_string(gen):
    return ''.join([x for x in list(gen) if isinstance(x, str)])


class AbortEvaluation(Exception):
    pass


class Process(object):
    def __init__(self, cmd,
                 color=False,
                 on_output=None,
                 bufsize=None,
                 delim=None,):
        self.cmd = cmd
        self.color = color  # Requires slime-repl-ansi-color
        self.on_output = on_output or self.default_print_callback
        self.bufsize = bufsize or BUFSIZE
        self.delim = delim or DELIM
        self.output = []
        self.read_mode = False
        self.accumulate_output = False
        self.finished_output = Event()
        self.process = None
        self.threads = None

    def start(self):
        slime_env = os.environ.copy()
        # Add the following to force line buffering in ROS logger,
        # as explained in section 8 of http://wiki.ros.org/rosconsole
        slime_env['ROSCONSOLE_STDOUT_LINE_BUFFERED'] = '1'

        self.accumulate_output = True  # ignore init messages
        log.debug("Starting process with command %s" % self.cmd)
        self.process = subprocess.Popen(
            self.cmd,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            stdin=subprocess.PIPE,
            bufsize=self.bufsize,
            close_fds=IS_POSIX,
            env=slime_env,
        )

        self.threads = [
            Thread(target=self._get_stream_thread,
                   args=("stdout", self.process.stdout, self.on_output)),
            # Thread(target=self._get_stream_thread,
            #        args=("stderr", self.process.stderr, self.on_output)),
        ]
        for t in self.threads:
            t.daemon = True
            t.start()

    def stop(self):
        if self.process.poll() is None:
            try:
                self.process.terminate()
                self.process.communicate()
            except Exception as e:
                log.warn("failed to terminate: %s" % e)

    def reset(self):
        try:
            cmd = "(lisp:reset)" + self.delim
            self.exec_internal(cmd, force_repl_socket=True)
        except AbortEvaluation:
            pass

    def _get_stream_thread(self, name, stream, callback):
        while self.process.poll() is None:
            try:
                buf = os.read(stream.fileno(), self.buflen)
                if not self.color:
                    buf = no_color(buf)
                has_token = buf.split(self.token)
                if len(has_token) >= 3:
                    log.error('More than one token detected on output!')
                for out in [x for x in has_token if x]:
                    if self.accumulate_output:
                        log.debug('accumulating output: %s' % out)
                        self.output.append(out)
                    else:
                        callback(out)
                if len(has_token) >= 2:
                    log.debug('repl output finished')
                    self.finished_output.set()
                if len(buf) < self.buflen * 0.8:
                    time.sleep(self.rate)
            except Exception:
                log.error(traceback.format_exc())
        else:
            log.debug("Thread %s is dead" % name)

    def default_print_callback(self, msg):
        log.warn('default_print_callback: %s', msg)

    def check_poll(self):
        if self.process.poll() is not None:
            signum = abs(self.process.returncode)
            signame = get_signal(signum)
            msg = "Process exited with code {}".format(signum)
            if signame:
                msg += " ({})".format(signame)
            raise EuslispFatalError(msg)

    def input(self, cmd):
        cmd = cmd.strip().encode('utf-8')
        if not cmd.endswith(self.delim):
            cmd += self.delim
        self.process.stdin.write(cmd)
        self.process.stdin.flush()


class EuslispError(Exception):
    def __init__(self, message, stack=None):
        self.stack = stack
        # capitalize() converts the rest of the string to downcase
        if len(message) > 0:
            message = message[0].upper() + message[1:]
        super(EuslispError, self).__init__(message)


class EuslispInternalError(EuslispError):
    # Only CONTINUE option
    pass


class EuslispFatalError(EuslispError):
    # Only RESTART option
    pass


class EuslispResult(object):
    def __init__(self, value, response_type='ok'):
        self.response_type = response_type
        self.value = value


class EuslispProcess(Process):
    def __init__(self, program=None, init_file=None, exec_rate=None,
                 buflen=None, on_output=None, color=False):
        self.program = program
        self.init_file = init_file

        self.socket = self._start_socket()
        self.internal_socket = self._start_socket()
        host1, port1 = self.socket.getsockname()
        host2, port2 = self.internal_socket.getsockname()
        self.token = '{}euslime-token-{}'.format(chr(29), port1)

        super(EuslispProcess, self).__init__(
            cmd=[self.program,
                 self.init_file,
                 "--port1={}".format(port1), "--port2={}".format(port2)],
            color=color,
            on_output=on_output,
        )

        self.rate = exec_rate or EXEC_RATE
        self.buflen = buflen or BUFLENGTH

    def start(self):
        super(EuslispProcess, self).start()
        self.euslime_connection = self._socket_connect(self.socket)
        self.euslime_internal_connection = self._socket_connect(
            self.internal_socket)
        self.euslime_connection_lock = Lock()
        self.euslime_internal_connection_lock = Lock()
        self.input('(slime:slimetop)')

    def _start_socket(self):
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.bind(("0.0.0.0", 0))
        s.listen(5)
        return s

    def _socket_connect(self, sock):
        host, port = sock.getsockname()
        log.info("Connecting to euslime socket on %s:%s..." % (host, port))
        conn, _ = sock.accept()
        log.info("...Connected to euslime socket!")
        return conn

    def recv_socket_length(self, connection, hex_len):
        if hex_len == str():
            # recv() returns null string on EOF
            raise EuslispFatalError('Socket connection closed')
        length = int(hex_len, 16)
        while length > 0:
            msg = connection.recv(length)
            log.debug("Socket Response: %s" % msg)
            length -= len(msg)
            yield msg
        return

    def recv_socket_data(self, connection, wait=True):
        def recv_next(wait=True):
            while True:
                try:
                    head_data = connection.recv(
                        HEADER_LENGTH, socket.MSG_DONTWAIT)
                    return self.recv_socket_length(connection, head_data)
                except socket.error:
                    if not wait:
                        return
                    time.sleep(self.rate)
                    self.check_poll()
                    continue
        command = recv_next(wait=wait)
        if command is not None:
            command = gen_to_string(command)
            log.debug('Waiting for socket data...')
            data = recv_next(wait=True)
            return command, data
        return None, None

    def get_socket_response(self, connection, recursive=False):
        command, data = self.recv_socket_data(connection)
        log.debug('Socket Request Type: %s' % command)
        if command == 'result':
            return data
        # Process generator to avoid pendant messages
        data = gen_to_string(data)
        if command == 'read':
            return [Symbol(":read-string"), 0, 1]
        if command == 'read-mode':
            log.debug("Entering read mode...")
            self.read_mode = True
            return [Symbol(":read-string"), 0, 1]
        if command == 'error':
            if recursive:
                log.debug('Waiting for repl output...')
                self.finished_output.wait()
                return
            msg = loads(data)
            stack = self.get_callstack()
            if connection == self.euslime_internal_connection:
                raise EuslispInternalError(msg, stack)
            else:
                raise EuslispError(msg, stack)
        if command == 'abort':
            msg = loads(data)
            if msg:
                msg = "'{}'".format(msg)  # Better formatting
            raise AbortEvaluation(msg)
        raise Exception("Unhandled Socket Request Type: %s" % command)

    def get_socket_result(self, connection, wait=False):
        while True:
            gen = self.get_socket_response(connection)
            if isinstance(gen, list):
                # read-string
                yield gen
            elif gen is not None:
                # Wait until output is finished
                if wait:
                    log.debug('Waiting for repl output...')
                    self.finished_output.wait()
                for val in gen:
                    yield val
                return

    def clear_socket_stack(self, connection):
        while True:
            command, data = self.recv_socket_data(connection, wait=False)
            if command is None:
                return
            msg = gen_to_string(data)
            log.debug("Ignoring: [%s] %s" % (command, msg))

    def get_callstack(self, end=10):
        self.output = []
        self.finished_output.clear()
        self.accumulate_output = True
        self.clear_socket_stack(self.euslime_connection)
        cmd_str = '(slime:print-callstack {})'.format(end + 4)
        log.info('exec: %s' % cmd_str)
        self.euslime_connection.send(cmd_str + self.delim)
        self.get_socket_response(self.euslime_connection, recursive=True)
        self.accumulate_output = False
        stack = gen_to_string(self.output)
        stack = [x.strip() for x in stack.split(self.delim)]
        # Remove 'Call Stack' and dummy error messages
        #  'Call Stack (max depth: 10):',
        #  '0: at (slime:print-callstack 10)',
        #  '1: at slime:slime-error',
        #  '2: at slime:slime-error'
        stack = stack[4:]
        strace = []
        for i, line in enumerate(stack):
            split_line = line.split(": at ", 1)
            if len(split_line) == 2:
                strace.append(
                    [i, split_line[1], [Symbol(":restartable"), False]])
            else:
                break
        self.euslime_connection.send(
            '(lisp:reset lisp:*replevel*)' + self.delim)
        return strace

    def exec_internal(self, cmd_str, force_repl_socket=False):
        if force_repl_socket:
            # When the command must be evaluated in the main thread due to
            # e.g. Thread Special variables
            # Locks are performed from outside to yield results before release
            connection = self.euslime_connection
            log.info('exec_internal(repl): %s' % cmd_str)
        else:
            connection = self.euslime_internal_connection
            log.info('exec_internal: %s' % cmd_str)
            lock = self.euslime_internal_connection_lock
            log.debug('Acquiring lock: %s' % lock)
            lock.acquire()
        try:
            self.clear_socket_stack(connection)
            connection.send(cmd_str + self.delim)
            gen = self.get_socket_result(connection)
            res = gen_to_string(gen)
            if not force_repl_socket:
                lock.release()
            # Keep nil as a symbol to be dump-reversible
            res = loads(res, nil=None)
            # But return python-false objects if the response is solely 'nil'
            if res == Symbol("lisp:nil") or res == Symbol("nil"):
                return []
            return res
        except Exception:
            if not force_repl_socket and lock.locked():
                lock.release()
            raise

    def eval(self, cmd_str):
        connection = self.euslime_connection
        self.clear_socket_stack(connection)
        self.finished_output.clear()
        log.info('eval: %s' % cmd_str)
        self.input(cmd_str)
        # Print Results
        # Do not use :repl-result presentation to enable copy-paste of
        # previous results, which are signilized as swank objects otherwise
        # e.g. #.(swank:lookup-presented-object-or-lose 0.)
        for r in self.get_socket_result(connection, wait=True):
            yield r
