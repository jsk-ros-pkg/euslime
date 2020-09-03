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

try:
    from Queue import Queue
except ImportError:
    from queue import Queue

log = get_logger(__name__)
IS_POSIX = 'posix' in sys.builtin_module_names
HEADER_LENGTH = 6
BUFSIZE = 1
BUFLENGTH = 7000
EXEC_RATE = 0.005
DELIM = os.linesep
REGEX_ANSI = re.compile(r'\x1b[^m]*m')


def get_signal(signum):
    return [v for v, k in signal.__dict__.iteritems() if k == signum][0]


def no_color(msg):
    return REGEX_ANSI.sub(str(), msg)


def gen_to_string(gen):
    return ''.join(list(gen))


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
        self.output = Queue()
        self.accumulate_output = Event()
        self.process = None
        self.threads = None

    def start(self):
        slime_env = os.environ.copy()
        # Add the following to force line buffering in ROS logger,
        # as explained in section 8 of http://wiki.ros.org/rosconsole
        slime_env['ROSCONSOLE_STDOUT_LINE_BUFFERED'] = '1'

        self.accumulate_output.set()  # ignore init messages
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
            self.exec_internal("(lisp:reset)" + self.delim, force_repl_socket=True)
        except AbortEvaluation:
            pass

    def _get_stream_thread(self, name, stream, callback):
        while self.process.poll() is None:
            try:
                buf = os.read(stream.fileno(), self.buflen)
                if not self.color:
                    buf = no_color(buf)
                if self.accumulate_output.is_set():
                    self.output.put(buf)
                else:
                    callback(buf)
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
            msg = "Process exited with code {0} ({1})".format(
                signum, get_signal(signum))
            raise EuslispError(msg, fatal=True)

    def input(self, cmd):
        cmd = cmd.strip().encode('utf-8')
        if not cmd.endswith(self.delim):
            cmd += self.delim
        self.process.stdin.write(cmd)
        self.process.stdin.flush()


class EuslispError(Exception):
    def __init__(self, message, stack=None, fatal=False):
        self.stack = stack
        self.fatal = fatal
        # capitalize() converts the rest of the string to downcase
        message = message[0].upper() + message[1:]
        super(EuslispError, self).__init__(message)


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

        super(EuslispProcess, self).__init__(
            cmd=[self.program, self.init_file, "--port1={}".format(port1), "--port2={}".format(port2)],
            color=color,
            on_output=on_output,
        )

        self.rate = exec_rate or EXEC_RATE
        self.buflen = buflen or BUFLENGTH

    def start(self):
        super(EuslispProcess, self).start()
        self.euslime_connection = self._socket_connect(self.socket)
        self.euslime_internal_connection = self._socket_connect(self.internal_socket)
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
            raise EuslispError('Socket connection closed', fatal=True)
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
        if command == 'error':
            if recursive:
                return
            msg = loads(data)
            stack = self.get_callstack()
            raise EuslispError(msg, stack)
        if command == 'abort':
            msg = loads(data)
            if msg:
                msg = "'{}'".format(msg)  # Better formatting
            raise AbortEvaluation(msg)
        raise Exception("Unhandled Socket Request Type: %s" % command)

    def get_socket_result(self, connection):
        while True:
            gen = self.get_socket_response(connection)
            if gen is not None:
                return gen

    def clear_socket_stack(self, connection):
        while True:
            command, data = self.recv_socket_data(connection, wait=False)
            if command is None:
                return
            msg = gen_to_string(data)
            log.debug("Ignoring: [%s] %s" % (command, msg))

    def get_callstack(self, end=10):
        self.output = Queue()
        self.accumulate_output.set()
        self.clear_socket_stack(self.euslime_connection)
        cmd_str = '(slime:print-callstack {})'.format(end + 4)
        self.euslime_connection.send(cmd_str + self.delim)
        self.get_socket_response(self.euslime_connection, recursive=True)
        stack = list(self.output.queue)
        self.accumulate_output.clear()
        stack = gen_to_string(stack)
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
            # Locks are performed from outside to yield results before releasing
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
            res = loads(res)
            if res == Symbol("lisp:nil"):
                return []
            return res
        except Exception as e:
            if not force_repl_socket and lock.locked():
                lock.release()
            raise e

    def eval(self, cmd_str):
        connection = self.euslime_connection
        self.clear_socket_stack(connection)
        log.info('eval: %s' % cmd_str)
        self.input(cmd_str)
        # Print Results
        # Do not use :repl-result presentation to enable copy-paste of
        # previous results, which are signilized as swank objects otherwise
        # e.g. #.(swank:lookup-presented-object-or-lose 0.)
        for r in self.get_socket_result(connection):
            # Colors are not allowed in :repl-result formatting
            yield [Symbol(":write-string"), no_color(r),
                   Symbol(":repl-result")]
        yield [Symbol(":write-string"), '\n', Symbol(":repl-result")]
