from __future__ import print_function

import os
import sys
import errno
import re
import subprocess
import signal
import time
from threading import Thread
from threading import Lock
from Queue import Queue, Empty
from sexpdata import dumps, loads, Symbol
from uuid import uuid1


from euslime.logger import get_logger

log = get_logger(__name__)
IS_POSIX = 'posix' in sys.builtin_module_names
BUFSIZE = 1
BUFLENGTH = 5000
POLL_RATE = 0.5
EXEC_RATE = 0.05
DELIM = os.linesep
REGEX_ANSI = re.compile(r'\x1b[^m]*m')

def get_signal(signum):
    return [v for v,k in signal.__dict__.iteritems() if k == signum][0]

def no_color(msg):
    return REGEX_ANSI.sub(str(), msg)

class Process(object):
    def __init__(self, cmd,
                 on_output=None,
                 on_error=None,
                 poll_rate=None,
                 bufsize=None,
                 delim=None,):
        self.cmd = cmd
        self.on_output = on_output or self.default_print_callback
        self.on_error = on_error or self.default_print_callback
        self.poll_rate = poll_rate or POLL_RATE
        self.bufsize = bufsize or BUFSIZE
        self.delim = delim or DELIM
        self.lock = Lock()
        self.process = None
        self.threads = None
        self.input_queue = None

    def start(self, daemon=True):
        self.input_queue = Queue()

        slime_env = os.environ.copy()
        # Add the following to force line buffering in ROS logger,
        # as explained in section 8 of http://wiki.ros.org/rosconsole
        slime_env['ROSCONSOLE_STDOUT_LINE_BUFFERED'] = '1'

        self.process = subprocess.Popen(
            self.cmd,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            stdin=subprocess.PIPE,
            bufsize=self.bufsize,
            close_fds=IS_POSIX,
            env=slime_env,
        )

        self.threads = [
            Thread(target=self._get_stream_thread,
                   args=("stdout", self.process.stdout, self.on_output)),
            Thread(target=self._get_stream_thread,
                   args=("stderr", self.process.stderr, self.on_error)),
            Thread(target=self._put_stream_thread,
                   args=("stdin", self.process.stdin)),
        ]
        for i, t in enumerate(self.threads):
            t.daemon = True
            t.start()
        main = Thread(target=self._main_thread)
        self.threads.append(main)
        if not daemon:
            main.daemon = True
        main.start()

    def stop(self):
        if self.process.poll() is None:
            try:
                self.process.terminate()
                self.process.communicate()
            except Exception as e:
                log.warn("failed to terminate: %s" % e)
        log.debug("Stop Finished")

    def reset(self):
        self.process.stdin.write('"euslime-internal-token" (reset)' + self.delim)
        self.process.stdin.flush()

    def ping(self):
        log.debug("Ping...")
        list(self.exec_command(self.delim, internal=True, timeout=self.poll_rate))
        log.debug("...Pong")

    def _get_stream_thread(self, name, stream, callback):
        buf = str()
        while self.process.poll() is None:
            try:
                c = stream.read(1)
            except ValueError:
                break
            if c == self.delim:
                try:
                    callback(buf)
                except Exception as e:
                    log.error(e)
                buf = str()
            else:
                buf += c
        else:
            try:
                buf += stream.read()
            except ValueError:
                pass
            if buf:
                callback(buf)
            log.debug("Thread %s is dead" % name)

    def _put_stream_thread(self, name, stream):
        while self.process.poll() is None:
            try:
                buf = self.input_queue.get(timeout=self.poll_rate)
            except Empty:
                continue
            try:
                if buf:
                    stream.write(buf)
                    stream.flush()
            except IOError as e:
                if e.errno == errno.EINTR:
                    pass
            except Exception as e:
                log.error(e)
        else:
            log.debug("Thread %s is dead" % name)

    def _main_thread(self):
        while self.process.poll() is None:
            time.sleep(self.poll_rate)

        signum = abs(self.process.returncode)
        self.error.put("Process exited with code %d (%s)" %
                       (signum, get_signal(signum)))


    def input(self, cmd):
        cmd = cmd.strip()
        if not cmd.endswith(self.delim):
            cmd += self.delim
        self.input_queue.put(cmd)


class EuslispError(Exception):
    def __init__(self, message, stack=None):
        self.stack = stack
        super(EuslispError, self).__init__(message)

class EuslispFatalError(EuslispError):
    def __init__(self, message, continuable=False):
        self.continuable = continuable
        super(EuslispFatalError, self).__init__(message)

class IntermediateResult(object):
    def __init__(self, value):
        self.value = value

class EuslispProcess(Process):
    def __init__(self, exec_rate=None, buflen=None, color=False):
        super(EuslispProcess, self).__init__(
            cmd=["roseus", "~/.euslime/slime-loader.l"],
            on_output=self.on_output,
            on_error=self.on_error,
        )

        self.color = color # Requires slime-repl-ansi-color
        self.processing = False
        self.output = Queue()
        self.error = Queue()
        self.stack_error = False
        self.rate = exec_rate or EXEC_RATE
        self.buflen = buflen or BUFLENGTH

    def on_output(self, msg):
        if not self.color:
            msg = no_color(msg)
        log.debug("output: %s" % msg)
        self.output.put(msg)

    def on_error(self, msg):
        msg_nc = no_color(msg)
        if not self.color:
            msg = msg_nc
        log.debug("error: %s" % msg)
        if self.stack_error:
            self.error.put(msg)
        elif msg_nc.startswith(("Call Stack", ";; Segmentation Fault")):
            # Since *error-output* is re-binded to *standard-output*,
            # the error stack only have lower level (eus-c level) errors.
            # Therefore, we can parse without worrying about clashing
            self.stack_error = True
            self.error.put(msg)
        else:
            # Redirect warnings to output queue
            self.output.put(msg)

    def parse_stack(self, desc):
        # get description
        while True:
            if desc is not None:
                split = desc.split("irteusgl 0 error: ", 1)
                if len(split) == 2:
                    desc = split[-1]
                    break
                yield IntermediateResult(desc)
                desc = None
            else:
                try:
                    desc = self.output.get(timeout=self.rate)
                except Empty:
                    continue
                except KeyboardInterrupt:
                    # Rarely Call Stack is activated without an error message
                    # e.g. When sending KeyboardInterrupt to the following:
                    # (let ((i 0)) (while t (format t "~a~%" (incf i))))
                    raise Exception('Keyboard Interrupt')
        if self.color:
            # No colors are allowed in sldb
            desc = no_color(desc)
        desc = desc.strip().capitalize()

        # wait for error messages
        # TODO: use custom tokens on euserror
        time.sleep(self.rate)

        # get call stack
        strace = list()
        while not self.error.empty():
            e = self.error.get(timeout=self.rate)
            log.debug("Stack error: %s" % e)
            split = e.strip().split(": at ", 1)
            if len(split) == 2:
                num, msg = split
                strace.append([int(num), msg,
                               [Symbol(":restartable"), False]])
            else:
                break
        yield desc, strace

    def handle_error(self, desc):
        err = self.error.get(timeout=self.rate)
        err_nc = no_color(err)
        if err_nc.startswith("Call Stack"):
            for r in self.parse_stack(desc):
                if isinstance(r, IntermediateResult):
                    yield r
                else:
                    raise EuslispError(*r)
        elif err_nc.startswith(";; Segmentation Fault"):
            err = "Segmentation Fault"
            continuable = True
            self.reset()
        else:
            continuable = False
        err = [err]
        while not self.error.empty():
            err.append(self.error.get(timeout=self.rate))
        raise EuslispFatalError(self.delim.join(err), continuable=continuable)

    def exec_command(self, cmd_str, internal=False, timeout=None):
        self.output = Queue()
        self.error = Queue()
        self.stack_error = False

        if internal:
            # Tokens matching this prefix are not added to history
            # during the repl-loop redifined in slime-toplevel.l
            token = 'euslime-internal-token' + str(uuid1())
        else:
            token = 'euslime-token' + str(uuid1())

        cmd_str = """"{0}" {1}""".format(token, cmd_str.encode('utf-8'))
        self.input(cmd_str)
        self.processing = True

        if timeout:
            start = time.time()
        out_stack = list()
        out_len = 0
        # token is placed before and after the command result
        # yield printed messages and finally the result itself
        while True:
            # get output from queue
            try:
                out = self.output.get(timeout=self.rate)
                have_token = out.split('"%s"' % token, 1)
            except Empty:
                if out_stack:
                    yield IntermediateResult(self.delim.join(out_stack))
                    out_stack = list()
                    out_len = 0
                out = None

            # check for presence of the first token
            # representing the start of the final result
            if out and len(have_token) == 2:
                # Pickup outputs that do not end with newline
                if have_token[0]:
                    out_stack.append(have_token[0])
                if out_stack:
                    yield IntermediateResult(self.delim.join(out_stack))

                # Gather the final result value,
                # which may be accross multiple lines
                result = list()
                while True:
                    try:
                        out = self.output.get(timeout=self.rate)
                    except Empty:
                        continue
                    if out[1:-1] == token:
                        # finished evaluation
                        yield self.delim.join(result)
                        self.processing = False
                        return
                    else:
                        result.append(out)
            else:
                if timeout and time.time() - start > timeout:
                    raise EuslispFatalError("Timeout Reached", continuable=True)
                if self.error.empty():
                    # Pickup Intermediate Results
                    # (messages printed before the final result)
                    if out is not None:
                        # Pack output for increased performance
                        if out_len > self.buflen:
                            yield IntermediateResult(self.delim.join(out_stack))
                            out_stack = list()
                            out_len = 0
                        out_stack.append(out)
                        out_len += len(out)
                else:
                    for r in self.handle_error(out):
                        yield r


    def eval(self, cmd_str, internal=False):
        cmd_str = cmd_str.strip()
        log.info("eval: %s" % cmd_str)
        if not cmd_str:
            yield '; No value'
        else:
            for r in self.exec_command(cmd_str, internal):
                yield r

    def eval_block(self, cmd_str, only_result=False):
        res = list(self.eval(cmd_str, only_result))
        log.info("result: %s" % res)
        if only_result:
            return res[-1]
        else:
            return res

    def toplevel_prompt(self, package):
        cmd = """(slime::slime-prompt "{0}")""".format(package)
        result = self.eval_block(cmd, only_result=True)
        return loads(result)

    def find_symbol(self, start):
        cmd = """(slime::slime-find-symbol "{0}")""".format(start)
        result = self.eval_block(cmd, only_result=True)
        return loads(result)

    def find_keyword(self, start, form):
        cmd = """(slime::slime-find-keyword "{0}" '{1})""".format(start, form)
        result = self.eval_block(cmd, only_result=True)
        return loads(result)

    def find_character(self, start):
        cmd = """(slime::slime-find-character "{0}")""".format(start)
        result = self.eval_block(cmd, only_result=True)
        return loads(result)

    def arglist(self, func, cursor=None, form=None):
        cmd = """(slime::autodoc "{0}" {1} '{2})""".format(func, dumps(cursor), dumps(form))
        result = self.eval_block(cmd, only_result=True)
        if loads(result):
            return result
        else:
            return list()
