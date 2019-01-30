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
POLL_RATE = 1.0
DELIM = os.linesep
REGEX_ANSI = re.compile(r'\x1b[^m]*m')
EXEC_TIMEOUT = 0.05


def get_signal(signum):
    return [v for v,k in signal.__dict__.iteritems() if k == signum][0]


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

        self.process = subprocess.Popen(
            self.cmd,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            stdin=subprocess.PIPE,
            bufsize=self.bufsize,
            close_fds=IS_POSIX,
            env=os.environ.copy(),
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
        if self.process.poll() is None:
            try:
                self.process.kill()
                self.process.communicate()
            except Exception as e:
                log.warn("Failed to kill: %s" % e)
        if self.process.poll() is None:
            log.warn("Failed to kill process %s?" % self.process)

        for t in self.threads:
            if t.is_alive:
                t.join()
        log.debug("Stop Finished")

    def reset(self):
        self.process.stdin.write('"euslime-internal-token" reset' + self.delim)

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
                buf = self.input_queue.get(block=True, timeout=1)
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
        self.error.put("[Fatal Error] Process exited with code %d (%s)" %
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

class IntermediateResult(object):
    def __init__(self, value):
        self.value = value

class EuslispProcess(Process):
    def __init__(self, timeout=None):
        super(EuslispProcess, self).__init__(
            cmd=["roseus", "~/.euslime/slime-loader.l"],
            on_output=self.on_output,
            on_error=self.on_error,
        )

        self.processing = False
        self.output = None
        self.error = None
        self.timeout = timeout or EXEC_TIMEOUT

    def on_output(self, msg):
        msg = REGEX_ANSI.sub(str(), msg) + self.delim
        log.debug("output: %s" % msg)
        self.output.put(msg)

    def on_error(self, msg):
        msg = REGEX_ANSI.sub(str(), msg)
        log.debug("error: %s" % msg)
        self.error.put(msg)

    def parse_stack(self, desc):
        log.info("parse_stack")
        strace = list()
        # get description
        while not desc:
            log.info("get desc {}".format(time.time()))
            try:
                desc = self.output.get(timeout=self.timeout)
            except Empty:
                continue
        desc = desc.split("irteusgl 0 error: ")[-1]
        desc = desc.strip().capitalize()

        # wait for error messages
        # TODO: use custom tokens on euserror
        time.sleep(self.timeout)

        # get call stack
        while not self.error.empty():
            e = self.error.get(timeout=self.timeout)
            log.info("stack error: %s" % e)
            split = e.strip().split(": at ")
            if len(split) == 2:
                num, msg = split
                strace.append([int(num), msg,
                               [Symbol(":restartable"), False]])
            else:
                break
        return desc, strace

    def exec_command(self, cmd_str, internal=False):
        log.info("cmd_str: %s", cmd_str)
        self.output = Queue()
        self.error = Queue()

        if internal:
            # Tokens matching this prefix are not added to history
            # during the repl-loop redifined in slime-toplevel.l
            token = 'euslime-internal-token' + str(uuid1())
        else:
            token = 'euslime-token' + str(uuid1())
        cmd_str = """"{0}" {1}""".format(token, cmd_str)

        self.input(cmd_str)

        while self.process.poll() is None:
            # token is placed before and after the command result
            # yield printed messages and finally the result itself
            self.processing = True
            while True:
                # get output from queue
                try:
                    out = self.output.get(timeout=self.timeout)
                    have_token = out.split('"%s"' % token)
                except Empty:
                    out = None

                # check for presence of the first token
                # representing the start of the final result
                if out and len(have_token) > 1:
                    # Pickup outputs that do not end with newline
                    if have_token[0]:
                        yield IntermediateResult(have_token[0])

                    # Gather the final result value,
                    # which may be accross multiple lines
                    result = ""
                    while True:
                        try:
                            out = self.output.get(timeout=self.timeout)
                        except Empty:
                            continue
                        if out[1:-2] == token:
                            # finished evaluation
                            yield result
                            self.processing = False
                            return
                        else:
                            result += out
                # Pickup Intermediate Results
                # (messages printed before the final result)
                else:
                    if self.error.empty():
                        if out:
                            yield IntermediateResult(out)
                    else:
                        # Catch error
                        while not self.error.empty():
                            # Since *error-output* is re-binded to *standard-output*,
                            # the error stack only have lower level (eus-c level) errors.
                            # Therefore, we can parse without worrying about clashing
                            err = self.error.get(timeout=self.timeout)
                            if "Call Stack" in err:
                                raise EuslispError(*self.parse_stack(out))
                            if "Segmentation Fault" in err:
                                err = ["Segmentation Fault"]
                                while not self.error.empty():
                                    err.append(self.error.get(timeout=self.timeout))
                                self.reset()
                                raise EuslispError(self.delim.join(err))
                            if err:
                                yield IntermediateResult(err + self.delim)
                        if out:
                            yield IntermediateResult(out)


    def eval(self, cmd_str, internal=False):
        log.info("eval: %s" % cmd_str)
        if not cmd_str.strip():
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
            # remove newline
            return result[:-1]
        else:
            return list()
