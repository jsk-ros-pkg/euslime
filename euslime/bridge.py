from __future__ import print_function

import os
import sys
import errno
import re
import subprocess
import time
from threading import Thread
from threading import Lock
from Queue import Queue, Empty
from sexpdata import dumps, loads, Symbol

from euslime.logger import get_logger

log = get_logger(__name__)
IS_POSIX = 'posix' in sys.builtin_module_names
BUFSIZE = 1
POLL_RATE = 1.0
DELIM = os.linesep
REGEX_ANSI = re.compile(r'\x1b[^m]*m')
EXEC_TIMEOUT = 10


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

        if self.process.returncode == 0:
            log.info("process exited successfully")
        else:
            log.warn("process exited (code: %d)" % self.process.returncode)

    def input(self, cmd):
        cmd = cmd.strip()
        if not cmd.endswith(self.delim):
            cmd += self.delim
        self.input_queue.put(cmd)


class EuslispError(Exception):
    pass


class EuslispProcess(Process):
    def __init__(self, timeout=None):
        super(EuslispProcess, self).__init__(
            cmd=["roseus", "slime-toplevel.l", "slime-util.l"],
            on_output=self.on_output,
            on_error=self.on_error,
        )

        self.output = None
        self.error = None
        self.timeout = timeout or EXEC_TIMEOUT

    def on_output(self, msg):
        msg = REGEX_ANSI.sub(str(), msg) + self.delim
        log.debug("output: %s" % msg)
        self.output.put(msg)

    def on_error(self, msg):
        msg = REGEX_ANSI.sub(str(), msg)
        # log.debug("error: %s" % msg)
        if not msg.startswith(";"):
            self.error.put(msg)

    def exec_command(self, cmd_str):
        log.info("cmd_str: %s", cmd_str)
        self.output = Queue()
        self.error = Queue()

        self.input(cmd_str)

        last_output = time.time()
        while self.process.poll() is None:
            # catch error
            err = []
            while not self.error.empty():
                err.append(self.error.get())
            if err:
                raise EuslispError(self.delim.join(err))
            try:
                yield self.output.get(timeout=0.1)
                while not self.output.empty():
                    last_output = time.time()
                    yield self.output.get()
                return
            except Empty:
                if time.time() - last_output > self.timeout:
                    raise EuslispError("Timed out")

    def eval(self, cmd_str):
        log.info("eval: %s" % cmd_str)
        if not cmd_str.strip():
            yield '; No value'
        else:
            for r in self.exec_command(cmd_str):
                yield r

    def eval_block(self, cmd_str, only_result=False):
        res = list(self.eval(cmd_str))
        log.info("result: %s" % res)
        if only_result:
            return res[-1]
        else:
            return res

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


def eus_eval_once(cmd):
    euslisp = EuslispProcess()
    euslisp.start()
    result = None
    try:
        result = euslisp.eval_block(cmd, only_result=True)
    except EuslispError as e:
        log.error("Failed to exec: %s" % e)
    finally:
        euslisp.stop()
    return result


if __name__ == '__main__':
    print("answer:", eus_eval_once("(lisp-implementation-version)"))
    print("answer:", eus_eval_once('(apropos-list "vec")'))
