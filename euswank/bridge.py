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
from uuid import uuid1

from euswank.logger import get_logger

log = get_logger(__name__)
IS_POSIX = 'posix' in sys.builtin_module_names
BUFSIZE = 1
POLL_RATE = 1.0
DELIM = os.linesep
REGEX_ANSI = re.compile(r'\x1b[^m]*m')


class Process(object):
    def __init__(self, cmd,
                 on_output=None,
                 on_error=None,
                 poll_rate=None,
                 bufsize=None,
                 delim=None,):
        self.cmd = cmd
        self.on_output = on_output or self.default_print_callback
        self.on_erorr = on_error or self.default_print_callback
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
    def __init__(self):
        super(EuslispProcess, self).__init__(
            cmd=["rosrun", "roseus", "roseus"],
            on_output=self.on_output,
            on_error=self.on_error,
        )

        self.output = None
        self.error = None

    def on_output(self, msg):
        msg = REGEX_ANSI.sub(str(), msg)
        log.debug("output: %s" % msg)
        self.output.append(msg)

    def on_error(self, msg):
        msg = REGEX_ANSI.sub(str(), msg)
        log.debug("error: %s" % msg)
        if not msg.startswith(";"):
            self.error.append(msg)

    def exec_command(self, cmd_str):
        token = "token-" + str(uuid1())
        sexp = loads(cmd_str)
        sexp = [
            Symbol('let'),
            [[Symbol(token), sexp]],
            [Symbol('format'), True, "~%%%s~%%" % token],
            Symbol(token),
        ]
        cmd_str = dumps(sexp)
        self.output = list()
        self.error = list()

        self.input(cmd_str)

        while self.process.poll() is None:
            time.sleep(0.1)
            if self.error:
                raise EuslispError(self.delim.join(self.error))
            if self.output:
                line = self.delim.join(self.output)
                pos = line.find(token)
                if pos != -1:
                    return line[pos+len(token)+1:]


def eus_eval_once(cmd):
    euslisp = EuslispProcess()
    euslisp.start()
    result = None
    try:
        result = euslisp.exec_command(cmd)
    except EuslispError as e:
        log.error("Failed to exec: %s" % e)
    finally:
        euslisp.stop()
    return result


if __name__ == '__main__':
    print("answer:", eus_eval_once("(lisp-implementation-version)"))
    print("answer:", eus_eval_once('(apropos-list "vec")'))
