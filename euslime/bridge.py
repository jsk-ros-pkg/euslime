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
    def __init__(self, timeout=None):
        super(EuslispProcess, self).__init__(
            cmd=["rosrun", "roseus", "roseus"],
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
        log.debug("error: %s" % msg)
        if not msg.startswith(";"):
            self.error.put(msg)

    def exec_command(self, cmd_str):
        token = "token-" + str(uuid1())
        sexp = loads(cmd_str)
        sexp = [
            Symbol('let'),
            [[Symbol(token), sexp]],
            [Symbol('format'), True, "%s" % token],
            Symbol(token),
        ]
        cmd_str = dumps(sexp)
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
                out = self.output.get(timeout=0.1)
                pos = out.find(token)
                if pos >= 0:
                    while not self.output.empty():
                        out += self.output.get()
                    start = pos + len(token)
                    yield out[:pos]
                    yield out[start:]
                    return
                else:
                    yield out
                    last_output = time.time()
            except Empty:
                if time.time() - last_output > self.timeout:
                    raise EuslispError("Timed out")

    def eval(self, cmd_str):
        if not cmd_str.strip():
            cmd_str = 'nil'
        log.info("eval: %s" % cmd_str)
        for r in self.exec_command(cmd_str):
            yield r

    def eval_block(self, cmd_str, only_result=False):
        res = list(self.eval(cmd_str))
        log.info("res: %s" % res)
        if only_result:
            return res[-1]
        else:
            return res

    def find_function(self, start, pkg=None):
        if start.find(":") > 0:
            # e.g. ros::tf-pose->coords
            ns = start.split(":")[0]
            func = ":".join(start.replace(":", " ").split()[1:])
            # use #'functions
            cmd = """(functions "{0}" '{1})""".format(func, ns)
        else:
            # e.g. format
            # use #'apropos-list
            cmd = """(remove-if-not
                       '(lambda (x)
                         (string= "{0}" (subseq (string x) 0 (length "{0}"))))
                       (apropos-list "{0}"))""".format(start.upper())
        result = self.eval_block(cmd, only_result=True) or list()
        log.info("result: %s" % result)
        resexp = list()
        for s in loads(result):
            if isinstance(s, Symbol):
                s = s.value()
            if not s.startswith(":"):
                resexp.append(s)
        return resexp

    def arglist(self, func, pkg=None, cursor=None):
        cmd = """(with-output-to-string (s) (pf {0} s))""".format(func)
        result = loads(self.eval_block(cmd, only_result=True))
        log.info("result: %s" % result)
        if result.strip().find("compiled-code") >= 0:
            return result.strip()
        sexp = loads(result)
        if not sexp:
            return result
        elif sexp[0].value() in ['lambda', 'macro']:
            arg_list = [Symbol(func)] + sexp[1]
            if isinstance(cursor, int):
                if 0 <= cursor < len(arg_list):
                    arg_list.insert(cursor, Symbol('===>'))
                    arg_list.insert(cursor + 2, Symbol('<==='))
            return dumps(arg_list)
        else:
            return result


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
