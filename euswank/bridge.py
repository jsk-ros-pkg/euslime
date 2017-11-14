from __future__ import print_function

import os
import sys
import errno
import subprocess
import time
from threading import Thread
from threading import Lock

from euswank.logger import get_logger

log = get_logger(__name__)
IS_POSIX = 'posix' in sys.builtin_module_names
BUFF_SIZE = 1
POLL_DURATION = 0.01
DELIM = os.linesep


class EuslispCallError(Exception):
    pass


class Buffer(object):
    def __init__(self):
        self.lock = Lock()
        self.buffer = str()

    def put(self, s):
        with self.lock:
            self.buffer += s

    def empty(self):
        return len(self.buffer) == 0

    def get(self, wait=True):
        if not wait:
            with self.lock:
                buf = self.buffer
                self.buffer = str()
            return buf
        while True:
            ss = self.buffer.split(DELIM)
            if len(ss) > 1 and ss[-2]:
                break
            time.sleep(POLL_DURATION)
        with self.lock:
            log.debug(self.buffer)
            buf = self.buffer.split(DELIM)[-2]
            self.buffer = str()
        return buf


class EuslispBridge(object):
    def __init__(self):
        self.process = None
        self.buff_out = None
        self.buff_err = None
        self.buff_in = None
        self.threads = None

    def start(self, cmd=None, blocking=False):
        if cmd is None:
            cmd = ['rosrun', 'roseus', 'roseus']
        self.process = subprocess.Popen(
            cmd,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            stdin=subprocess.PIPE,
            bufsize=BUFF_SIZE,
            close_fds=IS_POSIX,
            env=os.environ.copy(),
            # preexec_fn=os.setpgrp(),
        )

        self.buff_out = Buffer()
        self.buff_err = Buffer()
        self.buff_in = Buffer()

        self.threads = [
            Thread(target=self._get_output,
                   args=(self.process.stdout, self.buff_out)),
            Thread(target=self._get_output,
                   args=(self.process.stderr, self.buff_err)),
        ]
        if not blocking:
            self.threads.insert(0, Thread(target=self._loop))
        for t in self.threads:
            t.daemon = True
            t.start()
        if blocking:
            self._loop()

    def stop(self):
        try:
            self.process.terminate()
        except:
            self.process.kill()

        for t in self.threads:
            if t.is_alive:
                t.join()

    def write(self, cmd):
        cmd = cmd.strip()
        if not cmd.endswith(os.linesep):
            cmd += os.linesep
        self.buff_in.put(cmd)

    def read_out(self):
        return self.buff_out.get().strip()

    def read_err(self):
        return self.buff_err.get(wait=False).strip()

    def _get_output(self, fp, buf):
        while self.process.poll() is None:
            data = fp.read(BUFF_SIZE)
            if data:
                buf.put(str(data))
            time.sleep(POLL_DURATION)
        else:
            log.debug("thread is dead: %s", fp)

    def _capsulize(self, sexp):
        return '(let ((retvalval %s)) (format t "~%%") retvalval)' % sexp

    def _loop(self):
        while self.process.poll() is None:
            # put to stdin
            if not self.buff_in.empty():
                try:
                    cmd = self.buff_in.get()
                    log.debug("cmd: %s", cmd)
                    log.debug("cap: %s", self._capsulize(cmd))
                    self.process.stdin.write(self._capsulize(cmd) + os.linesep)
                    self.process.stdin.flush()
                except IOError as e:
                    if e.errno == errno.EINTR:
                        pass
                except Exception as e:
                    log.warn(e)
            time.sleep(POLL_DURATION)
        else:
            log.debug("process stopped")

        if self.process.returncode != 0:
            log.info("process exited (code: %d)" % self.process.returncode)


def eus_call_once(cmd):
    bridge = EuslispBridge()
    bridge.start()
    bridge.write(cmd)
    result = bridge.read_out()
    bridge.stop()
    return result


if __name__ == '__main__':
    print("answer:", eus_call_once("(lisp-implementation-version)"))
