import os
import platform
from sexpdata import dumps

from euswank import __version__
from euswank.bridge import EuslispBridge
from euswank.bridge import eus_call_once
from euswank.logger import get_logger

log = get_logger(__name__)


class EUSwankHandler(object):
    def __init__(self, socket):
        self.socket = socket
        self.bridge = EuslispBridge()
        self.bridge.start()

    def swank_connection_info(self):
        return {
            'pid': os.getpid(),
            'style': False,
            'encoding': {
                'coding-systems': ['utf-8-unix', 'iso-latin-1-unix'],
            },
            'lisp-implementation': {
                'type': 'EUSLISP',
                'name': 'irteusgl',
                'version': eus_call_once('(lisp-implementation-version)'),
                'program': False,
            },
            'machine': {
                'instance': '%s [%d]' % self.socket.getsockname(),
                'type': platform.machine().upper(),
                'version': platform.machine().upper(),
            },
            'package': {
                'name': 'irteusgl',
                'prompt': 'irteusgl',
            },
            'version': "2.19",  # swank version
        }

    def swank_buffer_first_change(self, filename):
        return False

    def swank_eval(self, sexp):
        log.debug("input: %s", sexp)
        self.bridge.write(sexp)
        return self.bridge.read_out()

    def swank_interactive_eval(self, sexp):
        return self.swank_eval(sexp)

    def swank_interactive_eval_region(self, sexp):
        return self.swank_eval(sexp)

    def swank_pprint_eval(self, sexp):
        return self.swank_eval(sexp)

    def swank_invoke_nth_restart_for_emacs(self, sexp):
        log.debug("input: %s", sexp)
        self.bridge.stop()
        self.bridge = EuslispBridge()
        self.bridge.start()
        return None

if __name__ == '__main__':
    h = EUSwankHandler()
    print dumps(h.swank_connection_info())