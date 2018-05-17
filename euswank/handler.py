from __future__ import print_function

import os
import platform
from sexpdata import dumps
from sexpdata import Symbol

from euswank.bridge import EuslispBridge
from euswank.bridge import eus_call_once
from euswank.logger import get_logger

log = get_logger(__name__)


class EUSwankHandler(object):
    def __init__(self):
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
                'type': 'irteusgl',
                'name': 'irteusgl',
                'version': eus_call_once('(lisp-implementation-version)'),
                'program': False,
            },
            'machine': {
                'type': platform.machine().upper(),
                'version': platform.machine().upper(),
            },
            'package': {
                'name': 'irteusgl',
                'prompt': 'irteusgl',
            },
            'version': "2.20",  # swank version
        }

    def swank_create_repl(self, sexp):
        return ["irteusgl", "irteusgl"]

    def swank_buffer_first_change(self, filename):
        return False

    def swank_eval(self, sexp):
        self.bridge.write(sexp)
        return [Symbol(":values"), self.bridge.read_out()]

    def swank_interactive_eval(self, sexp):
        return self.swank_eval(sexp)

    def swank_interactive_eval_region(self, sexp):
        return self.swank_eval(sexp)

    def swank_repl_listener_eval(self, sexp):
        return self.swank_eval(sexp)

    def swank_pprint_eval(self, sexp):
        return self.swank_eval(sexp)

    def swank_autodoc(self, *sexp):
        # FIXME: maybe return the first line of the result of `pf <function>`?
        return [Symbol(":not-available"), True]

    def swank_fuzzy_completions(self, *sexp):
        # FIXME: tab completion
        log.warn(sexp)
        return [[], []]  # FIXME

    def swank_simple_completions(self, *sexp):
        log.warn(sexp)
        return [[], []]  # FIXME

    def swank_quit_lisp(self, sexp):
        self.bridge.stop()

    def swank_invoke_nth_restart_for_emacs(self, sexp):
        log.debug("input: %s", sexp)
        self.bridge.stop()
        self.bridge = EuslispBridge()
        self.bridge.start()
        return None

    def swank_swank_require(self, *sexp):
        log.info(sexp)

    def swank_init_presentations(self, *sexp):
        log.info(sexp)

    def swank_repl_create_repl(self, *sexp):
        return self.swank_create_repl(sexp)

    def swank_compile_string_for_emacs(self, sexp, *args):
        # (sexp buffer-name (:position 1) (:line 1) () ())
        # FIXME: This does not comple actually, just eval instead.
        return self.swank_eval(sexp)

    # TODO: some other functions maybe missing?


if __name__ == '__main__':
    h = EUSwankHandler()
    print(dumps(h.swank_connection_info()))
