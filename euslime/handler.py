from __future__ import print_function

import os
import platform
import traceback
from sexpdata import dumps
from sexpdata import Symbol

from euslime.bridge import EuslispProcess
from euslime.bridge import eus_eval_once
from euslime.logger import get_logger

log = get_logger(__name__)


def findp(s, l):
    assert isinstance(l, list)
    lt = [e for e in l if e not in ['', u'']]
    for e in lt:
        if e == s:
            return lt
        elif isinstance(e, list):
            return findp(s, e)
    return list()


def current_scope(sexp):
    marker = Symbol(u'swank::%cursor-marker%')
    scope = findp(marker, sexp)
    cursor = -1
    if len(scope) <= 1:
        raise ValueError("scope not found in %s" % scope)
    for i, e in enumerate(scope):
        if marker == e:
            cursor = i
    return scope, cursor


class EuslimeHandler(object):
    def __init__(self):
        self.euslisp = EuslispProcess()
        self.euslisp.start()

    def swank_connection_info(self):
        yield {
            'pid': os.getpid(),
            'style': False,
            'encoding': {
                'coding-systems': ['utf-8-unix', 'iso-latin-1-unix'],
            },
            'lisp-implementation': {
                'type': 'irteusgl',
                'name': 'irteusgl',
                'version': eus_eval_once('(lisp-implementation-version)'),
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
        yield ["irteusgl", "irteusgl"]

    def swank_repl_create_repl(self, *sexp):
        for r in self.swank_create_repl(sexp):
            yield r

    def swank_buffer_first_change(self, filename):
        yield False

    def swank_eval(self, sexp):
        last_msg = None
        for out in self.euslisp.eval(sexp):
            if last_msg is not None:
                yield [Symbol(":write-string"), last_msg]
            last_msg = out
        yield [Symbol(":values"), last_msg]

    def swank_interactive_eval(self, sexp):
        for r in self.swank_eval(sexp):
            yield r

    def swank_interactive_eval_region(self, sexp):
        for r in self.swank_eval(sexp):
            yield r

    def swank_repl_listener_eval(self, sexp):
        for r in self.swank_eval(sexp):
            yield r

    def swank_pprint_eval(self, sexp):
        for r in self.swank_eval(sexp):
            yield r

    def swank_autodoc(self, sexp, _, line_width):
        """
(:emacs-rex
 (swank:autodoc
  '("ql:quickload" "" swank::%cursor-marker%)
  :print-right-margin 102)
 "COMMON-LISP-USER" :repl-thread 19)
(:return
 (:ok
  ("(quickload ===> systems <=== &key
     (verbose quicklisp-client:*quickload-verbose*) silent\n
     (prompt quicklisp-client:*quickload-prompt*) explain &allow-other-keys)"
   t))
 19)
        """
        try:
            sexp = sexp[1]  # unquote
            scope, cursor = current_scope(sexp)
            func = str(scope[0])
            log.info("scope: %s, cursor: %s" % (scope, cursor))
            result = self.euslisp.arglist(func, cursor=cursor)
            assert result
            yield [result, True]
        except Exception as e:
            log.error(e)
            log.error(traceback.format_exc())
            yield [Symbol(":not-available"), True]

    def swank_simple_completions(self, start, pkg):
        # (swank:simple-completions "vector-" (quote "irteusgl"))
        # TODO: support eus method
        pkg = pkg[1]
        result = self.euslisp.find_function(start, pkg)
        if len(result) == 1:
            yield [result, result[0]]
        else:
            yield [result, start]

    def swank_fuzzy_completions(self, prefix, pkg, _, limit, *args):
        # (swank:fuzzy-completions "a" "irteusgl"
        #       :limit 300 :time-limit-in-msec 1500)
        if len(prefix) >= 2:
            for resexp, prefix in self.swank_simple_completions(prefix, pkg):
                yield [resexp[:limit], prefix]

    def swank_complete_form(self, *args):
        # (swank:complete-form
        #    (quote ("float-vector" swank::%cursor-marker%))
        return

    def swank_quit_lisp(self, *args):
        self.euslisp.stop()

    def swank_invoke_nth_restart_for_emacs(self, level, num):
        if num == 0:  # QUIT
            pass
        elif num == 1:  # RESTART
            self.euslisp.stop()
            self.euslisp = EuslispProcess()
            self.euslisp.start()

    def swank_swank_require(self, *sexp):
        return

    def swank_init_presentations(self, *sexp):
        log.info(sexp)

    def swank_compile_string_for_emacs(self, sexp, *args):
        # (sexp buffer-name (:position 1) (:line 1) () ())
        # FIXME: This does not comple actually, just eval instead.
        for out in self.euslisp.eval(sexp):
            log.info(out)
            yield [Symbol(":write-string"), out]
        errors = []
        seconds = 0.01
        yield [Symbol(":compilation-result"), errors, True,
               seconds, None, None]

    def swank_compile_notes_for_emacs(self, *args):
        for r in self.swank_compile_string_for_emacs(*args):
            yield r

    def swank_compile_file_for_emacs(self, *args):
        for r in self.swank_compile_string_for_emacs(*args):
            yield r

    def swank_operator_arglist(self, func, pkg):
        #  (swank:operator-arglist "format" "irteusgl")
        # TODO: support eus method
        try:
            yield self.euslisp.arglist(func, pkg)
        except:
            yield ["", True]

    def swank_inspect_current_condition(self):
        # (swank:inspect-current-condition)
        return

    def swank_sldb_abort(self, *args):
        return

    def swank_find_definitions_for_emacs(self, keyword):
        return

    def swank_describe_symbol(self, sym):
        cmd = """(with-output-to-string (s) (describe '{0} s))""".format(sym)
        yield self.euslisp.eval_block(cmd, only_result=True)

    def swank_describe_function(self, func):
        cmd = """(documentation '{0})""".format(func)
        yield self.euslisp.eval_block(cmd, only_result=True)

    def swank_describe_definition_for_emacs(self, name, type):
        yield self.swank_describe_symbol(name)


if __name__ == '__main__':
    h = EuslimeHandler()
    print(dumps(h.swank_connection_info().next()))
