from __future__ import print_function

import os
import platform
import traceback
from sexpdata import dumps, loads
from sexpdata import Symbol

from euslime.bridge import EuslispProcess
from euslime.bridge import EuslispError
from euslime.bridge import eus_eval_once
from euslime.logger import get_logger

log = get_logger(__name__)


def findp(s, l):
    assert isinstance(l, list)
    # Reverse search list
    for e in l[::-1]:
        if e == s:
            return [x for x in l if x not in ['', u'']]
        elif isinstance(e, list):
            res = findp(s, e)
            if res:
                return res
    return list()


def current_scope(sexp):
    marker = Symbol(u'swank::%cursor-marker%')
    scope = findp(marker, sexp)
    cursor = -1
    for i, e in enumerate(scope):
        if marker == e:
            cursor = i
    return scope, cursor


class DebuggerHandler(object):
    restarts = [
        ["QUIT", "Quit to the SLIME top level"],
        ["CONTINUE", "Ignore the error and continue in the same stack level"],
        ["RESTART", "Restart euslisp process"]
    ]

    def __init__(self, id, error):
        msg, stack = self.parse_message(error)
        self.id = id
        self.message = msg
        self.stack = stack

    def parse_message(self, err):
        desc = str()
        strace = list()
        if isinstance(err, EuslispError):
            err_msgs = err.message.strip().splitlines()
            if err_msgs and err_msgs[0].startswith("Call Stack"):
                # parse stack trace
                for l in err_msgs[1:]:
                    split = l.strip().split(": at ")
                    if len(split) == 2:
                        num, msg = split
                        strace.append([int(num), msg,
                                       [Symbol(":restartable"), False]])
                    else:
                        break
                desc = err_msgs[-1].split("irteusgl 0 error: ")[-1]
                desc = desc.strip().capitalize()
            else:
                desc = err.message.strip()
        elif isinstance(err, Exception):
            desc = err.message.strip()
        else:
            desc = err

        return desc, strace


class EuslimeHandler(object):
    def __init__(self):
        self.euslisp = EuslispProcess()
        self.euslisp.start()
        self.package = None
        self.debugger = []

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
                'name': 'USER',
                'prompt': 'irteusgl',
            },
            'version': "2.20",  # swank version
        }

    def swank_create_repl(self, sexp):
        yield self.euslisp.toplevel_prompt('')

    def swank_repl_create_repl(self, *sexp):
        return self.swank_create_repl(sexp)

    def swank_buffer_first_change(self, filename):
        yield False

    def swank_eval(self, sexp):
        last_msg = None
        for out in self.euslisp.eval(sexp):
            if last_msg is not None:
                yield [Symbol(":write-string"), last_msg]
            last_msg = out
        new_prompt = self.euslisp.toplevel_prompt(self.package)
        if new_prompt:
            yield [Symbol(":new-package")] + new_prompt
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

    def swank_autodoc(self, sexp, _=None, line_width=None):
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
            log.info("scope: %s, cursor: %s" % (scope, cursor))
            assert cursor > 0
            func = scope[0]
            scope = scope[:-1] # remove marker
            result = self.euslisp.arglist(func, cursor, scope)
            assert result
            yield [result, True]
        except Exception as e:
            log.error(e)
            log.error(traceback.format_exc())
            yield [Symbol(":not-available"), True]

    def swank_completions(self, start, pkg):
        if start and start[0] == ':':
            return self.swank_completions_for_keyword(start, None)
        else:
            return self.swank_simple_completions(start, pkg)
        #return self.swank_fuzzy_completions(start, pkg, ':limit', 300, ':time-limit-in-msec', 1500)

    def swank_simple_completions(self, start, pkg):
        # (swank:simple-completions "vector-" (quote "USER"))
        yield self.euslisp.find_symbol(start)

    def swank_fuzzy_completions(self, prefix, pkg, _, limit, *args):
        # (swank:fuzzy-completions "a" "USER"
        #       :limit 300 :time-limit-in-msec 1500)
        if len(prefix) >= 2:
            for resexp, prefix in self.swank_simple_completions(prefix, pkg):
                yield [resexp[:limit], prefix]
        else:
            yield [None, None]

    def swank_completions_for_keyword(self, start, sexp):
        # args: [u':meth', [Symbol(u'quote'), [u'send', u'c', u'', Symbol(cursor)]]]
        if sexp:
            sexp = sexp[1] # unquote
            scope, _ = current_scope(sexp)
            scope = scope[:-1] # remove marker
        else:
            scope = None
        yield self.euslisp.find_keyword(start, dumps(scope))

    def swank_completions_for_character(self, start):
        yield self.euslisp.find_character(start)

    def swank_complete_form(self, *args):
        # (swank:complete-form
        #    (quote ("float-vector" swank::%cursor-marker%))
        return

    def swank_quit_lisp(self, *args):
        self.euslisp.stop()

    def swank_backtrace(self, start, end):
        return []

    def swank_invoke_nth_restart_for_emacs(self, level, num):
        deb = self.debugger.pop(level - 1)
        if num == 0:  # QUIT
            self.debugger = []
            self.euslisp.input('"token" reset')
        elif num == 1:  # CONTINUE
            pass
        elif num == 2:  # RESTART
            self.debugger = []
            self.euslisp.stop()
            self.euslisp = EuslispProcess()
            self.euslisp.start()

        msg = repr(deb.message.rsplit(' in ', 1)[0])
        yield [Symbol(':debug-return'), 0, level, Symbol('nil')]
        yield [Symbol(':return'), {'abort': msg}, deb.id]
        yield {'abort': 'NIL'}

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
        #  (swank:operator-arglist "format" "USER")
        try:
            yield self.euslisp.arglist(func)
        except:
            yield ["", True]

    def swank_inspect_current_condition(self):
        # (swank:inspect-current-condition)
        return

    def swank_sldb_abort(self, *args):
        return

    def swank_sldb_out(self, *args):
        return

    def swank_frame_locals_and_catch_tags(self, *args):
        return

    def swank_find_definitions_for_emacs(self, keyword):
        return

    def swank_describe_symbol(self, sym):
        cmd = """(slime::slime-describe-symbol "{0}")""".format(sym.strip())
        result =  self.euslisp.eval_block(cmd, only_result=True)
        yield loads(result)

    def swank_describe_function(self, func):
        return self.swank_describe_symbol(func)

    def swank_describe_definition_for_emacs(self, name, type):
        return self.swank_describe_symbol(name)

    def swank_swank_expand_1(self, form):
        cmd = """(slime::slime-macroexpand '{0})""".format(form)
        result = self.euslisp.eval_block(cmd, only_result=True)
        yield loads(result)

if __name__ == '__main__':
    h = EuslimeHandler()
    print(dumps(h.swank_connection_info().next()))
