from __future__ import print_function

import os
import platform
from sexpdata import dumps, loads
from sexpdata import Symbol

from euswank.bridge import EuslispProcess
from euswank.bridge import eus_eval_once
from euswank.logger import get_logger

log = get_logger(__name__)


class EUSwankHandler(object):
    def __init__(self):
        self.euslisp = EuslispProcess()
        self.euslisp.start()

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
        return ["irteusgl", "irteusgl"]

    def swank_buffer_first_change(self, filename):
        return False

    def swank_eval(self, sexp):
        if not sexp.strip():
            sexp = 'nil'
        result = self.euslisp.exec_command(sexp)
        return [Symbol(":values"), result]

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

    def swank_simple_completions(self, prefix, pkg):
        # (swank:simple-completions "vector-" (quote "irteusgl"))
        pkg = pkg[1]
        if prefix.find(":") > 0:
            ns = prefix.split(":")[0]
            func = ":".join(prefix.replace(":", " ").split()[1:])
            # use #'functions
            cmd = """(functions "{0}" '{1})""".format(func, ns)
        else:
            # use #'apropos-list
            cmd = """(remove-if-not
                       '(lambda (x)
                         (string= "{0}" (subseq (string x) 0 (length "{0}"))))
                       (apropos-list "{0}"))""".format(prefix.upper())
        result = self.euslisp.exec_command(cmd)
        resexp = list()
        for s in loads(result):
            if isinstance(s, Symbol):
                s = s.value()
            if not s.startswith(":"):
                resexp.append(s)
        if len(resexp) == 1:
            retval = [resexp, resexp[0]]
        else:
            retval = [resexp, prefix]
        return retval

    def swank_fuzzy_completions(self, prefix, pkg, _, limit, *args):
        # (swank:fuzzy-completions "a" "irteusgl"
        #       :limit 300 :time-limit-in-msec 1500)
        if len(prefix) >= 2:
            resexp, prefix = self.swank_simple_completions(prefix, pkg)
            return [resexp[:limit], prefix]

    def swank_complete_form(self, *args):
        # (swank:complete-form
        #    (quote ("float-vector" swank::%cursor-marker%))
        pass

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
        pass

    def swank_init_presentations(self, *sexp):
        log.info(sexp)

    def swank_repl_create_repl(self, *sexp):
        return self.swank_create_repl(sexp)

    def swank_compile_string_for_emacs(self, sexp, *args):
        # (sexp buffer-name (:position 1) (:line 1) () ())
        # FIXME: This does not comple actually, just eval instead.
        result = self.swank_eval(sexp)
        log.info("result: %s" % result)
        errors = []
        seconds = 0.01
        return [Symbol(":compilation-result"), errors, True,
                seconds, None, None]

    def swank_compile_notes_for_emacs(self, *args):
        return self.swank_compile_string_for_emacs(*args)

    def swank_compile_file_for_emacs(self, *args):
        return self.swank_compile_string_for_emacs(*args)

    def swank_operator_arglist(self, func, pkg):
        #  (swank:operator-arglist "format" "irteusgl")
        cmd = """(with-output-to-string (s) (pf {0} s))""".format(func)
        result = self.euslisp.exec_command(cmd)
        return result

    def swank_inspect_current_condition(self):
        # (swank:inspect-current-condition)
        pass

    def swank_sldb_abort(self, *args):
        pass

    def swank_find_definitions_for_emacs(self, keyword):
        pass

    def swank_describe_symbol(self, sym):
        cmd = """(with-output-to-string (s) (describe '{0} s))""".format(sym)
        res = self.euslisp.exec_command(cmd)
        return res

    def swank_describe_function(self, func):
        cmd = """(documentation '{0})""".format(func)
        result = self.euslisp.exec_command(cmd)
        return result

    def swank_describe_definition_for_emacs(self, name, type):
        return self.swank_describe_symbol(name)


if __name__ == '__main__':
    h = EUSwankHandler()
    print(dumps(h.swank_connection_info()))
