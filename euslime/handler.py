from __future__ import print_function

import os
import platform
import traceback
from sexpdata import dumps, loads
from sexpdata import Symbol

from euslime.bridge import IntermediateResult
from euslime.bridge import EuslispProcess
from euslime.bridge import EuslispError
from euslime.bridge import EuslispFatalError
from euslime.logger import get_logger

log = get_logger(__name__)


def findp(s, l):
    assert isinstance(l, list)
    # Reverse search list
    for e in l[::-1]:
        if e == s:
            return l
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
        self.id = id
        if isinstance(error, EuslispFatalError):
            if error.continuable:
                self.restarts = self.restarts[1:] # No QUIT
            else:
                self.restarts = self.restarts[2:] # No QUIT & CONTINUE
        else:
            self.restarts = self.restarts
        self.restarts_dict = {}
        for num, item in enumerate([x[0] for x in self.restarts]):
            self.restarts_dict[item] = num
        if isinstance(error, EuslispError):
            self.message = error.message
            self.stack = error.stack
        elif isinstance(error, Exception):
            self.message = error.message.strip()
            self.stack = None
        else:
            self.message = error
            self.stack = None


class EuslimeHandler(object):
    def __init__(self):
        self.euslisp = EuslispProcess()
        self.euslisp.start()
        self.package = None
        self.debugger = []
        self.had_output = False

    def restart_euslisp_process(self):
        color = self.euslisp.color
        self.euslisp.stop()
        self.euslisp = EuslispProcess(color=color)
        self.euslisp.start()

    def fresh_line(self):
        if self.had_output:
            yield [Symbol(":write-string"), self.euslisp.delim, Symbol(":repl-result")]
            self.had_output = False

    def swank_connection_info(self):
        version = self.euslisp.eval_block('(slime::implementation-version)', only_result=True)
        yield {
            'pid': os.getpid(),
            'style': False,
            'encoding': {
                'coding-systems': ['utf-8-unix', 'iso-latin-1-unix'],
            },
            'lisp-implementation': {
                'type': 'irteusgl',
                'name': 'irteusgl',
                'version': version,
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
        if self.euslisp.processing:
            log.error("Process is busy!")
            yield Symbol('nil')
            return
        self.had_output = False
        finished = False
        for out in self.euslisp.eval(sexp):
            if finished:
                log.debug('Additional result: %s' % out)
                raise Exception('More than one result in %s' % sexp)
            if isinstance(out, IntermediateResult):
                if not self.had_output:
                    self.had_output = True
                else:
                    out.value = self.euslisp.delim + out.value
                out.value = [Symbol(":write-string"), out.value]
                yield out
            else:
                new_prompt = self.euslisp.toplevel_prompt(self.package)
                if new_prompt:
                    yield IntermediateResult([Symbol(":new-package")] + new_prompt)
                for r in self.fresh_line():
                    yield IntermediateResult(r)
                yield [Symbol(":values"), out]
                finished = True

    def swank_interactive_eval(self, sexp):
        return self.swank_eval(sexp)

    def swank_interactive_eval_region(self, sexp):
        return self.swank_eval(sexp)

    def swank_repl_listener_eval(self, sexp):
        return self.swank_eval(sexp)

    def swank_pprint_eval(self, sexp):
        return self.swank_eval(sexp)

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
            log.debug("scope: %s, cursor: %s" % (scope, cursor))
            assert cursor > 0
            func = scope[0]
            scope = scope[:-1] # remove marker
            result = self.euslisp.arglist(func, cursor, scope)
            if result:
                if result.startswith('"') and result.endswith('"'):
                    result = loads(result) # unquote
                yield [result, True]
            else:
                yield [Symbol(":not-available"), True]
        except Exception as e:
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
            if scope:
                scope = scope[:-1] # remove marker
                if scope[-1] in ['', u'']: # remove null string
                    scope = scope[:-1]

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
        exit()

    def swank_backtrace(self, start, end):
        return []

    def swank_throw_to_toplevel(self):
        lvl = len(self.debugger)
        return self.swank_invoke_nth_restart_for_emacs(lvl, 0)

    def swank_invoke_nth_restart_for_emacs(self, level, num):
        deb = self.debugger.pop(level - 1)
        res_dict = deb.restarts_dict

        def check_key(key):
            return res_dict.has_key(key) and num == res_dict[key]

        if check_key('RESTART'):
            self.debugger = []
            self.restart_euslisp_process()
        elif check_key('CONTINUE'):
            pass
        elif check_key('QUIT'):
            self.debugger = []
            self.euslisp.reset()
        else:
            log.error("Restart not found!")

        msg = deb.message.split(self.euslisp.delim)[0]
        msg = repr(msg.rsplit(' in ', 1)[0])
        yield IntermediateResult([Symbol(':debug-return'), 0, level, Symbol('nil')])
        yield IntermediateResult([Symbol(':return'), {'abort': 'NIL'}, deb.id + 1])
        yield IntermediateResult([Symbol(':return'), {'abort': msg}, deb.id])
        self.euslisp.processing = False

    def swank_swank_require(self, *sexp):
        return

    def swank_init_presentations(self, *sexp):
        log.info(sexp)

    def swank_compile_string_for_emacs(self, sexp, *args):
        # (sexp buffer-name (:position 1) (:line 1) () ())
        # FIXME: This does not compile actually, just eval instead.
        for out in self.euslisp.eval(sexp):
            if isinstance(out, IntermediateResult):
                out.value = [Symbol(":write-string"), out.value + self.euslisp.delim]
                yield out
            else:
                yield IntermediateResult([Symbol(":write-string"), out])
        errors = []
        seconds = 0.01
        yield [Symbol(":compilation-result"), errors, True,
               seconds, None, None]

    def swank_compile_notes_for_emacs(self, *args):
        return self.swank_compile_string_for_emacs(*args)

    def swank_compile_file_for_emacs(self, *args):
        return self.swank_compile_file_if_needed(*args)

    def swank_compile_file_if_needed(self, filename, loadp):
        # FIXME: This returns without checking/compiling the file
        errors = []
        seconds = 0.01
        yield [Symbol(":compilation-result"), errors, True,
               seconds, loadp, filename]

    def swank_load_file(self, filename):
        yield IntermediateResult(
            [Symbol(":write-string"), "\nLoading file: %s ..." % filename])
        cmd = """(lisp:load "{0}")""".format(filename)
        result = self.euslisp.eval_block(cmd, only_result=True)
        yield IntermediateResult([Symbol(":write-string"), "\nLoaded."])
        yield result

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

    def swank_list_all_package_names(self, nicknames=None):
        cmd = """(slime::slime-all-packages {0})""".format(dumps(nicknames))
        result = self.euslisp.eval_block(cmd, only_result=True)
        yield loads(result)

    def swank_apropos_list_for_emacs(self, key, external_only=None, case_sensitive=None,
                                     package=None):
        # ignore 'external_only' and 'case_sensitive' arguments
        package = package[-1] # unquote
        cmd = """(slime::slime-apropos-list "{0}" {1})""".format(key, dumps(package))
        result = self.euslisp.eval_block(cmd, only_result=True)
        yield loads(result)

    def swank_set_package(self, name):
        cmd = """(slime::set-package "{0}")""".format(name)
        result = self.euslisp.eval_block(cmd, only_result=True)
        yield loads(result)

    def swank_default_directory(self):
        result = self.euslisp.eval_block("(lisp:pwd)", only_result=True)
        yield loads(result)

    def swank_set_default_directory(self, dir):
        cmd = """(progn (lisp:cd "{0}") (lisp:pwd))""".format(dir)
        result = self.euslisp.eval_block(cmd, only_result=True)
        yield loads(result)

if __name__ == '__main__':
    h = EuslimeHandler()
    print(dumps(h.swank_connection_info().next()))
