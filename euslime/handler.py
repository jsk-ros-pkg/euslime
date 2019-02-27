from __future__ import print_function

import os
import platform
import traceback
from sexpdata import dumps, loads
from sexpdata import Symbol
from threading import Event

from euslime.bridge import EuslispResult
from euslime.bridge import EuslispProcess
from euslime.bridge import EuslispError
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


def qstr(s):
    return s.encode('utf-8').encode('string_escape')


class DebuggerHandler(object):
    restarts = [
        ["QUIT", "Quit to the SLIME top level"],
        ["CONTINUE", "Ignore the error and continue in the same stack level"],
        ["RESTART", "Restart euslisp process"]
    ]

    def __init__(self, id, error):
        self.id = id
        if isinstance(error, EuslispError) and error.fatal:
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
            self.message = '{}: {}'.format(type(error).__name__, error)
            self.stack = None
        else:
            self.message = error
            self.stack = None


class EuslimeHandler(object):
    def __init__(self):
        self.euslisp = EuslispProcess()
        self.close_request = Event()
        self.euslisp.start()
        self.command_id = None
        self.package = None
        self.debugger = []

    def restart_euslisp_process(self):
        color = self.euslisp.color
        self.euslisp.stop()
        self.euslisp = EuslispProcess(color=color)
        self.euslisp.start()

    def maybe_new_prompt(self):
        new_prompt = self.euslisp.exec_internal("(slime::slime-prompt)")
        if new_prompt:
            yield [Symbol(":new-package")] + new_prompt

    def arglist(self, func, cursor=None, form=None):
        cmd = """(slime::autodoc "{0}" {1} '{2})""".format(qstr(func), dumps(cursor), dumps(form))
        result = self.euslisp.exec_internal(cmd)
        if isinstance(result, str):
            return result
        elif result:
            return dumps(result)
        return result

    def _emacs_return_string(self, process, count, msg):
        self.euslisp.input(msg)
        yield [Symbol(":read-string"), 0, 1]

    def _emacs_interrupt(self, process):
        raise KeyboardInterrupt

    def swank_connection_info(self):
        # Wait for euslisp connection
        self.euslisp.recv_socket_data()
        log.info("Successfully started Euslisp process!")
        version = self.euslisp.exec_internal('(slime::implementation-version)')
        name = self.euslisp.exec_internal('(pathname-name *program-name*)')
        res = {
            'pid': os.getpid(),
            'style': False,
            'encoding': {
                'coding-systems': ['utf-8-unix', 'iso-latin-1-unix'],
            },
            'lisp-implementation': {
                'type': name,
                'name': name,
                'version': version,
                'program': False,
            },
            'machine': {
                'type': platform.machine().upper(),
                'version': platform.machine().upper(),
            },
            'package': {
                'name': 'USER',
                'prompt': name,
            },
            'version': "2.20",  # swank version
        }
        yield EuslispResult(res)

    def swank_create_repl(self, sexp):
        res = self.euslisp.exec_internal('(slime::slime-prompt)')
        yield EuslispResult(res)

    def swank_repl_create_repl(self, *sexp):
        return self.swank_create_repl(sexp)

    def swank_buffer_first_change(self, filename):
        yield EuslispResult(False)

    def swank_eval(self, sexp):
        yield [Symbol(":read-string"), 0, 1]
        try:
            for out in self.euslisp.eval(sexp):
                if isinstance(out, EuslispResult):
                    yield [Symbol(":read-aborted"), 0, 1]
                    for val in self.maybe_new_prompt():
                        yield val
                yield out
        except Exception as e:
            yield [Symbol(":read-aborted"), 0, 1]
            raise e

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
            result = self.arglist(func, cursor, scope)
            if result:
                if result.startswith('"') and result.endswith('"'):
                    result = loads(result) # unquote
                yield EuslispResult([result, True])
            else:
                yield EuslispResult([Symbol(":not-available"), True])
        except Exception as e:
            log.error(traceback.format_exc())
            yield EuslispResult([Symbol(":not-available"), True])

    def swank_operator_arglist(self, func, pkg):
        #  (swank:operator-arglist "format" "USER")
        yield EuslispResult(self.arglist(func))

    def swank_completions(self, start, pkg):
        if start and start[0] == ':':
            return self.swank_completions_for_keyword(start, None)
        else:
            return self.swank_simple_completions(start, pkg)
        #return self.swank_fuzzy_completions(start, pkg, ':limit', 300, ':time-limit-in-msec', 1500)

    def swank_simple_completions(self, start, pkg):
        # (swank:simple-completions "vector-" (quote "USER"))
        cmd = """(slime::slime-find-symbol "{0}")""".format(qstr(start))
        yield EuslispResult(self.euslisp.exec_internal(cmd))

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
        cmd = """(slime::slime-find-keyword "{0}" '{1})""".format(qstr(start), dumps(scope))
        yield EuslispResult(self.euslisp.exec_internal(cmd))

    def swank_completions_for_character(self, start):
        cmd = """(slime::slime-find-character "{0}")""".format(qstr(start))
        yield EuslispResult(self.euslisp.exec_internal(cmd))

    def swank_complete_form(self, *args):
        # (swank:complete-form
        #    (quote ("float-vector" swank::%cursor-marker%))
        return

    def swank_quit_lisp(self, *args):
        self.euslisp.stop()
        self.close_request.set()

    def swank_backtrace(self, start, end):
        res = self.euslisp.get_callstack(end)
        yield EuslispResult(res[start:])

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
        yield [Symbol(':debug-return'), 0, level, Symbol('nil')]
        yield [Symbol(':return'), {'abort': 'NIL'}, self.command_id]
        for val in self.maybe_new_prompt():
            yield val
        yield [Symbol(':return'), {'abort': msg}, deb.id]

    def swank_swank_require(self, *sexp):
        return

    def swank_init_presentations(self, *sexp):
        return

    def swank_compile_string_for_emacs(self, cmd_str, *args):
        # (sexp buffer-name (:position 1) (:line 1) () ())
        # FIXME: This does not compile actually, just eval instead.
        try:
            sexp = loads(cmd_str, nil=None)
            assert isinstance(sexp, list)
        except AssertionError:
            raise Exception('Invalid s-expression in %s' % cmd_str)
        self.euslisp.exec_internal(cmd_str)
        if len(sexp) > 2:
            msg = dumps(sexp[:2] + [None], none_as='...')
        else:
            msg = cmd_str
        yield [Symbol(":write-string"), "; Loaded {}".format(msg)]
        errors = []
        seconds = 0.01
        yield EuslispResult([Symbol(":compilation-result"), errors, True,
                             seconds, None, None])

    def swank_compile_notes_for_emacs(self, *args):
        return self.swank_compile_string_for_emacs(*args)

    def swank_compile_file_for_emacs(self, *args):
        return self.swank_compile_file_if_needed(*args)

    def swank_compile_file_if_needed(self, filename, loadp):
        # FIXME: This returns without checking/compiling the file
        errors = []
        seconds = 0.01
        yield EuslispResult([Symbol(":compilation-result"), errors, True,
                             seconds, loadp, filename])

    def swank_load_file(self, filename):
        yield [Symbol(":write-string"), "\nLoading file: %s ..." % filename]
        res = self.euslisp.exec_internal('(lisp:load "{0}")'.format(qstr(filename)))
        yield [Symbol(":write-string"), "\nLoaded."]
        yield EuslispResult(res)

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
        cmd = """(slime::slime-describe-symbol "{0}")""".format(qstr(sym.strip()))
        yield EuslispResult(self.euslisp.exec_internal(cmd))

    def swank_describe_function(self, func):
        return self.swank_describe_symbol(func)

    def swank_describe_definition_for_emacs(self, name, type):
        return self.swank_describe_symbol(name)

    def swank_swank_expand_1(self, form):
        cmd = """(slime::slime-macroexpand '{0})""".format(form)
        yield EuslispResult(self.euslisp.exec_internal(cmd))

    def swank_list_all_package_names(self, nicknames=None):
        cmd = """(slime::slime-all-packages {0})""".format(dumps(nicknames))
        yield EuslispResult(self.euslisp.exec_internal(cmd))

    def swank_apropos_list_for_emacs(self, key, external_only=None, case_sensitive=None,
                                     package=None):
        # ignore 'external_only' and 'case_sensitive' arguments
        package = package[-1] # unquote
        cmd = """(slime::slime-apropos-list "{0}" {1})""".format(qstr(key), dumps(package))
        yield EuslispResult(self.euslisp.exec_internal(cmd))

    def swank_set_package(self, name):
        cmd = """(slime::set-package "{0}")""".format(qstr(name))
        yield EuslispResult(self.euslisp.exec_internal(cmd))

    def swank_default_directory(self):
        res = self.euslisp.exec_internal("(lisp:pwd)")
        yield EuslispResult(res)

    def swank_set_default_directory(self, dir):
        cmd = """(progn (lisp:cd "{0}") (lisp:pwd))""".format(qstr(dir))
        yield EuslispResult(self.euslisp.exec_internal(cmd))

if __name__ == '__main__':
    h = EuslimeHandler()
    print(dumps(h.swank_connection_info().next()))
