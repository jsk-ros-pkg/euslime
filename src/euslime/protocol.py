import threading
import traceback
from sexpdata import dumps, loads, Symbol

from euslime.bridge import EuslispResult
from euslime.handler import DebuggerHandler
from euslime.logger import get_logger

log = get_logger(__name__)


class Protocol(object):
    def __init__(self, handler, *args, **kwargs):
        self.handler = handler(*args, **kwargs)
        self.thread_local = threading.local()

    def dumps(self, sexp):
        def with_header(sexp):
            res = dumps(sexp, false_as='nil', none_as='nil')
            # encode to adapt to japanese characters
            header = '{0:06x}'.format(len(res.encode('utf-8')))
            return header + res
        try:
            return with_header(sexp)
        except UnicodeDecodeError:
            # For example in (apropos "default")
            log.warn('UnicodeDecodeError at %s' % sexp)
            assert isinstance(sexp, list)
            sexp = [unicode(x, 'utf-8', 'ignore') if isinstance(x, str)
                    else x for x in sexp]
            return with_header(sexp)

    def make_error(self, err):
        lvl = len(self.handler.debugger) + 1
        deb = DebuggerHandler(self.thread_local.comm_id, lvl, err)
        self.handler.debugger.append(deb)
        yield deb.make_debug_response()

    def make_response(self, result_type, sexp):
        try:
            res = [Symbol(':return'),
                   {result_type: sexp},
                   self.thread_local.comm_id]
            yield res
        except Exception as e:
            for r in self.make_error(e):
                yield r

    def process(self, data):
        data = loads(data)
        if data[0] == Symbol(":emacs-rex"):
            cmd, form, pkg, thread, comm_id = data
            self.handler.package = pkg
        elif data[0] == Symbol(":euslime-test"):
            # Euslime Test Suite
            # process the form and return a handshake in the end
            cmd, comm_id, form = data
            for r in self.process(dumps(form)):
                yield r
            yield [Symbol(":euslime-test"), comm_id]
            return
        else:
            form = data
            comm_id = None
        func = form[0].value().replace(':', '_').replace('-', '_')
        args = form[1:]
        if comm_id:
            self.thread_local.comm_id = comm_id
            log.debug("Processing id: %s ..." % comm_id)

        log.info("func: %s" % func)
        log.info("args: %s" % args)

        try:
            gen = getattr(self.handler, func)(*args)
            if not gen:
                if comm_id:
                    for r in self.make_response('ok', None):
                        yield r
                return
            for resp in gen:
                if isinstance(resp, EuslispResult):
                    for r in self.make_response(resp.response_type,
                                                resp.value):
                        yield r
                else:
                    yield resp
        except Exception as e:
            log.error(traceback.format_exc())
            for r in self.make_error(e):
                yield r

        if comm_id:
            log.debug("... Finished processing id: %s" % comm_id)
