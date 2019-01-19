from sexpdata import dumps
from sexpdata import loads
from sexpdata import Symbol
import traceback

from euslime.handler import DebuggerHandler
from euslime.logger import get_logger

log = get_logger(__name__)


class Protocol(object):
    def __init__(self, handler, prompt='irteusgl$ '):
        self.handler = handler()
        self.prompt = prompt

    def dumps(self, sexp):
        res = dumps(sexp, false_as='nil', none_as='nil')
        header = '{0:06x}'.format(len(res))
        return header + res

    def make_error(self, id, err):
        debug = DebuggerHandler(id, err)
        self.handler.debugger.append(debug)

        res = [
            Symbol(':debug'),
            0,  # the thread which threw the condition
            len(self.handler.debugger),  # the depth of the condition
            [debug.message, str(), None],  # s-exp with a description
            DebuggerHandler.restarts,  # list of available restarts
            debug.stack,  # stacktrace
            [None],  # pending continuation
        ]
        return self.dumps(res)

    def make_response(self, id, sexp):
        try:
            res = [
                Symbol(':return'),
                {'ok': sexp},
                id,
            ]
            return self.dumps(res)
        except Exception as e:
            return self.make_error(id, e)

    def process(self, data):
        cmd, form, pkg, thread, comm_id = loads(data)
        func = form[0].value().replace(':', '_').replace('-', '_')
        args = form[1:]

        log.info("func: %s" % func)
        log.info("args: %s" % args)

        try:
            gen = getattr(self.handler, func)(*args)
            if not gen:
                yield self.make_response(comm_id, gen)
                return
            last_resp = gen.next()
            while True:
                try:
                    resp = gen.next()
                    yield self.dumps(last_resp)
                    last_resp = resp
                except StopIteration:
                    yield self.make_response(comm_id, last_resp)
                    return
        except Exception as e:
            # log.error(e)
            log.error(traceback.format_exc())
            yield self.make_error(comm_id, e)
