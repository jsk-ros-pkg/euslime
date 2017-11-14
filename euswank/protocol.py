from sexpdata import dumps
from sexpdata import loads
from sexpdata import Symbol

from euswank.logger import get_logger

log = get_logger(__name__)


class Protocol(object):
    def __init__(self, handler, prompt='irteusgl$ '):
        self.handler = handler()
        self.prompt = prompt

    def make_error(self, id, msg):
        assert isinstance(msg, str)
        res = [
            Symbol(':debug'), 0, 1,
            [msg, str(), None], [], [],
            [None],
        ]
        res = dumps(res, false_as='nil', none_as='nil')
        header = '{0:06x}'.format(len(res))
        return header + res

    def make_response(self, id, sexp):
        try:
            res = [
                Symbol(':return'),
                {'ok': sexp},
                id,
            ]
            res = dumps(res, false_as='nil', none_as='nil')
            header = '{0:06x}'.format(len(res))
            return header + res
        except Exception as e:
            return self.make_error(id, str(e))

    def process(self, data):
        cmd, form, pkg, thread, comm_id = loads(data)
        func = form[0].value().replace(':', '_').replace('-', '_')
        args = form[1:]

        log.info("func: %s" % func)
        log.info("args: %s" % args)

        try:
            resexp = getattr(self.handler, func)(*args)
            return self.make_response(comm_id, resexp)
        except Exception as e:
            import traceback
            log.error(e)
            log.error(traceback.format_exc())
            return self.make_error(comm_id, str(e))
