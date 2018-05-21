import os
from sexpdata import dumps
from sexpdata import loads
from sexpdata import Symbol
import traceback

from euswank.bridge import EuslispError
from euswank.logger import get_logger

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
        desc = str()
        strace = list()
        if isinstance(err, EuslispError):
            err_msgs = err.message.strip().split()
            if err_msgs and err_msgs[0].startswith("Call Stack"):
                # parse stack trace
                desc_idx = 0
                for i, l in enumerate(err_msgs[1:]):
                    try:
                        num, msg = l.strip().split(": at ")
                        strace.append([int(num), msg,
                                       [Symbol(":restartable"), False]])
                    except:
                        desc_idx = i
                        break
                desc = os.linesep.join(err_msgs[desc_idx:])
            else:
                desc = err.message.strip()
        elif isinstance(err, Exception):
            desc = err.message.strip()
        else:
            desc = err

        restarts = [
            ["QUIT", "Quit to the SLIME top level"],
            ["RESTART", "Restart euslisp process"]
        ]

        res = [
            Symbol(':debug'),
            0,  # the thread which threw the condition
            1,  # the depth of the condition
            [desc, str(), None],  # s-exp with a description
            restarts,  # list of available restarts for the condition
            strace,  # stacktrace
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
            log.error(e)
            log.error(traceback.format_exc())
            yield self.make_error(comm_id, e)
