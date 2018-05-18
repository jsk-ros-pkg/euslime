from sexpdata import dumps
from sexpdata import loads
from sexpdata import Symbol

from euswank.bridge import EuslispError
from euswank.logger import get_logger

log = get_logger(__name__)


class Protocol(object):
    def __init__(self, handler, prompt='irteusgl$ '):
        self.handler = handler()
        self.prompt = prompt

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
            return self.make_error(id, e)

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
            return self.make_error(comm_id, e)
