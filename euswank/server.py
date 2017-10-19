try:
    import SocketServer as S
except ImportError:
    import socketserver as S

import socket
from thread import start_new_thread
import traceback

from euswank.swank import Swank
from euswank.handler import EUSwankHandler
from euswank.logger import get_logger

ENCODINGS = {
    'iso-latin-1-unix': 'latin-1',
    'iso-utf-8-unix': 'utf-8'
}
HEADER_LENGTH = 6

log = get_logger(__name__)


class EUSwankRequestHandler(S.BaseRequestHandler, object):
    def __init__(self, request, client_address, server):
        self.swank = Swank(EUSwankHandler, server.socket)
        self.encoding = ENCODINGS.get(server.encoding, 'utf-8')
        super(EUSwankRequestHandler, self).__init__(request, client_address, server)

    def handle(self):
        """This method handles packets from swank client.
        The basic Slime packet consists of a 6 char hex-string followed by a S-exp with newline on the end.
        e.g.) 000016(:return (:ok nil) 1)\n
        """
        log.debug("handle")
        while True:
            try:
                raw_packet = self.request.recv(HEADER_LENGTH)
                log.debug('raw header: %s', raw_packet)
                if not raw_packet:
                    log.error('Empty header received')
                    self.request.close()
                    break
                length = int(raw_packet, 16)
                data = self.request.recv(length)
                log.info('raw data: %s', data)

                data = data.decode(self.encoding)
                res = self.swank.process(data)

                log.info('response: %s', res)
                res = res.encode(self.encoding)
                self.request.send(res)
            except socket.timeout:
                log.error('socket timeout')
                break
            except KeyboardInterrupt:
                break
            except Exception as e:
                log.error(e)
                log.error(traceback.format_exc())
                break

        # to kill daemon
        def kill_server(s):
            s.shutdown()
        start_new_thread(kill_server, (self.server,))


class EUSwankServer(S.TCPServer, object):
    def __init__(self, server_address, handler_class=EUSwankRequestHandler,
                 port_filename=None, encoding='utf-8'):
        self.port_filename = port_filename
        self.encoding = encoding

        super(EUSwankServer, self).__init__(server_address, handler_class)

        addr, port = self.server_address
        log.info('Serving on %s:%d', addr, port)
        if port_filename:
            with open(port_filename, 'w') as f:
                log.debug('writing port file: %s', port_filename)
                f.write(str(port))

    def server_bind(self):
        self.socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self.socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEPORT, 1)
        self.socket.settimeout(3)
        self.socket.bind(self.server_address)


def serve(address='127.0.0.1', port=4005, port_filename=None, encoding='utt-8'):
    server = EUSwankServer((address, port),
                           port_filename=port_filename,
                           encoding=encoding)
    try:
        server.serve_forever()
    except Exception as e:
        log.error(e)
        log.error(traceback.format_exc())
    finally:
        server.server_close()


if __name__ == '__main__':
    serve()