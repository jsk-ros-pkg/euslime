try:
    import SocketServer as S
except ImportError:
    import socketserver as S

import socket
from thread import start_new_thread
import traceback

from euswank.protocol import Protocol
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
        self.swank = Protocol(EUSwankHandler)
        self.encoding = ENCODINGS.get(server.encoding, 'utf-8')
        super(EUSwankRequestHandler, self).__init__(
            request, client_address, server)

    def handle(self):
        """This method handles packets from swank client.
        The basic Slime packet consists of a 6 char hex-string
        followed by a S-exp with newline on the end.

        e.g.) 000016(:return (:ok nil) 1)\n
        """
        log.debug("handle")
        while True:
            try:
                head_data = self.request.recv(HEADER_LENGTH)
                log.debug('raw header: %s', head_data)
                if not head_data:
                    log.error('Empty header received. Closing socket.')
                    self.request.close()
                    break
                length = int(head_data, 16)
                recv_data = self.request.recv(length)
                log.info('raw data: %s', recv_data)
                recv_data = recv_data.decode(self.encoding)
                result = self.swank.process(recv_data)
                while True:
                    try:
                        send_data = result.next()
                        log.info('response: %s', send_data)
                        send_data = send_data.encode(self.encoding)
                        self.request.send(send_data)
                    except StopIteration:
                        break
            except socket.timeout:
                log.error('socket timeout')
                break
            except KeyboardInterrupt:
                break
            except Exception as e:
                log.error(e)
                log.error(traceback.format_exc())
                break

        log.info("Server is shutting down")

        # to kill daemon
        def kill_server(s):
            s.shutdown()
        start_new_thread(kill_server, (self.server,))


class EUSwankServer(S.TCPServer, object):
    def __init__(self, server_address,
                 handler_class=EUSwankRequestHandler,
                 encoding='utf-8'):
        self.encoding = encoding

        super(EUSwankServer, self).__init__(server_address, handler_class)

        addr, port = self.server_address
        log.info('Serving on %s:%d', addr, port)

    def server_bind(self):
        self.socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self.socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEPORT, 1)
        self.socket.settimeout(3)
        self.socket.bind(self.server_address)


def serve(host='0.0.0.0', port=4005, port_filename=str(), encoding='utt-8'):
    server = EUSwankServer((host, port),
                           encoding=encoding)

    # writing port number to file
    if port_filename:
        try:
            with open(port_filename, "w") as f:
                f.write("%s" % port)
        except Exception as e:
            log.error("Failed to write port number: %s" % str(e))

    try:
        server.serve_forever()
    except Exception as e:
        log.error(e)
        log.error(traceback.format_exc())
    finally:
        server.server_close()


if __name__ == '__main__':
    serve()
