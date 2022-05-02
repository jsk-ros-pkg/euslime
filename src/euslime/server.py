try:
    import SocketServer as S
except ImportError:
    import socketserver as S

import socket
import time
import traceback
from threading import Thread

from euslime.bridge import gen_to_string
from euslime.bridge import HEADER_LENGTH
from euslime.handler import EuslimeHandler
from euslime.logger import get_logger
from euslime.protocol import Protocol
from sexpdata import Symbol

ENCODINGS = {
    'iso-latin-1-unix': 'latin-1',
    'iso-utf-8-unix': 'utf-8'
}
ERROR_STRING = """;; You are still in a signal handler.
;;Try reset or throw to upper level as soon as possible.
"""

log = get_logger(__name__)


class EuslimeRequestHandler(S.BaseRequestHandler, object):
    def __init__(self, request, client_address, server):
        self.swank = Protocol(EuslimeHandler, server.program, server.loader,
                              on_output=self._process_output)
        self.swank.handler.euslisp.color = server.color
        self.encoding = ENCODINGS.get(server.encoding, 'utf-8')
        super(EuslimeRequestHandler, self).__init__(
            request, client_address, server)

    def _process_data(self, recv_data):
        for data in self.swank.process(recv_data):
            log.debug('response: %s', data)
            self.send_data(data)

    def _process_output(self, out=None):
        if not out:
            return
        log.debug('output: %s', out)
        self.send_data([Symbol(":write-string"), out])
        if ERROR_STRING in out:
            # TODO: find a better way to detect error signals
            # Even if the user issues a command with same output,
            # read-mode will be desativated when the evaluation ends
            log.error("Error signal received!")
            log.debug("Entering read mode...")
            self.swank.handler.euslisp.read_mode = True
            # Default prompt is not displayed because it is not a tty process
            # and cannot afford to manually print one because ERROR_STRING
            # could have beend issued by the user
            # self.send_data([Symbol(":write-string"), "Fatal: "])
            self.send_data([Symbol(":write-string"), "Entering read mode...\n",
                            Symbol(":repl-result")])
            self.send_data([Symbol(":write-string"), "$ ",
                            Symbol(":repl-result")])
            self.send_data([Symbol(":read-string"), 0, 1])

    def send_data(self, data):
        send_data = self.swank.dumps(data)
        send_data = send_data.encode(self.encoding)
        self.request.send(send_data)

    def handle(self):
        """This method handles packets from swank client.
        The basic Slime packet consists of a 6 char hex-string
        followed by a S-exp with newline on the end.

        e.g.) 000016(:return (:ok nil) 1)\n
        """

        log.debug("Starting output handling loop...")
        Thread(target=self._process_output).start()
        log.debug("Entering handle loop...")
        while not self.swank.handler.close_request.is_set():
            try:
                head_data = self.swank.handler.euslisp.recv_socket_length(
                    self.request, HEADER_LENGTH, socket.MSG_DONTWAIT)
                head_data = gen_to_string(head_data)
                if not head_data:
                    log.error('Empty header received. Closing socket.')
                    self.request.close()
                    break
                hex_len = int(head_data, 16)
                recv_data = self.swank.handler.euslisp.recv_socket_length(
                    self.request, hex_len)
                recv_data = gen_to_string(recv_data)
                log.debug('raw data: %s %s', head_data, recv_data)
                recv_data = recv_data.decode(self.encoding)
                Thread(target=self._process_data, args=(recv_data,)).start()
            except socket.timeout:
                log.error('Socket Timeout')
                break
            except socket.error:
                try:
                    time.sleep(self.swank.handler.euslisp.rate)
                except KeyboardInterrupt:
                    log.info("server keyboard interrupt")
                continue
            except Exception:
                log.error(traceback.format_exc())
                break

        log.warn("Server is shutting down")

        self.swank.handler.swank_quit_lisp()

        # to kill daemon
        def kill_server(s):
            s.shutdown()
        Thread(target=kill_server, args=(self.server,)).start()


class EuslimeServer(S.TCPServer, object):
    def __init__(self, server_address,
                 handler_class=EuslimeRequestHandler,
                 encoding='utf-8',
                 program='roseus',
                 loader='~/.euslime/slime-loader.l',
                 color=False):
        log.info("Starting server with encoding {} and color {}".format(
            encoding, color))
        self.encoding = encoding
        self.program = program
        self.loader = loader
        self.color = color

        super(EuslimeServer, self).__init__(server_address, handler_class)

        addr, port = self.server_address

    def server_bind(self):
        self.socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self.socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEPORT, 1)
        self.socket.settimeout(3)
        self.socket.bind(self.server_address)
        log.info('Serving on %s:%d', *self.socket.getsockname())


def serve(host='0.0.0.0', port=0, port_filename=str(), encoding='utf-8',
          program='roseus', loader='~/.euslime/slime-loader.l', color=False):
    server = EuslimeServer((host, port),
                           encoding=encoding,
                           program=program,
                           loader=loader,
                           color=color)

    host, port = server.socket.getsockname()

    # writing port number to file
    if port_filename:
        try:
            with open(port_filename, "w") as f:
                f.write("%s" % port)
        except Exception as e:
            log.error("Failed to write port number: %s" % str(e))

    try:
        server.serve_forever()
    except Exception:
        log.error(traceback.format_exc())
    finally:
        server.server_close()


if __name__ == '__main__':
    serve()
