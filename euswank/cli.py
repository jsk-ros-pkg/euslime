#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Author: furushchev <furushchev@jsk.imi.i.u-tokyo.ac.jp>

import argparse
import sys

import euswank
import sexpdata
from euswank.logger import get_logger
from euswank.server import serve

try:
    _input = raw_input
except:
    _input = input


log = get_logger(__name__)


def main():
    p = argparse.ArgumentParser(
        prog=euswank.__name__,
        version=euswank.__version__,
        description=euswank.__doc__)
    p.add_argument("--host", type=str,
                   help="Host to serve",
                   default="0.0.0.0")
    p.add_argument("--port", "-p", type=int,
                   help="Port number to serve",
                   default=4005)
    p.add_argument("--encoding", "-e", type=str,
                   help="Encoding for communication",
                   default="utf-8")
    p.add_argument("--port-filename", "-w", type=str,
                   help="Path to file where port number is written",
                   default=str())

    if len(sys.argv) == 1:
        log.info("Launched with emacs mode")
        init_sexp = sexpdata.loads(_input("Waiting for initialization command..."))
        port_filename = init_sexp[-1][-1]
        serve(port_filename=port_filename)
    else:
        args = p.parse_args()
        serve(host=host, port=port,
              port_filename=port_filename,
              encoding=encoding)


if __name__ == '__main__':
    main()
