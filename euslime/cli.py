#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Author: furushchev <furushchev@jsk.imi.i.u-tokyo.ac.jp>

import argparse
import sys

import euslime
import sexpdata
from euslime.logger import get_logger, set_log_level, LOG_LEVELS
from euslime.server import serve

try:
    _input = raw_input
except:
    _input = input


log = get_logger(__name__)


def main():
    p = argparse.ArgumentParser(
        prog=euslime.__name__,
        version=euslime.__version__,
        description=euslime.__doc__,
        formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    p.add_argument("--emacs-mode", action="store_true",
                   help="Launch with emacs mode")
    p.add_argument("--color", action="store_true",
                   help="Support colored output")
    p.add_argument("--host", type=str,
                   help="Host to serve",
                   default="0.0.0.0")
    p.add_argument("--port", "-p", type=int,
                   help="Port number to serve",
                   default=0)
    p.add_argument("--encoding", "-e", type=str,
                   help="Encoding for communication",
                   default="utf-8")
    p.add_argument("--port-filename", "-w", type=str,
                   help="Path to file where port number is written",
                   default=str())
    p.add_argument("--log-level", "-l", type=str,
                   help="Log Level", default="info",
                   choices=LOG_LEVELS.keys())

    args = p.parse_args()
    set_log_level(args.log_level)

    if args.emacs_mode:
        log.info("Launched with emacs mode")
        init_sexp = sexpdata.loads(
            _input("Waiting for initialization command..."))
        args.port = init_sexp[-1][-2]
        args.port_filename = init_sexp[-1][-1]

    serve(host=args.host, port=args.port,
          port_filename=args.port_filename,
          encoding=args.encoding,
          color=args.color)

if __name__ == '__main__':
    main()
