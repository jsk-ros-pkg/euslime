#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Author: furushchev <furushchev@jsk.imi.i.u-tokyo.ac.jp>

import sys


if __name__ == '__main__':
    from euswank.server import serve

    port_file = sys.argv[1]
    if port_file:
        with open(port_file, "w") as f:
            f.write("4005\n")

    serve(port=port)
