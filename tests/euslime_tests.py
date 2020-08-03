#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Test suite for EusLisp SLIME

## RUN ALL TESTS:
# ./euslime_tests.py

## RUN TESTS FOR EUSLISP PROGRAM
# ./euslime_tests.py eus
# ./euslime_tests.py irteusgl
# ./euslime_tests.py roseus

from euslime.logger import set_log_level
import unittest

# Import tests
from eus import eus
from irteusgl import irteusgl
from roseus import roseus

if __name__ == '__main__':
    set_log_level('debug')
    try:
        unittest.main()
    except KeyboardInterrupt:
        pass
