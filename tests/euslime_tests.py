#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Test suite for EusLisp SLIME

## RUN ALL TESTS:
# ./euslime_tests.py

## RUN TESTS FOR EUSLISP PROGRAM
# ./euslime_tests.py eus
# ./euslime_tests.py irteusgl
# ./euslime_tests.py roseus

## RUN A SINGLE TEST
# ./euslime_tests.py eus.test_eval_1

## RUN MATCHING TESTS
# ./euslime_tests.py eus.test_eval_*
# ./euslime_tests.py *.test_eval_1


from euslime.logger import set_log_level
import unittest

# Import tests
from eus import eus
from eus_color import eus_color
from irteusgl import irteusgl
from irteusgl_color import irteusgl_color
from roseus import roseus
from roseus_color import roseus_color

# For wildcard matching
import fnmatch
import inspect
import sys


def list_all_methods():
    def class_methods(cls):
        mths = inspect.getmembers(cls, predicate=inspect.ismethod)
        return [cls.__name__ + '.' + x[0] for x in mths if x[0].startswith('test_')]
    def all_methods(*cls):
        res = []
        for c in cls:
            res += class_methods(c)
        return res

    # TODO: automatically detect available TestCase subclasses
    # https://stackoverflow.com/questions/10099491/how-does-pythons-unittest-module-detect-test-cases
    return all_methods(eus, eus_color,
                       irteusgl, irteusgl_color,
                       roseus, roseus_color)


if __name__ == '__main__':
    set_log_level('debug')

    # implement wildcard method search
    all_methods = None
    new_argv = sys.argv[:1]
    for arg in sys.argv[1:]:
        if '*' in arg:
            if not all_methods:
                all_methods = list_all_methods()
            matches = fnmatch.filter(all_methods, arg)
            if not matches:
                raise AttributeError('No tests found for %s' % arg)
            new_argv += matches
        else:
            new_argv.append(arg)
    sys.argv = new_argv

    try:
        unittest.main()
    except KeyboardInterrupt:
        pass
