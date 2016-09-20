#!/usr/bin/env python3
#
#    Copyright © Manoel Vilela
#
#       @team: Limbocode
#    @project: Python Project Template
#     @author: Manoel Vilela
#      @email: manoel_vilela@engineer.com
#



import unittest


class TestModule(unittest.TestCase):

    def test_colorize(self):
        ...
        self.assertNotEqual('a', 'b', "Assert message")

    def test_running(self):
        ...
        self.assertEqual('a', 'a', "Assert message")

    def test_failing(self):
        ...
        self.assertTrue(True, "Assert message")


if __name__ == '__main__':
    unittest.main()
