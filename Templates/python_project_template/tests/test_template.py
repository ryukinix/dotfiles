#!/usr/bin/env python3
#
#    Copyright © Nexus Edge
#
#       @team: Data Science & Artificial Intelligence
#    @project: Television Content Recognition
#     @author: Marcellus Amadeus & Manoel Vilela
#      @email: {marcellus, manoel.vilela}@nexusedge.com.br
#

import unittest


class TestModule(unittest.TestCase):

    def test_colorize(self):
        ...
        self.assertNotEqual('obj1', 'obj2', "Assert message")

    def test_running(self):
        ...
        self.assertEqual('obj1', 'obj2', "Assert message")

    def test_failing(self):
        ...
        self.assertTrue('obj1', 'obj2', "Assert message")


if __name__ == '__main__':
    unittest.main()
