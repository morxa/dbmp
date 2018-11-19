#! /usr/bin/env python
# -*- coding: utf-8 -*-
# vim:fenc=utf-8
#
#  Created:  Mon 19 Nov 2018 18:27:36 CET
#  Copyright  2018  Till Hofmann <hofmann@kbsg.rwth-aachen.de>
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU Library General Public License for more details.
#
#  Read the full text in the LICENSE.GPL file in the doc directory.

"""
Some utility functions for evaluators
"""

def get_standard_evaluators():
    evaluators = []
    for f in range(0, 101, 10):
        for l in range(0, 11):
            for c in range(0, 11):
                evaluators.append('clfp_f{}_l{}_c{}'.format(f, l, c))
    return evaluators
