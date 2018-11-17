#! /usr/bin/env python3
# -*- coding: utf-8 -*-
# vim:fenc=utf-8
#
#  Created:  Tue 16 Jan 2018 11:13:19 CET
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
Fetch solutions from the database.
"""

import argparse
import db

def main():
    arg_parser = argparse.ArgumentParser(
        description="Fetch solutions from the database")
    arg_parser.add_argument('--host', type=str, default='enterprise',
                            help='Host to connect to')
    arg_parser.add_argument('-u', '--user', type=str, default='planner',
                            help='database user')
    arg_parser.add_argument('-p', '--password', type=str,
                            help='password of the database user')
    arg_parser.add_argument('domain', type=str,
                            help='the domain to fetch the solutions for')
    args = arg_parser.parse_args()
    database = db.auth(args)
    print('Fetching results for domain {}.'.format(args.domain))
    for problem in database.problems.find({'domain': args.domain}):
        name = problem['name']
        solution = database.solutions.find_one({'problem': problem['_id'],
                                                'use_for_macros': True})
        if solution:
            solution_file = open(name + '.pddl.soln', 'w')
            solution_file.write(solution['raw'])
            solution_file.close()

if __name__ == '__main__':
    main()
