#! /usr/bin/env python3
# -*- coding: utf-8 -*-
# vim:fenc=utf-8
#
#  Created:  Wed 10 Jan 2018 11:35:05 CET
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
Validate the results in the database.
"""

import argparse
import db
import dbmp
import logging
import subprocess
import tempfile
import traceback

def main():
    """ Connect to the database and check all plans that are not validated. """
    parser = argparse.ArgumentParser(
        description='Connect to the database and check all plans that are not'
                    ' validated.')
    parser.add_argument('-c', '--config-file',
                        help='config file to read database info from')
    parser.add_argument('-f', '--force', action='store_true',
                        help='also validate solutions which have already been'
                             ' validated')
    args = parser.parse_args()
    database = db.auth(args)
    domain_coll = database.domains
    macro_coll = database.macros
    problem_coll = database.problems
    solution_coll = database.solutions
    query = { 'error': { '$exists': False } }
    if not args.force:
        query['validated'] = { '$ne': True }
    for solution in solution_coll.find(query):
        try:
            domain = domain_coll.find_one({'_id': solution['domain']})['raw']
            domain_file = tempfile.NamedTemporaryFile(mode='w')
            domain_file.write(domain)
            domain_file.flush()
            problem = problem_coll.find_one({'_id': solution['problem']})['raw']
            problem_file = tempfile.NamedTemporaryFile(mode='w')
            problem_file.write(problem)
            problem_file.flush()
            solution_file = tempfile.NamedTemporaryFile(mode='w')
            macro_extractor = dbmp.MacroExtractor()
            solution_file.write(
                macro_extractor.translate_solution(solution['raw']))
            solution_file.flush()
            val_res = subprocess.run(
                ['pddl-validate', domain_file.name, problem_file.name,
                 solution_file.name],
                stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                universal_newlines=True)
            plan_valid = False
            for line in val_res.stdout.splitlines():
                if line == 'Plan valid':
                    plan_valid = True
                    break
            solution_coll.update({'_id': solution['_id']},
                                 {'$set':
                                  {'validated': True,
                                   'validation_success': plan_valid,
                                   'validation_log': val_res.stdout}})
            if not plan_valid:
                print('Error validating solution {}! VAL output:\n{}'.format(
                        solution['_id'], val_res.stdout))
        except Exception as e:
            logging.error('Failed to validate solution {}: {}\n{}'.format(
                    solution['_id'], e, traceback.format_exc()))

if __name__ == '__main__':
    main()
