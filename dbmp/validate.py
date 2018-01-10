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
import configparser
import subprocess
import logging
import tempfile
import traceback
import pymongo

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
    db_host = 'localhost'
    db_user = 'planner'
    db_passwd = ''
    if args.config_file:
        config = configparser.ConfigParser()
        config.read(args.config_file)
        if 'plan_database' in config:
            if 'host' in config['plan_database']:
                db_host = config['plan_database']['host']
            if 'user' in config['plan_database']:
                db_user = config['plan_database']['user']
            if 'passwd' in config['plan_database']:
                db_passwd = config['plan_database']['passwd']
    client = pymongo.MongoClient(host=db_host)
    database = client.macro_planning
    database.authenticate(db_user, db_passwd)
    domain_coll = client.macro_planning.domains
    macro_coll = client.macro_planning.macros
    problem_coll = client.macro_planning.problems
    solution_coll = client.macro_planning.solutions
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
            solution_file.write(solution['raw'])
            solution_file.flush()
            val_res = subprocess.run(
                ['pddl-validate', domain_file.name, problem_file.name,
                 solution_file.name],
                stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                universal_newlines=True)
            solution_coll.update({'_id': solution['_id']},
                                 {'$set':
                                  {'validated': True,
                                   'validation_result': val_res.returncode,
                                   'validation_log': val_res.stdout}})
            if val_res.returncode != 0:
                print('Error validating solution {}! VAL output:\n{}'.format(
                        solution['_id'], val_res.stdout))
        except Exception as e:
            logging.error('Failed to validate solution {}: {}\n{}'.format(
                    solution['_id'], e, traceback.format_exc()))

if __name__ == '__main__':
    main()
