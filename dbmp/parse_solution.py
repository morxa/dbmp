#! /usr/bin/env python3
# -*- coding: utf-8 -*-
# vim:fenc=utf-8
#
#  Created:  Fri 11 Nov 2016 03:28:57 PM CET
#  Copyright  2016  Till Hofmann <hofmann@kbsg.rwth-aachen.de>
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
Parse a solution given as raw text and save the solution in a structured way in
the database.
"""

import argparse
import bson.objectid
import configparser
import dbmp
import pprint
import pymongo
import re

def parse_solution(raw_solution):
    """Parse a solution given as text into a structured format.

    Args:
        raw_solution: The solution as raw text.

    Returns:
        The solution as a list of actions, where each action is a dict.
    """
    lines = raw_solution.splitlines()
    time_match = re.match('time\s+(\d+)', lines[0].lower())
    if time_match:
        time = time_match.group(1)
        del lines[0]
    else:
        time = 0
    actions = []
    for line in lines:
        if line == '' or re.match(r'^;', line): continue
        inside_parentheses = re.match('.*\((.*)\).*', line.lower()).group(1)
        split_line = re.split('\s+', inside_parentheses)
        operator = split_line[0]
        parameters = split_line[1:]
        actions.append( { 'operator': operator, 'parameters': parameters } )
    return { 'time': time, 'actions': actions }


def main():
    parser = argparse.ArgumentParser(
        description='Upload a PDDL domain or problem file to the database,'
                    ' and optionally start a Kubernetes job.')
    parser.add_argument('-v', '--verbose', action='store_true',
                        help='print the parsed solutions')
    parser.add_argument('-c', '--config-file',
                        help='config file to read database info from')
    parser.add_argument('-H', '--db-host', help='the database hostname')
    parser.add_argument('-u', '--db-user', help='the database username')
    parser.add_argument('-p', '--db-passwd', help='the database password')
    parser.add_argument('-i', '--solution-id', action='append',
                        dest='solution_ids',
                        help='an ID of a solution to parse')
    parser.add_argument('-f', '--force', action='store_true',
                        help='Update db entry even parsed solution already'
                             'exists')
    parser.add_argument('-a', '--all', action='store_true',
                        help='check the database for any unparsed solutions '
                             'and parse them')
    parser.add_argument('solutions', metavar='problem_name', nargs='*',
                        help='a name of a problem/solution to parse')

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
    if args.db_host:
        db_host = args.db_host
    if args.db_user:
        db_user = args.db_user
    if args.db_passwd:
        db_passwd = db_passwd
    client = pymongo.MongoClient(host=db_host)
    database = client.macro_planning
    database.authenticate(db_user, db_passwd)
    coll = database.solutions
    solution_entries = []
    if args.all:
        if args.force:
            solution_entries = list(coll.find())
        else:
            solution_entries = list(coll.find(
                { 'actions': { '$exists': False } } ))
    if args.solution_ids:
        for solution_id in args.solution_ids:
            res = coll.find_one({ '_id': bson.objectid.ObjectId(solution_id) })
            if res:
                solution_entries.append(res)
            else:
                print('Could not find ID {}, skipping!'.format(solution_id))
    if args.solutions:
        for solution in args.solutions:
            res = coll.find_one({ 'problem': solution })
            if res:
                solution_entries.append(res)
            else:
                print('Could not find problem {}, skipping!'.format(solution))
    for solution in solution_entries:
        if 'error' in solution:
            print('Planner failed on problem {} with error "{}", skipping!'\
                    .format(solution['problem'], solution['error']))
            continue
        if 'actions' in solution and not args.force:
            print('Solution {} is already parsed, '
                  'skipping!'.format(solution['_id']))
            continue
        try:
            raw_solution = solution['raw']
        except KeyError:
            print('ERROR: Solution for problem "{}" with ID {} has no "raw" '
                  'entry!'.format(solution['problem'], solution['_id']))
            continue
        domain = database.domains.find_one({'_id': solution['domain']})
        assert domain, \
                'No domain for solution {} found!'.format(solution['_id'])
        is_augmented = False
        if 'augmented' in domain and domain['augmented'] == True:
            is_augmented = True
        if is_augmented:
            extractor = dbmp.MacroExtractor()
            domain_string = domain['raw']
            extractor.extract_macros_from_string(domain_string)
            translated_solution = extractor.translate_solution(raw_solution)
            parsed_solution = parse_solution(translated_solution)
        else:
            parsed_solution = parse_solution(raw_solution)
        if args.verbose:
            pp = pprint.PrettyPrinter()
            print('Result for ID {} (problem "{}"):'\
                    .format(solution['_id'], solution['problem']))
            pp.pprint(parsed_solution)
        coll.update(solution, { '$set': parsed_solution })

if __name__ == '__main__':
    main()
