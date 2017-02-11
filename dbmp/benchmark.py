#! /usr/bin/env python3
# -*- coding: utf-8 -*-
# vim:fenc=utf-8
#
#  Created:  Mon 30 Jan 2017 10:49:28 CET
#  Copyright  2017  Till Hofmann <hofmann@kbsg.rwth-aachen.de>
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
Run planners on domains augmented with macros.
"""

import argparse
import configparser
import bson.objectid
import pymongo
import sys

# TODO This is copy-pasted, move to common module instead.
def start_job(planner, job_template, domain, problem):
    """ Start a job for the given planner, domain, and problem.

    This reads the job template and substitutes all its parameters. It then
    prints the substituted template to stdout. The output can be piped into
    kubectl.

    Args:
        planner: The name of the planner to use for this job.
        domain: The ID of the domain to use for this job.
        problem: The ID of the problem to use for this job.
    """
    template_file = open(job_template, 'r')
    job_string = template_file.read()\
        .replace('$DOMAIN', str(domain))\
        .replace('$PROBLEM', str(problem))\
        .replace('$LOWERDOMAIN', str(domain).lower())\
        .replace('$LOWERPROBLEM', str(problem).lower())\
        .replace('$PLANNER', planner)
    print(job_string)
    print('---')

def main():
    """ Main program.

    Run planning tasks for all given macros and problems in the domain. All
    parameters are given on the command line or in a config file.
    """
    parser = argparse.ArgumentParser(
        description='Run planners on domains augmented with macros.')
    parser.add_argument('-c', '--config-file',
                        help='config file to read database info from')
    parser.add_argument('-H', '--db-host', help='the database hostname')
    parser.add_argument('-u', '--db-user', help='the database username')
    parser.add_argument('-p', '--db-passwd', help='the database password')
    parser.add_argument('--planner', help='the planner to use')
    parser.add_argument('-t', '--kubernetes-template',
                        help='the job template for the Kubernetes job')
    parser.add_argument('--domain', help='the name of the domain to run')
    parser.add_argument('-a', '--all', action='store_true',
                        help='run all macros that fit the given criteria')
    parser.add_argument('--missing', action='store_true',
                        help='run all problems without a solution')
    parser.add_argument('--evaluator',
                        help='the name of the evaluation function'
                             'to use for filtering macros')
    parser.add_argument('--min-score', type=int, default=0,
                        help='the min score the macro has to have')
    parser.add_argument('--num-macros', type=int, default=0,
                        help='limit to the n highest scoring macros')
    parser.add_argument('macros', metavar='macro', nargs='*',
                        help='the ID of a macro to use')
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
    if not db_passwd:
        db_passwd = getpass.getpass()
    client = pymongo.MongoClient(host=db_host)
    database = client.macro_planning
    database.authenticate(db_user, db_passwd)
    domain_coll = client.macro_planning.domains
    macro_coll = client.macro_planning.macros
    problem_coll = client.macro_planning.problems
    solutions_coll = client.macro_planning.solutions
    domains = set()
    for macro in args.macros:
        domain = domain_coll.find_one(
            {'macros': bson.objectid.ObjectId(macro)})
        if domain:
            domains.add(domain['_id'])
        else:
           print('Warning: Could not find a domain with macro {}. Did you '
                 'generate the augmented domain?'.format(macro),
                 file=sys.stderr)
    if args.all or args.missing:
        assert(args.evaluator), 'Please provide an evaluator'
        query = { 'evaluation.' + args.evaluator: { '$gte': args.min_score }}
        sorter = [ ('evaluation.' + args.evaluator, -1) ]
        for macro in \
                macro_coll.find(query).sort(sorter).limit(args.num_macros):
            domain = domain_coll.find_one( {'macros': macro['_id'] })
            if domain:
                domains.add(domain['_id'])
            else:
                print('Warning: Could not find a domain with macro {}. Did you '
                      'generate the augmented domain?'.format(macro['_id']),
                     file=sys.stderr)
    for domain in domains:
        for problem in problem_coll.find({ 'domain': args.domain }):
            if not args.all and None != solutions_coll.find_one(
                { 'domain': bson.objectid.ObjectId(domain),
                  'problem': bson.objectid.ObjectId(problem['_id']),
                  'planner': args.planner }):
                # solution already exists, skip this problem
                continue
            start_job(args.planner, args.kubernetes_template, domain,
                      problem['_id'])

if __name__ == "__main__":
    main()
