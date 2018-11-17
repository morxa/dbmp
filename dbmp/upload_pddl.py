#! /usr/bin/env python3
# -*- coding: utf-8 -*-
# vim:fenc=utf-8
#
#  Created:  Tue 08 Nov 2016 07:25:09 PM CET
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
Upload a domain file or a problem file to the database
"""

import argparse
import bson.objectid
import db
import getpass
import re

def get_domainname(domain_string):
    """Gets the domain name from the domain given as string.

    Parses the first line of the string and returns the domain name specified in
    the domain string.

    Args:
        domain_string: A string representation of the domain.

    Returns:
        A domain as string.

    Raises:
        AssertionError: The domain string does not contain a domain name.
    """
    for line in domain_string.splitlines():
        # Skip empty lines and commented lines.
        if re.match(r'^\s*$', line) or re.match(r'^;', line):
            continue
        match = re.search('\(domain ([^)]+)\)', line)
        assert match is not None, \
            'Could not extract domain name from ' \
            'first non-empty line "{}".'.format(line)
        return match.group(1)

def get_problemname(problem_string):
    """Get the problem name from the problem given as string.

    Parses the first lines of the string until it finds the problem definition
    and returns the problem name.

    Args:
        problem_string: A string representation of the problem.

    Returns:
        The problem name as string.

    Raises:
        AssertionError: The problem name could not be found.
    """
    for line in problem_string.splitlines():
        # Skip empty lines and commented lines.
        if re.match(r'^\s*$', line) or re.match(r'^;', line):
            continue
        match = re.search('\(problem ([^)]+)\)', line)
        assert match is not None, \
            'Could not extract problem name from ' \
            'first non-empty line "{}".'.format(line)
        return match.group(1)

def get_domain_of_problem(problem_string):
    """Get the domain of the given problem.

    Parses the problem string and returns the domain name of the problem.
    
    Args:
        problem_string: A string representation of the problem.

    Returns:
        The domain name of the domain this problem is part of.

    Raises:
        AssertionError: The domain name could not be found.
    """
    for line in problem_string.splitlines():
        # Skip empty lines.
        if re.match(r'^\s*$', line):
            continue
        match = re.search('\(:domain ([^)]+)\)', line)
        if match:
            break
    assert match is not None, \
        'Could not extract domain name from ' \
        'first non-empty line "{}".'.format(line)
    return match.group(1)

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

def main():
    parser = argparse.ArgumentParser(
        description='Upload a PDDL domain or problem file to the database,'
                    ' and optionally start a Kubernetes job.')
    parser.add_argument('-c', '--config-file',
                        help='config file to read database info from')
    parser.add_argument('-H', '--db-host', help='the database hostname')
    parser.add_argument('-u', '--db-user', help='the database username')
    parser.add_argument('-p', '--db-passwd', help='the database password')
    parser.add_argument('--start-job', action='store_true',
                        help='start a Kubernetes job for the given problem')
    parser.add_argument('-a', '--all', action='store_true',
                        help='start jobs for all problems in the domain')
    parser.add_argument('--all-missing', action='store_true',
                        help='start jobs for all problems without a solution')
    parser.add_argument('--all-failed', action='store_true',
                        help='start jobs for all problems that failed before')
    parser.add_argument('-t', '--kubernetes-template',
                        help='the job template for the Kubernetes job')
    group = parser.add_mutually_exclusive_group()
    group.add_argument('--training', dest='phase', default='training',
                        action='store_const', const='training',
                        help='mark uploaded problems as training problems')
    group.add_argument('--validation', dest='phase',
                        action='store_const', const='validation',
                        help='mark uploaded problems as validation problems')
    group.add_argument('--test', dest='phase',
                        action='store_const', const='test',
                        help='mark uploaded problems as test problems')
    parser.add_argument('--skip-upload', action='store_true',
                        help='do not upload the problem')
    dom_group = parser.add_mutually_exclusive_group(required=True)
    dom_group.add_argument('--domainfile', help='the domain file to add')
    dom_group.add_argument('--domain',
                           help='the name of the domain the problems belong to')
    dom_group.add_argument('--domain-id',
                           help='the ID of the domain the problems belong to')
    parser.add_argument('--problem', action='append', dest='problems',
                        help='Additional problem to start a job for')
    parser.add_argument('--planner', default='ff', help='the planner to use')
    parser.add_argument('problemfiles', metavar='problemfile', nargs='*',
                        help='the problem files to add')
    args = parser.parse_args()
    if args.problems:
        problems = set(args.problems)
    else:
        problems = set()
    database = db.auth(args)
    domain_coll = database.domains
    problem_coll = database.problems
    solution_coll = database.solutions
    if args.domainfile:
        domainfile = open(args.domainfile, 'r')
        domain_string = domainfile.read()
        domain_name = get_domainname(domain_string)
        if not args.skip_upload:
            assert domain_coll.find({ 'name': domain_name }).count() == 0, \
                'Domain "{}" already exists in the database.'.format(
                    domain_name)
            domain = domain_coll.insert(
                { 'name': domain_name, 'raw': domain_string})
        if not domain:
            domain = domain_coll.find_one({'name': domain_name,
                                           'augmented': { '$ne': True }})['_id']
    elif args.domain_id:
        domain = args.domain_id
        domain_entry = domain_coll.find_one(
            { '_id': bson.objectid.ObjectId(domain) })
        assert(domain_entry), \
                'Could not find domain with ID "{}"'.format(domain)
        domain_name = domain_entry['name']
    else:
        domain_name = args.domain
        domain_entry = domain_coll.find_one(
            { 'name': domain_name })
        assert domain_entry, \
                'Could not find domain with name "{}"'.format(domain_name)
        domain = domain_entry['_id']
    for problempath in args.problemfiles:
        problemfile = open(problempath, 'r')
        problem_string = problemfile.read()
        problem_name = get_problemname(problem_string)
        problem_domain = get_domain_of_problem(problem_string)
        assert problem_domain == domain_name, \
            'Domain "{}" in problem "{}" does not match given domain name ' \
            '"{}".'.format(problem_domain, problem_name, domain_name)
        if not args.skip_upload:
            assert problem_coll.find({ 'name': problem_name }).count() == 0, \
                'Problem "{}" already exists in database.'.format(problem_name)
            problem_id = problem_coll.insert(
                {'name': problem_name, 'domain': domain_name,
                 'raw': problem_string, 'phase': args.phase })
        else:
            problem_id =  problem_coll.find_one({ 'name': problem_name })['_id']
        problems.add(problem_id)
    if args.all or args.all_missing or args.all_failed:
        all_problems = list(
            problem_coll.find({ 'domain': domain_name }, { 'name': True }))
    if args.all:
        for problem in all_problems:
            problems.add(problem['_id'])
    if args.all_missing:
        for problem in all_problems:
            if not solution_coll.find_one(
                    { 'domain_id': domain, 'problem': problem['_id'],
                      'planner': args.planner}):
                problems.add(problem['_id'])
    if args.all_failed:
        for problem in all_problems:
            if solution_coll.find_one(
                    { 'domain_id': domain, 'problem': problem['_id'],
                      'raw': { '$exists': False }}):
                problems.add(problem['_id'])
    if args.start_job:
        for problem in problems:
            start_job(args.planner, args.kubernetes_template, domain, problem)
            print('---')

if __name__ == '__main__':
    main()

