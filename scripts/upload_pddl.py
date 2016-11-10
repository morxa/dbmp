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
import getpass
import pymongo
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
        # Skip empty lines.
        if re.match(r'^\s*$', line):
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
        # Skip empty lines.
        if re.match(r'^\s*$', line):
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

def start_job(job_template, domain, problem):
    template_file = open(job_template, 'r')
    job_string = template_file.read()\
        .replace('$DOMAIN', domain)\
        .replace('$PROBLEM', problem)\
        .replace('$LOWERDOMAIN', domain.lower())\
        .replace('$LOWERPROBLEM', problem.lower())
    print(job_string)

def main():
    parser = argparse.ArgumentParser(
        description='Upload a PDDL domain or problem file to the database,'
                    ' and optionally start a Kubernetes job.')
    parser.add_argument('-H', '--db-host', default='localhost',
                        help='the database hostname')
    parser.add_argument('-u', '--db-user', default='planmgr',
                        help='the database username')
    parser.add_argument('-p', '--db-passwd',
                        help='the database password')
    parser.add_argument('--start-job', action='store_true',
                        help='start a Kubernetes job for the given problem')
    parser.add_argument('-t', '--kubernetes-template',
                        help='the job template for the Kubernetes job')
    parser.add_argument('--skip-upload', action='store_true',
                        help='do not upload the problem')
    parser.add_argument('--domainfile', help='the domain file to add')
    parser.add_argument('--domain', help='the domain the problems belong to')
    parser.add_argument('--problem', action='append', dest='problems',
                        help='Additional problem to start a job for')
    parser.add_argument('problemfiles', metavar='problemfile', nargs='*',
                        help='the problem files to add')
    args = parser.parse_args()
    if args.problems:
        problems = args.problems
    else:
        problems = []
    if not args.skip_upload:
        if not args.db_passwd:
            args.db_passwd = getpass.getpass()
        client = pymongo.MongoClient(host=args.db_host)
        database = client.macro_planning
        database.authenticate(args.db_user, args.db_passwd)
        domain_coll = client.macro_planning.domains
        problem_coll = client.macro_planning.problems
    if args.domainfile:
        domainfile = open(args.domainfile, 'r')
        domain_string = domainfile.read()
        domain = get_domainname(domain_string)
        if args.domain:
            assert domain == args.domain, \
                'Domain "{}" in domain file does not match ' \
                'given domain "{}".'.format(domain, args.domain)
        if not args.skip_upload:
            assert domain_coll.find({ 'name': domain }).count() == 0, \
                'Domain "{}" already exists in the database.'.format(domain)
            domain_coll.insert({ 'name': domain, 'raw': domain_string})
    else:
        assert args.domain, \
            'You need to specify a domain file or a domain name.'
        domain = args.domain
    for problempath in args.problemfiles:
        problemfile = open(problempath, 'r')
        problem_string = problemfile.read()
        problem = get_problemname(problem_string)
        problem_domain = get_domain_of_problem(problem_string)
        assert problem_domain == domain, \
            'Domain "{}" in problem "{}" does not match ' \
            'given domain name "{}".'.format(problem_domain, problem, domain)
        if not args.skip_upload:
            assert domain_coll.find({ 'name': domain }).count() > 0, \
                'Missing domain "{}" in database for problem "{}".'.format(
                    domain, problem)
            assert problem_coll.find({ 'name': problem }).count() == 0, \
                'Problem "{}" already exists in database.'.format(problem)
            problem_coll.insert(
                {'name': problem, 'domain': domain, 'raw': problem_string}
            )
        problems.append(problem)
    for problem in problems:
        print('---')
        start_job(args.kubernetes_template, domain, problem)

if __name__ == '__main__':
    main()

