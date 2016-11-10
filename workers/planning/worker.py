#! /usr/bin/env python3
# -*- coding: utf-8 -*-
# vim:fenc=utf-8
#
#  Created:  Wed 09 Nov 2016 01:17:44 PM CET
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
PDDL worker which takes a job definition for a planning job, executes the
planner, saves the result to the database, and exits.
The worker expects exactly two arguments:
    - domain: the name of the domain
    - problem: the name of the problem
Both names must exist in the database. The worker expects the arguments in
stdin, e.g. to run the worker with the domain dom and the problem prob, you run:
    echo 'dom prob' | worker.py
You cannot pass the arguments on the command line, i.e. the following will not
work:
    worker.py dom prob # gets stuck reading from stdin, arguments are ignored
"""

import os
import pymongo
import subprocess
import sys

class Error(Exception):
    """Base class for errors in this module."""
    pass

class DatabaseConnector(object):
    def __init__(self):
        """Initialize the connection to the database.

        All connection parameters can be defined with environment variables:
            - PLANDB_HOST: the host name of the plan database
            - PLANDB_USER: the user name
            - PLANDB_PWD: the password
        """
        client = pymongo.MongoClient(
            host=os.environ.get('PLANDB_HOST', 'localhost'),
            ssl=True,
            ssl_certfile="/secrets/mongo-certs/mongo-client.pem",
            ssl_ca_certs="/secrets/mongo-certs/ca.crt"
        )
        self.db = client.macro_planning
        user = os.environ.get('PLANDB_USER', 'planner')
        pwd = os.environ.get('PLANDB_PWD', 'planner')
        self.db.authenticate(user, pwd)
    def get_domain(self, domain_name):
        """Get the domain with the given domain name from the database.

        Args:
            domain_name: The name of the domain to fetch.

        Returns:
            The string representation of the domain.

        Raises:
            AssertionError: The file could not be found in the database.
        """
        res = self.db.domains.find_one({ 'name': domain_name })
        assert res, \
            'The domain "{}" could not be found ' \
            'in the database.'.format(domain_name)
        return res['raw']
    def get_problem(self, problem_name):
        """Get the problem with the given problem name from the database.

        Args:
            problem_name: The name of the problem to fetch.

        Returns:
            The string representation of the problem.

        Raises:
            AssertionError: The problem could not be found
        """
        res = self.db.problems.find_one({ 'name': problem_name })
        assert res, \
            'The problem "{}" could not be found ' \
            'in the database.'.format(problem_name)
        return res['raw']
    def upload_solution(self, problem, solution_string):
        """Upload the solution given as string to the database.

        Args:
            problem: The name of the problem the solution belongs to.
            solution_string: The solution given as string.
        """
        self.db.solutions.insert_one(
            { 'problem': problem, 'raw': solution_string })

class Planner(object):
    def __init__(self, domain, problem):
        self.domain = domain
        self.problem = problem
    def run(self):
        """Run the planner."""
        raise NotImplementedError
    def get_solution(self):
        """Get the solution as a string."""
        raise NotImplementedError
    def factory(domain, problem):
        return FFPlanner(domain, problem)
    factory = staticmethod(factory)

class FFPlanner(Planner):
    def __init__(self, domain, problem):
        super().__init__(domain, problem)
    def run(self):
        result = subprocess.run(
            ['ff', '-o', self.domain, '-f', self.problem],
            stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
            universal_newlines=True,
        )
        #print('Planner returned with return code {}'.format(result.returncode))
        return result
    def get_solution(self):
        solution_file = open('problem.pddl.soln', 'r')
        return solution_file.read()

def main():
    args = sys.stdin.read().split()
    assert len(args) == 2, \
        'Exactly two arguments expected, given {}.'.format(len(args))
    domain = args[0]
    problem = args[1]
    print('Starting job for domain {} and problem {}'.format(domain, problem))
    db_connector = DatabaseConnector()
    domain_file = open('domain.pddl', 'w')
    domain_string = db_connector.get_domain(domain)
    domain_file.write(domain_string)
    domain_file.close()
    problem_file = open('problem.pddl', 'w')
    problem_string = db_connector.get_problem(problem)
    problem_file.write(problem_string)
    problem_file.close()
    planner = Planner.factory('domain.pddl', 'problem.pddl')
    result = planner.run()
    if result.returncode != 0:
        print('Planner failed with return code {}'.format(result.returncode))
        print('output:\n' + result.stdout)
    else:
        print('Planner was successful. Uploading results.')
        solution = planner.get_solution()
        db_connector.upload_solution(problem, solution)




if __name__ == '__main__':
    main()
