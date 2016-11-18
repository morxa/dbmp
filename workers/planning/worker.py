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
"""

import argparse
import datetime
import glob
import os
import pymongo
import resource
import subprocess

class Error(Exception):
    """Base class for errors in this module."""
    pass

class NoSolutionFoundError(Error):
    """Error thrown when no solution was found."""
    pass

class DatabaseConnector(object):
    def __init__(self, use_for_macros):
        """Initialize the connection to the database.

        All connection parameters can be defined with environment variables:
            - PLANDB_HOST: the host name of the plan database
            - PLANDB_USER: the user name
            - PLANDB_PWD: the password

        Args:
            use_for_macros: Set to true if the results shall be used for macros.
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
        self.use_for_macros = use_for_macros
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
    def upload_solution(self, domain, problem, solution_string, resources,
            start_time):
        """Upload the solution given as string to the database.

        Args:
            domain: The name of the domain.
            problem: The name of the problem the solution belongs to.
            solution_string: The solution given as string.
            resources: resources used by the planner.
            start_time: The time when the job started.
        """
        self.db.solutions.insert_one(
                { 'domain': domain, 'problem': problem,
                  'raw': solution_string, 'resources': resources,
                  'use_for_macros': self.use_for_macros,
                  'start_time': start_time,
                  'end_time': datetime.datetime.utcnow()})
    def report_failure(self, domain, problem, error, output, start_time):
        """Report failure for the given domain and problem to the database.

        Args:
            domain: The name of the domain.
            problem: The name of the problem.
            error: The error that occurred.
            output: The planner's stdout + stderr.
            start_time: The time when the job was started.
        """
        self.db.solutions.insert_one(
                { 'domain': domain, 'problem': problem,
                  'error': error, 'output': output,
                  'start_time': start_time,
                  'end_time': datetime.datetime.utcnow()})

class Planner(object):
    def __init__(self, domain, problem, time_limit):
        self.domain = domain
        self.problem = problem
        self.time_limit = time_limit
    def run(self):
        """Run the planner."""
        raise NotImplementedError
    def get_solution(self):
        """Get the solution as a string."""
        raise NotImplementedError
    def get_resources(self):
        """Get the resources the planner needed to find a solution."""
        return resource.getrusage(resource.RUSAGE_CHILDREN)
    def get_success_return_codes(self):
        """Get a list of return codes that indicate success."""
        raise NotImplementedError
    def factory(domain, problem, planner='ff', time_limit=30*60):
        if planner in ['ff', 'fastforward', 'fast-forward']:
            return FFPlanner(domain, problem, time_limit)
        elif planner in ['fd', 'fastdownward', 'fast-downward']:
            return FDPlanner(domain, problem, time_limit)
        else:
            raise NotImplementedError
    factory = staticmethod(factory)

class FFPlanner(Planner):
    def __init__(self, domain, problem, time_limit):
        super().__init__(domain, problem, time_limit)
    def run(self):
        result = subprocess.run(
            ['ff', '-o', self.domain, '-f', self.problem],
            stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
            universal_newlines=True,
        )
        #print('Planner returned with return code {}'.format(result.returncode))
        return result
    def get_success_return_codes(self):
        """Get a list of return codes that indicate success."""
        return [0]
    def get_solution(self):
        try:
            solution_file = open('problem.pddl.soln', 'r')
            return solution_file.read()
        except IOError:
            raise NoSolutionFoundError

class FDPlanner(Planner):
    def __init__(self, domain, problem, time_limit):
        super().__init__(domain, problem, time_limit)
    def run(self):
        result = subprocess.run(
            ['fast-downward',
             '--overall-memory-limit', '4G',
             '--overall-time-limit', str(self.time_limit),
             '--alias', 'seq-sat-lama-2011',
             self.domain, self.problem],
            stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
            universal_newlines=True,
        )
        return result
    def get_success_return_codes(self):
        """Get a list of return codes that indicate success."""
        return [0, 6, 7, 8]
    def get_solution(self):
        """Get the last solution, which is always the best solution."""
        solutions = glob.glob('sas_plan*')
        solutions.sort()
        if solutions:
            solution_file = open(solutions[-1], 'r')
            return solution_file.read()
        else:
            raise NoSolutionFoundError

def main():
    parser = argparse.ArgumentParser(
        description='Planning worker that plans one problem and saves the '
                    'result in the database.')
    parser.add_argument('-p', '--planner', default='ff',
                        help='the planner to use')
    parser.add_argument('--use-for-macros', action='store_true',
                        help='Whether the results can be used for macro '
                             'generation.')
    parser.add_argument('--time-limit', type=int, default=30*60,
                        help='the time limit (in secs)')
    parser.add_argument('domain', help='the name of the domain')
    parser.add_argument('problem', help='the name of the problem')
    args = parser.parse_args()
    db_connector = DatabaseConnector(use_for_macros=args.use_for_macros)
    domain_file = open('domain.pddl', 'w')
    domain_string = db_connector.get_domain(args.domain)
    domain_file.write(domain_string)
    domain_file.close()
    problem_file = open('problem.pddl', 'w')
    problem_string = db_connector.get_problem(args.problem)
    problem_file.write(problem_string)
    problem_file.close()
    planner = Planner.factory('domain.pddl', 'problem.pddl', args.planner,
                              args.time_limit)
    if args.time_limit:
        # set time soft limit to time_limit + 60s to allow some overhead
        resource.setrlimit(
            resource.RLIMIT_CPU,
            (args.time_limit + 60, resource.getrlimit(resource.RLIMIT_CPU)[1]))
    start_time = datetime.datetime.utcnow()
    result = planner.run()
    try:
        solution = planner.get_solution()
        print('Planner was successful. Uploading results.')
        db_connector.upload_solution(args.domain, args.problem,
                                     planner.get_solution(),
                                     planner.get_resources(),
                                     start_time)
    except NoSolutionFoundError:
        print('Planner output:\n' + result.stdout)
        print('Could not find a solution. Planner failed.'
              'Planner return code: {}'.format(result.returncode))
        db_connector.report_failure(args.domain, args.problem,
                                    'no solution found', result.stdout,
                                    start_time)

if __name__ == '__main__':
    main()
