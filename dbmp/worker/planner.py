#! /usr/bin/env python3
# -*- coding: utf-8 -*-
# vim:fenc=utf-8
#
#  Created:  Fri 10 Feb 2017 11:07:54 CET
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
Planner interface to call various planners and get the results.
"""
import glob
import multiprocessing
import os
import re
import resource
import subprocess
import time

class Error(Exception):
    """Base class for errors in this module."""
    pass

class NoSolutionFoundError(Error):
    """Error thrown when no solution was found."""
    pass

class Planner(object):
    def __init__(self, domain, problem, time_limit, memory_limit):
        self.domain = domain
        self.problem = problem
        self.time_limit = time_limit
        self.memory_limit = memory_limit
        self.common_kwargs = {
            'stdout': subprocess.PIPE,
            'stderr': subprocess.STDOUT,
            'universal_newlines': True,
        }
        self.result = None
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
    def was_successful(self):
        """ Return true if a result was found. """
        if not self.result:
            return False
        if not self.result.returncode:
            return False
        return self.result.returncode in self.get_success_return_codes()
    def obeys_limits(self):
        """Whether this planner has its own resource manager to obey limits."""
        return False
    def get_output(self):
        """Return the output of the subprocess."""
        return self.result.stdout
    def factory(planner, *args, **kwargs):
        if planner in ['ff', 'fastforward', 'fast-forward']:
            return FFPlanner(*args, **kwargs)
        elif planner in ['fd', 'fastdownward', 'fast-downward']:
            return FDPlanner(*args, **kwargs)
        elif planner in ['macroff', 'macro-ff']:
            return MacroFFPlanner(*args, **kwargs)
        elif planner in ['macroff-solep', 'macro-ff-solep']:
            return MacroFFSolEPlanner(*args, **kwargs)
        elif planner == 'marvin':
            return MarvinPlanner(*args, **kwargs)
        elif planner == 'fd-sat':
            return FDSatPlanner(*args, **kwargs)
        elif planner == 'ensemble':
            return EnsemblePlanner(*args, **kwargs)
        else:
            raise NotImplementedError
    factory = staticmethod(factory)

class FFPlanner(Planner):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
    def run(self):
        try:
            os.remove(self.problem + '.soln')
        except FileNotFoundError:
            pass
        self.result = subprocess.run(
            ['ff', '-o', self.domain, '-f', self.problem],
            **self.common_kwargs
        )
        if not self.was_successful():
            print('Error: {}'.format(self.result.stdout))
        return self.result
    def get_success_return_codes(self):
        """Get a list of return codes that indicate success."""
        return [0]
    def get_solution(self):
        try:
            solution_file = open(self.problem + '.soln', 'r')
            return solution_file.read().upper()
        except IOError:
            raise NoSolutionFoundError
    def was_successful(self):
        return os.path.isfile(self.problem + '.soln')

class MacroFFPlanner(FFPlanner):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
    def run(self):
        self.result = subprocess.run(
            ['macroff', '-m', 'C', '-o', self.domain, '-f', self.problem],
            **self.common_kwargs
        )
        return self.result

class MacroFFSolEPlanner(FFPlanner):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
    def run(self):
        self.result = subprocess.run(
            ['macroff', '-q', 'macros.pddl',
             '-o', self.domain, '-f', self.problem],
            **self.common_kwargs
        )
        return self.result

class FDPlanner(Planner):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
    def run(self):
        self.result = subprocess.run(
            ['fast-downward',
             '--overall-memory-limit', str(self.memory_limit),
             '--overall-time-limit', str(self.time_limit),
             '--alias', 'seq-sat-lama-2011',
             self.domain, self.problem],
            **self.common_kwargs
        )
        return self.result
    def get_success_return_codes(self):
        """Get a list of return codes that indicate success."""
        return [0, 6, 7, 8]
    def get_solution(self):
        """Get the last solution, which is always the best solution."""
        solutions = glob.glob('sas_plan*')
        solutions.sort()
        if solutions:
            solution_file = open(solutions[-1], 'r')
            return solution_file.read().upper()
        else:
            raise NoSolutionFoundError
    def obeys_limits(self):
        """Whether this planner has its own resource manager to obey limits."""
        return True
    def was_successful(self):
        return glob.glob('sas_plan*') != []

class MarvinPlanner(Planner):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
    def run(self):
        self.result = subprocess.run(
            ['marvin', self.domain, self.problem],
            **self.common_kwargs)
        return self.result
    def get_solution(self):
        stdout_lines = self.result.stdout.splitlines()
        for i, line in enumerate(stdout_lines):
            if re.match(';+\s*Solution Found.*', line):
                return '\n'.join(stdout_lines[i:]).upper()
        raise NoSolutionFoundError

class FDSatPlanner(FDPlanner):
    """ Fast Downward which stops at the first solution. """
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.output = ''
    def run(self):
        for plan_file in glob.glob('sas_plan*'):
            os.remove(plan_file)
        proc = subprocess.Popen(
            ['fast-downward',
             '--overall-memory-limit', str(self.memory_limit),
             '--overall-time-limit', str(self.time_limit),
             '--alias', 'seq-sat-lama-2011',
             self.domain, self.problem],
            **self.common_kwargs
        )
        while True:
            try:
                proc.wait(timeout=1)
                break
            except subprocess.TimeoutExpired:
                pass
            if os.path.isfile('sas_plan.1'):
                proc.terminate()
                break
        if os.path.isfile('sas_plan.1'):
            proc.returncode = 0
        self.result = proc
        self.output = self.result.stdout.read()
        return proc
    def get_output(self):
        """Get the output of the subprocess."""
        return self.output

class EnsemblePlanner(Planner):
    """ Ensemble planning of Fast-Forward and Fast Downward. """
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.ff_planner = Planner.factory('ff', *args, **kwargs)
        self.fd_planner = Planner.factory('fd-sat', *args, **kwargs)
        self.successful_planner = None
    def run(self):
        class Result(object):
            def __init__(self):
                returncode = None
                stdout = ''
        res = Result()
        ff_process = multiprocessing.Process(target=self.ff_planner.run)
        fd_process = multiprocessing.Process(target=self.fd_planner.run)
        ff_process.start()
        fd_process.start()
        while ff_process.is_alive() and fd_process.is_alive():
            time.sleep(0.1)
        while True:
            if self.fd_planner.was_successful():
                print('FD was successful!')
                self.successful_planner = self.fd_planner
                res.returncode = fd_process.exitcode
                # TODO stdout
                res.stdout = 'FD was successful!'
                ff_process.terminate()
                return res
            if self.ff_planner.was_successful():
               print('FF was successful with return code {}!'.format(
                   ff_process.exitcode))
               self.successful_planner = self.ff_planner
               res.returncode = ff_process.exitcode
               res.stdout = 'FF was successful!'
               fd_process.terminate()
               return res
            if not (ff_process.is_alive() or fd_process.is_alive()):
               break
            time.sleep(0.5)
        res.returncode = 1
        res.stdout = 'Both planners failed!'
        return res

    def get_solution(self):
        assert(self.successful_planner), 'No planner was successful'
        return self.successful_planner.get_solution()
    def get_success_return_codes(self):
        return [0]
