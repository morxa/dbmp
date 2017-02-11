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
import re
import resource
import subprocess

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
    def obeys_limits(self):
        """Whether this planner has its own resource manager to obey limits."""
        return False
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
        else:
            raise NotImplementedError
    factory = staticmethod(factory)

class FFPlanner(Planner):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
    def run(self):
        result = subprocess.run(
            ['ff', '-o', self.domain, '-f', self.problem],
            **self.common_kwargs
        )
        return result
    def get_success_return_codes(self):
        """Get a list of return codes that indicate success."""
        return [0]
    def get_solution(self):
        try:
            solution_file = open(self.problem + '.soln', 'r')
            return solution_file.read()
        except IOError:
            raise NoSolutionFoundError

class MacroFFPlanner(FFPlanner):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
    def run(self):
        result = subprocess.run(
            ['macroff', '-m', 'C', '-o', self.domain, '-f', self.problem],
            **self.common_kwargs
        )
        return result

class MacroFFSolEPlanner(FFPlanner):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
    def run(self):
        result = subprocess.run(
            ['macroff', '-q', 'macros.pddl',
             '-o', self.domain, '-f', self.problem],
            **self.common_kwargs
        )
        return result

class FDPlanner(Planner):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
    def run(self):
        result = subprocess.run(
            ['fast-downward',
             '--overall-memory-limit', str(self.memory_limit),
             '--overall-time-limit', str(self.time_limit),
             '--alias', 'seq-sat-lama-2011',
             self.domain, self.problem],
            **self.common_kwargs
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
    def obeys_limits(self):
        """Whether this planner has its own resource manager to obey limits."""
        return True

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
                return '\n'.join(stdout_lines[i:])
        raise NoSolutionFoundError

