#! /usr/bin/env python3
# -*- coding: utf-8 -*-
# vim:fenc=utf-8
#
#  Created:  Fri 10 Feb 2017 12:10:47 CET
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
DBMP Planner that plans with an augmented domain and returns a plan with macros
replaces with the respective action sequence.
"""

import argparse
import ast
import re

import worker.planner

class MacroExtractor(object):
    """ Extract the macro definitions from an augmented PDDL. """
    def __init__(self):
        self.macros = {}
        self.action_regex = r'[-\w]+'
    def extract_macro_def_from_line(self, line):
        """ Extract the macro definition from a single line. """
        action_list_regex = r'\[(?:{0},)*{0}\]'.format(self.action_regex)
        param_list_regex = r'\[(?:\s*\d+,)*\s*\d*\s*\]'
        param_assignment_regex = \
            r'\[(?:\s*{0},)*\s*(?:{0})\s*\]'.format(param_list_regex)
        return re.match(
            '\s*;\s*MACRO\s+({0})\s+'
            'ACTIONS\s+({1})\s+'
            'PARAMETERS\s+({2})'.format(
                self.action_regex,
                action_list_regex,
                param_assignment_regex),
            line
        )
    def extract_macros_from_file(self, domain_file):
        """ Extract macro definitions from the given domain file.
    
        Args:
            domain_file: The augmented domain that contains the macro defitions.
    
        Returns:
            A list of macro actions with their parameter assignments.
        """
        return extract_macros_from_string(open(domain_file, 'r').read())
    def extract_macros_from_string(self, domain_string):
        """ Extract macro definition from the given string.

        Args:
            domain: A string representation of the domain, lines separated by
            newline characters '\n'

        Returns:
            A list of macro actions with their parameter assignments.
        """
        domain = domain_string.splitlines()
        for line in domain:
            match = self.extract_macro_def_from_line(line)
            if match:
                macro = match.group(1)
                actions = match.group(2)
                parameters = match.group(3)
                sub_list = ast.literal_eval(parameters)
                quoted_actions = re.sub('(?<!["\'])(?P<action>[\w-]+)(?![\w-])',
                                        '"\g<action>"', actions)
                action_list = [ a.upper() for a in
                               ast.literal_eval(quoted_actions) ]
                self.macros[macro.upper()] = {
                    'actions': action_list,
                    'parameters': sub_list
                }
        return self.macros
    def translate_solution(self, solution):
        """ Translate each macro in the given solution into an action sequence.

        Args:
            solution: The solution string that may contain macros.
        Returns:
            A solution string with all macros replaced by the corresponding
            action sequence.
        """
        translated_solution = []
        for line in solution.splitlines():
            if re.fullmatch('Time.*', line):
                translated_solution.append(line)
                continue
            if re.fullmatch('', line):
                continue
            if re.match('^;', line):
                continue
            match = re.fullmatch(
                '(\d+:)?\s*\(({0})((?:\s+[-\w]+)*)\)\s*(\[.*\])?'.format(self.action_regex), line)
            assert(match), 'Unexpected solution line "{}"'.format(line)
            action = match.group(2).upper()
            if action in self.macros.keys():
                macro = self.macros[action]
                params = match.group(3).split()
                for i in range(0, len(macro['actions'])):
                    sub_action = '(' + macro['actions'][i]
                    for param in macro['parameters'][i]:
                        sub_action += ' ' + params[param-1]
                    sub_action += ')'
                    translated_solution.append(sub_action)
            else:
                translated_solution.append(line)
        return '\n'.join(translated_solution)

def main():
    """ Main function.

    Read commandline parameters, start the planner, and translate the solution.
    """
    parser = argparse.ArgumentParser(
        description='DBMP Planner that plans with an augmented domain and '
                    'returns a plan with macros replaces with the respective '
                    'action sequence.')
    parser.add_argument('--output', type=argparse.FileType('w'),
                        help='output file for the resulting plan')
    parser.add_argument('-p', '--planner', type=str,
                        help='the planner to use')
    parser.add_argument('-t', '--time-limit', type=int, default=1800,
                        help='the time limit in seconds')
    parser.add_argument('-m', '--memory-limit', type=str, default='4G',
                        help='the memory limit, e.g. 4K, 2G')
    parser.add_argument('domain', type=str,
                        help='the input domain')
    parser.add_argument('problem', type=str,
                        help='the input problem')
    args = parser.parse_args()

    macro_extractor = MacroExtractor()
    macros = macro_extractor.extract_macros_from_file(args.domain)
    planner = worker.planner.Planner.factory(
        args.planner, args.domain, args.problem, args.time_limit,
        args.memory_limit)
    planner.run()
    solution = planner.get_solution()
    translated_solution = macro_extractor.translate_solution(solution)
    print(translated_solution)
    if args.output:
        args.output.write(translated_solution)


if __name__ == '__main__':
    main()
