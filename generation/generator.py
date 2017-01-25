#! /usr/bin/env python3
# -*- coding: utf-8 -*-
# vim:fenc=utf-8
#
#  Created:  Mon 16 Jan 2017 17:23:08 CET
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

import pyswip
import subprocess
import tempfile

"""

"""

class MacroAction(object):
    """A macro with all its properties."""
    def __init__(self):
        """Initialize the macro."""
        self.initialized = False
    def generate_with_pyswip(self, domain_file_path, actions, parameters):
        """ Generate a macro using pyswip.

        This generates a macro by using pyswip to call generate_macro from
        macro_generator.pl. Note that this currently does not work because of
        serveral issues in pyswip.

        Args:
            domain_file_path: The path to the domain file to read from.
            actions: The actions that the macro should consist of.
            parameters: The parameter assignment for the macro, e.g. [[1,2],[1]]
        """
        macro_file = tempfile.NamedTemporaryFile()
        prolog = pyswip.Prolog()
        prolog.consult('macro_generator.pl')
        print('result file: ' + macro_file.name)
        query_string = 'generate_macro_to_file({}, {}, {}, {})'.format(
            domain_file_path, actions, parameters, macro_file.name)
        print('query: ' + query_string)
        prolog_query = prolog.query(query_string)
        query_result = next(prolog_query)
        print('result: ' + query_result)
        self.initialized = True
    def generate_with_run(self, domain_file_path, actions, parameters):
        """ Generate a macro by calling SWI-Prolog with subprocess.run.

        Instead of using the Python interface from pyswip, simply call swipl to
        run the Prolog program. Note that this only works because we let the
        Prolog program write its result into a file, which we then can read
        again.

        Args:
            domain_file_path: The path to the domain file to read from.
            actions: The actions that the macro should consist of.
            parameters: The parameter assignment for the macro, e.g. [[1,2],[1]]
        """
        self.actions = actions
        macro_file = tempfile.NamedTemporaryFile(mode='r')
        query = 'generate_macro_to_file("{}", {}, {}, "{}").'.format(
            domain_file_path, actions, parameters, macro_file.name)
        subprocess.run(["swipl", "-q", "-l", "macro_generator.pl", "-t", query])
        self.macro = macro_file.read()
        print('\nresult:\n' + self.macro)
        self.initialized = True

def main():
    """ Test MacroAction with a macro from the test domain. """
    m = MacroAction()
    m.generate_with_run('test_data/domain.pddl', ["unstack", "stack"],
        [[1,2],[1,3]])

if __name__ == "__main__":
    main()
