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

import argparse
import configparser
import macro_evaluator
import getpass
import itertools
import pymongo
import pyswip
import re
import subprocess
import tempfile

"""

"""

import itertools

class MacroAction(object):
    """A macro with all its properties."""
    def __init__(self):
        """Initialize the macro."""
        self.initialized = False
        self.num_actions = 0
        self.actions = []
        self.parameters = []
        self.count = 0
        self.type = 'dbmp'
        self.evaluation = {}
        self.domain = ''
    def from_db(self, db_macro):
        """ Initialize the macro from the existing database entry. """
        self.initialized = True
        self.actions = db_macro['actions']
        self.num_actions = len(self.actions)
        self.parameters = db_macro['parameters']
        self.parameter_reduction = db_macro['parameter_reduction']
        self.evaluation = db_macro['evaluation']
        self.count = db_macro['count']
        self.domain = db_macro['domain']
    def generate(self, domain_file_path, actions, parameters):
        """ Generate the macro.

        This calls Prolog (with one of the other class methods) to generate a
        PDDL string representation of the macro.

        Args:
            domain_file_path: The path to the domain file to read from.
            actions: The actions that the macro should consist of.
            parameters: The parameter assignment for the macro, e.g. [[1,2],[1]]
        """
        self.actions = actions
        self.num_actions = len(actions)
        self.parameters = parameters
        flat_parameters = list(itertools.chain.from_iterable(parameters))
        self.parameter_reduction = len(flat_parameters) - max(flat_parameters)
        self.generate_with_run(domain_file_path, actions, parameters)
    def generate_with_pyswip(self, domain_file_path, actions, parameters):
        """ Generate a macro using pyswip.

        This generates a macro by using pyswip to call generate_macro from
        ../generation/macro_generator.pl. Note that this currently does not work
        because of serveral issues in pyswip.

        Args:
            domain_file_path: The path to the domain file to read from.
            actions: The actions that the macro should consist of.
            parameters: The parameter assignment for the macro, e.g. [[1,2],[1]]
        """
        macro_file = tempfile.NamedTemporaryFile()
        prolog = pyswip.Prolog()
        prolog.consult('../generation/macro_generator.pl')
        query_string = 'generate_macro_to_file({}, {}, {}, {})'.format(
            domain_file_path, actions, parameters, macro_file.name)
        prolog_query = prolog.query(query_string)
        query_result = next(prolog_query)
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
        subprocess.call(["swipl", "-q", "-l",
                         "../generation/macro_generator.pl", "-t", query])
        self.macro = macro_file.read()
        self.initialized = True

# TODO: this is copied from scripts and should be separated into a module such
# that it can be reused.
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

def augment_domain(domain, macro):
    """Augment a domain with a macro.

    Args:
        domain: The domain string to augment.
        macro: The macro string to add to the domain.

    Returns:
        The augmented domain as string.
    """
    last_parenthesis = domain.rindex(')')
    return domain[:last_parenthesis] + '\n' + macro + '\n)'



def main():
    """ Test MacroAction with a macro from the test domain. """

    parser = argparse.ArgumentParser(
        description='Read frequent action patterns from the database and '
                    'generate PDDL macro actions for those action patterns.')
    parser.add_argument('--domain', help='the domain the problems belong to')
    parser.add_argument('--domainfile',
                        help='path to the domain this macro belongs to')
    parser.add_argument('-c', '--config-file',
                        help='config file to read database info from')
    parser.add_argument('-H', '--db-host', help='the database hostname')
    parser.add_argument('-u', '--db-user', help='the database username')
    parser.add_argument('-p', '--db-passwd', help='the database password')
    parser.add_argument('-s', '--save', action='store_true',
                        help='upload the resulting macro into the database')
    parser.add_argument('--from-db', action='store_true',
                        help='fetch domain and actions from the database')
    parser.add_argument('-a', '--all', action='store_true',
                        help='generate macros for all action sequences in the '
                             'database for the given domain')
    parser.add_argument('-l', '--occurrence-threshold', type=int, default=1,
                        help='the minimal number of occurrences of the action '
                             'sequence such that a macro is generated from it')
    parser.add_argument('-v', '--verbose', action='store_true',
                        help='print the generated macro actions')
    parser.add_argument('-g', '--augment-domain', action='store_true',
                        help='augment the domain with the macro and upload the '
                             'resulting domain macro')
    parser.add_argument('-m', '--max-num-macros', type=int, default=1,
                        help='the maximum number of macros to add to a domain')
    parser.add_argument('-e', '--evaluate', action='store_true',
                        help='evaluate the resulting macros for their '
                             'usefulness')
    parser.add_argument('--re-evaluate', action='store_true',
                        help='re-evaluate the macros in the database')
    parser.add_argument('action', nargs='*',
                        help='an action and its parameters to include into the '
                             'macro, e.g. "unstack 1,2"')
    args = parser.parse_args()
    assert(len(args.action) % 2 == 0), \
            'You need to specify parameters for each action, given actions: ' \
            + str(args.actions)
    assert(args.domain or args.domainfile), \
            'Please specify a domain name or a domain file'
    assert(args.max_num_macros > 0), \
            'Max number of macros needs to be higher than 0'
    if args.re_evaluate:
        args.from_db = True
        args.evaluate = True
        assert(not args.action), 'Cannot re-evaluate locally'
        assert(not args.save), 'Cannot save macros when re-evaluating'
        assert((not args.all) and (not args.augment_domain)), \
                'Cannot generate macro or augment domain when re-evaluating'
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
    macros = set()
    if args.from_db:
        assert(db_host and db_user), \
                'Please specify database host and user'
        if not db_passwd:
            db_passwd = getpass.getpass()
        client = pymongo.MongoClient(host=db_host)
        database = client.macro_planning
        database.authenticate(db_user, db_passwd)
        # TODO: need to adapt identificaton scripts to write result here
        action_seqs_coll = client.macro_planning.action_sequences
        domain_coll = client.macro_planning.domains
        macros_coll = client.macro_planning.macros
        if not args.domainfile:
            domain_entry = domain_coll.find_one({ 'name': args.domain })
            assert(domain_entry), \
                'Could not find domain {} in the database!'.format(args.domain)
            tmpfile = tempfile.NamedTemporaryFile(mode='w')
            tmpfile.write(domain_entry['raw'])
            tmpfile.flush()
            args.domainfile = tmpfile.name
        if not args.domain:
            args.domain = get_domainname(args.domainfile)
        if args.all:
            domain_id = domain_coll.find_one(
                {'name': args.domain, 'augmented': { '$ne': True }})['_id']
            for sequence in action_seqs_coll.find(
                    { 'value.domain': domain_id }):
                for parameters in sequence['value']['parameters']:
                    if parameters['count'] < args.occurrence_threshold:
                        continue
                    actions = sequence['value']['actions']
                    parameter_list = []
                    for params in parameters['assignment']:
                        parameter_list.append([int(param) for param in params])
                    m = MacroAction()
                    m.generate(args.domainfile, actions, parameter_list)
                    m.count = int(parameters['count'])
                    m.domain = args.domain
                    macros.add(m)
    if not args.domain:
        dfile = open(args.domainfile, 'r')
        domain_string = dfile.read()
        args.domain = get_domainname(domain_string)
    if args.action:
        actions = args.action[0::2]
        parameters = []
        for params in args.action[1::2]:
            if params == 'none':
                parameters.append([])
            else:
                parameters.append([int(param) for param in params.split(',') ])
        assert(args.domainfile), 'Domain was not specified'
        m = MacroAction()
        m.generate(args.domainfile, actions, parameters)
        m.domain = args.domain
        # We don't really know the count, so assume it is 1.
        m.count = 1
        macros.add(m)
    if args.evaluate:
        evaluators = []
        for weight in range(0,101,10):
            evaluators.append(
                macro_evaluator.WeightedFPEvaluator(weight, 100 - weight))
        for macro in macros:
            evaluation = {}
            for evaluator in evaluators:
                evaluation[evaluator.name()] = evaluator.evaluate(macro)
            macro.evaluation = evaluation
    for macro in macros:
        if args.save:
            macros_coll.insert_one(macro.__dict__)
        if args.verbose:
            print(macro.__dict__)
    if args.augment_domain:
        evaluators = []
        for weight in range(0,101,10):
            evaluators.append(
                macro_evaluator.MacroComplementarityWeightedFPEvaluator(
                    weight, 100 - weight))
        for num_macros in range(1, args.max_num_macros+1):
            for macro_combination in itertools.combinations(macros, num_macros):
                assert(args.save), \
                    'You must provide --save if you want to augment the domain'
                domain_entry = domain_coll.find_one(
                    {'name': args.domain, 'augmented': { '$ne': True }})
                assert(domain_entry), \
                        'Could not find domain {}'.format(args.domain)
                augmented_domain_entry = domain_entry
                # remove the ID so we can upload the domain as a new document
                augmented_domain_entry['base_domain'] = domain_entry['_id']
                del augmented_domain_entry['_id']
                augmented_domain_entry['macros'] = \
                        [ macro._id for macro in macro_combination ]
                augmented_domain_entry['augmented'] = True
                domain_string = domain_entry['raw']
                for macro in macro_combination:
                    domain_string = augment_domain(domain_string, macro.macro)
                augmented_domain_entry['raw'] = domain_string
                evaluation = {}
                for evaluator in evaluators:
                    evaluation[evaluator.name()] = \
                        evaluator.evaluate_list(list(macro_combination))
                augmented_domain_entry['evaluation'] = evaluation
                if args.verbose:
                    print('Inserting {}'.format(augmented_domain_entry))
                    print('Evaluation: {}'.format(evaluation))
                domain_coll.insert(augmented_domain_entry)
    if args.re_evaluate:
        for db_macro in macros_coll.find():
            macro = MacroAction()
            macro.from_db(db_macro)
            evaluation = {}
            for evaluator in evaluators:
                evaluation[evaluator.name()] = evaluator.evaluate(macro)
            macros_coll.update_one({'_id': db_macro['_id']},
                                   { '$set': { 'evaluation': evaluation } })


if __name__ == "__main__":
    main()
