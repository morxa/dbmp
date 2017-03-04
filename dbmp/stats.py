#! /usr/bin/env python3
# -*- coding: utf-8 -*-
# vim:fenc=utf-8
#
#  Created:  Sat 25 Feb 2017 14:21:15 CET
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
Compute statistics for the different augmented domains and generate plots.
"""

import argparse
import copy
import bson.objectid
import configparser
import jinja2
import numpy
import pymongo
import scipy.stats
import subprocess

pretty_names = {
    'complementarity_weighted_fp_evaluator_50_50': 'CFP',
    'complementarity_weighted_fp_evaluator_100_0': 'CF',
    'complementarity_weighted_fp_evaluator_0_100': 'CP',
    'ff': 'FF',
    'fast-downward': 'FastDownward',
    'fd': 'FastDownward',
    'marvin': 'Marvin',
    'macro-ff': 'MacroFF',
    'macroff': 'MacroFF',
    'macroff-solep': 'MacroFF',
    'blocksworld': 'Blocksworld',
    'cleanup_with_kif': 'Cleanup (FF Seeding)',
    'cleanup_fd': 'Cleanup (FD Seeding)',
    'maintenance-scheduling-domain': 'Maintenance',
    'cave-diving-adl-no-costs': 'Cave Diving',
    'barman': 'Barman',
    'hiking': 'Hiking',
    'rcll-production': 'RCLL',
}

def get_pretty_name(name):
    """ Get a pretty name for the given name. """
    return pretty_names.get(name, name)

def plot_evaluation_vs_planning_time(db, domain_name, evaluator):
    """ Create a plot to analyze evaluation functions.

    Plot the domain evaluation score on the x axis and the planning times on
    the y axis.
    Args:
        db: The mongodb db object to use to fetch data.
        domain_name: The name of the domain to create the plot for.
    """
    data = []
    data_avgs = []
    for domain in db.domains.find( { 'name': domain_name, 'augmented': True
                                   }).sort([('evaluation.'+evaluator, 1)]):
        eval_score = domain['evaluation'][evaluator]
        time_sum = 0
        solution_count = 0
        for solution in db.solutions.find(
            {'domain': domain['_id']}):
            if 'resources' in solution:
                planning_time = solution['resources'][0]
            else:
                planning_time = 1800.0
            time_sum += planning_time
            solution_count += 1
            data.append([eval_score, planning_time])
        if solution_count:
            data_avgs.append([eval_score, time_sum / solution_count])
    base_path = 'stats/' + domain_name.replace(' ', '_') + '_times'
    data_file_path = base_path + '.dat'
    with open(data_file_path, 'w') as data_file:
        for datum in data_avgs:
            data_file.write(' '.join(map(str, datum)) + '\n')
    scores = [ datapoint[0] for datapoint in data ]
    times = [ datapoint[1] for datapoint in data ]
    correlation = scipy.stats.pearsonr(scores, times)
    print('evaluator: {}\ndomain: {}'.format(evaluator, domain_name))
    print('times correlation: {}'.format(correlation[0]))
    print('\n')
    env = jinja2.Environment(loader=jinja2.FileSystemLoader('stats/templates'))
    plot_template = env.get_template('evaluation_vs_time.p.j2')
    plot = plot_template.render(
        domain=get_pretty_name(domain_name), data_file=data_file_path,
        output=base_path)
    plot_file_path = base_path + '.p'
    with open(plot_file_path, 'w') as plot_file:
        plot_file.write(plot)
    subprocess.call(['gnuplot', plot_file_path])

def plot_evaluation_vs_num_completions(db, domain_name, evaluator):
    data = []
    for domain in db.domains.find( { 'name': domain_name, 'augmented': True
                                   }).sort([('evaluation.'+evaluator, 1)]):
        eval_score = domain['evaluation'][evaluator]
        successful = db.solutions.find(
            {'domain': domain['_id'], 'error': { '$exists': False }}).count()
        failed = db.solutions.find(
            {'domain': domain['_id'], 'error': { '$exists': True }}).count()
        if successful or failed:
            data.append([eval_score, float(successful)/(successful + failed)])
    scores = [ datapoint[0] for datapoint in data ]
    completions = [ datapoint[1] for datapoint in data ]
    correlation = scipy.stats.pearsonr(scores, completions)
    print('evaluator: {}\ndomain: {}'.format(evaluator, domain_name))
    print('plan length correlation: {}'.format(correlation[0]))
    print('\n')
    base_path = 'stats/' + domain_name.replace(' ', '_') + '_completions'
    data_file_path = base_path + '.dat'
    with open(data_file_path, 'w') as data_file:
        for datum in data:
            data_file.write(' '.join(map(str, datum)) + '\n')
    env = jinja2.Environment(loader=jinja2.FileSystemLoader('stats/templates'))
    plot_template = env.get_template('evaluation_vs_completions.p.j2')
    plot = plot_template.render(
        domain=get_pretty_name(domain_name), data_file=data_file_path,
        output=base_path)
    plot_file_path = base_path + '.p'
    with open(plot_file_path, 'w') as plot_file:
        plot_file.write(plot)
    subprocess.call(['gnuplot', plot_file_path])

def plot_best_vs_other_planner(db, domain_name, other_planner, evaluator):
    """ Plot the best augmented domain against another planner.
    Args:
        db: The mongodb db object to use to fetch data.
        domain_name: The name of the domain to create the plot for.
        other_planner: The other planner to compare to.
    """
    other_data = []
    best_data = []
    if domain_name == 'cleanup_fd':
        other_domain_name = 'cleanup_with_kif'
    else:
        other_domain_name = domain_name
    other_domain = db.domains.find_one(
        {'name': other_domain_name, 'augmented': { '$ne': True }})
    best_domain = db.domains.find(
        {'name': domain_name, 'augmented': True}).sort(
            [('evaluation.' + evaluator, -1)])[0]
    assert(best_domain), 'Could not find an augmented domain'
    for problem in db.problems.find({'domain': domain_name}):
        other_problem = db.problems.find_one(
            {'domain': other_domain_name, 'name':
             problem['name'].replace('prob_cleanup_fd', 'prob_cleanup_with_kif')})
        if not other_problem:
            continue
        other_solution = db.solutions.find_one(
            {'domain': other_domain['_id'],
             'problem': other_problem['_id'],
             'planner': other_planner,
             'use_for_macros': { '$ne': True }})
        if not other_solution or 'error' in other_solution:
            continue
        other_time = other_solution['resources'][0]
        other_data.append([other_time, other_time])
        best_solution = db.solutions.find_one(
            {'domain': best_domain['_id'],
             'problem': problem['_id']})
        if not best_solution or 'error' in best_solution:
            continue
        best_time = best_solution['resources'][0]
        best_data.append([other_time, best_time])
    base_path = 'stats/' + domain_name.replace(' ', '_') \
            + '_times_best_vs_' + other_planner
    other_data_file_path = base_path + '_other.dat'
    with open(other_data_file_path, 'w') as data_file:
        for datum in other_data:
            data_file.write(' '.join(map(str, datum)) + '\n')
    best_data_file_path = base_path + '_best.dat'
    with open(best_data_file_path, 'w') as data_file:
        for datum in best_data:
            data_file.write(' '.join(map(str, datum)) + '\n')
    env = jinja2.Environment(loader=jinja2.FileSystemLoader('stats/templates'))
    plot_template = env.get_template('times_best_vs_other.p.j2')
    plot = plot_template.render(
        domain=get_pretty_name(domain_name),
        other_data_file=other_data_file_path,
        best_data_file=best_data_file_path,
        other_planner=get_pretty_name(other_planner),
        evaluator=get_pretty_name(evaluator),
        output=base_path)
    plot_file_path = base_path + '.p'
    with open(plot_file_path, 'w') as plot_file:
        plot_file.write(plot)
    subprocess.call(['gnuplot', plot_file_path])

def plot_three(db, domain_name, evaluator, planner1, planner2):
    """ Create a plot for DBMP against two other planners. """
    best_data = []
    planner1_data = []
    planner2_data = []
    if domain_name == 'cleanup_fd':
        other_domain_name = 'cleanup_with_kif'
    else:
        other_domain_name = domain_name
    other_domain = db.domains.find_one(
        {'name': other_domain_name, 'augmented': { '$ne': True }})
    best_domain = db.domains.find(
        {'name': domain_name, 'augmented': True}).sort(
            [('evaluation.' + evaluator, -1)])[0]
    problem_index = 1
    for problem in db.problems.find({'domain': domain_name}):
        solution1 = db.solutions.find_one(
            {'domain': other_domain['_id'],
             'planner': planner1,
             'problem': problem['_id']})
        planner1_data.append([problem_index,
                              solution1.get('resources', [1800])[0]])
        solution2 = db.solutions.find_one(
            {'domain': other_domain['_id'],
             'planner': planner2,
             'problem': problem['_id']})
        planner2_data.append([problem_index,
                              solution2.get('resources', [1800])[0]])
        solution_best = db.solutions.find_one(
            {'domain': best_domain['_id'],
             'planner': 'ff',
             'problem': problem['_id']})
        best_data.append([problem_index,
                          solution_best.get('resources', [1800])[0]])
        problem_index += 1
    base_path = 'stats/' + domain_name.replace(' ', '_') \
            + '_times_three_domains'
    p1_data_file_path = base_path + '_' + planner1 + '.dat'
    with open(p1_data_file_path, 'w') as data_file:
        for datum in planner1_data:
            data_file.write(' '.join(map(str, datum)) + '\n')
    p2_data_file_path = base_path + '_' + planner2 + '.dat'
    with open(p2_data_file_path, 'w') as data_file:
        for datum in planner2_data:
            data_file.write(' '.join(map(str, datum)) + '\n')
    best_data_file_path = base_path + '_' + 'dbmp' + '.dat'
    with open(best_data_file_path, 'w') as data_file:
        for datum in best_data:
            data_file.write(' '.join(map(str, datum)) + '\n')
    env = jinja2.Environment(loader=jinja2.FileSystemLoader('stats/templates'))
    plot_template = env.get_template('times_three.p.j2')
    plot = plot_template.render(
        domain=get_pretty_name(domain_name),
        p1_data_file=p1_data_file_path,
        p2_data_file=p2_data_file_path,
        best_data_file=best_data_file_path,
        planner1=get_pretty_name(planner1),
        planner2=get_pretty_name(planner2),
        evaluator=get_pretty_name(evaluator),
        output=base_path)
    plot_file_path = base_path + '.p'
    with open(plot_file_path, 'w') as plot_file:
        plot_file.write(plot)
    subprocess.call(['gnuplot', plot_file_path])


def get_descriptives(db, domain_name, planner, evaluator):
    """ Get some basic descriptives such as mean time, # solved, quantiles.

    This computes descriptives for the given domain name and planner. It checks
    the database for the best augmented and the unaugmented domain. If any
    solutions exists, descriptives are computed.

    All stats are printed to stdout.

    Args:
        domain_name: The name of the domain.
        planner: The name of the planner.
    """
    best_domain = db.domains.find(
        {'name': domain_name, 'augmented': True}).sort(
            [('evaluation.' + evaluator, -1)])[0]
    orig_domain = db.domains.find_one(
        {'name': domain_name, 'augmented': { '$ne': True}})
    for domain in [ orig_domain, best_domain ]:
        get_domain_descriptives(db, domain['_id'], planner, evaluator)

def get_domain_descriptives(db, domain_id, planner, evaluator):
    domain = db.domains.find_one({'_id': bson.objectid.ObjectId(domain_id)})
    if not domain:
        print('Could not find domain with ID "{}"!'.format(domain_id))
        return
    is_augmented = 'augmented' in domain and domain['augmented'] == True
    domain_name = domain['name']
    failed_count = db.solutions.find(
            {'domain': domain['_id'],
             'planner': planner,
             'use_for_macros': { '$ne': True },
             'error': { '$exists': True } }
        ).count()
    solutions = db.solutions.find(
            {'domain': domain['_id'],
             'planner': planner,
             'use_for_macros': { '$ne': True },
             'error': { '$exists': False } }
        )
    successful_count = solutions.count()
    if not (failed_count or successful_count):
        print('No solutions for domain ID {} and planner {} '
              'found, skipping!'.format(domain['_id'], planner))
        return
    print('Planner: {}\ndomain: {}\naugmented: {}\nevaluator: {}'.format(
        planner, domain_name, is_augmented, evaluator))
    print('successful: {}, failed: {}'.format(
        successful_count, failed_count))
    times = []
    solution_lengths = []
    for solution in solutions:
        times.append(solution['resources'][0])
        solution_lengths.append(len(solution['actions']))
    all_times = copy.copy(times)
    all_lengths = copy.copy(solution_lengths)
    for _ in range(failed_count):
        all_times.append(1800)
        all_lengths.append(10000)
    print('Time Mean: {}.'.format(numpy.mean(times)))
    print('Time Quantiles: {}.'.format(
        scipy.stats.mstats.mquantiles(all_times)))
    print('Mean solution length: {}.'.format(numpy.mean(solution_lengths)))
    print('Length Quantiles: {}.'.format(
            scipy.stats.mstats.mquantiles(all_lengths)))
    print('\n')

def main():
    parser = argparse.ArgumentParser(
        description='Compute statistics and generate plots to analyze planner'
                    'performance.')
    parser.add_argument('-H', '--db-host', help='the database hostname')
    parser.add_argument('-u', '--db-user', help='the database username')
    parser.add_argument('-p', '--db-passwd', help='the database password')
    parser.add_argument('-c', '--config-file',
                        help='config file to read database info from')
    parser.add_argument('-a', '--all', action='store_true',
                        help='evaluate all domains')
    parser.add_argument('-d', '--descriptives', action='store_true',
                        help='get descriptives for the given domain and'
                             'planners')
    parser.add_argument('--plot-evaluators', action='store_true',
                        help='create plots to analyze evaluators')
    parser.add_argument('--plot-against-planner', action='store_true',
                        help='create a comparison plot between the best '
                             'DBMP domain and the original domains with the '
                             'given planners')
    parser.add_argument('-3', '--plot-three', action='store_true',
                        help='compare DBMP to two other planners')
    parser.add_argument('--planner', action='append',
                        help='the planner to evaluate')
    parser.add_argument('-e', '--evaluator', action='append',
                        default=[],
                        help='the evaluator to use')
    parser.add_argument('domains', metavar='domain', nargs='*',
                        help='the name of the domain to evaluate')
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
    assert(not (args.all and args.domains)), \
            'You cannot specify domain with --all'
    if args.all:
        domains = database.domains.distinct('name')
    else:
        domains = args.domains
    for domain in domains:
        if args.descriptives:
            for planner in args.planner:
                for evaluator in args.evaluator:
                    get_descriptives(database, domain, planner, evaluator)
                    get_descriptives(database, domain, planner, evaluator)
                    get_descriptives(database, domain, planner, evaluator)
        if args.plot_evaluators:
            for evaluator in args.evaluator:
                plot_evaluation_vs_planning_time(database, domain, evaluator)
                plot_evaluation_vs_num_completions(database, domain, evaluator)
        if args.plot_against_planner:
            for planner in args.planner:
                for evaluator in args.evaluator:
                    plot_best_vs_other_planner(database, domain, planner,
                                               evaluator)
        if args.plot_three:
            assert(len(args.planner) == 2), 'Need two other planners.'
            for evaluator in args.evaluator:
                plot_three(database, domain, evaluator, args.planner[0],
                           args.planner[1])

if __name__ == '__main__':
    main()
