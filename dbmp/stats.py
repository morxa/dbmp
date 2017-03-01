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

evaluator = 'complementarity_weighted_fp_evaluator_50_50'

def plot_evaluation_vs_planning_time(db, domain_name):
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
    print('correlation: {}'.format(correlation[0]))
    env = jinja2.Environment(loader=jinja2.FileSystemLoader('stats/templates'))
    plot_template = env.get_template('evaluation_vs_time.p.j2')
    plot = plot_template.render(
        domain=domain_name, data_file=data_file_path, output=base_path)
    plot_file_path = base_path + '.p'
    with open(plot_file_path, 'w') as plot_file:
        plot_file.write(plot)
    subprocess.call(['gnuplot', plot_file_path])

def plot_evaluation_vs_num_completions(db, domain_name):
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
    print('correlation: {}'.format(correlation[0]))
    base_path = 'stats/' + domain_name.replace(' ', '_') + '_completions'
    data_file_path = base_path + '.dat'
    with open(data_file_path, 'w') as data_file:
        for datum in data:
            data_file.write(' '.join(map(str, datum)) + '\n')
    env = jinja2.Environment(loader=jinja2.FileSystemLoader('stats/templates'))
    plot_template = env.get_template('evaluation_vs_completions.p.j2')
    plot = plot_template.render(
        domain=domain_name, data_file=data_file_path, output=base_path)
    plot_file_path = base_path + '.p'
    with open(plot_file_path, 'w') as plot_file:
        plot_file.write(plot)
    subprocess.call(['gnuplot', plot_file_path])

def plot_best_vs_other_planner(db, domain_name, other_planner):
    """ Plot the best augmented domain against another planner.
    Args:
        db: The mongodb db object to use to fetch data.
        domain_name: The name of the domain to create the plot for.
        other_planner: The other planner to compare to.
    """
    other_data = []
    best_data = []
    other_domain = db.domains.find_one(
        {'name': domain_name, 'augmented': { '$ne': True }})
    best_domain = db.domains.find(
        {'name': domain_name, 'augmented': True}).sort(
            [('evaluation.' + evaluator, -1)])[0]
    assert(best_domain), 'Could not find an augmented domain'
    for problem in db.problems.find({'domain': domain_name}):
        other_solution = db.solutions.find_one(
            {'domain': other_domain['_id'],
             'problem': problem['_id'],
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
        domain=domain_name.replace('_', '\\\\_'),
        other_data_file=other_data_file_path,
        best_data_file=best_data_file_path,
        other_planner=other_planner.title(),
        output=base_path)
    plot_file_path = base_path + '.p'
    with open(plot_file_path, 'w') as plot_file:
        plot_file.write(plot)
    subprocess.call(['gnuplot', plot_file_path])

def get_descriptives(db, domain_name, planner):
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
        get_domain_descriptives(domain['_id'], planner)

def get_domain_descriptives(domain_id, planner):
    domain = db.domains.find({'_id': bson.objectid.ObjectId(domain_id))
    if not domain:
        print('Could not find domain with ID "{}"!'.format(domain_id))
        return
    is_augmented = 'augmented' in domain and domain['augmented'] == True
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
        continue
    print('Planner {}, domain {}, augmented: {}'.format(
        planner, domain_name, is_augmented))
    print('successful: {}, failed: {}'.format(
        successful_count, failed_count))
    times = [ solution['resources'][0] for solution in solutions ]
    all_times = copy.copy(times)
    for _ in range(failed_count):
        all_times.append(1800)
    print('Mean: {}.'.format(numpy.mean(times)))
    print('Quantiles: {}.'.format(scipy.stats.mstats.mquantiles(all_times)))
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
        plot_evaluation_vs_planning_time(database, domain)
        plot_evaluation_vs_num_completions(database, domain)
        plot_best_vs_other_planner(database, domain, 'marvin')
        plot_best_vs_other_planner(database, domain, 'ff')
        get_descriptives(database, domain, 'ff')
        get_descriptives(database, domain, 'marvin')

if __name__ == '__main__':
    main()
