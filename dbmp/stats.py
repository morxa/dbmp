#! /usr/bin/env python
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
import ConfigParser
import Gnuplot
import pymongo
import scipy.stats

evaluator = 'complementarity_weighted_fp_evaluator_50_50'

def plot_evaluation_vs_planning_time(db, domain_name):
    """ Create a plot to analyze evaluation functions.

    Plot the domain evaluation score on the x axis and the planning times on
    the y axis.
    Args:
        db: The mongodb db object to use to fetch data.
        domain_name: The name of the domain to create the plot for.
    """
    print('Plotting domain {}.'.format(domain_name))
    plot = Gnuplot.Gnuplot()
    plot.title(domain_name.replace('_', '-'))
    plot.xlabel('Evaluation Score')
    plot.ylabel('Planning Time')
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
    plot.plot(data, data_avgs)
    plot.hardcopy(domain_name.replace(' ', '_') + '_times.pdf', enhanced=1,
                  color=1)
    scores = [ datapoint[0] for datapoint in data ]
    times = [ datapoint[1] for datapoint in data ]
    correlation = scipy.stats.pearsonr(scores, times)
    print('correlation: {}'.format(correlation[0]))

def plot_evaluation_vs_num_completions(db, domain_name):
    plot = Gnuplot.Gnuplot()
    plot.title(domain_name.replace('_', '-'))
    plot.xlabel('Evaluation Score')
    plot.ylabel('Completions %')
    data = []
    for domain in db.domains.find( { 'name': domain_name, 'augmented': True
                                   }).sort([('evaluation.'+evaluator, 1)]):
        eval_score = domain['evaluation'][evaluator]
        successful = db.solutions.find(
            {'domain': domain['_id'], 'error': { '$exists': False }}).count()
        failed = db.solutions.find(
            {'domain': domain['_id'], 'error': { '$exists': True }}).count()
        print('Found {} successful, {} failed'.format(successful, failed))
        if successful or failed:
            data.append([eval_score, float(successful)/(successful + failed)])
    print('data: {}'.format(data))
    plot.plot(data)
    plot.hardcopy(domain_name.replace(' ', '_') + '_completions.pdf',
                  enhanced=1, color=1)


def main():
    parser = argparse.ArgumentParser(
        description='Compute statistics and generate plots to analyze planner'
                    'performance.')
    parser.add_argument('-H', '--db-host', help='the database hostname')
    parser.add_argument('-u', '--db-user', help='the database username')
    parser.add_argument('-p', '--db-passwd', help='the database password')
    parser.add_argument('-c', '--config-file',
                        help='config file to read database info from')
    parser.add_argument('-a', '--all', help='evaluate all domains')
    parser.add_argument('domains', metavar='domain', nargs='*',
                        help='the name of the domain to evaluate')
    args = parser.parse_args()
    db_host = 'localhost'
    db_user = 'planner'
    db_passwd = ''
    if args.config_file:
        config = ConfigParser.ConfigParser()
        config.read(args.config_file)
        if config.has_section('plan_database'):
            if config.has_option('plan_database', 'host'):
                db_host = config.get('plan_database', 'host')
            if config.has_option('plan_database', 'user'):
                db_user = config.get('plan_database', 'user')
            if config.has_option('plan_database', 'passwd'):
                db_passwd = config.get('plan_database', 'passwd')
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
        domains = db.domains.distinct('name')
    else:
        domains = args.domains
    for domain in domains:
        plot_evaluation_vs_planning_time(database, domain)
        plot_evaluation_vs_num_completions(database, domain)

if __name__ == '__main__':
    main()
