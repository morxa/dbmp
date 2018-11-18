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
import db
import evaluator_heatmap
import evaluator_plot
import jinja2
import math
import numpy
import os
import pprint
import scipy.stats
import subprocess

from operator import itemgetter

MAX_TIME = 300

pretty_names = {
    'complementarity_weighted_fp_evaluator_50_50': 'CFP',
    'complementarity_weighted_fp_evaluator_100_0': 'CF',
    'complementarity_weighted_fp_evaluator_0_100': 'CP',
    'cfp_50_50': 'CFP',
    'clfp_50_50': 'CLFP',
    'cfp_100_0': 'CF',
    'cfp_0_100': 'CP',
    'cfp_80_20': 'CFFP',
    'clfp_80_20': 'CLFFP',
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

def plot_evaluation_vs_planning_time(db, domain_name, evaluator, fit):
    """ Create a plot to analyze evaluation functions.

    Plot the domain evaluation score on the x axis and the planning times on
    the y axis.
    Args:
        db: The mongodb db object to use to fetch data.
        domain_name: The name of the domain to create the plot for.
        fit: If true, a linear fit is added to the plot.
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
                planning_time = MAX_TIME
            time_sum += planning_time
            solution_count += 1
            data.append([eval_score, planning_time])
        if solution_count:
            data_avgs.append([eval_score, time_sum / solution_count])
    base_path = 'stats/' + domain_name.replace(' ', '_') + '_eval_times_' \
            + evaluator
    data_file_path = base_path + '.dat'
    with open(data_file_path, 'w') as data_file:
        for datum in data_avgs:
            data_file.write(' '.join(map(str, datum)) + '\n')
    scores = [ datapoint[0] for datapoint in data ]
    times = [ datapoint[1] for datapoint in data ]
    print('evaluator: {}\ndomain: {}'.format(evaluator, domain_name))
    spearman_rho, spearman_p = scipy.stats.spearmanr(scores, times)
    print('times spearman: rho={}, p={}'.format( spearman_rho, spearman_p))
    print('\n')
    env = jinja2.Environment(loader=jinja2.FileSystemLoader('stats/templates'))
    plot_template = env.get_template('evaluation_vs_time.p.j2')
    plot = plot_template.render(
        domain=get_pretty_name(domain_name), data_file=data_file_path,
        evaluator=get_pretty_name(evaluator),
        fit=fit,
        output=base_path)
    plot_file_path = base_path + '.p'
    with open(plot_file_path, 'w') as plot_file:
        plot_file.write(plot)
    subprocess.call(['gnuplot', plot_file_path])

def plot_evaluation_vs_num_completions(db, domain_name, evaluator, fit):
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
    print('evaluator: {}\ndomain: {}'.format(evaluator, domain_name))
    spearman_rho, spearman_p= scipy.stats.spearmanr(scores, completions)
    print('completions spearman: rho={}, p={}'.format(spearman_rho,spearman_p))
    print('\n')
    base_path = 'stats/' + domain_name.replace(' ', '_') + '_completions'
    base_path = 'stats/' + domain_name.replace(' ', '_') + \
            '_eval_completions_' + evaluator
    data_file_path = base_path + '.dat'
    with open(data_file_path, 'w') as data_file:
        for datum in data:
            data_file.write(' '.join(map(str, datum)) + '\n')
    env = jinja2.Environment(loader=jinja2.FileSystemLoader('stats/templates'))
    plot_template = env.get_template('evaluation_vs_completions.p.j2')
    plot = plot_template.render(
        domain=get_pretty_name(domain_name), data_file=data_file_path,
        evaluator=get_pretty_name(evaluator),
        fit=fit,
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
                              solution1.get('resources', [MAX_TIME])[0]])
        solution2 = db.solutions.find_one(
            {'domain': other_domain['_id'],
             'planner': planner2,
             'problem': problem['_id']})
        planner2_data.append([problem_index,
                              solution2.get('resources', [MAX_TIME])[0]])
        solution_best = db.solutions.find_one(
            {'domain': best_domain['_id'],
             'planner': 'ff',
             'problem': problem['_id']})
        best_data.append([problem_index,
                          solution_best.get('resources', [MAX_TIME])[0]])
        problem_index += 1
    base_path = 'stats/' + domain_name.replace(' ', '_') \
            + '_times_three_planners'
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

def plot_meta(db, domains, planners, evaluator, print_domains=False,
              dbmp_domain=''):
    """ Create a plot for all problems of the given domains.

    Args:
        domains: A list of domains to show.
        planners: A list of planners to compare DBMP to.
        evaluator: The evaluator to use for DBMP.
        print_domains: If set to true, write the domain names into the plot.
    """
    times = {'dbmp': []}
    for planner in planners:
        times[planner] = []
    for domain in db.domains.find({'augmented': { '$ne': True},
                                   'name': { '$in': domains},
                                   }):
        try:
            if dbmp_domain:
                best_domain = db.domains.find_one(
                    {'_id': bson.objectid.ObjectId(dbmp_domain)})
                assert(best_domain['name'] == domain['name']), \
                        'Mismatching domain names "{}" and "{}"'.format(
                            best_domain['name'], domain['name'])
            else:
                best_domain = db.domains.find(
                    {'base_domain': domain['_id'], 'augmented': True}).sort(
                        [('evaluation.' + evaluator, -1)])[0]
        except IndexError:
            print('Could not find a domain for {}!'.format(domain['name']))
            continue
        for problem in db.problems.find({'domain': domain['name']}):
            for planner in planners:
                solution = db.solutions.find_one(
                    {'planner': planner, 'use_for_macros': False,
                     'domain': domain['_id'], 'problem': problem['_id']})
                if solution and 'resources' in solution:
                    times[planner].append(solution['resources'][0])
                else:
                    times[planner].append(MAX_TIME)
            best_solution = db.solutions.find_one(
                {'planner': 'ff', 'use_for_macros': { '$ne': True },
                 'problem': problem['_id'], 'domain': best_domain['_id']})
            if best_solution and 'resources' in best_solution:
                times['dbmp'].append(best_solution['resources'][0])
            else:
                times['dbmp'].append(MAX_TIME)
    data = {
        'ff': [],
        'fast-downward': [],
        'marvin': [],
        'macroff-solep': [],
        'dbmp': []
    }
    for planner in planners:
        data[planner] = []
    print('dbmp times: {}'.format(times['dbmp']))
    for time in range(MAX_TIME):
        for planner, planner_times in times.items():
            num_solutions = len(planner_times)
            if not num_solutions: continue
            count = sum([ ptime <= time for ptime in planner_times])
            quotient = count / num_solutions
            data[planner].append([time, quotient])
    base_path = 'stats/meta_' + domain['name']
    data_files = dict()
    for planner in data.keys():
        data_files[planner] = base_path + '_' + planner + '.dat'
        with open(data_files[planner], 'w') as data_file:
            for datum in data[planner]:
                data_file.write(' '.join(map(str, datum)) + '\n')
    env = jinja2.Environment(loader=jinja2.FileSystemLoader('stats/templates'))
    plot_template = env.get_template('meta.p.j2')
    plot = plot_template.render(
        ff_data_file=data_files['ff'],
        fd_data_file=data_files['fast-downward'],
        marvin_data_file=data_files['marvin'],
        macroff_data_file=data_files['macroff-solep'],
        dbmp_data_file=data_files['dbmp'],
        evaluator=get_pretty_name(evaluator),
        output=base_path,
        domains=', '.join([get_pretty_name(domain) for domain in domains]),
        print_domains=print_domains,
    )
    plot_file_path = base_path + '.p'
    with open(plot_file_path, 'w') as plot_file:
        plot_file.write(plot)
    subprocess.call(['gnuplot', plot_file_path])

def get_descriptives(db, domain_name, planner, phase, evaluator=None):
    """ Get some basic descriptives such as mean time, # solved, quantiles.

    This computes descriptives for the given domain name and planner. It checks
    the database for the best augmented and the unaugmented domain. If any
    solutions exists, descriptives are computed.

    All stats are printed to stdout.

    Args:
        domain_name: The name of the domain.
        planner: The name of the planner.
    """
    if planner == 'ptt':
        actual_planner = 'ff'
        actual_domain = domain_name + '_ptt'
    else:
        actual_planner = planner
        actual_domain = domain_name
    if evaluator:
        try:
            best_domain = db.domains.find(
                {'name': actual_domain, 'augmented': True}).sort(
                    [('evaluation.' + evaluator, -1)])[0]
            print('best domain ID: {}'.format(best_domain['_id']))
        except IndexError:
            print('Could not find best domain')
            return {}
        d = get_domain_descriptives(db, best_domain['_id'], actual_planner,
                                    phase, evaluator)
        if d:
            d['config'] = evaluator
            d['planner'] = planner
            return d
    else:
        orig_domain = db.domains.find_one(
            {'name': actual_domain, 'augmented': { '$ne': True}})
        d = get_domain_descriptives(db, orig_domain['_id'], actual_planner,
                                    phase)
        if d:
            d['config'] = 'original'
            d['planner'] = planner
            return d
    return {}

def get_domain_descriptives(db, domain_id, planner, phase, evaluator=None):
    domain = db.domains.find_one({'_id': bson.objectid.ObjectId(domain_id)})
    if not domain:
        print('Could not find domain with ID "{}"!'.format(domain_id))
        return
    is_augmented = 'augmented' in domain and domain['augmented'] == True
    if not is_augmented:
        evaluator = None
    domain_name = domain['name']
    problems = db.problems.find({'domain': domain_name, 'phase': phase})
    problem_ids = [ problem['_id'] for problem in problems ]
    failed_count = db.solutions.find(
            {'domain': domain['_id'],
             'planner': planner,
             'problem': { '$in': problem_ids },
             'error': { '$exists': True } }
        ).count()
    solutions = db.solutions.find(
            {'domain': domain['_id'],
             'planner': planner,
             'problem': { '$in': problem_ids },
             'error': { '$exists': False } }
        )
    successful_count = solutions.count()
    if not (failed_count or successful_count):
        print('No solutions for domain ID {} and planner {} '
              'found, skipping!'.format(domain['_id'], planner))
        return
    print('Planner: {}\ndomain: {}\ndomain ID: {}\naugmented: {}\n'
          'evaluator: {}'.format(
            planner, domain_name, domain['_id'], is_augmented, evaluator))
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
        all_times.append(MAX_TIME)
        all_lengths.append(10000)
    mean_time = numpy.mean(times)
    quantiles_time = scipy.stats.mstats.mquantiles(all_times)
    mean_length = numpy.mean(solution_lengths)
    quantiles_length = scipy.stats.mstats.mquantiles(all_lengths)
    score = 0
    for time in times:
        if time < 1:
            score += 1
        elif time <= MAX_TIME:
            score += 1 - math.log10(time) / math.log(MAX_TIME)
    print('Time Mean: {}.'.format(mean_time))
    print('Time Quantiles: {}.'.format(quantiles_time))
    print('Mean solution length: {}.'.format(mean_length))
    print('Length Quantiles: {}.'.format(quantiles_length))
    print('Agile Score: {}.'.format(score))
    print('\n')
    return {
        'solved': successful_count,
        'failed': failed_count,
        'mean_time': mean_time,
        'quantiles_time': quantiles_time,
        'mean_length': mean_length,
        'quantiles_length': quantiles_length,
        'score': score,
    }

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
    phase_group = parser.add_mutually_exclusive_group(required=True)
    phase_group.add_argument('--validation', dest='phase',
                             action='store_const', const='validation',
                             help='get stats for the validation group')
    phase_group.add_argument('--test', dest='phase',
                             action='store_const', const='test',
                             help='get stats for the test group')
    parser.add_argument('-d', '--descriptives', action='store_true',
                        help='get descriptives for the given domain and'
                             'planners')
    parser.add_argument('--best', type=int, default=0,
                        help='only get descriptives for the best n'
                             ' configurations')
    table_group = parser.add_mutually_exclusive_group()
    table_group.add_argument('-t', '--table', action='store_true',
                             help='generate a latex table showing the results')
    table_group.add_argument('--score-table', action='store_true',
                             help='generate a latex table showing the scores')
    parser.add_argument('--fit', action='store_true',
                        help='add a linear fit to evaluator plots')
    parser.add_argument('--plot-evaluators', action='store_true',
                        help='create plots to analyze evaluators')
    parser.add_argument('--plot-weights', action='store_true',
                        help='create plots that show the performance change by'
                             ' changing one particular weight')
    parser.add_argument('--plot-against-planner', action='store_true',
                        help='create a comparison plot between the best '
                             'DBMP domain and the original domains with the '
                             'given planners')
    parser.add_argument('--plot-evaluator-heatmap', action='store_true',
                        help='plot a heatmap showing the score depending on the'
                             ' evaluator')
    parser.add_argument('-3', '--plot-three', action='store_true',
                        help='compare DBMP to two other planners')
    parser.add_argument('--meta', action='store_true',
                        help='create a plot completion vs planning time '
                             'with all given domains in one plot')
    parser.add_argument('--planner', action='append',
                        help='the planner to evaluate')
    parser.add_argument('-e', '--evaluator', action='append',
                        default=[],
                        help='the evaluator to use')
    parser.add_argument('--dbmp-domain', type=str, default='',
                        help='Domain ID of the DBMP domain to use instead of '
                             'the domain with the best evaluation score')
    parser.add_argument('domains', metavar='domain', nargs='*',
                        help='the name of the domain to evaluate')
    args = parser.parse_args()
    database = db.auth(args)
    assert(not (args.all and args.domains)), \
            'You cannot specify domain with --all'
    printer = pprint.PrettyPrinter()
    if args.all:
        domains = database.domains.distinct('name')
    else:
        domains = args.domains
    descriptives = {}
    for domain in domains:
        domain_descriptives = []
        if args.descriptives:
            for planner in args.planner:
                # original domain
                d = get_descriptives(database, domain, planner, args.phase)
                if d:
                    domain_descriptives.append(d)
                planner_descriptives = []
                for evaluator in args.evaluator:
                    d = get_descriptives(database, domain, planner, args.phase,
                                         evaluator)
                    if d:
                        planner_descriptives.append(d)
                if args.best:
                    planner_descriptives = sorted(planner_descriptives,
                                                  key=itemgetter('score'),
                                                  reverse=True)
                    planner_descriptives = planner_descriptives[0:args.best]
                domain_descriptives += planner_descriptives
#            for planner in args.planner:
#                if not planner in domain_descriptives:
#                    domain_descriptives[planner] = {
#                            'solved': 0,
#                            'mean_length': 10000,
#                            'mean_time': MAX_TIME,
#                            'quantiles_length': [ 10000, 10000, 10000 ],
#                            'quantiles_time': [ MAX_TIME, MAX_TIME, MAX_TIME ],
#                        }
            descriptives[domain] = sorted(domain_descriptives,
                                          key=itemgetter('score'),
                                          reverse=True)

        if args.plot_evaluators:
            for evaluator in args.evaluator:
                plot_evaluation_vs_planning_time(database, domain, evaluator,
                                                 args.fit)
                plot_evaluation_vs_num_completions(database, domain, evaluator,
                                                  args.fit)
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
        if args.plot_evaluator_heatmap:
            for planner in args.planner:
                evaluator_heatmap.plot_heatmap(database, planner, domain,
                                               args.phase)
        if args.plot_weights:
            for planner in args.planner:
                evaluator_plot.plot_weight_factors(database, planner, domain,
                                                   args.phase)

    printer.pprint(descriptives)
    if args.table or args.score_table:
        env = jinja2.Environment(
            block_start_string = '\BLOCK{',
            block_end_string = '}',
            variable_start_string = '\VAR{',
            variable_end_string = '}',
            comment_start_string = '\#{',
            comment_end_string = '}',
            line_statement_prefix = '%%',
            line_comment_prefix = '%#',
            lstrip_blocks=True,
            trim_blocks=True,
            loader=jinja2.FileSystemLoader('stats/templates'))
        if args.table:
            template = env.get_template('table.tex.j2')
        else:
            template = env.get_template('score_table.tex.j2')
        results = {}
        for domain in domains:
            results[domain] = {}
            for planner in args.planner:
                for d in descriptives[domain]:
                    if d['planner'] == planner:
                        if d['config'] == 'original':
                            results[domain][planner] = d
                        else:
                            results[domain]['dbmp' + planner] = d
        table = template.render(planners=args.planner,domains=domains,
                                results=results)
        os.chdir(os.path.join(os.getcwd(), 'stats'))
        table_path = 'table_core.tex'
        with open(table_path, 'w') as table_file:
            table_file.write(table)
        tex_path = 'table.tex'
        subprocess.call(['pdflatex', tex_path])
    if args.meta:
        if args.dbmp_domain:
            assert(len(args.evaluator) == 0), \
                    'Conflicting arguments, cannot use evaluator if domain ' \
                    'is given!'
            evaluator = 'custom'
            assert(len(args.domains) == 1), \
                    'Can only plot one domain if domain ID is given!'
        else:
            assert(len(args.evaluator) == 1), 'Expected exactly one evaluator'
            evaluator = args.evaluator[0]
        plot_meta(database, args.domains, args.planner, evaluator, True,
                 args.dbmp_domain)

if __name__ == '__main__':
    main()
