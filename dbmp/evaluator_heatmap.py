#! /usr/bin/env python
# -*- coding: utf-8 -*-
# vim:fenc=utf-8
#
#  Created:  Sat 17 Nov 2018 16:52:45 CET
#  Copyright  2018  Till Hofmann <hofmann@kbsg.rwth-aachen.de>
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
Compare evaluator scores with a 3D heatmap
"""

import db
import itertools
import jinja2
import math
import numpy
import subprocess
import tempfile

MAX_TIME = 300


def get_evaluator_name(f, l, c):
    return 'clfp_f{}_l{}_c{}'.format(f, l, c)


def get_best_domain(database, domain, evaluator):
    try:
        domain_entries = database.domains.find({
            'name': domain,
            'augmented': True,
        })
        return sorted(domain_entries,
                      key=lambda x: x['evaluation'][evaluator],
                      reverse=True)[0]
    except IndexError:
        print('Could not find any domain with name {}'.format(domain))
        raise


def get_score(time):
    if time < 1:
        return 1
    elif time <= MAX_TIME:
        return math.log10(time) / math.log(MAX_TIME)
    else:
        return 0


def get_problem_score(database, planner, domain, problem):
    solution = database.solutions.find_one({
        'domain': domain['_id'],
        'problem': problem['_id'],
        'planner': planner,
        'error': {
            '$exists': False
        },
    })
    if not solution:
        return 0
    time = solution['resources'][0]
    return get_score(time)


def get_domain_score(database, planner, domain, phase):
    scores = []
    for problem in database.problems.find({
            'domain': domain['name'],
            'phase': phase
    }):
        scores.append(get_problem_score(database, planner, domain, problem))
    return numpy.mean(scores)


def get_data(database, planner, domains, phase):
    fs = range(0, 101, 10)
    ls = range(0, 11, 1)
    cs = range(0, 11, 1)
    data = []
    for (domain, f, l, c) in itertools.product(domains, fs, ls, cs):
        domain_entry = get_best_domain(database, domain,
                                       get_evaluator_name(f, l, c))
        score = get_domain_score(database, planner, domain_entry, phase)
        data.append([f / 100, l / 10, c / 10, score])
    return data


def create_datafile(data,
                    outfile=tempfile.NamedTemporaryFile(mode='w',
                                                        delete=False)):
    for datum in data:
        outfile.write(' '.join(map(str, datum)) + '\n')
    outfile.close()
    return outfile.name


def plot_heatmap(database, planner, domains, phase):
    print('Plotting heatmap for domains {}'.format(domains))
    datafile = create_datafile(get_data(database, planner, domains, phase))
    jinja_env = jinja2.Environment(
        loader=jinja2.FileSystemLoader('stats/templates'))
    plot_template = jinja_env.get_template('heatmap.p.j2')
    plot = plot_template.render(
        #planner=get_pretty_name(planner),
        planner=planner,
        #domain=get_pretty_name(domain),
        domain=domain,
        datafile=datafile,
        output='stats/heatmap_{}_{}'.format(planner, '_'.join(domains)))

    #plotfile = tempfile.NamedTemporaryFile(mode='w', delete=False)
    plotfile = open('stats/heatmap.p', 'w')
    plotfile.write(plot)
    plotfile.close()
    subprocess.call(['gnuplot', plotfile.name])
