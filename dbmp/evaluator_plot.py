#! /usr/bin/env python
# -*- coding: utf-8 -*-
# vim:fenc=utf-8
#
#  Created:  Sat 17 Nov 2018 20:06:28 CET
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
Plot the score of an evaluator against the performance.
"""

import evaluator_heatmap
import jinja2
import numpy
import subprocess
import tempfile

FACTORS=['f', 'l', 'c']

def create_datafile(data,
                    outfile=tempfile.NamedTemporaryFile(mode='w', delete=False)
                   ):
    for datum in data:
        outfile.write(' '.join(map(str, datum)) + '\n')
    outfile.close()
    return outfile.name

def get_mean_data(data, factor):
    column = FACTORS.index(factor)
    sorted_data = {}
    for datum in data:
        if not datum[column] in sorted_data:
            sorted_data[datum[column]] = []
        sorted_data[datum[column]].append(datum[3])
    mean_data = []
    for weight, scores in sorted_data.items():
        mean = numpy.mean(scores)
        stddev = numpy.std(scores)
        mean_data.append([weight, mean, stddev])
    return mean_data

def plot_weight_factors(database, planner, domain, phase, factors=FACTORS):
    data = evaluator_heatmap.get_data(database, planner, domain, phase)
    datafile = create_datafile(
        data,
        open('stats/weights_{}_{}.dat'.format(planner, domain), 'w'))
    for factor in factors:
        plot_weight_factor(datafile, planner, domain, factor)

def plot_weight_factor(datafile, planner, domain, factor):
    assert factor in FACTORS, \
            'Factor needs to be one of ' + FACTORS
    jinja_env = jinja2.Environment(
        loader=jinja2.FileSystemLoader('stats/templates'))
    plot_template = jinja_env.get_template('weight_factors.p.j2')
    plot = plot_template.render(
        planner=planner,
        domain=domain,
        datafile=datafile,
        column=FACTORS.index(factor)+1,
        xlabel='w_{}'.format(factor),
        output='stats/weights_{}_{}_{}'.format(factor, planner, domain)
    )
    plotfile = open('stats/weights_{}_{}_{}.p'.format(factor, planner, domain),
                    'w')
    plotfile.write(plot)
    plotfile.close()
    subprocess.call(['gnuplot', plotfile.name])
