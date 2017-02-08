#! /usr/bin/env python
# -*- coding: utf-8 -*-
# vim:fenc=utf-8
#
#  Created:  Fri 27 Jan 2017 16:41:29 CET
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
Evaluate the usefulness of macros.
"""

class Evaluator(object):
    def evaluate(self, macro):
        """ Evaluate the given macro and return an evaluation score.

        Args:
            macro: A dictionary representing the macro action.

        Returns:
            A score for the macro; the higher the score the better the macro.
        """
        raise NotImplementedError()
    def evaluate_list(self, macros):
        """ Evaluate the given list of macros and return an evaluation score.

        This evaluates the usefulness of the combination of the macros in the
        given list.

        Args:
            macros: A list of macros represented as dictionaries.
        Returns:
            A score for the macro list.
        """
        macro_evaluations = [ self.evaluate(macro) for macro in macros ]
        return sum(macro_evaluations)
    def name(self):
        """ The name of the evaluator.

        The name is used to refer to the evaluation result in the database.
        """
        raise NotImplementedError()

class FrequencyEvaluator(Evaluator):
    """ An Evaluator based on the frequency of the macro.

    We use the total count of the macro as its frequency, because we assume that
    every macro was evaluated on the same dataset, and thus the number of plans
    and thus the number of all possible action sequences is the same. The idea
    of this evaluation is that action sequences that occur very often are also
    very useful during search.
    """
    def evaluate(self, macro):
        """ Evaluate the given macro based on its frequency.

        Args:
            macro: A dictionary representing the macro action.

        Returns:
            A score for the macro; the higher the score the better the macro.
        """
        return macro.count
    def name(self):
        return 'frequency_evaluator'

class ReductionEvaluator(Evaluator):
    """ An Evaluator based on the macro's parameter reduction.

    The parameter reduction is the number of parameters of the original action
    sequence minus the number of parameters of the macro. The idea behind this
    evaluation is that parameter reduction may improve search because the number
    of instances of the macro is smaller than the number of instances of both
    actions in sequence, and thus the size of the search space is reduced.
    """
    def evaluate(self, macro):
        """ Evaluate the given macro based on its parameter reduction.

        Args:
            macro: A dictionary representing the macro action.

        Returns:
            A score for the macro; the higher the score the better the macro.
        """
        return macro.parameter_reduction
    def name(self):
        return 'reduction_evaluator'

class WeightedFPEvaluator(Evaluator):
    """ An evaluator that combines frequency and parameter reduction.

    This evaluator combines the FrequencyEvaluator and the ReductionEvaluator
    according to the given weights.
    """
    def __init__(self, frequency_weight, reduction_weight):
        """ Initialization.

        Initialize the evaluator with the given weights. Note that the weights
        do not need to be normalized, because we use the same evaluator against
        all macros.

        Args:
            frequency_weight: The weight to assign to the FrequencyEvaluator.
            reduction_weight: The weight to assign to the ReductionEvaluator.
        """
        self.frequency_weight = frequency_weight
        self.reduction_weight = reduction_weight
        self.frequency_evaluator = FrequencyEvaluator()
        self.reduction_evaluator = ReductionEvaluator()
    def evaluate(self, macro):
        """ Evaluate the macro by combining frequency and parameter reduction.

        Always add 1 to the evaluation result of the base evaluators to avoid an
        evaluation result of 0.

        Args:
            macro: A dictionary representing the macro action.

        Returns:
            A score for the macro; the higher the score the better the macro.
        """
        return self.frequency_weight * \
                (self.frequency_evaluator.evaluate(macro) + 1) + \
                self.reduction_weight * \
                (self.reduction_evaluator.evaluate(macro) + 1)
    def name(self):
        return 'weighted_fp_evaluator_{}_{}'.format(self.frequency_weight,
                                                    self.reduction_weight)

class MacroComplementarityWeightedFPEvaluator(WeightedFPEvaluator):
    """ An evaluator that additionally considers the macros' complementarity.

    This evaluator evaluates each single macro in the same way as the
    WeightedFPEvaluator, but it additionally computes how different the macros
    in the given macro list are. Completely complementary actions in the macros
    are evaluated higher than macros that consist of the same actions.
    """
    def evaluate_list(self, macros):
        """ Evaluate the complementarity of the given macros.

        Args:
            macros: A list of dictionaries representing macros.

        Returns:
            A score for the macros.
        """
        # The sum over distinct actions in each macro.
        num_actions = sum([ len(set(macro.actions)) for macro in macros ])
        distinct_actions = set()
        for macro in macros:
            distinct_actions.update(macro.actions)
        # The number of distinct actions in all macros
        num_distinct_actions = len(distinct_actions)
        complementarity = num_distinct_actions / num_actions
        list_evaluation = super(WeightedFPEvaluator, self).evaluate_list(macros)
        return complementarity * list_evaluation
    def name(self):
        return 'complementarity_weighted_fp_evaluator_{}_{}'.format(
            self.frequency_weight, self.reduction_weight)
