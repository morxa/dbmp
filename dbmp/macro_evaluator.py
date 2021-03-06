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

import math


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
        macro_evaluations = [self.evaluate(macro) for macro in macros]
        return sum(macro_evaluations)

    def __str__(self):
        """ Get a string representation of the evaluator.

        Use the evaluator's name as string representation.
        """
        return self.name()

    def name(self):
        """ The name of the evaluator.

        The name is used to refer to the evaluation result in the database.
        """
        raise NotImplementedError()


class FrequencyEvaluator(Evaluator):
    """ An Evaluator based on the frequency of the macro.

    Use the total count divided by the normalizer as evaluation score. The idea
    of this evaluation is that action sequences that occur very often are also
    very useful during search.
    """
    def __init__(self, normalizer):
        """ Initialize the evaluator with the given normalizer.

        Args:
            normalizer: The normalizer to use.
        """
        assert (normalizer > 0), 'Normalizer must be > 0'
        self.normalizer = normalizer

    def evaluate(self, macro):
        """ Evaluate the given macro based on its frequency.

        Args:
            macro: A dictionary representing the macro action.

        Returns:
            A score for the macro; the higher the score the better the macro.
        """
        return len(macro.actions) * macro.count / self.normalizer

    def name(self):
        return 'frequency'


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
        if macro.parameter_reduction == 0:
            return 0
        return macro.parameter_reduction / len(macro.flat_parameters)

    def name(self):
        return 'param_reduction'


class WeightedFPEvaluator(Evaluator):
    """ An evaluator that combines frequency and parameter reduction.

    This evaluator combines the FrequencyEvaluator and the ReductionEvaluator
    according to the given weights.
    """
    def __init__(self, frequency_weight, normalizer):
        """ Initialization.

        Initialize the evaluator with the given weights. Note that the weights
        do not need to be normalized, because we use the same evaluator against
        all macros.

        Args:
            frequency_weight: The weight to assign to the FrequencyEvaluator.
            normalizer: The factor to normalize with.
        """
        assert(0 <= frequency_weight <= 100), \
                'Invalid weight {}, must be in [0,100]'.format(frequency_weight)
        self.frequency_weight = frequency_weight
        self.reduction_weight = 100 - frequency_weight
        self.frequency_evaluator = FrequencyEvaluator(normalizer)
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
                (self.frequency_evaluator.evaluate(macro)) + \
                self.reduction_weight * \
                (self.reduction_evaluator.evaluate(macro))

    def name(self):
        return 'fp_{}_{}'.format(self.frequency_weight, self.reduction_weight)


class MacroComplementarityWeightedFPEvaluator(WeightedFPEvaluator):
    """ An evaluator that additionally considers the macros' complementarity.

    This evaluator evaluates each single macro in the same way as the
    WeightedFPEvaluator, but it additionally computes how different the macros
    in the given macro list are. Completely complementary actions in the macros
    are evaluated higher than macros that consist of the same actions.
    """
    def __init__(self, frequency_weight, frequency_normalizer,
                 complementarity_weight):
        WeightedFPEvaluator.__init__(self,
                                     frequency_weight=frequency_weight,
                                     normalizer=frequency_normalizer)

        self.complementarity_weight = complementarity_weight

    def evaluate_list(self, macros):
        """ Evaluate the complementarity of the given macros.

        Args:
            macros: A list of dictionaries representing macros.

        Returns:
            A score for the macros.
        """
        # The sum over distinct actions in each macro.
        num_actions = sum([len(set(macro.actions)) for macro in macros])
        distinct_actions = set()
        for macro in macros:
            distinct_actions.update(macro.actions)
        # The number of distinct actions in all macros
        num_distinct_actions = len(distinct_actions)
        complementarity = num_distinct_actions / num_actions
        complementarity_factor = math.pow(complementarity,
                                          self.complementarity_weight / 10)
        list_evaluation = WeightedFPEvaluator.evaluate_list(self, macros)
        return complementarity_factor * list_evaluation

    def name(self):
        return 'cfp_{}_{}'.format(self.frequency_weight, self.reduction_weight)


class MCWithLengthWeightedFPEvaluator(MacroComplementarityWeightedFPEvaluator):
    """ The MacroComplementarityWeightedFPEvaluator with penalty for many macros

    Having a lot of macros in a domain may be a disadvantage, this evaluator
    tries to minimize the number of macros in a domain.
    """
    def __init__(self, frequency_weight, frequency_normalizer,
                 complementarity_weight, length_weight):
        MacroComplementarityWeightedFPEvaluator.__init__(
            self,
            frequency_weight=frequency_weight,
            frequency_normalizer=frequency_normalizer,
            complementarity_weight=complementarity_weight)
        self.length_weight = length_weight

    def evaluate_list(self, macros):
        return math.pow(len(macros), -self.length_weight/10) * \
                MacroComplementarityWeightedFPEvaluator.evaluate_list(self, macros)

    def name(self):
        return 'clfp_f{}_l{}_c{}'.format(self.frequency_weight,
                                         self.length_weight,
                                         self.complementarity_weight)


class PRSquaredEvaluator(Evaluator):
    """ An evaluator that squares the parameter reduction so a high parameter
    reduction gets a very high value.
    """
    def evaluate(self, macro):
        """ Evaluate the given macro and square the parameter reduction.

        Args:
            macro: The macro as dictionary.
        Returns:
            The macro's score.
        """
        return (macro.parameter_reduction + 1)**2 * macro.count

    def name(self):
        return "pr2"


class ComplementarityPRSquaredEvaluator(PRSquaredEvaluator):
    """
    Consider the macros' complementarity with a squared parameter reduction.
    """
    def evaluate_list(self, macros):
        """ Evaluate the complementarity of the given macros.

        Args:
            macros: A list of dictionaries representing macros.

        Returns:
            A score for the macros.
        """
        # The sum over distinct actions in each macro.
        num_actions = sum([len(set(macro.actions)) for macro in macros])
        distinct_actions = set()
        for macro in macros:
            distinct_actions.update(macro.actions)
        # The number of distinct actions in all macros
        num_distinct_actions = len(distinct_actions)
        complementarity = num_distinct_actions / num_actions
        list_evaluation = PRSquaredEvaluator.evaluate_list(self, macros)
        return complementarity * list_evaluation

    def name(self):
        return "compl_pr2"
