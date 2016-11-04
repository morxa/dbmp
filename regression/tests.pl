#! /usr/bin/env swipl

/**
 *  tests.pl - Unit tests for regression
 *
 *  Created:  Thu 20 Oct 2016 14:28:54 CEST
 *  Copyright  2016  Till Hofmann <hofmann@kbsg.rwth-aachen.de>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  Read the full text in the LICENSE.GPL file in the doc directory.
 */

:- begin_tests(regression).
:- [regression].

:- dynamic effect/2.


init_location_types :-
  assertz(type_of_object(room, kitchen)),
  assertz(type_of_object(location, kitchen)),
  assertz(type_of_object(location, hall)).

test(substitute_simple) :-
  substitute(a, [a,b], c, [c,b]).

test(substitute_args) :-
  substitute(a, [p(a), p(p(a))], b, [p(b), p(p(b))]).

test(substitute_predicate) :-
  substitute(a, [a(b)], b, [b(b)]).

test(substitute_predicate_args) :-
  substitute(a, [a(a)], b, [b(b)]).

test(
  substitute_with_constraint,
  [ setup(init_location_types),
    cleanup(retractall(type_of_object(_,_)))
  ]
) :-
  substitute(_, [goto(kitchen,hall)], location, type_of_object(room),
    [goto(location,hall)]).


init_goto_action :-
  assertz(effect(goto(L1,L2),and(not(at(L1)),at(L2)))),
  init_location_types.
init_dropall_action :-
  assertz(effect(dropall,all(o,not(holding(o))))).
init_clearall_action :-
  assertz(effect(clearall,all(o,clear(o)))).
init_typed_clearall_action :-
  assertz(subtype_of_type(T,T)),
  assertz(subtype_of_type(cup,object)),
  assertz(effect(clearall,all(o,object,clear(o)))).
init_condeffect_action :-
  assertz(effect(drop(O),impl(fragile(O),broken(O)))).
cleanup_actions :-
  retractall(effect(_,_)).
cleanup_actions_and_types :-
  cleanup_actions,
  retractall(type_of_object(_,_)).

test(regress_empty_action_list) :-
  regress([], a, a).

test(
  regress_simple_goto,
  [setup(init_goto_action),cleanup(cleanup_actions)]
) :-
  regress([goto(hall,kitchen)], at(kitchen), true),
  regress([goto(hall,kitchen)], not(at(hall)), true).

test(
  regress_forall,
  [setup(init_clearall_action),cleanup(cleanup_actions)]
) :-
  regress([clearall], clear(a), true),
  regress([clearall], not(clear(a)), false),
  regress([clearall], other_predicate(a), other_predicate(a)).

test(
  regress_forall_with_negation,
  [setup(init_dropall_action),cleanup(cleanup_actions)]
) :-
  regress([dropall], not(holding(a)), true),
  regress([dropall], holding(a), false),
  regress([dropall], other_predicate(a), other_predicate(a)).

test(
  regress_conditional_effect,
  [setup(init_condeffect_action), cleanup(cleanup_actions)]
) :-
  regress([drop(o)], broken(o), or(fragile(o),broken(o))),
  regress([drop(o)], not(broken(o)), and(not(fragile(o)),not(broken(o)))).

test(
  regress_implication,
  [setup(init_goto_action),cleanup(cleanup_actions)]
) :-
  regress([goto(hall,kitchen)], impl(true,at(kitchen)), true).

test(
  regress_existential_quantifier,
  [ nondet,
    setup(init_goto_action),
    cleanup(cleanup_actions_and_types)]
) :-
  regress([goto(hall,kitchen)], some(l,location,at(l)), true).

test(
  regress_universal_quantifier,
  [setup(init_clearall_action),cleanup(cleanup_actions)]
) :-
  regress([clearall], all(c,clear(c)), true).

test(
  regress_universal_quantifier_with_types,
  [setup(init_typed_clearall_action),cleanup(cleanup_actions_and_types)]
) :-
  regress([clearall], all(c,object,clear(c)), true).

tet(
  regress_universal_quantifier_with_subtypes,
  [setup(init_typed_clearall_action),cleanup(cleanup_actions_and_types)]
) :-
  regress([clearall], all(c,cup,clear(c)), true).

:- end_tests(regression).

:- begin_tests(simplify).
:- use_module(simplify).

test(simplify_conjunction) :-
  assertion(simplify(and(true,true),true)),
  assertion(simplify(and(a,true),a)),
  assertion(simplify(and(true,b),b)),
  assertion(simplify(and(true,false),false)),
  assertion(simplify(and(false,true),false)).

test(simplify_disjunction) :-
  assertion(simplify(or(true,true),true)),
  assertion(simplify(or(a,true),true)),
  assertion(simplify(or(true,b),true)),
  assertion(simplify(or(true,false),true)),
  assertion(simplify(or(false,true),true)).

test(simplify_nested_disjunction) :-
  assertion(simplify(or(or(true,a),b),true)),
  assertion(simplify(or(or(or(true,a),b),c),true)),
  assertion(simplify(or(or(or(or(a,true),b),c),d),true)).

test(simplify_nested_conjunction) :-
  assertion(simplify(and(and(true,a),b),and(a,b))),
  assertion(simplify(and(and(and(true,a),b),c),and(a,b,c))),
  assertion(simplify(and(and(and(and(a,true),b),c),d),and(a,b,c,d))).

test(simplify_negated_literal_in_conjunction) :-
  assertion(simplify(and(a,not(a)),false)),
  assertion(simplify(and(not(a),not(not(a))),false)),
  assertion(simplify(and(a,or(not(a),b)),and(a,b))),
  assertion(simplify(and(c,or(not(a),b),a),and(c,b,a))),
  assertion(simplify(and(c,or(d,not(a),b),a),and(c,or(d,b),a))),
  assertion(simplify(and(not(a),or(a,b)),and(not(a),b))).

test(simplify_remove_negated_literal_in_disjunction) :-
  assertion(simplify(and(or(a,b),or(not(a),b)), b)),
  assertion(simplify(and(or(a,b,not(c)),or(a,c,b)),or(a,b))),
  assertion(simplify(and(or(d,c,not(b),a),or(a,b,c,d)),or(a,c,d))).

test(simplify_remove_subset_conjunctions_in_disjunction) :-
  assertion(simplify(or(and(a,b),and(a,b,c)),and(a,b))),
  assertion(simplify(or(a,and(a,b)),a)).
test(simplify_remove_negated_conjunct_in_disjunction) :-
  assertion(simplify(or(a,and(not(a),b)),or(a,b))).
test(simplify_implication) :-
  assertion(simplify(impl(true,a),a)),
  assertion(simplify(and(impl(not(a),b),impl(a,b)), b)).


:- end_tests(simplify).

:- initialization run_and_exit.

run_and_exit :-
  run_tests,
  halt(0).
run_and_exit :-
  halt(1).
