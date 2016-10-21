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
  assertz(effect(goto(L1,L2),and(not(at(L1)),at(L2)))).
init_dropall_action :-
  assertz(effect(dropall,all(o,not(holding(o))))).
init_clearall_action :-
  assertz(effect(clearall,all(o,clear(o)))).
init_condeffect_action :-
  assertz(effect(drop(O),impl(fragile(O),broken(O)))).
cleanup_actions :-
  retractall(effect(_,_)).

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
  regress([drop(o)], broken(o), broken(o)),
  regress([drop(o)], not(broken(o)), and(not(fragile(o)),not(broken(o)))).

test(
  regress_implication,
  [setup(init_goto_action),cleanup(cleanup_actions)]
) :-
  regress([goto(hall,kitchen)], impl(true,at(kitchen)), or(not(true),true)).

:- end_tests(regression).

:- initialization run_and_exit.

run_and_exit :-
  run_tests,
  halt(0).
run_and_exit :-
  halt(1).
