#! /usr/bin/env swipl

/**
 *  regression.pl - ADL regression
 *
 *  Created:  Wed 19 Oct 2016 17:44:44 CEST
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

:- module(regression, [regress/3]).
:- use_module(library(apply)).
:- use_module(simplify).
:- use_module(substitute).

%% regress(+ActionList, +Condition, -RegressedCondition)
%
%  Regresses the formula Condition with ActionList, giving RegressedCondition.
%  For each action, the action's effect must be declared with
%  domain:action_effect(Action,Effect).
%  Compares Condition with the effects of all actions. Any term that is an
%  effect of one of the actions is removed from Condition.
regress([], Cond, Cond) :- !.
regress(Actions, Cond, SimplifiedCondRes) :-
  Cond =.. [Op|Conjuncts],
  member(Op,[and,or]),
  maplist(regress(Actions),Conjuncts,RegressedConjuncts),
  CondRes =.. [Op|RegressedConjuncts],
  simplify(CondRes, SimplifiedCondRes).
regress(Actions, Cond, CondRes) :-
  Cond =.. [impl,Implicant,Implicate],
  !,
  once(regress(Actions, or(not(Implicant),Implicate), CondRes)).
% TODO this expects exactly one var, but PDDL allows lists of vars
% also rename the operator
regress(Actions, some(Var,Type,Cond), CondRes) :-
  !,
  domain:type_of_object(Type, TypedObject),
  substitute(Var, [Cond], TypedObject, [SubstitutedCond]),
  regress(Actions, SubstitutedCond, CondRes).
regress([Action|R], Cond, CondRes) :-
  domain:action_effect(Action,Effect),
  once(regress_on_effects(Cond,[Effect],CondInter)),
  once(regress(R,CondInter,CondRes)).

%% regress_on_effects(+Term, +EffectList, -RegressedTerm)
%
%  Regresses a single term Term with EffectList, giving RegressedTerm.
%  If Term occurs in the EffectList, RegressedTerm is true, similarly false for
%  negated Term/Effect.
%  This also considers effects of the form and(Effect1,Effect2) and
%  all(var,type,Effect).
regress_on_effects(Term, Effects, SimplifiedRegressedTerm) :-
  regress_on_effects_(Term, Effects, RegressedTerm),
  simplify(RegressedTerm, SimplifiedRegressedTerm).
regress_on_effects_(Term, [], Term).
regress_on_effects_(Term, [Effect|R], TermRes) :-
  Effect =.. [and|Conjuncts],
  append(Conjuncts, R, Effects),
  regress_on_effects(Term, Effects, TermRes).
regress_on_effects_(Term, [Term|_], true).
regress_on_effects_(not(Term), [Term|_], false).
regress_on_effects_(Term, [not(Term)|_], false).
regress_on_effects_(all(X,Term), [all(Y,Effect)|_], true) :-
  substitute(X, [Term], _, [NewTerm]),
  substitute(Y, [Effect], _,[NewEffect]),
  regress_on_effects(NewTerm, [NewEffect], true).
% Note: subtype_of_type must be reflexive.
regress_on_effects_(all(X,TermType,Term), [all(Y,EffectType,Effect)|R], Res) :-
  domain:subtype_of_type(TermType, EffectType),
  regress_on_effects(all(X,Term), [all(Y,Effect)|R], Res).
regress_on_effects_(Term, [all(X,Type,Effect)|R], TermRes) :-
  Effect =.. [Predicate|Args],
  substitute(X, Args, _, domain:type_of_object(Type), NArgs),
  QuantifiedEffect =.. [Predicate|NArgs],
  regress_on_effects(Term, [QuantifiedEffect|R], TermRes).
regress_on_effects_(Term, [all(X,Effect)|R], TermRes) :-
  Effect =.. [Predicate|Args],
  substitute(X, Args, _, NArgs),
  QuantifiedEffect =.. [Predicate|NArgs],
  regress_on_effects(Term, [QuantifiedEffect|R], TermRes).
% conditional effect: regress Term for both cases (Cond true/false). The
% resulting term is a disjunction of both cases.
% TODO rename operator to imply
regress_on_effects_(Term, [impl(Cond,Effect)|Effects], TermRes) :-
  % cut here because we don't want to skip the cond effect if regression fails
  !,
  regress_on_effects(Term, [Effect|Effects], TermResIfCond),
  regress_on_effects(Cond, Effects, CondRes),
  regress_on_effects(not(Cond), Effects, NegCondRes),
  regress_on_effects(Term, Effects, TermResIfNotCond),
  !,
  TermRes = or(and(CondRes,TermResIfCond),and(NegCondRes,TermResIfNotCond)).

regress_on_effects_(Term, [_|R], TermRes) :-
  regress_on_effects(Term, R, TermRes).


:- begin_tests(regression).

init_location_types :-
  assertz(domain:type_of_object(room, kitchen)),
  assertz(domain:type_of_object(location, kitchen)),
  assertz(domain:type_of_object(location, hall)).

init_goto_action :-
  assertz(domain:action_effect(goto(L1,L2),and(not(at(L1)),at(L2)))),
  init_location_types.
init_dropall_action :-
  assertz(domain:action_effect(dropall,all(o,not(holding(o))))).
init_clearall_action :-
  assertz(domain:action_effect(clearall,all(o,clear(o)))).
init_typed_clearall_action :-
  assertz(domain:subtype_of_type(T,T)),
  assertz(domain:subtype_of_type(cup,object)),
  assertz(domain:action_effect(clearall,all(o,object,clear(o)))).
init_condeffect_action :-
  assertz(domain:action_effect(drop(O),impl(fragile(O),broken(O)))).
init_fix_action :-
  assertz(domain:action_effect(fix_green(C),impl(green(C),fixed(C)))),
  assertz(domain:action_effect(fix_other(C),impl(not(green(C)),fixed(C)))).
cleanup_actions :-
  retractall(domain:action_effect(_,_)).
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
  regress_action_sequence,
  [setup(init_goto_action),cleanup(cleanup_actions)]
) :-
  regress([goto(hall,kitchen),goto(kitchen,office)], at(office), true).

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
  regress_conditional_effect_with_two_cases,
  [setup(init_fix_action),cleanup(cleanup_actions)]
) :-
  regress([fix_green(c),fix_other(c)], fixed(c), true).

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

test(
  regress_universal_quantifier_with_subtypes,
  [setup(init_typed_clearall_action),cleanup(cleanup_actions_and_types)]
) :-
  regress([clearall], all(c,cup,clear(c)), true).


:- end_tests(regression).
