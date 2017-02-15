#! /usr/bin/env swipl

/**
 *  regression.pl - ADL regression
 *
 *  Created:  Wed 19 Oct 2016 17:44:44 CEST
 *  Copyright  2016, 2017  Till Hofmann <hofmann@kbsg.rwth-aachen.de>
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

:- module(regression, [regress/4]).
:- use_module(library(apply)).
:- use_module(library(lambda)).
:- use_module(simplify).
:- use_module(substitute).

:- dynamic domain:subtype_of_type/2.

%% regress(+Effects, +Types, +Condition, -RegressedCondition)
%
%  Regresses the formula Condition with Effects, giving RegressedCondition.
%  Compares Condition with the effects of all actions. Any term that is an
%  effect of one of the actions is removed from Condition.
%  Types must define the types of all parameters used in Effects. Types is a
%  list of pairs of the form (TypeName, ListOfParameters).
%  The resulting Condition is simplified.
regress(Effects, Types, Cond, SimplifiedRegressedCond) :-
  once(regress_(Effects, Types, Cond, RegressedCond)),
  simplify(RegressedCond, SimplifiedRegressedCond).

%% regress_(+Effects, +Types, +Condition, -RegressedCondition)
%
%  Helper predicate for regress/4. This predicate does the actual regression
%  without simplification of the resulting term.

regress_([], _, Cond, Cond) :- !.
regress_(Effects, Types, Cond, SimplifiedCondRes) :-
  Cond =.. [Op|Conjuncts],
  member(Op,[and,or]),
  maplist(regress_(Effects, Types),Conjuncts,RegressedConjuncts),
  CondRes =.. [Op|RegressedConjuncts],
  simplify(CondRes, SimplifiedCondRes).
regress_(Effects, Types, Cond, CondRes) :-
  Cond =.. [imply,Implicant,Implicate],
  !,
  regress_(Effects, Types, or(not(Implicant),Implicate), CondRes).
% TODO this expects exactly one var, but PDDL allows lists of vars
% also rename the operator
regress_(Effects, Types, exists([],Cond), CondRes) :-
  regress_(Effects, Types, Cond, CondRes).
regress_(Effects, Types, exists([(_,[])|Vars], Cond), CondRes) :-
  !,
  regress_(Effects, Types, exists(Vars, Cond), CondRes).
regress_(
  Effects, Types, exists([(Type,TypedVars)|Vars],Cond), SimplifiedCondRes
) :-
  findall(C,
  ( member((ParamType, TypedParams), Types),
    ( Type = ParamType ; domain:subtype_of_type(ParamType, Type) ),
    member(Param, TypedParams),
    member(Var, TypedVars),
    substitute(Var, [Cond], Param, [QuantifiedCond]),
    exclude(=(Var), TypedVars, RestTypedVars),
    regress_(Effects, Types, exists([(Type,RestTypedVars)|Vars],QuantifiedCond),
      C)
  ), CondBag),
  CondRes =.. [or,exists([(Type,TypedVars)|Vars],Cond)|CondBag],
  simplify(CondRes, SimplifiedCondRes).

regress_([Effect|R], Types, Term, TermRes) :-
  Effect =.. [and|Conjuncts],
  append(Conjuncts, R, Effects),
  regress_(Effects, Types, Term, TermRes).
regress_([Term|_], _, Term, true).
regress_(Effects, Types, not(Term), not(TermRes)) :-
  regress_(Effects, Types, Term, TermRes).
regress_([not(Term)|_], _, Term, false).
regress_([Effect|_], _, Term, TermRes) :-
  Effect =.. [Predicate|EffectArgs],
  Term =.. [Predicate|TermArgs],
  \+ member(Predicate, [and,or,all,imply,when]),
  maplist(\EffectArg^TermArg^(=(EffectArg = TermArg)),
    EffectArgs, TermArgs, Equations),
    ( length(Equations, 1) -> [Constraint] = Equations ;
      Constraint =.. [and|Equations]
    ),
  TermRes = or(Constraint,and(not(Constraint),Term)).

regress_(Effects, Types, not(Term), true) :-
  regress_(Effects, Types, Term, false).
regress_(Effects, Types, not(Term), false) :-
  regress_(Effects, Types, Term, true).
regress_([all([],Effect)|R], Types, Term, TermRes) :-
  member(TermRes, [true,false]),
  regress_([Effect|R], Types, Term, TermRes).
regress_([all([(_,[])|VarList],Effect)|R], Types, Term, TermRes) :-
  regress_([all(VarList,Effect)|R], Types, Term, TermRes).
regress_([all([(VarType,[Var|Vars])|VarList],Effect)|R], Types, Term, TermRes) :-
  member((ParamType, TypedParams), Types),
  ( ParamType = VarType ; domain:subtype_of_type(ParamType, VarType) ),
  member(Param, TypedParams),
  substitute(Var, [Effect], Param, [QuantifiedEffect]),
  member(TermRes, [true,false]),
  regress_(
    [all([(VarType,Vars)|VarList],QuantifiedEffect)|R], Types, Term, TermRes
  ).

regress_(Effects, Types, all([],QuantifiedTerm), TermRes) :-
  regress_(Effects, Types, QuantifiedTerm, TermRes).
regress_(Effects, Types, all([(_,[])|Vars],QuantifiedTerm), TermRes) :-
  regress_(Effects, Types, all(Vars,QuantifiedTerm), TermRes).
regress_(Effects, Types, all(Vars,Term), TermRes) :-
  append(Vars, Types, NewTypes),
  member(TermRes, [true,false]),
  regress_(Effects, NewTypes, Term, TermRes).

regress_([when(Cond,Effect)|Effects], Types, Term, TermRes) :-
  % cut here because we don't want to skip the cond effect if regression fails
  !,
  regress_([Effect|Effects], Types, Term, TermResIfCond),
  regress_(Effects, Types, Cond, CondRes),
  regress_(Effects, Types, not(Cond), NegCondRes),
  regress_(Effects, Types, Term, TermResIfNotCond),
  !,
  TermRes = or(and(CondRes,TermResIfCond),and(NegCondRes,TermResIfNotCond)).

regress_([_|R], Types, Term, TermRes) :-
  regress_(R, Types, Term, TermRes).
regress_([_|R], [], Term, TermRes) :-
  regress_(R, [], Term, TermRes).

%% regress_on_actions(+Actions, +Types, +Cond, -RegressedCond).
%
%  Regress the Cond with the effects of all Actions. This expects
%  domain:action_effect/2 to be defined for each action in Actions.
%  Types defines the types of all parameters used in the effects of all Actions.
%  Types must be a list of pairs of the form (TypeName, ParameterList).
regress_on_actions([], _, Cond, Cond).
regress_on_actions([Action|Actions], Types, Cond, CondRes) :-
  domain:action_effect(Action, Effect),
  regress([Effect], Types, Cond, CondWithFirstAction),
  regress_on_actions(Actions, CondWithFirstAction, CondRes).

%% regress_on_actions(+Actions, +Cond, -RegressedCond).
%
%  Regress the Cond with the effects of all Actions. This expects
%  domain:action_effect/2 to be defined for each action in Actions.
%  This is the same as regress_on_actions/4 with an empty type list. Note that
%  the resulting term may be incorrect for any terms or actions that contain
%  quantifiers, because of the incomplete type list.
regress_on_actions(Actions, Cond, CondRes) :-
  regress_on_actions(Actions, [], Cond, CondRes).

:- begin_tests(regression).

init_location_types :-
  assertz(domain:type_of_object(room, kitchen)),
  %assertz(domain:type_of_object(location, kitchen)),
  assertz(domain:subtype_of_type(room, location)),
  assertz(domain:type_of_object(location, hall)).

init_goto_action :-
  assertz(domain:action_effect(goto(L1,L2),and(not(at(L1)),at(L2)))),
  init_location_types.
init_dropall_action :-
  assertz(domain:action_effect(dropall,all([(object,[o])],not(holding(o))))).
init_typed_clearall_action :-
  assertz(domain:subtype_of_type(T,T)),
  assertz(domain:subtype_of_type(cup,object)),
  assertz(domain:action_effect(clearall,all([(object,[o])],clear(o)))).
init_condeffect_action :-
  assertz(domain:action_effect(drop(O),when(fragile(O),broken(O)))).
init_fix_action :-
  assertz(domain:action_effect(fix_green(C),when(green(C),fixed(C)))),
  assertz(domain:action_effect(fix_other(C),when(not(green(C)),fixed(C)))).
cleanup_actions :-
  retractall(domain:action_effect(_,_)),
  retractall(type_of_object(_,_)),
  retractall(domain:subtype_of_type(_,_)).
cleanup_actions_and_types :- cleanup_actions.

test(regress_empty_action_list) :-
  regress_on_actions([], a, a).

test(
  regress_simple_goto,
  [setup(init_goto_action),cleanup(cleanup_actions)]
) :-
  regress_on_actions([goto(hall,kitchen)], at(kitchen), true),
  regress_on_actions([goto(hall,kitchen)], not(at(hall)), true).

test(
  regress_action_sequence,
  [setup(init_goto_action),cleanup(cleanup_actions)]
) :-
  regress_on_actions(
    [goto(hall,kitchen),goto(kitchen,office)], at(office), true
  ).

test(
  regress_forall,
  [setup(init_typed_clearall_action),cleanup(cleanup_actions)]
) :-
  assertion(regress_on_actions([clearall], [(object,[a])], clear(a), true)),
  assertion(
    regress_on_actions([clearall], [(object,[a])], not(clear(a)), false)),
  assertion(regress_on_actions(
    [clearall], [(object,[a])], other_predicate(a), other_predicate(a))
  ).

test(
  regress_forall_on_subtypes,
  [setup(init_typed_clearall_action),cleanup(cleanup_actions)]
) :-
  regress_on_actions([clearall], [(cup,[a])], clear(a), true),
  regress_on_actions([clearall], [(cup,[a])], not(clear(a)), false),
  regress_on_actions(
    [clearall], [(cup,[a])], other_predicate(a), other_predicate(a)
  ).

test(
  regress_forall_with_negation,
  [setup(init_dropall_action),cleanup(cleanup_actions)]
) :-
  regress_on_actions([dropall], [(object,[a])], not(holding(a)), true),
  regress_on_actions([dropall], [(object,[a])], holding(a), false),
  regress_on_actions(
    [dropall], [(object,[a])], other_predicate(a), other_predicate(a)
  ).

test(
  regress_conditional_effect,
  [setup(init_condeffect_action), cleanup(cleanup_actions)]
) :-
  regress_on_actions([drop(o)], broken(o), R1),
  assertion(R1=or(fragile(o),broken(o))),
  regress_on_actions(
    [drop(o)], not(broken(o)), R2
  ),
  assertion(R2=and(not(fragile(o)),not(broken(o)))).

test(
  regress_conditional_effect_with_two_cases,
  [setup(init_fix_action),cleanup(cleanup_actions)]
) :-
  regress_on_actions([fix_green(c),fix_other(c)], fixed(c), true).

test(
  regress_implication,
  [setup(init_goto_action),cleanup(cleanup_actions)]
) :-
  regress_on_actions([goto(hall,kitchen)], imply(true,at(kitchen)), true).

test(
  regress_existential_quantifier,
  [ nondet,
    setup(init_goto_action),
    cleanup(cleanup_actions_and_types)]
) :-
  assertion(regress_on_actions(
    [goto(hall,kitchen)], [(location, [kitchen])],
    exists([(location,[l])],at(l)), true)
  ).

test(
  regress_universal_quantifier_with_types,
  [setup(init_typed_clearall_action),cleanup(cleanup_actions_and_types)]
) :-
  regress_on_actions([clearall], all([(object,[c])],clear(c)), R),
  assertion(R=true).

test(
  regress_universal_quantifier_with_subtypes,
  [setup(init_typed_clearall_action),cleanup(cleanup_actions_and_types)]
) :-
  regress_on_actions([clearall], all([(cup,[c])],clear(c)), R),
  assertion(R=true).

test(regress_unrelated_term) :-
  regress([holding(x)], [[]], at(l), R),
  assertion(R=at(l)).

test(regress_forall_with_multiple_parameters) :-
  regress([all([(object,[o])],p(o))], [(object,[a,b])], p(b), R),
  assertion(R=true).

test(regress_complementary_forall) :-
  regress([all([(object,[o])],p(o))], [], all([(object,[o])],not(p(o))),R),
  assertion(R=false).

test(regress_forall_against_single_fact) :-
  regress([p(o)], [(object,[o])], all(o,object,p(o)), R),
  assertion(R=all(o,object,p(o))).

test(regress_exists_with_multiple_parameters) :-
  regress([p(b)], [(object,[a,b])], exists([(object,[o])],p(o)), R),
  assertion(R=true).

test(regress_exists_with_alternatives, fixme(simplification)) :-
  regress(
    [p(a),q(b)],
    [(object,[a,b])],
    exists([(object,[o])], and(p(o),q(o))),
    R),
  R=or(exists([(object,[o])],and(p(o),q(o))),q(a),p(b),a=b).

test(regress_forall_with_conjunctions) :-
  regress(
    [all([(object,[o])],p(o)),all([(object,[o])],q(o))],
    [(object,[a]), (object,[a])],
    and(p(a),q(a)),
    R),
  assertion(R=true).

test(regress_forall_with_var_lists) :-
  regress(
    [all([(t1,[o1,o2]),(t2,[o3,o4])], p(o1,o2,o3,o4))],
    [(t1,[a,b]),(t2,[c,d])],
    p(a,b,c,d),
    R1),
  assertion(R1=true),
  regress(
    [all([(t2,[o4,o3]),(t1,[o1,o2])], p(o1,o2,o3,o4))],
    [(t1,[a,b]),(t2,[c,d])],
    p(a,b,c,d),
    R2),
  assertion(R2=true),
  regress(
    [all([(t1,[o1,o2]),(t2,[o3])], p(o1,o2,o3,o3))],
    [(t1,[a,b]),(t2,[c,d])],
    p(a,b,c,d),
    R3),
  assertion(R3=p(a,b,c,d)),
  regress(
    [all([(t1,[o1,o2]),(t2,[o3,o4])], p(o1,o2,o3,o4))],
    [(t1,[a,b]),(t2,[c,d])],
    not(p(a,b,c,d)),
    R4),
  assertion(R4=false).
test(regress_forall_with_var_lists_in_term) :-
  regress(
    [all([(t1,[o1,o2]),(t2,[o3,o4])], p(o1,o2,o3,o4))],
    [(t1,[a,b]),(t2,[c,d])],
    all([(t1,[a,b]),(t2,[c,d])],p(a,b,c,d)),
    R1),
  assertion(R1=true),
  regress(
    [all([(t1,[o1,o2]),(t2,[o3,o4])], p(o1,o2,o3,o4))],
    [(t1,[a,b]),(t2,[c,d])],
    all([(t2,[d,c]),(t1,[a,b])],p(a,b,c,d)),
    R2),
  assertion(R2=true),
  regress(
    [all([(t1,[o1,o2]),(t2,[o3,o4])], p(o1,o2,o3,o4))],
    [(t1,[a,b]),(t2,[c,d])],
    all([(t1,[a,b]),(t2,[c,d])],not(p(a,b,c,d))),
    R3),
  assertion(R3=false),
  regress(
    [all([(t1,[o1,o2]),(t2,[o3,o4])], p(o1,o2,o3,o4))],
    [(t1,[a,b]),(t2,[c,d])],
    not(all([(t1,[a,b]),(t2,[c,d])],p(a,b,c,d))),
    R4),
  assertion(R4=false).

:- end_tests(regression).
