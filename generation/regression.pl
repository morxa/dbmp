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

:- use_module(library(apply)).

:- use_module(simplify).

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


%% substitute(+Old, +Terms, +New, -NewTerms)
%
%  Replace all occurrences of Old in Terms by New, giving NewTerms.
%  This replaces terms recursively, i.e.
%  substitute(a, [p(a)], b, L) gives L = p(b).
substitute(Old, Terms, New, NewTerms) :-
  substitute(Old, Terms, New, true, NewTerms).


%% substitute(+Old, +Terms, +New, +Constraint, -NewTerms)
%
%  Replace all occurrences of Old in Terms by New if Constraint is true for the
%  term to be substituted, giving NewTerms.
%  This calls Constraint(Term) for each term that is a candidate to be
%  substituted. Constraint must be callable, i.e. sufficiently instantiated.
%  As an example, Constraint can be used to restrict substitution to a certain
%  type of variable. However, any other constraint can be used.
substitute(_, [], _, _, []) :- !.
substitute(Old, [Term|Terms], New, Constraint, [New|NewTerms]) :-
  Old = Term,
  call(Constraint, Term),
  % Cut here, otherwise the non-substituted list will succeed, too.
  !,
  substitute(Old, Terms, New, NewTerms).
substitute(Old, [Term|Terms], New, Constraint, [NewTerm|NewTerms]) :-
  \+ atom(Term),
  Term =.. Subterms,
  substitute(Old, Subterms, New, Constraint, NewSubterms),
  !,
  NewTerm =.. NewSubterms,
  substitute(Old, Terms, New, Constraint, NewTerms).
substitute(Old, [Term|Terms], New, Constraint, [Term|NewTerms]) :-
  substitute(Old, Terms, New, Constraint, NewTerms).

%% true(?Any)
%  Auxiliary predicate that is always true.
true(_).
