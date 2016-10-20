#! /usr/bin/env swipl

/**
 *  strips.pl - Short description
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

%% regress(+ActionList, +Condition, -RegressedCondition)
%
%  Regresses the formula Condition with ActionList, giving RegressedCondition.
%  For each action, the action's effect must be declared with
%  effect(Action,Effect).
%  Compares Condition with the effects of all actions. Any term that is an
%  effect of one of the actions is removed from Condition.
regress([], Cond, Cond).
regress(Actions, Cond, CondRes) :-
  Cond =.. [and|Conjuncts],
  maplist(regress(Actions),Conjuncts,RegressedConjuncts), !,
  CondRes =.. [and|RegressedConjuncts].
regress([Action|R], Cond, CondRes) :-
  effect(Action,Effect),
  regress_on_effects(Cond,[Effect],CondInter), !,
  regress(R,CondInter,CondRes).

%% regress_on_effects(+Term, +EffectList, -RegressedTerm)
%
%  Regresses a single term Term with EffectList, giving RegressedTerm.
%  If Term occurs in the EffectList, RegressedTerm is true, similarly false for
%  negated Term/Effect.
%  This also considers effects of the form and(Effect1,Effect2) and
%  all(var,type,Effect).
regress_on_effects(Term, [], Term).
regress_on_effects(Term, [Effect|R], TermRes) :-
  Effect =.. [and|Conjuncts],
  append(Conjuncts, R, Effects),
  regress_on_effects(Term, Effects, TermRes).
regress_on_effects(Term, [Term|_], true) :- !.
regress_on_effects(not(Term), [Term|_], false) :- !.
regress_on_effects(Term, [not(Term)|_], false) :- !.
regress_on_effects(Term, [all(X,_,Effect)|R], TermRes) :-
  Effect =.. [Predicate|Args],
  substitute(X,Args,_,NArgs),
  QuantifiedEffect =.. [Predicate|NArgs],
  regress_on_effects(Term,[QuantifiedEffect|R], TermRes).
regress_on_effects(Term, [_|R], TermRes) :-
  regress_on_effects(Term, R, TermRes).


%% substitute(+OldElem, +OldList, +NewElem, -NewList)
%
%  Replace all occurrences of OldElem in OldList by NewElem, giving NewList.
%  This replaces terms recursively, i.e.
%  substitute(a, [p(a)], b, p(b)) succeeds.
substitute(_, [], _, []).
substitute(OldElem, [Old|OldList], NewElem, [NewElem|NewList]) :-
  Old == OldElem, !,
  substitute(OldElem, OldList, NewElem, NewList).
substitute(OldElem, [Old|OldList], NewElem, [New|NewList]) :-
  \+ atom(Old),
  Old =.. ElemList, !,
  substitute(OldElem, ElemList, NewElem, NewElemList),
  New =.. NewElemList,
  substitute(OldElem, OldList, NewElem, NewList), !.
substitute(OldElem, [E|OldList], NewElem, [E|NewList]) :-
  substitute(OldElem, OldList, NewElem, NewList), !.
