#! /usr/bin/env swipl

/**
 *  effects.pl - Compute effects of action sequences
 *
 *  Created:  Thu 08 Dec 2016 11:04:36 AM CET
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

:- module(effects, [compute_effect/2]).

:- use_module(regression).
:- use_module(library(lambda)).

%% compute_effect(*Actions, -Effect)
%
%  Compute the effect of the given list of Actions. Actions are expected to be
%  given as pairs of (ActionName,ParameterAssignment), where ParameterAssignment
%  is a list of pairs of reassigned parameters, e.g. [(a,b)] denotes that
%  parameter a was renamed to parameter b.
compute_effect(Actions, Effect) :-
  add_action_effects(Actions, Effects),
  merge_effects(Effects, Effect).

%% merge_effects(+Effects, -Effect)
%
%  Merge the given list of effects into a single effect which is the conjunct of
%  all effects.
%  TODO add some simplification of the resulting effect
merge_effects([Effect], Effect) :- !.
merge_effects(Effects, Effect) :-
  Effect =.. [and|Effects].

%% add_action_effects(*Actions, -Effects)
%
%  Compute a list of effects of the given action sequence. The computed Effects
%  is a list of effects that is understood as a conjunction of the list
%  elements. Actions are expected to be given as pairs of
%  (ActionName,ParameterAssignment), where ParameterAssignment is a list of
%  pairs reassigned parameters, e.g. [(a,b)] denotes that parameter a was
%  renamed to parameter b.
add_action_effects([(Action,ParameterAssignment)], Effects) :-
  domain:action_effect(Action, Effect),
  reassign_parameters(Effect, ParameterAssignment, ReassignedEffect),
  split_effect(ReassignedEffect, Effects).
add_action_effects([(Action,ParameterAssignment)|RActions], ResultingEffects) :-
  add_action_effects(RActions, RestEffects),
  domain:action_effect(Action, ActionEffect),
  reassign_parameters(ActionEffect, ParameterAssignment, ReassignedEffect),
  domain:action_parameters(Action, ParameterTypes),
  reassign_types(ParameterTypes, ParameterAssignment, ReassignedParameterTypes),
  split_effect(ReassignedEffect, ActionEffects),
  exclude(effect_collision(RestEffects, ReassignedParameterTypes),
          ActionEffects,
          FilteredActionEffects),
  append(FilteredActionEffects, RestEffects, ResultingEffects).

%% reassign_parameters(+Formula, +Assignment, -AssignedFormula)
%
%  Apply the parameter assignment Assignment on the Formula, resulting in
%  AssignedFormula.
reassign_parameters(Formula, [], Formula).
reassign_parameters(Formula, [(OldParam, NewParam)|Params], NewFormula) :-
  substitute(OldParam, [Formula], NewParam, [SubstitutedFormula]),
  reassign_parameters(SubstitutedFormula, Params, NewFormula).

%% reassign_types(+Types, +Assignment, -AssignedTypes)
%
%  Apply the parameter assignment Assignment on the list of Types, resulting in
%  AssignedTypes. This expects Types to be a list of types, where each type is a
%  pair of a type name and a list of variables of that type, e.g.
%  [(block,[a,b,c]),(table,[t1,t2])]
reassign_types([], _, []).
reassign_types([Type|Types], Assignment, [NewType|NewTypes]) :-
  reassign_type(Type, Assignment, NewType),
  reassign_types(Types, Assignment, NewTypes).
reassign_type((Type, Vars), [], (Type, Vars)).
reassign_type((Type, Vars), [(OldParam,NewParam)|Params], (Type, NewVars)) :-
  substitute(OldParam, Vars, NewParam, SubstitutedVars),
  reassign_type((Type, SubstitutedVars), Params, (Type, NewVars)).

%% split_effect(+Effect, -Effects)
%
%  Split the given Effect into a list of atomic Effects.
split_effect(Effect, [Effect]) :- atomic_formula(Effect).
split_effect(Effect, FlattenedSplitConjuncts) :-
  Effect =.. [and|Conjuncts],
  maplist(split_effect,Conjuncts,SplitConjuncts),
  flatten(SplitConjuncts, FlattenedSplitConjuncts).
split_effect(Effect, Effects) :-
  Effect =.. [all,Var,Type,QuantifiedEffect],
  split_effect(QuantifiedEffect, QuantifiedEffects),
  maplist(\X^(=([all,Var,Type,X])),QuantifiedEffects,EffectUnivs),
  maplist(=..(), Effects, EffectUnivs).

%% atomic_formula(+A)
%
%  True if A is an atomic formula.
atomic_formula(A) :- atom(A), !.
atomic_formula(F) :-
  F =.. [Op|Args],
  Args \= [],
  \+ member(Op, [all,some,or,and]),
  maplist(atomic_formula, Args).

%% effect_collision(+Effects, +ParameterTypes, +Formula)
%
%  True if Formula collides with one of the effects in Effects. Colliding means
%  that the effect implies either the formula or its negation.
effect_collision(Effects, _, Formula) :- member(Formula, Effects).
effect_collision(Effects, _, Formula) :- 
  member(Effect, Effects),
  negated_formula(Effect, Formula).
effect_collision(Effects, ParameterTypes, Formula) :-
  member(all(Var,Type,QuantifiedEffect),Effects),
  member((TypeInFormula, Params), ParameterTypes),
  domain:subtype_of_type(TypeInFormula, Type),
  member(Param, Params),
  substitute(Var, [QuantifiedEffect], Param, [SubstitutedEffect]),
  effect_collision([SubstitutedEffect], ParameterTypes, Formula).
effect_collision(Effects,  ParameterTypes, Formula) :-
  member(all(_,_,QuantifiedEffect),Effects),
  effect_collision([QuantifiedEffect], ParameterTypes, Formula).

%% negated_formula(?F1, ?F2)
%
%  True if F1 is the negation of F2.
negated_formula(F,not(F)).
negated_formula(not(F),F).

:- begin_tests(split_effects).
test(atomic) :-
  assertion(split_effect(a,[a])).
test(negated) :-
  assertion(split_effect(not(a),[not(a)])).
test(conjunction) :-
  assertion(split_effect(and(a,b),[a,b])).
test(nested_conjunction) :-
  assertion(split_effect(and(a,and(b,c),d),[a,b,c,d])).
:- end_tests(split_effects).

:- begin_tests(effect_collision).
test(simple) :-
  assertion(effect_collision([a], [(block,[a])], a)),
  assertion(\+ effect_collision([a],[(block,[b])], b)).
test(negated) :-
  assertion(effect_collision([not(a)], [(block,[a])], a)),
  assertion(effect_collision([a], [(block,[a])], not(a))).
test(quantified) :-
  assertion(effect_collision([all(b,block,clean(b))], [(block,[a])], clean(a))),
  assertion(effect_collision([all(b,block,clean(b))], [(block,[a])],
    not(clean(a)))).
test(nested_quantified) :-
  assertion(effect_collision([all(b,block,all(c,block,clean(b)))],
    [(block,[a])], clean(a))),
  assertion(effect_collision([all(b,block,all(c,table,clean(b)))],
    [(block,[a])], clean(a))),
  assertion(effect_collision([all(b,block,all(c,block,clean(c)))],
    [(block,[a])], clean(a))),
  assertion(effect_collision([all(b,table,all(c,block,clean(c)))],
    [(block,[a])], clean(a))),
  assertion(\+ effect_collision([all(b,block,all(c,block,clean(c)))],
    [(table,[d])], clean(d))),
  assertion(\+ effect_collision([all(b,table,all(c,block,clean(c)))],
    [(table,[d])], clean(d))),
  assertion(effect_collision([all(t,table,all(b,block,on(b,t)))],
    [(block,[a]),(table,[t1])], on(a,t1))).
:- end_tests(effect_collision).

:- begin_tests(action_effects).
test(
  single_action,
  [setup(assertz(domain:action_effect(pick_up(b),holding(b))))]
) :-
  assertion(add_action_effects([(pick_up(b),[])],[holding(b)])).
test(
  simple_reassignment,
  [setup(maplist(call,
    [assertz(domain:action_effect(drop,not(holding(b)))),
     assertz(domain:action_parameters(drop,[(block,[b])]))])),
   cleanup(maplist(call,
    [retractall(domain:action_effect(_,_)),
     retractall(domain:action_parameters(_,_))]))
  ]
) :-
  assertion(add_action_effects([(drop,[(b,a)])],[not(holding(a))])).
test(
  forall_effect,
  [setup(maplist(call,
    [assertz(domain:action_effect(dropall,all(b,block,not(holding(b))))),
     assertz(domain:action_parameters(drop,[])),
     assertz(domain:action_effect(pickup,holding(b))),
     assertz(domain:action_parameters(pickup,[(block,[b])]))
    ])),
   cleanup(maplist(call,
    [retractall(domain:action_effect(_,_)),
     retractall(domain:action_parameters(_,_))]))
  ]
) :-
  assertion(add_action_effects(
    [(pickup,[(b,c)]),(dropall,[])],[all(b,block,not(holding(b)))])).
:- end_tests(action_effects).
