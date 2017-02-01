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

:- module(effects, [compute_effect/2, compute_effect/3]).

:- use_module(regression).
:- use_module(simplify).
:- use_module(substitute).
:- use_module(library(lambda)).

:- dynamic domain:subtype_of_type/2.

% Effect trees are only used if enable_effect_trees is true.
:- dynamic enable_effect_trees/0.
% Comment the following line to disable effect trees.
enable_effect_trees.

:- enable_effect_trees -> ensure_loaded(effect_tree) ; true.

%  compute_effect(*Actions, *ParameterAssignment, -Effect)
%
%  Compute the effect of the given list of Actions respecting the given
%  ParameterAssignment. This is the same as compute_effect/2, but with a
%  separate assignment.
compute_effect(Actions, ParameterAssignment, Effect) :-
  get_pair_representation(Actions, ParameterAssignment, ActionParameterPairs),
  compute_effect(ActionParameterPairs, Effect).

%% get_pair_representation(+Actions, +Assignments, -ActionsWithAssignments)
%
%  Compute a pair representation of the given Actions and Assignments. This is a
%  helper predicate for compute_effect/3.
get_pair_representation([], [], []).
get_pair_representation(
  [Action|Actions],
  [Assignment|Assignments],
  [(Action,Assignment)|ActionParameterPairs]
) :-
  get_pair_representation(Actions, Assignments, ActionParameterPairs).

%% compute_effect(*Actions, -Effect)
%
%  Compute the effect of the given list of Actions. Actions are expected to be
%  given as pairs of (ActionName,ParameterAssignment), where ParameterAssignment
%  is a list of pairs of reassigned parameters, e.g. [(a,b)] denotes that
%  parameter a was renamed to parameter b.
compute_effect(Actions, Effect) :-
  enable_effect_trees,
  maplist(get_reassigned_effect, Actions, Effects),
  regress_cond_effects(Effects, RegressedEffects),
  reverse(RegressedEffects, ReversedEffects),
  effect_tree(ReversedEffects, EffectTree),
  get_effect_from_tree(EffectTree, Effect).

compute_effect(Actions, Effect) :-
  \+ enable_effect_trees,
  add_action_effects(Actions, Effects),
  merge_effects(Effects, Effect).

%% get_reassigned_effect(+ParameterizedAction, -ReassignedEffect)
%
%  Get the effect for the given action with the given parameter assignment.
%  ParameterizedAction is expected to be a pair (Action, ParameterAssignment).
%  The result is a pair (Effect, Parameters).
get_reassigned_effect(
  (Action,ParameterAssignment),
  (ReassignedEffect, ReassignedParameters)
) :-
  domain:action_effect(Action, Effect),
  reassign_parameters(Effect, ParameterAssignment, ReassignedEffect),
  domain:action_parameters(Action, Parameters),
  reassign_types(Parameters, ParameterAssignment, ReassignedParameters).

%% regress_cond_effects(+Effects, -RegressedEffects)
%
%  Regress all conditions in Effects to the beginning of the sequence. This
%  expects the first effect in the list to be the first effect of the sequence.
%  Effects is expected to be a list of pairs of the form (Effect, Parameters).
%  The resulting RegressedEffects are a list of pairs of the same form, where
%  each condition of a conditional effect is substitued by the regressed
%  condition.
regress_cond_effects(Effects, RegressedEffects) :-
  regress_cond_effects([], [], Effects, RegressedEffects).

%% regress_cond_effects(
%    +PreviousEffects, +PreviousParams, +Effects, -RegressedEffects)
%
%  Helper predicate for regress_cond_effects/2. PreviousEffects are the effects
%  of the action sequence that occur before Effects, i.e., the effects that are
%  used for regression. Similarly, PreviousParams is a typed parameter list of
%  the parameters occurring in the actions previous to the current action.
regress_cond_effects(_, _, [], []).
regress_cond_effects(
  PreviousEffects,
  PreviousParameters,
  [(Effect,Parameters)|Effects],
  [(SimplifiedRegressedEffect,Parameters)|RegressedEffects]
) :-
  findall(Cond, contains_term(when(Cond, _), Effect), Conds),
  append(PreviousParameters, Parameters, CurrentParameters),
  maplist(regress(PreviousEffects, CurrentParameters), Conds, RegressedConds),
  maplist(\Old^New^(=((Old,New))), Conds, RegressedConds, Substitutions),
  substitute_list([Effect], Substitutions, [RegressedEffect]),
  simplify_effect(RegressedEffect, SimplifiedRegressedEffect),
  regress_cond_effects([SimplifiedRegressedEffect|PreviousEffects],
    CurrentParameters, Effects, RegressedEffects).


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
%  WARNING: THIS IS DEPRECATED. Please use effect trees instead.
%  In particular, conflicting forall-quantified effects are not resolved
%  correctly. See tests for an example.

add_action_effects([], []).
add_action_effects([(Action,ParameterAssignment)|RActions], ResultingEffects) :-
  add_action_effects(RActions, RestEffects),
  domain:action_effect(Action, ActionEffect),
  reassign_parameters(ActionEffect, ParameterAssignment, ReassignedEffect),
  domain:action_parameters(Action, ParameterTypes),
  reassign_types(ParameterTypes, ParameterAssignment, ReassignedParameterTypes),
  split_effect(ReassignedEffect, ActionEffects),
  check_for_cond_effects(ActionEffects),
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
split_effect(Effect, Effects) :-
  Effect =.. [all,TypedVars,QuantifiedEffect],
  split_effect(QuantifiedEffect, QuantifiedEffects),
  maplist(\X^(=([all,TypedVars,X])),QuantifiedEffects,EffectUnivs),
  maplist(=..(), Effects, EffectUnivs).
split_effect(Effect, Effects) :-
  Effect =.. [when,Cond,CondEffect],
  split_effect(CondEffect, CondEffects),
  maplist(\X^(=([when,Cond,X])), CondEffects, EffectUnivs),
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
  ( TypeInFormula = Type ; domain:subtype_of_type(TypeInFormula, Type)),
  member(Param, Params),
  substitute(Var, [QuantifiedEffect], Param, [SubstitutedEffect]),
  effect_collision([SubstitutedEffect], ParameterTypes, Formula).
% Note: This assumes that the names used in forall quantifiers are disjunct with
% the parameter names.
effect_collision(Effects,  ParameterTypes, Formula) :-
  member(all(_,_,QuantifiedEffect),Effects),
  effect_collision([QuantifiedEffect], ParameterTypes, Formula).
effect_collision(Effects, ParameterTypes, Formula) :-
  member(all(TypedVars,QuantifiedEffect), Effects),
  ground_formula(
    QuantifiedEffect, TypedVars, ParameterTypes, GroundedEffect
  ),
  effect_collision([GroundedEffect], ParameterTypes, Formula).
effect_collision(Effects, ParameterTypes, all(TypedVars, Formula)) :-
  append(ParameterTypes, TypedVars, NewParameterTypes),
  effect_collision(Effects, NewParameterTypes, Formula).
effect_collision(Effects, ParameterTypes, when(_, Formula)) :-
  effect_collision(Effects, ParameterTypes, Formula).
effect_collision(Effects, ParameterTypes, when(Condition, Formula)) :-
  member(when(Condition, Effect), Effects),
  effect_collision([Effect], ParameterTypes, Formula).

%% constrain_effect(+Effects, +ParameterTypes, +Formula, -RemainingEffect)
%
%  Same as effect_collision/3, but not all effects of Formula are obliterated by
%  Effects. RemainingEffect is the effect that remains of Formula after
%  computing all collisions with Effects.
constrain_effect(Effects, ParameterTypes, Formula, nil) :-
  effect_collision(Effects, ParameterTypes, Formula).
constrain_effect(
  Effects, ParameterTypes, all(TypedVars,Formula), RemainingEffect
) :-
  member((Type, Vars), TypedVars),
  member(Var, Vars),
  member((ParameterType, Parameters), ParameterTypes),
  ( Type = ParameterType ; domain:subtype_of_type(ParameterType, Type) ),
  member(Parameter, Parameters),
  exclude(=(Var), Vars, RemainingVars),
  exclude(=((Type, Vars)), TypedVars, RemainingTypedVars),
  substitute(Var, [Formula], Parameter, [SubstitutedFormula]),
  ( effect_collision(
      Effects, ParameterTypes,
      all([(Type, RemainingVars)|RemainingTypedVars], SubstitutedFormula)
    ) ->
    !, RemainingEffect = all(TypedVars,when(not(Var = Parameter),Formula))
  ;
    constrain_effect(
      Effects, ParameterTypes,
      all([(Type,RemainingVars)|RemainingTypedVars], SubstitutedFormula),
      RemainingSubEffect
    ),
    substitute(Parameter, [RemainingSubEffect], Var, [SubstitutedSubEffect]),
    RemainingEffect =
      all([(Type,[Var])],when(not(Var = Parameter),SubstitutedSubEffect))
  ).

constrain_effect(_, _, Effect, Effect).

%% ground_formula(+Formula, +TypedVars, +TypedParams, GroundedFormula)
%
%  Substitute all variables in TypedVars by an arbitrary parameter from
%  TypedParams, obtaining GroundedFormula.
ground_formula(Formula, [], _, Formula).
ground_formula(
  Formula, [(_,[])|TypedVars], TypedParameters, GroundedFormula
) :-
  ground_formula( Formula, TypedVars, TypedParameters, GroundedFormula).
ground_formula(
  Formula, [(Type,[Var|Vars])|TypedVars], TypedParameters, GroundedFormula
) :-
  member((TypeInFormula, Parameters), TypedParameters),
  ( TypeInFormula = Type ; domain:subtype_of_type(TypeInFormula, Type)),
  member(Parameter, Parameters),
  substitute(Var, [Formula], Parameter, [SubstitutedFormula]),
  ground_formula(
    SubstitutedFormula, [(Type,Vars)|TypedVars], TypedParameters,
    GroundedFormula
  ).


%% negated_formula(?F1, ?F2)
%
%  True if F1 is the negation of F2.
negated_formula(F,not(F)).
negated_formula(not(F),F).

%% check_for_cond_effects(-Effects)
%
%  We currently do not support conditional effects in macros. Thus, if Effects
%  contains a conditional effect, print an error message and fail.  Otherwise,
%  silently succeed.
check_for_cond_effects(Effects) :-
  \+ member(when(_,_), Effects), !.
check_for_cond_effects(Effects) :-
  member(when(_,_), Effects), !,
  format('Error: ~w contains a conditional effect. \c
         Conditional effects are currently not supported.', [Effects]),
  fail.



:- begin_tests(split_effects).
test(atomic) :-
  assertion(split_effect(a,[a])).
test(negated) :-
  assertion(split_effect(not(a),[not(a)])).
test(conjunction) :-
  assertion(split_effect(and(a,b),[a,b])).
test(nested_conjunction) :-
  assertion(split_effect(and(a,and(b,c),d),[a,b,c,d])).
test(conditional_effect) :-
  assertion(split_effect(when(cond,and(eff1,eff2)),
    [when(cond,eff1),when(cond,eff2)])).
test(quantified_effect) :-
  assertion(split_effect(all(var,type,and(e1,e2)),
    [all(var,type,e1),all(var,type,e2)])).
test(quantified_effect_with_typed_list) :-
  assertion(split_effect(
    all([(t1,[v1,v2]),(t2,[v3])],and(e1,e2)),
    [all([(t1,[v1,v2]),(t2,[v3])],e1),all([(t1,[v1,v2]),(t2,[v3])],e2)])).
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
test(conditional_effect_with_same_nonconditional_effect) :-
  assertion(effect_collision([effect], [], when(cond,effect))),
  assertion(\+ effect_collision([effect1], [], when(cond,effect2))).
test(two_conditional_effects) :-
  assertion(effect_collision([when(cond,effect)],[],when(cond,effect))),
  assertion(effect_collision(
    [when(cond,all(b,block,clean(b)))],[(block,[a])],when(cond,clean(a)))).
test(quantified_list_simple) :-
  assertion(effect_collision(
    [all([(block,[b])],clean(b))], [(block,[a])], clean(a))).
test(quantified_list_two_vars) :-
  assertion(effect_collision(
    [all([(table,[t]),(block,[b])],on(b,t))],
    [(block,[b1]),(table,[t1])],
    on(b1,t1))).
test(quantified_list_two_vars_conjunctive_effect) :-
  assertion(effect_collision(
    [all([(table,[t]),(block,[b])],clear(b))],
    [(block,[b1]),(table,[t1])],
    clear(b1))).
test(two_quantified_effects) :-
  assertion(effect_collision(
    [all([(obj,[o])],p(o))],
    [],
    all([(obj,[o])],p(o)))).
test(
  two_quantified_effects_with_subtypes,
  [ setup(assertz(domain:subtype_of_type(room,location))),
    cleanup(retractall(domain:subtype_of_type(_,_)))
  ]
  ) :-
  assertion(effect_collision(
    [all([(location,[l])],p(l))],
    [],
    all([(room,[r])],p(r)))
  ).
test(
  two_quantified_effects_different_var_count,
  [ setup(assertz(domain:subtype_of_type(room,location))),
    cleanup(retractall(domain:subtype_of_type(_,_)))
  ]
  ) :-
  assertion(effect_collision(
    [all([(location,[l1,l2])],p(l1,l2))],
    [],
    all([(room,[r])],p(r,r)))
  ).
test(
  two_quantified_effects_more_general_effect_does_not_collide,
  [ setup(assertz(domain:subtype_of_type(room,location))),
    cleanup(retractall(domain:subtype_of_type(_,_)))
  ]
  ) :-
  assertion(\+ effect_collision(
    [all([(room,[r])],p(r,r))],
    [],
    all([(location,[l1,l2])],p(l1,l2)))
  ).
test(nested_quantified_effects) :-
  assertion(effect_collision(
    [all([(obj,[o1])],all([(obj,[o2])],p(o1,o2)))],
    [],
    all([(obj,[o1,o2])],p(o1,o2)))
  ),
  assertion(effect_collision(
    [all([(obj,[o1])],all([(obj,[o2])],p(o1,o2)))],
    [],
    all([(obj,[o1])],p(o1,o1)))
  ),
  assertion(effect_collision(
    [all([(obj,[o1,o2])],p(o1,o2))],
    [],
    all([(obj,[o1])],all([(obj,[o2])],p(o1,o2))))
  ),
  assertion(\+ effect_collision(
    [all([(obj,[o1])],p(o1,o1))],
    [],
    [all([(obj,[o1])],all([(obj,[o2])],p(o1,o2)))])
  ).
test(simple_effect_constraint) :-
  assertion(constrain_effect(
    [p(a)], [(obj,[a])],
    all([(obj,[o])],not(p(o))),
    all([(obj,[o])],when(not(o=a),not(p(o)))))).
test(effect_constraint_with_two_vars) :-
  assertion(constrain_effect(
    [p(a,b)], [(obj,[a,b])],
    all([(obj,[o1,o2])],not(p(o1,o2))),
    all([(obj,[o1])],
      when(not(o1=a),all([(obj,[o2])],when(not(o2=b),not(p(o1,o2)))))))).
 test(effect_constraint_with_quantified_effect) :-
   assertion(constrain_effect(
    [all([(obj,[o])],p(a,o))],
    [(obj,[a])],
    all([(obj,[o1,o2])],not(p(o1,o2))),
    all([(obj,[o1,o2])],when(not(o1=a),not(p(o1,o2)))))).
:- end_tests(effect_collision).

:- begin_tests(merge_effects).
test(single_effect) :-
  assertion(merge_effects([a],a)),
  assertion(merge_effects([holding(a)],holding(a))).
test(two_effects) :-
  assertion(merge_effects([holding(a),holding(b)], and(holding(a),holding(b)))).
test(three_effects) :-
  assertion(merge_effects([e1,e2,e3], and(e1,e2,e3))).
:- end_tests(merge_effects).

:- begin_tests(action_effects).
test(
  single_action,
  [setup(maplist(call,
    [assertz(domain:action_effect(pick_up,holding(b))),
     assertz(domain:action_parameters(pick_up,[(block,[b])]))])),
   cleanup(maplist(call,
    [retractall(domain:action_effect(_,_)),
     retractall(domain:action_parameters(_,_))]))
  ]
) :-
  assertion(add_action_effects([(pick_up,[])],[holding(b)])).
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
     assertz(domain:action_parameters(dropall,[])),
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
test(
  action_repetition,
  [setup(maplist(call,
    [assertz(domain:action_effect(drop,not(holding(b)))),
     assertz(domain:action_effect(pickup,holding(b))),
     assertz(domain:action_parameters(drop,[(block,[b])])),
     assertz(domain:action_parameters(pickup,[(block,[b])]))
    ])),
   cleanup(maplist(call,
    [retractall(domain:action_effect(_,_)),
     retractall(domain:action_parameters(_,_))]))
  ]
) :-
  assertion(add_action_effects(
    [(pickup,[]),(drop,[]),(pickup,[])],
    [holding(b)])).
test(
  action_repetition_with_reasssignment,
  [setup(maplist(call,
    [assertz(domain:action_effect(drop,not(holding(b)))),
     assertz(domain:action_effect(pickup,holding(b))),
     assertz(domain:action_parameters(drop,[(block,[b])])),
     assertz(domain:action_parameters(pickup,[(block,[b])]))
    ])),
   cleanup(maplist(call,
    [retractall(domain:action_effect(_,_)),
     retractall(domain:action_parameters(_,_))]))
  ]
) :-
  assertion(add_action_effects(
    [(pickup,[]),(drop,[]),(pickup,[(b,a)]),(drop,[(b,a)]),(pickup,[(b,a)])],
    [not(holding(b)),holding(a)])).
test(
  action_forall_effect_with_collision,
  [fixme(collision_not_detected),
   setup(maplist(call,
    [assertz(domain:action_effect(dropall,all(b,block,not(holding(b))))),
     assertz(domain:action_effect(pickup,holding(b))),
     assertz(domain:action_parameters(dropall,[])),
     assertz(domain:action_parameters(pickup,[(block,[b1])]))
    ])),
   cleanup(maplist(call,
    [retractall(domain:action_effect(_,_)),
     retractall(domain:action_parameters(_,_))]))
  ]
) :-
  add_action_effects(
    [(dropall,[]),(pickup,[])],
    [all(b,block,when(not(b=b1),not(holding(b)))),holding(b1)]).
test(
  action_with_cond_effect_fails,
  [setup(maplist(call,
    [assertz(domain:action_effect(drop,when(holding(b),not(holding(b))))),
     assertz(domain:action_parameters(drop,[(block,[b])]))
    ])),
   cleanup(maplist(call,
    [retractall(domain:action_effect(_,_)),
     retractall(domain:action_parameters(_,_))]))
  ]
) :-
  assertion(\+ with_output_to(string(_), add_action_effects([(drop,[])], _))).
test(regress_cond_effects) :-
  assertion(regress_cond_effects(
    [(at(l),[]),(when(at(l),p),[])],
    [(at(l),[]),(p,[])])),
  assertion(regress_cond_effects(
    [(at(l),[]),(when(and(q,at(l)),p),[])],
    [(at(l),[]),(when(q,p),[])]))
    .

:- end_tests(action_effects).
