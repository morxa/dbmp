#! /usr/bin/env swipl

/**
 *  effect_tree.pl - Compute conditional effects with effect trees
 *
 *  Created:  Tue 10 Jan 2017 02:24:01 PM CET
 *  Copyright  2017  Till Hofmann <hofmann@kbsg.rwth-aachen.de>
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

:- use_module(library(lambda)).
:- use_module(simplify).
:- use_module(substitute).

/*
* General procedure (WIP):
* 1. perform parameter assignment on each effect and on the action's parameters
* 1. find all conditions in all effects
* 3. regress all conditions to the beginning of the action sequence
* 4. compute the list of effects of all actions with reassigned parameters
* 5. compute the effect tree using the regressed conditions and computed effects
*/

%% effect_tree(+Effects, -EffectTree)
%
%  Compute an effect tree for the given Effects. Each node represents one effect
%  or one substituted conditional effect. For each effect in Effects, all
%  conditions are substituted one by one by the respective false effect and true
%  effect, and a child node is added for each. If there is no conditional
%  effect, the next effect in Effects is considered. The leave nodes represent
%  all possible conditions and their respective effects.
%  Effects are expected to be in reverse order, i.e., the first effect in
%  Effects should be the last effect of the action sequence.
%  The nodes in the tree have the form
%  (Effect, ResultingEffects, CurrentConditions, LeftSubTree, RightSubTree),
%  where Effect is the currently considered effect, ResultingEffects and
%  CurrentConditions are the accumulated effects and conditions of the path to
%  the root node including this node, and LeftSubTree is the subtree with the
%  currently considered condition set to true, and RightSubTree is the subtree
%  with the currently considered condition set to false. If no condition exists,
%  RightSubTree is nil. If Effects is empty and no condition exists, both
%  subtrees are nil.
effect_tree(Effects, EffectTree) :- effect_tree(Effects, [], [], EffectTree).

%% effect_tree(+Effects, +CurrentEffects, +CurrentConditions, -EffectTree)
%
%  Helper predicate for effect_tree/2. This is used to generate subtrees
%  recursively. CurrentEffects and CurrentConditions are all previous effects
%  and conditions excluding the root node of this subtree.
effect_tree([], _, _, nil).
effect_tree(
  [(Effect,Parameters)|Effects],
  CurrentEffects, CurrentConditions,
  (Effect, ResultingEffects, CurrentConditions, SubTree, nil)
) :-
  \+ has_free_cond_effect(Effect, Parameters, _),
  split_effect(Effect, SplitEffect),
  exclude(
    effect_collision(CurrentEffects, Parameters),
    SplitEffect,
    ActionEffectsWithoutCollisions
  ),
  maplist(constrain_effect(CurrentEffects, Parameters),
    ActionEffectsWithoutCollisions, ConstrainedActionEffects),
  maplist(simplify_effect, ConstrainedActionEffects, SimplifiedEffects),
  exclude(=(nil), SimplifiedEffects, FilteredActionEffects),
  append(FilteredActionEffects, CurrentEffects, ResultingEffects),
  effect_tree(Effects, ResultingEffects, CurrentConditions, SubTree).
effect_tree(
  [(Effect,Parameters)|Effects],
  CurrentEffects, CurrentConditions,
  (Effect, CurrentEffects, CurrentConditions,
    TrueEffectSubTree, FalseEffectSubTree)
) :-
  has_free_cond_effect(Effect, Parameters, when(Cond,CondEffect)),
  substitute(when(Cond,CondEffect), [Effect], CondEffect, [TrueEffect]),
  simplify_effect(TrueEffect, SimplifiedTrueEffect),
  substitute(when(Cond,CondEffect), [Effect], nil, [FalseEffect]),
  simplify_effect(FalseEffect, SimplifiedFalseEffect),
  effect_tree(
    [(SimplifiedTrueEffect,Parameters)|Effects],
    CurrentEffects, [Cond|CurrentConditions],
    TrueEffectSubTree
  ),
  effect_tree(
    [(SimplifiedFalseEffect,Parameters)|Effects],
    CurrentEffects, [not(Cond)|CurrentConditions],
    FalseEffectSubTree
  ).

%% has_free_cond_effect(+Effect, +Params, -CondEffect)
%
%  Checks the Effect if it contains a conditional effect which has a condition
%  with no variables bound by a quantifier, i.e., all variables are bound by the
%  effect's Params.
has_free_cond_effect(Effect, Params, when(Cond,CondEffect)) :-
  contains_term(when(Cond,CondEffect), Effect),
  get_free_vars(Cond, CondVars),
  maplist(\Var^is_in_typed_list(Var, Params), CondVars).

%% get_free_vars(+Formula, -FreeVars)
%
%  Get all variables of the form '?var_name' in Formula which are not bound
%  within Formula.
get_free_vars(Formula, Vars) :-
  get_free_vars_list(Formula, VarsList),
  list_to_set(VarsList, Vars).

%% get_free_vars_list(+Formula, -FreeVars)
%
%  Helper predicate for get_free_vars/2. This computes the list of free
%  variables without removing duplicates.
get_free_vars_list(and(), []).
get_free_vars_list(Formula, Vars) :-
  Formula =.. [Op|SubFormulas],
  member(Op, [and,or]),
  maplist(get_free_vars, SubFormulas, SubFormulaVars),
  flatten(SubFormulaVars, Vars).
get_free_vars_list(Formula, Vars) :-
  Formula =.. [Op,Formula1,Formula2],
  member(Op,[imply,when]),
  get_free_vars(Formula1, Formula1Vars),
  get_free_vars(Formula2, Formula2Vars),
  append(Formula1Vars, Formula2Vars, Vars).
get_free_vars_list(Formula, Vars) :-
  Formula =.. [Op,QuantifiedVars,QuantifiedFormula],
  member(Op, [all,some]),
  get_free_vars_list(QuantifiedFormula, FormulaVars),
  exclude(\Var^is_in_typed_list(Var, QuantifiedVars), FormulaVars, Vars).
get_free_vars_list(Formula, [Formula]) :-
  atom(Formula).
get_free_vars_list(Formula, Vars) :-
  \+ atom(Formula),
  Formula =.. [Predicate|Params],
  \+ member(Predicate, [and,or,some,all,when,imply]),
  maplist(get_free_vars_list, Params, ParamsVars),
  flatten(ParamsVars, Vars).

%% is_in_typed_list(+Var, +TypedVars)
%
%  True if Var is in the list of TypedVars. TypedVars is expected to be a list
%  of pairs, where each pair is of the form (Type, TypedVars), and TypedVars is
%  a list of variable names.
is_in_typed_list(Var, [(_,[])|Vars]) :- is_in_typed_list(Var, Vars).
is_in_typed_list(Var, [(_,[Var|_])|_]).
is_in_typed_list(Var, [(Type,[_|TypedVars])|Vars]) :-
  is_in_typed_list(Var, [(Type,TypedVars)|Vars]).

%% has_type(+Var, +TypedList, -Type)
%
%  True if Var has the type Type as defined in TypesList.
has_type(Var, [(Type,[Var|_])|_], Type).
has_type(Var, [(OtherType,[_|TypedVars])|Vars], Type) :-
  has_type(Var, [(OtherType,TypedVars)|Vars], Type).
has_type(Var, [(_,[])|Vars], Type) :-
  has_type(Var, Vars, Type).

merge_typed_lists([], []).
merge_typed_lists([TypedList|TypedLists], MergedList) :-
  merge_typed_lists(TypedLists, MergedRestList),
  merge_typed_lists(TypedList, MergedRestList, MergedList).
merge_typed_lists([], TypedList, TypedList).
merge_typed_lists([(Type,Vars)|TypedList], OtherTypedList, MergedList) :-
  member((Type, OtherVars), OtherTypedList),
  append(Vars, OtherVars, MergedVars),
  substitute((Type, OtherVars), OtherTypedList,
             (Type, MergedVars), SubstitutedList),
  merge_typed_lists(TypedList, SubstitutedList, MergedList).
merge_typed_lists([(Type,Vars)|TypedList], OtherTypedList, MergedList) :-
  \+ member((Type, _), OtherTypedList),
  merge_typed_lists(TypedList, [(Type,Vars)|OtherTypedList], MergedList).

remove_from_typed_list(_, [], []).
remove_from_typed_list(
  (Type, Var), [(Type,TypedVars)|Vars], [(Type,ResTypedVars)|Vars]
) :-
  member(Var, TypedVars),
  !,
  exclude(=(Var), TypedVars, ResTypedVars).
remove_from_typed_list(
  (Type, Var), [(OtherType,TypedVars)|Vars], [(OtherType,TypedVars)|ResVars]
) :-
  Type \= OtherType,
  !,
  remove_from_typed_list((Type,Var), Vars, ResVars).
remove_from_typed_list(
  (Type, Var), [(Type,TypedVars)|Vars], [(Type,TypedVars)|ResVars]
) :-
  \+ member(Var, TypedVars),
  remove_from_typed_list((Type, Var), Vars, ResVars).

remove_from_typed_list([], Vars, Vars).
remove_from_typed_list([(_,[])|Vars], OldVars, NewVars) :-
  remove_from_typed_list(Vars, OldVars, NewVars).
remove_from_typed_list([(Type,[Var|TypedVars])|Vars], OldVars, CleanedVars) :-
  remove_from_typed_list((Type, Var), OldVars, IntermediateVars),
  remove_from_typed_list([(Type, TypedVars)|Vars], IntermediateVars, NewVars),
  exclude(=((_,[])), NewVars, CleanedVars).


%% get_types_list(+Vars, +Types, -TypedVars)
%
%  Get a types list for all Vars, using the types defined by Types. This
%  effectively filters Types by the names in Vars.
get_types_of_list([], _, []).
get_types_of_list([Var|Vars], AllTypes, [(Type,[Var])|TypedVars]) :-
  has_type(Var, AllTypes, Type),
  get_types_of_list(Vars, AllTypes, TypedVars).

%% get_leave_nodes(-EffectTree, +LeaveNodes)
%
%  For the given tree, get a list of the leave nodes of the tree.
get_leave_nodes((_, _ ,_, LeftSubTree, RightSubTree), LeaveNodes) :-
  get_leave_nodes(LeftSubTree, LeftLeaveNodes),
  get_leave_nodes(RightSubTree, RightLeaveNodes),
  append(LeftLeaveNodes, RightLeaveNodes, LeaveNodes).
get_leave_nodes((_, _ ,_, SubTree, nil), LeaveNodes) :-
  get_leave_nodes(SubTree, LeaveNodes).
get_leave_nodes((_, _ ,_, nil, SubTree), LeaveNodes) :-
  get_leave_nodes(SubTree, LeaveNodes).
get_leave_nodes((_, Effects, Conditions, nil, nil), [(Effects, Conditions)]).

%% get_effect_from_tree(-EffectTree, +Effect)
%
%  For the given EffectTree, compute the resulting overall effect. This gets all
%  leave nodes of the tree, adds conditionals as necessary, and simplifies the
%  resulting effect.
get_effect_from_tree(EffectTree, SimplifiedEffect) :-
  get_leave_nodes(EffectTree, LeaveNodes),
  simplify_leave_nodes(LeaveNodes, SimplifiedLeaveNodes),
  maplist(get_effect_from_leave_node, SimplifiedLeaveNodes, Effects),
  maplist(simplify_effect, Effects, SimplifiedEffects),
  Effect =.. [and|SimplifiedEffects],
  simplify_effect(Effect, SimplifiedEffect).


%% simplify_leave_nodes(-LeaveNodes, +SimplifiedLeaveNodes)
%
%  Simplify the given LeaveNodes. Remove empty nodes and merge nodes if
%  possible.
%  TODO: add simplifications
simplify_leave_nodes(LeaveNodes, SimplifiedLeaveNodes) :-
  exclude(=(([],_)), LeaveNodes, SimplifiedLeaveNodes).

get_effect_from_leave_node((Effects, []), Effect) :-
  Effect =.. [and|Effects].
get_effect_from_leave_node((Effects, Conditions), CondEffect) :-
  Conditions \= [],
  Condition =.. [and|Conditions],
  Effect =.. [and|Effects],
  CondEffect = when(Condition, Effect).


tree_to_dot(Tree, File) :-
  open(File, write, Stream),
  write(Stream, "digraph effects {\n"),
  write(Stream, "rankdir=\"LR\";\n"),
  write(Stream, "node [shape=box];\n"),
  tree_to_dot(Stream, 1, Tree, _),
  write(Stream, "\n}"),
  close(Stream).
tree_to_dot(
  Stream, Index,
  (NodeEffect, Effects, Conditions, LeftSubTree, RightSubTree),
  NewIndex
) :-
  reverse(Effects, ReversedEffects),
  reverse(Conditions, ReversedConditions),
  format(Stream, "n~w [label=\"~w\\nEffects: ~w\\nConditions: ~w\"];\n",
    [Index, NodeEffect, ReversedEffects, ReversedConditions]),
  LeftSubTreeIndex is Index + 1,
  ( LeftSubTree = nil ->
    RightSubTreeIndex = LeftSubTreeIndex
  ;
    format(Stream, "n~w -> n~w;\n", [Index, LeftSubTreeIndex]),
    tree_to_dot(Stream, LeftSubTreeIndex, LeftSubTree, RightSubTreeIndex)
  ),
  ( RightSubTree = nil ->
    NewIndex = RightSubTreeIndex
  ;
    format(Stream, "n~w -> n~w;\n", [Index, RightSubTreeIndex]),
    tree_to_dot(Stream, RightSubTreeIndex, RightSubTree, NewIndex)
  ).

:- begin_tests(effect_tree).

test(effect_tree_leave_nodes, [nondet]) :-
  effect_tree(
    [(and(when(p(a),all(x,block,holding(x))),when(p(b),handempty)),[(t,[a,b])]),
     (not(holding(x)),[(block,[x])])
    ],
    [], [], EffectTree
  ),
  assertion(get_leave_nodes(EffectTree,
    [
      ([all(x, block, holding(x)), handempty], [p(b), p(a)]),
      ([all(x, block, holding(x))], [not(p(b)), p(a)]),
      ([not(holding(x)), handempty], [p(b), not(p(a))]),
      ([not(holding(x))], [not(p(b)), not(p(a))])
    ])).

test(contradicting_effects, [nondet]) :-
  effect_tree(
    [ (and(when(p(a),all(o,obj,p1(o))),when(p(b),p1(o1))),[ (obj,[o1,a,b])]),
      (when(p(c),not(p1(o1))),[ (obj,[o1,c])])
    ], [], [], EffectTree
  ),
  assertion(get_leave_nodes(EffectTree,
    [ ([all(o, obj, p1(o)), p1(o1)], [p(c), p(b), p(a)]),
      ([all(o, obj, p1(o)), p1(o1)], [not(p(c)), p(b), p(a)]),
      ([all(o, obj, p1(o))], [p(c), not(p(b)), p(a)]),
      ([all(o, obj, p1(o))], [not(p(c)), not(p(b)), p(a)]),
      ([p1(o1)], [p(c), p(b), not(p(a))]),
      ([p1(o1)], [not(p(c)), p(b), not(p(a))]),
      ([not(p1(o1))], [p(c), not(p(b)), not(p(a))]),
      ([], [not(p(c)), not(p(b)), not(p(a))])
    ] )).


:- end_tests(effect_tree).

:- begin_tests(free_vars).

test(simple) :-
  assertion(get_free_vars(p('?a'), ['?a'])),
  assertion(\+ get_free_vars(p('?a'), [])).
test(nested_functions) :-
  assertion(get_free_vars(p(f(f('?a'))), ['?a'])).
test(conjunction) :-
  assertion(get_free_vars(and(), [])),
  assertion(get_free_vars(and(p('?a')), ['?a'])),
  assertion(get_free_vars(and(p('?a'),q('?b')), ['?a','?b'])),
  assertion(get_free_vars(and(p('?a'),q('?b'),q('?a')), ['?a','?b'])),
  assertion(get_free_vars(and(p('?a'),q('?b'),q('?c')), ['?a','?b','?c'])),
  assertion(get_free_vars(and(and(p('?a'),p('?b')),p('?c')),['?a','?b','?c'])).
test(implication) :-
  assertion(get_free_vars(imply(p('?a'),p('?b')),['?a','?b'])),
  assertion(get_free_vars(
    imply(p('?a'),and(p('?b'),p('?c'))),['?a','?b', '?c'])
  ).
test(quantification) :-
  assertion(get_free_vars(all([(t,['?a'])],p('?a')),[])),
  assertion(get_free_vars(and(all([(t,['?a'])],p('?a')),p('?b')),['?b'])),
  assertion(get_free_vars(and(all([(t,['?a'])],p('?a')),p('?a')),['?a'])),
  assertion(get_free_vars(and(all([(t,['?a']),(t2,['?b','?c'])],
    p('?a','?b','?c')),p('?d')),['?d'])).

:- end_tests(free_vars).

:- begin_tests(typed_list).
test(remove_vars, [nondet]) :-
  remove_from_typed_list((t,a), [(t,[a,b])], R1),
  assertion(R1=[(t,[b])]),
  remove_from_typed_list([(t,[a,b]),(t2,[c])], [(t2,[c,d]),(t,[a,b,e])], R2),
  assertion(R2=[(t2,[d]),(t,[e])]).
:- end_tests(typed_list).

:- begin_tests(cond_effect).

test(simple) :-
  assertion(has_free_cond_effect(
    when(p('?a'),p('?b')), [(t,['?a'])], when(p('?a'),p('?b')))).
test(with_quantifier) :-
  assertion(\+ has_free_cond_effect(
    all([(t,['?a'])], when(p('?a'),p('?b'))), [], _)),
  assertion(\+ has_free_cond_effect(
    all([(t,['?a'])], when(and(p('?a'),p('?b')),p('?c'))),
    [(t,['?b','?c'])], _)).

:- end_tests(cond_effect).
