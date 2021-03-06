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
