#! /usr/bin/env swipl

/**
 *  utils.pl - Generic definitions that are used everywhere
 *
 *  Created:  Sun 19 Feb 2017 11:33:27 CET
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

:- module(
    utils,
    [
      is_in_typed_list/2,
      has_type/3,
      merge_typed_lists/2,
      merge_typed_lists/3,
      remove_from_typed_list/3,
      get_types_of_list/3,
      get_untyped_list/2,
      get_free_vars/2,
      get_free_vars_list/2,
      generate_new_vars/2
    ]
  ).

:- use_module(substitute).

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

%% merge_typed_lists(+TypedLists, -MergedTypedList)
%
%  Merge the list of typed lists into a single typed list.
merge_typed_lists([], []).
merge_typed_lists([TypedList|TypedLists], MergedList) :-
  merge_typed_lists(TypedLists, MergedRestList),
  merge_typed_lists(TypedList, MergedRestList, MergedList).
%% merge_typed_lists(+TypedList, +OtherTypedList, -MergedTypedList)
%
%  Merge the two typed lists into one typed list.
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

%% remove_from_typed_list(TypedVar, List, ListWithoutVar)
%
%  Remove the variable TypedVar from the typed list List, resulting in
%  the typed list ListWithoutVar. If TypedVar does not occur in List, then List
%  is unified with ListWithoutVar.
%
%  TypedVar is expected to be a pair (Type, Var), or a typed list of variables,
%  e.g. [(t1,[a,b]),(t2,[c])].
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

%% get_untyped_list(-TypedParameters, +UntypedParameterList)
%
%  Convert a typed parameter list of the form
%  [(Type1,[Param1,Param2]),(Type2,[Param3]),...) to a list of the form
%  [Param1,Param2,Param3].
get_untyped_list([], []).
get_untyped_list([(_,TypedParameters)|RTypedParameters], Parameters) :-
  get_untyped_list(RTypedParameters, RParameters),
  append(TypedParameters, RParameters, Parameters).

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
get_free_vars_list(true, []).
get_free_vars_list(false, []).
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

%% generate_new_vars(TypedVars, NewTypedVars)
%
%  Generates a typed list of new variables that do not occur anywhere else.
generate_new_vars([], []).
generate_new_vars([(Type,[])|Vars], [(Type, [])|NewVars]) :-
  generate_new_vars(Vars, NewVars).
generate_new_vars(
  [(Type,[_|TypedVars])|Vars], [(Type,[NewVar|NewTypedVars])|NewVars]
) :-
  once(with_output_to(atom(NewVar), ( write('?dbmp_var'), var(X), write(X) ))),
  once(
    generate_new_vars([(Type,TypedVars)|Vars], [(Type,NewTypedVars)|NewVars])
  ).



:- begin_tests(typed_list).
test(remove_vars, [nondet]) :-
  remove_from_typed_list((t,a), [(t,[a,b])], R1),
  assertion(R1=[(t,[b])]),
  remove_from_typed_list([(t,[a,b]),(t2,[c])], [(t2,[c,d]),(t,[a,b,e])], R2),
  assertion(R2=[(t2,[d]),(t,[e])]).
:- end_tests(typed_list).

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
