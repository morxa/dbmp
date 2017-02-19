#! /usr/bin/env swipl

/**
 *  macro_generator.pl - Generate macro actions from action sequences
 *
 *  Created:  Fri 09 Dec 2016 16:49:27 CET
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

:- use_module(utils).
:- use_module(pddl_parser).
:- use_module(effects).
:- use_module(regression).
:- use_module(simplify).
:- use_module(substitute).

:- use_module(library(lambda)).

%% generate_macro(+DomainFile, +Actions, +ParameterEnumeration, -Macro)
%
%  Compute the macro for the given Actions. Action preconditions and effects are
%  parse from the DomainFile, and parameters are reassigned according to the
%  ParameterEnumeration. ParameterEnumeration is expected to be a list of lists
%  of numbers that the parameters should be assigned to, e.g., [[1,2],[3,1]] if
%  the second parameter of the second action should be the same as the first
%  parameter of the first action.
%  The result Macro is a PDDL string representation of the macro action.
generate_macro(
  DomainFile, Actions, ParameterEnumeration, Macro
) :-
  parse_pddl_domain_file(DomainFile, ParsedDomain),
  assert_domain_facts(ParsedDomain),
  % Cut here so we parse and assert the domain only once.
  !,
  maplist(get_parameter_assignment,
    Actions, ParameterEnumeration, ParameterAssignment),
  compute_parameters(Actions, ParameterAssignment, Parameters),
  compute_precondition(Actions, ParameterAssignment, Parameters, Precondition),
  % Cut here, we don't want to backtrace if effect computation fails.
  !,
  compute_effect(Actions, ParameterAssignment, Effect),
  % Cut here, we really only want to generate the macro once.
  !,
  atomic_list_concat(Actions, '-', Name),
  generate_pddl_action(Name, Parameters, Precondition, Effect, MacroAction),
  format(string(Macro), '; MACRO ~w ACTIONS ~w PARAMETERS ~w\n~w',
    [Name, Actions, ParameterEnumeration, MacroAction]).

%% generate_macro_to_file(+DomainFile, +Actions, +ParameterEnum, +Filename)
%
%  The same as generate_macro/4, but write the resulting macro directly to
%  Filename.
generate_macro_to_file(DomainFile, Actions, ParameterEnum, Filename) :-
  generate_macro(DomainFile, Actions, ParameterEnum, Macro),
  setup_call_cleanup(
    open(Filename, write, Stream),
    write(Stream, Macro),
    close(Stream)
  ).

%% get_parameter_assignment(-Action, -ParamEnumeration, +ParamAssignment)
%
%  For the given Action, transform the enumeration of parameters
%  ParamEnumeration into a parameter assignment. A parameter enumeration is a
%  list new parameters that shall be used for the reassigned action, e.g.,
%  [1,2,1]. All parameters are reassigned to a variable of the name ?p<number>.
%  This expects the domain to be asserted already. In particular,
%  domain:action_parameters(Action, Parameters) must give the action's
%  parameters.
get_parameter_assignment(Action, ParamEnumeration, ParamAssignment) :-
  domain:action_parameters(Action, TypedParameters),
  get_untyped_list(TypedParameters, Parameters),
  length(Parameters, ParameterLength),
  length(ParamEnumeration, EnumerationLength),
  ( ParameterLength \= EnumerationLength ->
    format('Parameter length mismatch. Action ~w has ~w parameters, \c
            given enumeration has ~w',
           [Action, ParameterLength, EnumerationLength]),
    fail
  ;
    true
  ),
  maplist(atom_concat('?p'), ParamEnumeration, NewParams),
  maplist(\Param^NewParam^(=((Param,NewParam))),
    Parameters, NewParams, ParamAssignment).


%% compute_parameters(+Actions, +Assignments, -Parameters)
%
%  Compute the collected Parameters of the list of Actions while reassigning
%  parameters according to the Assignment. For each action in the list,
%  domain:action_parameters/2 is used to get the action's parameters.
%  Assignments is a list of assignments, each list item is the assignment for
%  the corresponding action in Actions. The result is undefined if Actions and
%  Assignments do not have the same length. The resulting Parameters do not
%  contain any duplicates.
compute_parameters([], [], []).
compute_parameters(
  [Action|Actions], [Assignment|Assignments], UniqueParameters
) :-
  domain:action_parameters(Action, ActionParameters),
  get_reassigned_parameters(ActionParameters, Assignment,
    ReassignedActionParameters),
  compute_parameters(Actions, Assignments, RemainingParameters),
  append(ReassignedActionParameters, RemainingParameters, Parameters),
  remove_duplicate_parameters(Parameters, UniqueParameters).

%% get_unique_reassigned_parameters(+Parameters, +Assignment, -UniqueParameters)
%
%  Reassign Parameters according to Assignment and remove any duplicates to
%  obtain UniqueParameters. The behavior is the same as
%  get_reassigned_parameters/3, except that all duplicate parameters are
%  removed.
get_unique_reassigned_parameters(
  Parameters, Assignment, UniqueReassignedParameters
) :-
  get_reassigned_parameters(Parameters, Assignment, ReassignedParameters),
  remove_duplicate_parameters(
    ReassignedParameters, UniqueReassignedParameters).
%% get_reassigned_parameters(+Parameters, +Assignment, -ReassignedParameters)
%
%  Assign the typed list of Parameters with Assignment to obtain
%  ReassignedParameters. This does not check for any duplicates in the
%  Parameters or duplicates that are created through reassignment.
get_reassigned_parameters([], _, []).
get_reassigned_parameters(
  [(Type,[])|R],
  Assignment,
  [(Type,[])|ReassignedParameters]
) :-
  get_reassigned_parameters(R, Assignment, ReassignedParameters).
get_reassigned_parameters(
  [(Type,[Parameter|Parameters])|R],
  Assignment,
  [(Type,[ReassignedParameter|ReassignedParameters])|ReassignedR]
) :-
  ( member((Parameter, ReassignedParameter), Assignment)
    -> true
  ;
    ReassignedParameter = Parameter
  ),
  get_reassigned_parameters(
    [(Type,Parameters)|R], Assignment,
    [(Type,ReassignedParameters)|ReassignedR]).

%% remove_duplicate_parameters(+TypedParameters, UniqueParameters)
%
%  Remove all duplicate parameters from the typed parameter list
%  TypedParameters. TypedParameters is expected to be a list of pairs, where
%  each pair consists of a type name and a list of parameters with that type.
%  This predicate does NOT check whether there are parameters of different type
%  with the same name. Such parameters will be treated as different.
remove_duplicate_parameters(TypedParameters, UniqueParameters) :-
  remove_duplicate_parameters(TypedParameters, [], UniqueParameters).
%% remove_duplicate_parameters(+TypedParameters, WorkingList, Result)
%
%  Helper predicate for remove_duplicate_parameters/2. WorkingList is the
%  currently computed list of unique parameters. When calling this predicate,
%  WorkingList should be set to the empty set.
remove_duplicate_parameters([], Parameters, Parameters).
remove_duplicate_parameters(
  [(_,[])|R], CurrentParameters, ResultingParameters
) :-
  remove_duplicate_parameters(R, CurrentParameters, ResultingParameters).
remove_duplicate_parameters(
  [(Type,[Parameter|Parameters])|R],
  CurrentParameters,
  ResultingParameters
) :-
  member((SubType,TypedParameters),CurrentParameters),
  ( Type = SubType ; domain:subtype_of_type(SubType, Type) ),
  member(Parameter,TypedParameters),
  remove_duplicate_parameters(
    [(Type,Parameters)|R], CurrentParameters, ResultingParameters).
remove_duplicate_parameters(
  [(SubType,[Parameter|Parameters])|R],
  CurrentParameters,
  ResultingParameters
) :-
  domain:subtype_of_type(SubType, Type),
  SubType \= Type,
  member((Type,TypedParameters), CurrentParameters),
  member(Parameter, TypedParameters),
  !,
  exclude(=(Parameter), TypedParameters, FilteredParameters),
  substitute((Type,TypedParameters), CurrentParameters,
    (Type,FilteredParameters), NewCurrentParameters),
  remove_duplicate_parameters(
    [(SubType,[Parameter|Parameters])|R], NewCurrentParameters, ResultingParameters).
remove_duplicate_parameters(
  [(Type,[Parameter|Parameters])|R],
  CurrentParameters,
  ResultingParameters
) :-
  forall(member((_,TypedParameters),CurrentParameters),
    \+ member(Parameter,TypedParameters)
  ),
  exclude(=((Type,TypedParameters)), CurrentParameters, FilteredParameters),
  append(TypedParameters,[Parameter],NewTypedParameters),
  append(FilteredParameters, [(Type,NewTypedParameters)], NewCurrentParameters),
  remove_duplicate_parameters(
    [(Type,Parameters)|R],
    NewCurrentParameters,
    ResultingParameters).
remove_duplicate_parameters(
  [(Type,[Parameter|Parameters])|R],
  CurrentParameters,
  ResultingParameters
) :-
  \+ member((Type,_),CurrentParameters),
  append(CurrentParameters, [(Type,[Parameter])], NewParameters),
  remove_duplicate_parameters(
    [(Type,Parameters)|R],
    NewParameters,
    ResultingParameters).

%% compute_preconditions(+Actions, +Assignments, +Parameters, -Precondition)
%
%  Compute the Precondition of the given Actions with the parameter assignment
%  Assignments and the typed parameter list Parameters. The result is a single
%  formula that describes the precondition of the whole action sequence.
%  Note the difference between Assignments and Parameters: Assignments is a list
%  with the same length as Actions and describes for each action in Actions what
%  the action's parameters should be reassigned to. Thus, Assignments is a list
%  of list of pairs, e.g., [[('?from','?1'),('?to','?2')],[('?loc','?2')]].
%  On the other hand, Parameters describes the cumulative parameters and their
%  types of the action sequence, e.g., [(location,['?1','?2'])].
compute_precondition([], _, _, true).
compute_precondition(
  [Action|Actions],
  [Assignment|Assignments],
  Parameters,
  Precondition
) :-
  compute_precondition(Actions, Assignments, Parameters, PreconditionR),
  compute_effect([Action], [Assignment], ActionEffect),
  regress([ActionEffect], Parameters, PreconditionR, RegressedPreconditionR),
  domain:action_precondition(Action, ActionPrecondition),
  substitute_list([ActionPrecondition], Assignment, [SubstitutedPrecondition]),
  simplify(and(SubstitutedPrecondition,RegressedPreconditionR), Precondition).


% TODO all these tests have choicepoints; only by grounding the result in the
% assertion we obtain determistic results. fix remove_duplicate_parameters such
% that no choicepoints exist.
:- begin_tests(remove_duplicate_parameters).
test(no_duplicates) :-
  assertion(remove_duplicate_parameters([(typeA,[a,b])], [(typeA,[a,b])])).
test(one_type_one_duplicate) :-
  assertion(remove_duplicate_parameters([(typeA,[a,b,a])], [(typeA,[a,b])])).
test(two_types_no_duplicates) :-
  assertion(remove_duplicate_parameters(
    [(typeA,[a,b]),(typeB,[c,d])], [(typeA,[a,b]),(typeB,[c,d])])).
test(two_types_with_same_parameter) :-
  assertion(remove_duplicate_parameters(
    [(typeA,[a,b]),(typeB,[b,c])], [(typeA,[a,b]),(typeB,[b,c])])).
test(split_duplicate_types) :-
  assertion(remove_duplicate_parameters(
    [(typeA,[a,b]),(typeA,[b,c])], [(typeA, [a,b,c])])).
test(
  duplicate_parameters_with_subtypes,
  [ setup(assertz(domain:subtype_of_type(subtype,type))),
    cleanup(retractall(domain:subtype_of_type(_,_)))
  ]
) :-
  assertion(remove_duplicate_parameters(
    [(type,[a,b]),(subtype,[b])], [(type,[a]),(subtype,[b])])),
  remove_duplicate_parameters(
    [(subtype,[b]),(type,[a,b])], R),
  assertion(R=[(subtype,[b]),(type,[a])]).
:- end_tests(remove_duplicate_parameters).

:- begin_tests(parameter_assignment).
test(empty_action_list) :-
  assertion(get_reassigned_parameters([], [], [])).

test(no_reassignment) :-
  assertion(get_unique_reassigned_parameters(
    [("block", ["?y"])], [], [("block", ["?y"])])).
test(simple_reassignment) :-
  assertion(get_unique_reassigned_parameters(
    [("block",["?y"])], [("?y","?z")], [("block", ["?z"])])).
test(two_params) :-
  assertion(get_unique_reassigned_parameters(
    [("block",["?y", "?z"])], [("?y","?z")], [("block", ["?z"])])).
test(
  parameters_from_actions,
  [setup(assertz(
    domain:action_parameters("stack", [("block", ['?x']), ("block", ['?y'])]))),
   cleanup(retract_domain_facts)
  ]
) :-
  assertion(compute_parameters(
    ["stack", "stack"],
    [[], [('?x', '?z')]],
    [("block", ['?x', '?y', '?z'])])).
test(
  parameter_enumeration_to_assignment,
  [setup(assertz(
    domain:action_parameters("stack", [("block", ['?x']), ("block", ['?y'])]))),
   cleanup(retract_domain_facts)
  ]
) :-
  assertion(
    get_parameter_assignment("stack", [1,2], [('?x', '?p1'), ('?y', '?p2')])),
  assertion(
    get_parameter_assignment("stack", [2,1], [('?x', '?p2'), ('?y', '?p1')])),
  assertion(
    get_parameter_assignment("stack", [2,2], [('?x', '?p2'), ('?y', '?p2')])),
  assertion(with_output_to(string(_),
     \+ get_parameter_assignment("stack", [1], [('?x', '?p2'), ('?y', '?p2')]))).

:- end_tests(parameter_assignment).

:- begin_tests(precondition).
test(
  no_reassignment,
  [ setup(assert_domain_file("test_data/domain.pddl")),
    cleanup(retract_domain_facts)
  ]
) :-
  assertion(compute_precondition(['pick-up', 'put-down'], [[],[]], [],
    and(clear('?x'), ontable('?x'), handempty))).

test(
  with_reassignment,
  [ setup(assert_domain_file("test_data/domain.pddl")),
    cleanup(retract_domain_facts)
  ]
) :-
  compute_precondition(['pick-up', 'put-down'], [[('?x','?y')],[]], [], R),
  assertion(
    R=and(clear('?y'), ontable('?y'), handempty, or('?y'='?x',holding('?x')))
  ).

:- end_tests(precondition).
