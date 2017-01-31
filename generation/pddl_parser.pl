#! /usr/bin/env swipl

/**
 *  pddl_parser.pl - A Prolog PDDL parser
 *
 *  Created:  Mon 05 Dec 2016 11:33:33 CET
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

:- module(pddl_parser,
    [ parse_pddl_domain/2, parse_pddl_domain_file/2, preprocess_pddl/2,
      assert_domain_facts/1, assert_domain_file/1, retract_domain_facts/0,
      generate_pddl_domain/2, generate_pddl_action/5]).

:- use_module(library(regex)).


%% preprocess_pddl(*String, -PreprocessedStringList)
%
%  Preprocess the given domain by splitting the domain into a list of operators.
%  Parantheses are considered operators and are always split from the rest of
%  the text, e.g.
%  preprocess("(define (domain d))", ["(","define","(","domain","d",")",")"]).
%  This allows simpler parsing of the domain in the subsequent parsing step.
preprocess_pddl(String, []) :-
  String =~ "^[\s\t\f\r]*;.*",
  !.
preprocess_pddl(String, PreprocessedStringList) :-
  add_padding(String, PaddedString),
  split_string(PaddedString, " \t\n", " \t\n", PreprocessedStringList).


%% add_padding(*String, -PaddedStringChars)
%
%  Turn the given String into a character list and pad all parantheses such that
%  there is always a whitespace left and right of each paranthesis.
add_padding(String, PaddedStringChars) :-
  string_chars(String, StringChars),
  add_char_padding(StringChars, PaddedStringChars).

%% add_char_padding(CharList, PaddedCharList)
%
%  Add padding of parantheses to a list of characters. For each paranthesis in
%  the list, add a whitespace character before and after the paranthesis.
add_char_padding([],[]).
add_char_padding(['('|Chars], [' ', '(', ' '|PaddedChars]) :-
  !, add_char_padding(Chars, PaddedChars).
add_char_padding([')'|Chars], [' ', ')', ' '|PaddedChars]) :-
  !, add_char_padding(Chars, PaddedChars).
add_char_padding([Char|Chars], [Char|PaddedChars]) :-
  add_char_padding(Chars, PaddedChars).

%% Grammar definition for PDDL domains.
%
%  This parses (part of) the PDDL domain definition defined by McDermott et al.
%  We use the strict subset of PDDL, which expects all the domain definition
%  parts to be in a certain order, see McDermott et al. for details.
pddl_domain(B) --> ["(", "define"], domain_body(B), [")"].
domain_body(
  [DomainName, Requirements, Types, Constants, Predicates, Actions]
) -->
  domain_name_def(DomainName),
  require_def(Requirements),
  types_def(Types),
  constants_def(Constants),
  predicates_def(Predicates),
  actions_defs(Actions).
domain_name_def((domain, NameAtom)) -->
  ["(", "domain"], [Name], { atom_string(NameAtom, Name) }, [")"].
require_def((requirements, [])) --> [].
require_def((requirements,Reqs)) -->
  ["(", ":requirements"], requirements_list(Reqs), [")"].
types_def((types, [])) --> [].
types_def((types, Types)) -->
  ["(", ":types"], types_list(Types), [")"].
% TODO add constants definitions.
constants_def((constants, [])) --> [].
predicates_def((predicates, [])) --> [].
predicates_def((predicates, Predicates)) -->
  ["(", ":predicates"],
  predicate_list(Predicates),
  [")"].
predicate_list([]) --> [].
predicate_list([(Predicate,PredicateTypes)|Predicates]) -->
  ["("], predicate(Predicate), typed_list(PredicateTypes), [")"],
  predicate_list(Predicates).
actions_defs((actions,Actions)) --> action_list(Actions).
action_list([]) --> [].
action_list([Action|Actions]) -->
  action_def(Action), action_list(Actions).
action_def([NameAtom,Params,Precondition,Effect]) -->
  ["(", ":action"], [Name], { atom_string(NameAtom, Name) },
  [":parameters"], ["("], typed_list(Params), [")"],
  [":precondition"], goal_description(Precondition),
  [":effect"], effect(Effect),
  [")"].

goal_description(Formula) --> atomic_formula(Formula).
goal_description(Goal) -->
  % Same workaround as for atomic_formula
  { \+ var(Goal) -> Goal =.. [and|GoalList] ; true },
  ["(", "and"], goal_description_list(GoalList), [")"],
  { var(Goal) -> Goal =.. [and|GoalList] ; true }.
goal_description(Goal) -->
  % Same workaround as for atomic_formula
  { \+ var(Goal) -> Goal =.. [or|GoalList] ; true },
  ["(", "or"], goal_description_list(GoalList), [")"],
  { var(Goal) -> Goal =.. [or|GoalList] ; true }.
goal_description(not(Goal)) --> ["(", "not"], goal_description(Goal), [")"].
goal_description(imply(Cond,Goal)) -->
  ["(", "imply"], goal_description(Cond), goal_description(Goal), [")"].
goal_description(exists(VarList,Goal)) -->
  ["(", "exists"],
  ["("], typed_list(VarList), [")"],
  goal_description(Goal), [")"].
% Note: different operator name to distinguish from Prolog forall/2.
goal_description(all(VarList,Goal)) -->
  ["(", "forall"],
  ["("], typed_list(VarList), [")"],
  goal_description(Goal), [")"].
goal_description(Var1 = Var2) -->
  ["(", "="], term(Var1), term(Var2), [")"].

goal_description_list([Goal]) --> goal_description(Goal).
goal_description_list([Goal|GoalList]) -->
  goal_description(Goal), goal_description_list(GoalList).

effect(Effect) --> atomic_formula(Effect).
effect(not(Effect)) -->
  ["(", "not"], atomic_formula(Effect), [")"].
effect(Effect) -->
  % Same workaround as for atomic_formula
  { \+ var(Effect) -> Effect =.. [and|Effects] ; true },
  ["(", "and"], effect_list(Effects), [")"],
  { var(Effect) -> Effect =.. [and|Effects] ; true }.
effect(when(Cond,Effect)) -->
  ["(", "when"], goal_description(Cond), effect(Effect), [")"].
effect(all(VarList,Effect)) -->
  ["(", "forall"],
  ["("], typed_list(VarList), [")"],
  effect(Effect), [")"].

effect_list([Effect]) --> effect(Effect).
effect_list([Effect|Effects]) --> effect(Effect), effect_list(Effects).

atomic_formula(Formula) -->
  % This is a workaround to support both parsing and generation. The problem
  % here is that for =../2 to work, one of the arguments has to be instantiated.
  % Thus, if we are parsing, we cannot enforce Formula =.. [...] at the
  % beginning. On the other hand, when generating, Predicate needs to be
  % instantiated, otherwise predicate(Predicate) will throw an error because
  % both arguments for atom_string/2 would be uninstantiated.
  { \+ var(Formula) -> Formula =.. [Predicate|Terms] ; true},
  ["("], predicate(Predicate), term_list(Terms), [")"],
  { var(Formula) -> Formula =.. [Predicate|Terms] ; true }.
term_list([]) --> [].
term_list([Term|Terms]) --> term(Term), term_list(Terms).
% TODO We might need some constraints on the symbols used for terms and
% predicates.
predicate(PredicateAtom) -->
  [Predicate],
  { atom_string(PredicateAtom, Predicate),
    \+ member(PredicateAtom, [not,and,or,forall,imply])}.
term(TermAtom) -->
  [Term],
  {
    % constrain TermAtom to be either a var (if we parse a string), or an atom
    % (if we generate the string), because atom_string/2 fails on non-atomic
    % terms.
    ( var(TermAtom) ; atom(TermAtom) ),
    atom_string(TermAtom, Term),
    \+ member(TermAtom, ['(', ')'])
  }.

requirement(RAtom) -->
  [R], { atom_string(RAtom, R), string_concat(":", _, R) }.
requirements_list([]) --> [].
requirements_list([Requirement|L2]) -->
  requirement(Requirement),
  requirements_list(L2).
% TODO we may want to add them to the type object
types_list(Types) --> subtypes_list(Types).
types_list([(Type,SubTypes)|T2]) -->
  subtypes_list(SubTypes),
  ["-"],
  type(Type),
  types_list(T2).
subtypes_list([]) --> [].
subtypes_list([Type|T2]) --> type(Type), subtypes_list(T2).
type(TypeAtom) -->
  [Type],
  {
    % constrain TypeAtom to be either a var (if we parse a string), or an atom
    % (if we generate the string), because atom_string/2 fails on non-atomic
    % terms.
    ( var(TypeAtom) ; atom(TypeAtom) ),
    atom_string(TypeAtom, Type),
    \+ member(Type, ["(", ")", "-"])
  }.
typed_list([]) --> [].
typed_list([TypedVars|Types]) --> typed_vars(TypedVars), typed_list(Types).
typed_vars((Type, [Var])) --> variable(Var), ["-"], type(Type).
typed_vars((Type,[Var|Vars])) --> variable(Var), typed_vars((Type, Vars)).

variable(VarAtom) -->
  [Var], { atom_string(VarAtom, Var), string_concat("?", _, Var) }.

%% parse_pddl_domain(*DomainString, -ParsedDomain)
%
%  Parse the given domain string and return the domain parts as a list of pairs,
%  where each pair is of the form (key,value), e.g. (requirements, [":adl"]).
parse_pddl_domain(DomainString, ParsedDomain) :-
  preprocess_pddl(DomainString, PreprocessedDomain),
  once(pddl_domain(ParsedDomain, PreprocessedDomain, [])).

%% generate_pddl_domain(*Domain, -PDDLString)
%
%  From the given Domain, generate a PDDL representation of the domain.
%  TODO: This currently produces a one-liner, add better line breaks.
generate_pddl_domain(Domain, PDDLString) :-
  once(pddl_domain(Domain, PDDLStringList, [])),
  atomic_list_concat(PDDLStringList, ' ', PDDLAtom),
  atom_string(PDDLAtom, PDDLString).

%% generate_pddl_action(+Parameters, +Precondition, +Effects, -PDDLString)
%
%  For the given action with Name, Parameters, Precondition, and Effects,
%  compute a the respective PDDL representation of the action.
generate_pddl_action(Name, Parameters, Precondition, Effects, PDDLString) :-
  once(action_def([Name, Parameters, Precondition, Effects], PDDLList, [])),
  atomic_list_concat(PDDLList, ' ', PDDLAtom),
  atom_string(PDDLAtom, PDDLString).

%% read_file(*Filename, -Strings)
%
%  Read the given file name in a list of strings.
read_file(Filename, Strings) :-
  setup_call_cleanup(
    open(Filename, read, Stream),
    read_stream_string(Stream, Strings),
    close(Stream)
  ).

%% read_stream_string(+Stream, -Strings)
%
%  Read a list of strings froom the given Stream.
read_stream_string(Stream, Strings) :-
  read_line_to_string(Stream, String),
  ( String == end_of_file -> Strings = []
  ;
    read_stream_string(Stream, RStrings),
    Strings = [String|RStrings]
  ).

%% preprocess_string_list(*Lines, PreprocessedLines)
%
%  Preprocess the given list of Strings by splitting the strings into atomic
%  strings expected by the parser. See preprocess_pddl/2 for more info.
preprocess_string_list([], []).
preprocess_string_list([Line|RLines], PreprocessedLines) :-
  preprocess_pddl(Line, PreprocessedLine),
  preprocess_string_list(RLines, PreprocessedRLines),
  append(PreprocessedLine, PreprocessedRLines, PreprocessedLines).

%% parse_pddl_domain_file(*Filename, ParsedDomain)
%
%  Parse the given PDDL Domain file into a structured domain representation.
%  ParsedDomain is unified with a list of pairs, where each pair is of the form
%  (key,value), e.g. (domain, "blocksworld").
%  See also parse_pddl_domain/2.
parse_pddl_domain_file(Filename, ParsedDomain) :-
  read_file(Filename, FileContent),
  preprocess_string_list(FileContent, PreprocessedDomain),
  exclude(=(""), PreprocessedDomain, FilteredDomain),
  once(pddl_domain(ParsedDomain, FilteredDomain, [])).

%% assert_domain_facts(*Domain)
%
%   assertz/1 all facts from the given domain. This asserts the following facts:
%   - domain:name/1 : the name of the domain
%   - domain:requirements/1 : the list of requirements
%   - domain:requires/1 : each requirement
%   - domain:types/1 : the list of types
%   - domain:type/1 : each type of the domain
%   - domain:constants/1 : all constants of the domain
%   - domain:predicate/1 : each predicate of the domain
%   - domain:predicate_parameters/2 : the parameters of each predicate
%   - domain:action/1 : each action name of the domain
%   - domain:action_parameters/2 : all parameters of each action
%   - domain:action_precondition/2 : the precondition of each action
%   - domain:action_effect/2 : the effect of each action
assert_domain_facts(Domain) :-
  member((domain,DomainName), Domain),
  assertz(domain:name(DomainName)),
  member((requirements,Requirements), Domain),
  assertz(domain:requirements(Requirements)),
  forall(
    member(Requirement, Requirements),
    assertz(domain:requires(Requirement))),
  member((types,Types), Domain),
  assertz(domain:types(Types)),
  forall(
    member(Type, Types),
    assertz(domain:type(Type))),
  member((constants,Constants), Domain),
  assertz(domain:constants(Constants)),
  member((predicates,Predicates), Domain),
  forall(
    member((PredicateName,PredicateParams), Predicates),
    ( assertz(domain:predicate(PredicateName)),
      assertz(domain:predicate_parameters(PredicateName, PredicateParams))
    )
  ),
  member((actions,Actions), Domain),
  forall(
    member([ActionName,ActionParams,ActionPrecondition,ActionEffect], Actions),
    ( assertz(domain:action(ActionName)),
      assertz(domain:action_parameters(ActionName, ActionParams)),
      assertz(domain:action_precondition(ActionName, ActionPrecondition)),
      assertz(domain:action_effect(ActionName, ActionEffect))
    )
  ).

%% retract_domain_facts
%
%  Retract all asserted facts about the domain.
retract_domain_facts :-
  retractall(domain:name(_)),
  retractall(domain:requirements(_)),
  retractall(domain:requires(_)),
  retractall(domain:types(_)),
  retractall(domain:type(_)),
  retractall(domain:constants(_)),
  retractall(domain:predicate(_)),
  retractall(domain:predicate_parameters(_,_)),
  retractall(domain:action(_)),
  retractall(domain:action_parameters(_,_)),
  retractall(domain:action_precondition(_,_)),
  retractall(domain:action_effect(_,_)).

%% assert_domain_file(*DomainFile)
%
%  Parse the given domain file and assert all facts about it.
%  See assert_domain_facts/1 for more information on the asserted facts.
assert_domain_file(DomainFile) :-
  parse_pddl_domain_file(DomainFile, Domain),
  once(assert_domain_facts(Domain)).


:- begin_tests(pddl_parser).

test(preprocessing) :-
  assertion(preprocess_pddl(
              "(define (domain blocksworld))",
              ["(", "define", "(", "domain", "blocksworld", ")", ")"])),
  assertion(preprocess_pddl(
              "( define(  domain  blocksworld  ) )",
              ["(", "define", "(", "domain", "blocksworld", ")", ")"])).

test(domain_name) :-
  parse_pddl_domain("(define (domain blocksworld))", ParserResult),
  assertion(member((domain, blocksworld), ParserResult)).

test(requirements) :-
  parse_pddl_domain("(define (domain d) (:requirements :adl :action-costs))",
                    ParserResult),
  assertion(member((requirements, [':adl', ':action-costs']), ParserResult)),
  % we do NOT validate requirements, i.e., we can have arbitrary requirements.
  parse_pddl_domain("(define (domain d) (:requirements :nonexistent-req))",
                    ParserResult2),
  assertion(member((requirements, [':nonexistent-req']), ParserResult2)).

test(predicates) :-
  parse_pddl_domain("(define (domain d) (:predicates (at ?l - location)))",
                    ParserResult),
  assertion(member((predicates, [(at,[(location,['?l'])])]), ParserResult)),
  parse_pddl_domain("(define (domain d) (:predicates (on ?x ?y - block)))",
                    ParserResult2),
  assertion(member( (predicates, [(on,[(block,['?x', '?y'])])]) ,
                    ParserResult2)),
  parse_pddl_domain("(define (domain d)
    (:predicates (at ?l - location) (on ?x ?y - block)))",
                    ParserResult3),
  assertion(member( (predicates,
                      [(at, [(location, ['?l'])]),
                       (on,[(block,['?x', '?y'])])]) ,
                    ParserResult3)).

test(subtypes, [nondet]) :-
  parse_pddl_domain(
    "(define (domain d) \c
      (:types st1 st2 - t1 \c
        st3 st4 - t2) \c
     )",
     ParserResult),
  member((types,Types), ParserResult),
  assertion(member((t1, [st1, st2]), Types)).

test(simple_action) :-
  parse_pddl_domain(
    "(define (domain d) \c
      (:action setx \c
        :parameters (?x - var) \c
        :precondition (cond1 ?x) \c
        :effect (cond2 ?x) \c
      )
    )",
    ParserResult),
    assertion(
      member(
        (actions,[[setx, [(var,['?x'])], cond1('?x'), cond2('?x')]]),
        ParserResult)
    ).
test(action_with_two_parameters) :-
  parse_pddl_domain(
    "(define (domain d) \c
      (:action setx \c
        :parameters (?x ?y - var) \c
        :precondition (cond1 ?x) \c
        :effect (cond2 ?y) \c
      )
    )",
    ParserResult),
    assertion(
      member(
        (actions,[[setx, [(var,['?x', '?y'])], cond1('?x'), cond2('?y')]]),
        ParserResult)
    ).
test(action_with_negated_precondition) :-
  parse_pddl_domain(
    "(define (domain d) \c
      (:action setx \c
        :parameters (?x ?y - var) \c
        :precondition (not (cond1 ?x)) \c
        :effect (cond2 ?y) \c
      )
    )",
    ParserResult),
    assertion(
      member(
        (actions,[[setx,
          [(var,['?x', '?y'])], not(cond1('?x')), cond2('?y')]]),
        ParserResult)
    ).
test(action_with_negated_effect) :-
  parse_pddl_domain(
    "(define (domain d) \c
      (:action setx \c
        :parameters (?x ?y - var) \c
        :precondition (cond1 ?x) \c
        :effect (not (cond2 ?y)) \c
      )
    )",
    ParserResult),
    assertion(
      member(
        (actions,[[setx,
          [(var,['?x', '?y'])], cond1('?x'), not(cond2('?y'))]]),
        ParserResult)
    ).
test(action_with_conjunctive_effect) :-
  parse_pddl_domain(
    "(define (domain d) \c
      (:action setx \c
        :parameters (?x ?y - var) \c
        :precondition (cond1 ?x) \c
        :effect (and (cond1 ?y) (cond2 ?y)) \c
      )
    )",
    ParserResult),
    assertion(
      member(
        (actions,[[setx,
          [(var,['?x', '?y'])], cond1('?x'), and(cond1('?y'),cond2('?y'))]]),
        ParserResult)
    ).
test(action_goto) :-
  parse_pddl_domain(
    "(define (domain d) \c
      (:action goto \c
        :parameters (?from ?to - location) \c
        :precondition (at ?from) \c
        :effect (and (not (at ?from)) (at ?to)) \c
      )
    )",
    ParserResult),
    assertion(
      member(
        (actions,[[goto,
          [(location,['?from', '?to'])], at('?from'),
          and(not(at('?from')),at('?to'))]]),
        ParserResult)
    ).

test(two_actions) :-
  parse_pddl_domain(
    "(define (domain d) \c
      (:action setx \c
        :parameters (?x ?y - var) \c
        :precondition (cond1 ?x) \c
        :effect (cond2 ?y) \c
      )
      (:action resetx \c
        :parameters (?x ?y - var) \c
        :precondition (cond1 ?x) \c
        :effect (not (cond2 ?y)) \c
      )
    )",
    ParserResult),
    assertion(
      member(
        (actions,[
          [setx,
            [(var,['?x', '?y'])], cond1('?x'), cond2('?y')],
          [resetx,
            [(var,['?x', '?y'])], cond1('?x'), not(cond2('?y'))]
          ]),
        ParserResult)
    ).

test(action_with_equality_and_constant) :-
  parse_pddl_domain(
    "(define (domain d) \c
      (:action setx \c
        :parameters (?x - var) \c
        :precondition (= ?x 3) \c
        :effect (cond2 ?x) \c
      )
    )",
    ParserResult),
    assertion(
      member(
        (actions,[[setx, [(var,['?x'])], '?x'='3', cond2('?x')]]),
        ParserResult)
    ).

test(action_with_equality_two_vars) :-
  parse_pddl_domain(
    "(define (domain d) \c
      (:action setx \c
        :parameters (?x ?y - var) \c
        :precondition (= ?x ?y) \c
        :effect (cond2 ?x) \c
      )
    )",
    ParserResult),
    assertion(
      member(
        (actions,[[setx, [(var,['?x', '?y'])], '?x'='?y', cond2('?x')]]),
        ParserResult)
    ).

test(action_with_implication_in_precondition) :-
  parse_pddl_domain(
    "(define (domain d) \c
      (:action setx \c
        :parameters (?x - var) \c
        :precondition (imply (cond1 ?x) (cond2 ?x)) \c
        :effect (cond3 ?x) \c
      )
    )",
    ParserResult),
    assertion(
      member(
        (actions,[
          [setx, [(var,['?x'])],
            imply(cond1('?x'),cond2('?x')), cond3('?x')]]),
        ParserResult)
    ).

test(action_with_conditional_effect) :-
  parse_pddl_domain(
    "(define (domain d) \c
      (:action setx \c
        :parameters (?x ?y - var) \c
        :precondition (cond1 ?x) \c
        :effect (when (cond2 ?y) (cond3 ?x)) \c
      )
    )",
    ParserResult),
    assertion(
      member(
        (actions,
          [[setx, [(var,['?x', '?y'])], cond1('?x'),
            when(cond2('?y'),cond3('?x'))]]),
        ParserResult)
    ).

test(action_with_forall_effect) :-
  parse_pddl_domain(
    "(define (domain d) \c
      (:action setx \c
        :parameters (?x ?y - var) \c
        :precondition (cond1 ?x) \c
        :effect (forall (?y - t1) (cond3 ?y)) \c
      )
    )",
    ParserResult),
    assertion(
      member(
        (actions,
          [[setx, [(var,['?x', '?y'])], cond1('?x'),
            all([(t1,['?y'])],cond3('?y'))]]),
        ParserResult)
    ).

test(load_domain_file) :-
  parse_pddl_domain_file("test_data/domain.pddl", ParserResult),
  assertion(member((domain, blocksworld), ParserResult)).

test(parsing_leaves_no_choicepoint) :-
  assert_domain_file("test_data/domain.pddl"),
  retract_domain_facts.

test(domain_file_with_comments) :-
  parse_pddl_domain_file("test_data/domain.pddl", ParserResult),
  assertion(parse_pddl_domain_file(
    "test_data/domain_with_comments.pddl", ParserResult)
  ).

:- end_tests(pddl_parser).

:- begin_tests(pddl_generator).

test(generate_domain_name_def) :-
  domain_name_def(D, ["(","domain","blub",")"], []),
  domain_name_def(D, S, []),
  assertion(S = ["(","domain","blub",")"]).

test(generate_require_def, [fixme(nondet)]) :-
  require_def(R, ["(",":requirements",":adl",":conditional-effects",")"], []),
  require_def(R, S, []),
  assertion(S = ["(",":requirements",":adl",":conditional-effects",")"]).

test(generate_types_def, [fixme(nondet)]) :-
  types_def(T, ["(",":types","a","b","-","c","d","-","e","f",")"], []),
  types_def(T, S, []),
  assertion(S = ["(",":types","a","b","-","c","d","-","e","f",")"]).

test(generate_predicates_def, [fixme(nondet)]) :-
  predicates_def(R,
    ["(",":predicates","(","at","?x","?y","-","location",")",")"], []),
  predicates_def(R, S, []),
  assertion(S = ["(",":predicates","(","at","?x","?y","-","location",")",")"]).

test(generate_atomic_formula, [fixme(nondet)]) :-
  atomic_formula(R, ["(","at","?from",")"], []),
  atomic_formula(R, S, []),
  assertion(S = ["(","at","?from",")"]).

test(generate_action_def, [fixme(nondet)]) :-
  Def =
    ["(",":action","goto",":parameters","(","?from","?to","-","location",")",
      ":precondition","(","at","?from",")",
      ":effect","(","and","(","not","(","at","?from",")",")",
        "(","at","?to",")",")",")"
    ],
  action_def(R, Def, []),
  action_def(R, S, []),
  assertion(S = Def).

test(parse_and_generate_domain) :-
  parse_pddl_domain_file("test_data/domain.pddl", ParserResult),
  generate_pddl_domain(ParserResult, PDDLString),
  parse_pddl_domain(PDDLString, OtherParserResult),
  assertion(ParserResult = OtherParserResult).

:- end_tests(pddl_generator).
