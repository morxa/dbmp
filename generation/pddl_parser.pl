#! /usr/bin/env swipl

/**
 *  pddl_parser.pl - A Prolog PDDL parser
 *
 *  Created:  Mon 05 Dec 2016 11:33:33 CET
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

:- module(pddl_parser,
    [ parse_pddl_domain/2, parse_pddl_domain_file/2, preprocess_pddl/2,
      assert_domain_facts/1, assert_domain_file/1, retract_domain_facts/0]).


%% preprocess_pddl(*String, -PreprocessedStringList)
%
%  Preprocess the given domain by splitting the domain into a list of operators.
%  Parantheses are considered operators and are always split from the rest of
%  the text, e.g.
%  preprocess("(define (domain d))", ["(","define","(","domain","d",")",")"]).
%  This allows simpler parsing of the domain in the subsequent parsing step.
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
domain_name_def((domain, N)) --> ["(", "domain"], [N], [")"].
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
predicate_list([(PredicateName,PredicateTypes)|Predicates]) -->
  ["("], [PredicateName], typed_list(PredicateTypes), [")"],
  predicate_list(Predicates).
actions_defs((actions,Actions)) --> action_list(Actions).
action_list([]) --> [].
action_list([Action|Actions]) -->
  action_def(Action), action_list(Actions).
action_def([Name,Params,Precondition,Effect]) -->
  ["(", ":action"], [Name],
  [":parameters"], ["("], typed_list(Params), [")"],
  [":precondition"], goal_description(Precondition),
  [":effect"], effect(Effect),
  [")"].

goal_description(Formula) --> atomic_formula(Formula).
goal_description(Goal) -->
  ["(", "and"], goal_description_list(GoalList), [")"],
  { Goal =.. [and|GoalList] }.
goal_description(Goal) -->
  ["(", "or"], goal_description_list(GoalList), [")"],
  { Goal =.. [or|GoalList] }.
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

goal_description_list([Goal]) --> goal_description(Goal).
goal_description_list([Goal|GoalList]) -->
  goal_description(Goal), goal_description_list(GoalList).

effect(Effect) --> atomic_formula(Effect).
effect(not(Effect)) -->
  ["(", "not"], atomic_formula(Effect), [")"].
effect(Effect) -->
  ["(", "and"], effect_list(Effects), [")"],
  { Effect =.. [and|Effects] }.
effect_list([Effect]) --> effect(Effect).
effect_list([Effect|Effects]) --> effect(Effect), effect_list(Effects).

atomic_formula(Formula) -->
  ["("], predicate(Predicate), term_list(Terms), [")"],
  {Formula =.. [Predicate|Terms]}.
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
  { atom_string(TermAtom, Term),
    \+ member(TermAtom, ['(', ')'])}.

requirement(R) --> [R], {string_chars(R,[':'|_])}.
requirements_list([]) --> [].
requirements_list([R|L2]) --> requirement(R), requirements_list(L2).
types_list([]) --> [].
types_list([Type|T2]) --> type(Type), types_list(T2).
type(T) --> [T], { \+ member(T, ["(", ")"]) }.
typed_list([]) --> [].
typed_list([TypedVars|Types]) --> typed_vars(TypedVars), typed_list(Types).
typed_vars((Type, [Var])) --> variable(Var), ["-"], type(Type).
typed_vars((Type,[Var|Vars])) --> variable(Var), typed_vars((Type, Vars)).

variable(VarAtom) -->
  [Var],
  { string_chars(Var,['?'|_]),
    atom_string(VarAtom, Var) }.


%% parse_pddl_domain(*DomainString, -ParsedDomain)
%
%  Parse the given domain string and return the domain parts as a list of pairs,
%  where each pair is of the form (key,value), e.g. (requirements, [":adl"]).
parse_pddl_domain(DomainString, ParsedDomain) :-
  preprocess_pddl(DomainString, PreprocessedDomain),
  once(pddl_domain(ParsedDomain, PreprocessedDomain, [])).

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
  retractall(domain:rquirements(_)),
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
  assertion(member((domain, "blocksworld"), ParserResult)).

test(requirements) :-
  parse_pddl_domain("(define (domain d) (:requirements :adl :action-costs))",
                    ParserResult),
  assertion(member((requirements, [":adl", ":action-costs"]), ParserResult)),
  % we do NOT validate requirements, i.e., we can have arbitrary requirements.
  parse_pddl_domain("(define (domain d) (:requirements :nonexistent-req))",
                    ParserResult2),
  assertion(member((requirements, [":nonexistent-req"]), ParserResult2)).

test(predicates) :-
  parse_pddl_domain("(define (domain d) (:predicates (at ?l - location)))",
                    ParserResult),
  assertion(member((predicates, [("at",[("location",['?l'])])]), ParserResult)),
  parse_pddl_domain("(define (domain d) (:predicates (on ?x ?y - block)))",
                    ParserResult2),
  assertion(member( (predicates, [("on",[("block",['?x', '?y'])])]) ,
                    ParserResult2)),
  parse_pddl_domain("(define (domain d)
    (:predicates (at ?l - location) (on ?x ?y - block)))",
                    ParserResult3),
  assertion(member( (predicates,
                      [("at", [("location", ['?l'])]),
                       ("on",[("block",['?x', '?y'])])]) ,
                    ParserResult3)).

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
        (actions,[["setx", [("var",['?x'])], cond1('?x'), cond2('?x')]]),
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
        (actions,[["setx", [("var",['?x', '?y'])], cond1('?x'), cond2('?y')]]),
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
        (actions,[["setx",
          [("var",['?x', '?y'])], not(cond1('?x')), cond2('?y')]]),
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
        (actions,[["setx",
          [("var",['?x', '?y'])], cond1('?x'), not(cond2('?y'))]]),
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
        (actions,[["setx",
          [("var",['?x', '?y'])], cond1('?x'), and(cond1('?y'),cond2('?y'))]]),
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
        (actions,[["goto",
          [("location",['?from', '?to'])], at('?from'),
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
          ["setx",
            [("var",['?x', '?y'])], cond1('?x'), cond2('?y')],
          ["resetx",
            [("var",['?x', '?y'])], cond1('?x'), not(cond2('?y'))]
          ]),
        ParserResult)
    ).

test(load_domain_file) :-
  parse_pddl_domain_file("test_data/domain.pddl", ParserResult),
  assertion(member((domain, "blocksworld"), ParserResult)).

test(parsing_leaves_no_choicepoint) :-
  assert_domain_file("test_data/domain.pddl"),
  retract_domain_facts.

:- end_tests(pddl_parser).

