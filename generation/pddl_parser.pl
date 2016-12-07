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
  [parse_pddl_domain/2, parse_pddl_domain_file/2, preprocess_pddl/2]).


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

variable(Var) --> [Var], {string_chars(Var,['?'|_])}.


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
