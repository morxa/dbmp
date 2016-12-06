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

:- module(pddl_parser, [parse_pddl_domain/2, preprocess_pddl/2]).



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
  action_defs(Actions).
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
% TODO: add action definitions. Currently, we only accept domains with no
% actions.
action_defs((actions, [])) --> [].
requirement(R) --> [R], {string_chars(R,[':'|_])}.
requirements_list([]) --> [].
requirements_list([R|L2]) --> requirement(R), requirements_list(L2).
types_list([]) --> [].
types_list([Type|T2]) --> type(Type), types_list(T2).
type(T) --> [T].
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
