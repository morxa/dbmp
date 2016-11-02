#! /usr/bin/env swipl

/**
 *  simplify.pl - Simplify formulas
 *
 *  Created:  Tue 01 Nov 2016 13:43:25 CET
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

:- module(simplify, [simplify/2]).


%% simplify(*Term, -SimplifiedTerm)
%
%  Simplify the given term. Naturally, this is an incomplete simplifier, but it
%  tries to capture most easy simplifications:
%  - not(true) -> false
%  - not(false) -> true
%  - not(not(Term)) -> Term
%  - and(...,false,...) -> false
%  - or(...,true,...) -> true
%  - and() -> true
%  - or() -> false
%  - and(Term) -> Term
%  - or(Term) -> Term
%  - and(t1,...,Tn,true,Tn+1,...) -> and(T1,...,Tn,Tn+1,...)
%  - or(t1,...,Tn,false,Tn+1,...) -> or(T1,...,Tn,Tn+1,...)
%  - and(and(...),...) -> and(...)
%  - or(or(...),...) -> or(...)
%  - and(...,Term,...,Term,...) -> and(...,Term,...)
%  - or(...,Term,...,Term,...) -> or(...,Term,...)
%  - and(Term,not(Term)) -> false
%  - or (Term,not(Term)) -> true
%  - impl(Cond,Term) -> or(not(Cond),Term)
%  - and(or(T1,...,Tn,T,Tn+1,...),or(T1,...,Tn,not(T),Tn+1,...),...)
%     -> and(or(T1,...,Tn,Tn+1,...),...)
%  - and(T,or(...,Ti,not(T),Ti+1,...)) -> and(T,or(...,Ti,Ti+1,...))
%  - and(not(T),or(...,Ti,T,Ti+1,...)) -> and(T,or(...,Ti,Ti+1,...))
%
%  simplify/2 never fails; if the term cannot be simplified, it stays the same.

%  Call simplify_or_fail/2 on Term and if the simplification fails, set the
%  resulting term to the input term.
simplify(Term, SimplifiedTerm) :-
  simplify_or_fail(Term, SimplifiedTerm) -> true
;
  SimplifiedTerm = Term.

%% simplify_or_fail(*Term, -SimplifiedTerm)
%
%  Predicate used by simplify/2. simplify_or_fail/2 does the actual
%  simplification. It fails if the term cannot be simplified.

% not(true) -> false
simplify_or_fail(not(true), false).
% not(false) -> true
simplify_or_fail(not(false), true).
% not(not(Term)) -> Term
simplify_or_fail(not(not(Term)), Term).
% and() -> true
simplify_or_fail(and(), true).
% or() -> false
simplify_or_fail(or(), false).
% and(Term) -> Term
simplify_or_fail(and(Term), SimplifiedTerm) :-
  simplify(Term, SimplifiedTerm).
% or(Term) -> Term
simplify_or_fail(or(Term), SimplifiedTerm) :-
  simplify(Term, SimplifiedTerm).
% and(...,Term,...,Term,...) -> and(...,Term,...)
% or(...,Term,...,Term,...) -> or(...,Term,...)
simplify_or_fail(Term, SimplifiedTerm) :-
  Term =.. [Op|SubTerms],
  list_to_set(SubTerms,SimplifiedSubTerms),
  SubTerms \= SimplifiedSubTerms,
  IntermediateSimplifiedTerm =.. [Op|SimplifiedSubTerms],
  simplify(IntermediateSimplifiedTerm, SimplifiedTerm).
% and(t1,...,Tn,true,Tn+1,...) -> and(T1,...,Tn,Tn+1,...)
simplify_or_fail(Term, SimplifiedTerm) :-
  Term =.. [and|SubTerms],
  member(true,SubTerms),
  exclude(=(true),SubTerms,SimplifiedSubTerms),
  IntermediateSimplifiedTerm =.. [and|SimplifiedSubTerms],
  simplify(IntermediateSimplifiedTerm, SimplifiedTerm).
% and(...,false,...) -> false
simplify_or_fail(Term, false) :-
  Term =.. [and|SubTerms],
  member(false,SubTerms).
% and(Term,not(Term)) -> false
simplify_or_fail(Term, false) :-
  Term =.. [and|SubTerms],
  member(SubTerm, SubTerms),
  member(not(SubTerm), SubTerms).
% or(t1,...,Tn,false,Tn+1,...) -> or(T1,...,Tn,Tn+1,...)
simplify_or_fail(Term, SimplifiedTerm) :-
  Term =.. [or|SubTerms],
  member(false,SubTerms),
  exclude(=(false),SubTerms,SimplifiedSubTerms),
  IntermediateSimplifiedTerm =.. [or|SimplifiedSubTerms],
  simplify(IntermediateSimplifiedTerm, SimplifiedTerm).
% or(...,true,...) -> true
simplify_or_fail(Term, true) :-
  Term =.. [or|SubTerms],
  member(true,SubTerms).
% or (Term,not(Term)) -> true
simplify_or_fail(Term, true) :-
  Term =.. [or|SubTerms],
  member(SubTerm, SubTerms),
  member(not(SubTerm), SubTerms).
% impl(Cond,Term) -> or(not(Cond),Term)
simplify_or_fail(impl(Cond,Term), SimplifiedTerm) :-
  simplify(or(not(Cond),Term), SimplifiedTerm).
% simplify subterms
simplify_or_fail(Term, SimplifiedTerm) :-
  Term =.. [Op|Terms],
  member(Op, [and,or]),
  maplist(simplify,Terms,SimplifiedTerms),
  TermOfSimplifiedSubTerms =.. [Op|SimplifiedTerms],
  TermOfSimplifiedSubTerms \= Term,
  simplify(TermOfSimplifiedSubTerms, SimplifiedTerm).
% and(and(...),...) -> and(...)
% or(or(...),...) -> or(...)
simplify_or_fail(Term, SimplifiedTerm) :-
  Term =.. [Op|Terms],
  member(Op, [and,or]),
  flatten_on_op(Op, Terms, FlattenedTerms),
  FlattenedTerm =.. [Op|FlattenedTerms],
  Term \= FlattenedTerm,
  simplify(FlattenedTerm, SimplifiedTerm).
%  and(or(T1,...,Tn,T,Tn+1,...),or(T1,...,Tn,not(T),Tn+1,...),...)
%   -> and(or(T1,...,Tn,Tn+1,...),...)
simplify_or_fail(Term, SimplifiedTerm) :-
  Term =.. [and|SubTerms],
  % Determine 2 subterms that are the same except for one literal l.
  % SubTerm1 must contain l, SubTerm2 must contain not(l).
  append(TermPrefix1, [SubTerm1|TermSuffix1], SubTerms),
  SubTerm1 =.. [or|SubSubTerms1],
  member(SubTerm2, SubTerms),
  SubTerm2 =.. [or|SubSubTerms2],
  member(SubSubTerm, SubSubTerms1),
  member(not(SubSubTerm), SubSubTerms2),
  % Remove the literal from the subterms.
  exclude(=(SubSubTerm), SubSubTerms1, FilteredSubSubTerms1),
  exclude(=(not(SubSubTerm)), SubSubTerms2, FilteredSubSubTerms2),
  % The resulting filtered subterms must be identical, but not necessarily in
  % the same order. The order of the term containing the positive literal is
  % retained.
  subset(FilteredSubSubTerms1, FilteredSubSubTerms2),
  subset(FilteredSubSubTerms2, FilteredSubSubTerms1),
  % Construct the new term.
  NewSubTerm =.. [or|FilteredSubSubTerms1],
  append(TermPrefix1, [NewSubTerm|TermSuffix1], NewSubTerms),
  % Remove the second subterm from the newly constructed term.
  exclude(=(SubTerm2), NewSubTerms, FilteredNewSubTerms),
  FilteredTerm =.. [and|FilteredNewSubTerms],
  simplify(FilteredTerm, SimplifiedTerm).
% and(T,or(...,Ti,not(T),Ti+1,...)) -> and(T,or(...,Ti,Ti+1,...))
% and(not(T),or(...,Ti,T,Ti+1,...)) -> and(T,or(...,Ti,Ti+1,...))
simplify_or_fail(Term, SimplifiedTerm) :-
  Term =.. [and|SubTerms],
  member(SubTermToRemove,SubTerms),
  append(TermPrefix, [SubTermToSimplify|TermSuffix], SubTerms),
  SubTermToSimplify =.. [or|SubSubTerms],
  negated_term(SubTermToRemove, NegatedSubTermToRemove),
  member(NegatedSubTermToRemove, SubSubTerms),
  exclude(=(NegatedSubTermToRemove), SubSubTerms, FilteredSubSubTerms),
  SimplifiedSubTerm =.. [or|FilteredSubSubTerms],
  append(TermPrefix, [SimplifiedSubTerm|TermSuffix], FilteredSubTerms),
  FilteredTerm =.. [and|FilteredSubTerms],
  simplify(FilteredTerm, SimplifiedTerm).

% flatten_on_op(+Op, +Terms, -FlattenedTerms)
%
% Given Op in [and,or]), remove any nested operators. For Op = 'and', flatten
% any term and(SubTerms) to SubTerms. This is used by simplify/2 to remove
% nested operators such as and(and(a,b),c).
flatten_on_op(_, [], []).
flatten_on_op(Op, [Term|RestTerms], FlattenedTerms) :-
  flatten_on_op(Op, RestTerms, FlattenedRestTerms),
  ( Term =.. [Op|SubTerms] ->
    append(SubTerms, FlattenedRestTerms, FlattenedTerms)
  ;
    FlattenedTerms = [Term|FlattenedRestTerms]
  ).

% negated_term(?Term1, ?Term2)
%
% True if Term1 is the negation of Term2.
negated_term(not(Term),Term).
negated_term(Term,not(Term)).
