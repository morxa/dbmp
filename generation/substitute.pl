#! /usr/bin/env swipl

/**
 *  substitute.pl - Substitute terms in formulas
 *
 *  Created:  Fri 09 Dec 2016 13:04:37 CET
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

:- module(substitute, [substitute/4, substitute/5]).

%% substitute(+Old, +Terms, +New, -NewTerms)
%
%  Replace all occurrences of Old in Terms by New, giving NewTerms.
%  This replaces terms recursively, i.e.
%  substitute(a, [p(a)], b, L) gives L = p(b).
substitute(Old, Terms, New, NewTerms) :-
  substitute(Old, Terms, New, true, NewTerms).


%% substitute(+Old, +Terms, +New, +Constraint, -NewTerms)
%
%  Replace all occurrences of Old in Terms by New if Constraint is true for the
%  term to be substituted, giving NewTerms.
%  This calls Constraint(Term) for each term that is a candidate to be
%  substituted. Constraint must be callable, i.e. sufficiently instantiated.
%  As an example, Constraint can be used to restrict substitution to a certain
%  type of variable. However, any other constraint can be used.
substitute(_, [], _, _, []) :- !.
substitute(Old, [Term|Terms], New, Constraint, [New|NewTerms]) :-
  Old = Term,
  call(Constraint, Term),
  % Cut here, otherwise the non-substituted list will succeed, too.
  !,
  substitute(Old, Terms, New, NewTerms).
substitute(Old, [Term|Terms], New, Constraint, [NewTerm|NewTerms]) :-
  \+ atom(Term),
  Term =.. Subterms,
  substitute(Old, Subterms, New, Constraint, NewSubterms),
  !,
  NewTerm =.. NewSubterms,
  substitute(Old, Terms, New, Constraint, NewTerms).
substitute(Old, [Term|Terms], New, Constraint, [Term|NewTerms]) :-
  substitute(Old, Terms, New, Constraint, NewTerms).

%% substitute_list(+Terms, +Substitutions, +Constraint, -NewTerms)
%
%  The same as substitute/5, but use the list of Substitutions. Substitutions is
%  a list of pairs, where the first element is the old term and the second
%  element is the new term.
substitute_list(Terms, [], _, Terms).
substitute_list(Terms, [(Old,New)|Substitutions], Constraint, NewTerms) :-
  substitute(Old, Terms, New, Constraint, SubstitutedTerms),
  substitute_list(SubstitutedTerms, Substitutions, Constraint, NewTerms).

%% substitute_list(+Terms, +Substitutions, -NewTerms)
%
%  The same as substitute_list/4, but without a constraint, i.e., the constraint
%  is always true.
substitute_list(Terms, Substitutions, NewTerms) :-
  once(substitute_list(Terms, Substitutions, true, NewTerms)).

%% true(?Any)
%  Auxiliary predicate that is always true.
true(_).

:- begin_tests(substitute).
test(substitute_simple) :-
  substitute(a, [a,b], c, [c,b]).

test(substitute_args) :-
  substitute(a, [p(a), p(p(a))], b, [p(b), p(p(b))]).

test(substitute_predicate) :-
  substitute(a, [a(b)], b, [b(b)]).

test(substitute_predicate_args) :-
  substitute(a, [a(a)], b, [b(b)]).

init_location_types :-
  assertz(domain:type_of_object(room, kitchen)),
  assertz(domain:type_of_object(location, kitchen)),
  assertz(domain:type_of_object(location, hall)).

test(
  substitute_with_constraint,
  [ setup(init_location_types),
    cleanup(retractall(type_of_object(_,_)))
  ]
) :-
  substitute(_, [goto(kitchen,hall)], location, domain:type_of_object(room),
    [goto(location,hall)]).
test(substitute_list) :-
  substitute_list([a,p(b),p(b(a))], [(a,c),(b,d)], [c,p(d),p(d(c))]).
:- end_tests(substitute).
