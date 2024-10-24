:- module(_, [], []).

:- use_module(library(dict)).
:- use_module(engine(term_basic), [cyclic_term/1]).
:- use_module(library(write), [numbervars/3]).
:- use_module(library(terms_vars), [term_variables/2, varset/2]).
:- use_module(library(sort), [sort/2]).

% TODO: dic_lookup is using ==/2; ==/2 is doing more work than needed;
%   so this implementation is not as efficient as it could be

% ---------------------------------------------------------------------------

:- export(varset/2).
varset(X, Vars) :-
    cyclic_term(X), !,
    term_variables_cy(X, Vars0),
    sort(Vars0, Vars).
varset(X, Vars) :-
    terms_vars:varset(X, Vars).

% ---------------------------------------------------------------------------

:- export(term_variables/2).
term_variables(X, Vars) :-
    cyclic_term(X), !,
    term_variables_cy(X, Vars).
term_variables(X, Vars) :-
    terms_vars:term_variables(X, Vars).

term_variables_cy(X, Vars) :-
    termvars_cy(X, _SeenT, _SeenV, Vars, []).

termvars_cy(T,_SeenT,SeenV,Vars,Vars0) :-
    var(T), !,
    dic_lookup(SeenV, T, seen, New),
    ( New = new -> Vars = [T|Vars0]
    ; Vars = Vars0
    ).
termvars_cy(T,_SeenT,_SeenV,Vars,Vars0) :-
    atomic(T), !,
    Vars = Vars0.
termvars_cy(T,SeenT,SeenV,Vars,Vars0) :-
    functor(T, _, N),
    dic_lookup(SeenT, T, seen, New),
    ( New = old -> Vars = Vars0
    ; functor(T, _, N),
      termvars_cy_args(1,N,T,SeenT,SeenV,Vars,Vars0)
    ).

termvars_cy_args(I, N, _, _, _, Vars, Vars0) :- I > N, !,
    Vars = Vars0.
termvars_cy_args(I, N, T, SeenT, SeenV, Vars, Vars0) :-
    I1 is I+1,
    arg(I, T, Ta),
    termvars_cy(Ta, SeenT, SeenV, Vars, Vars1),
    termvars_cy_args(I1, N, T, SeenT, SeenV, Vars1, Vars0).

% ---------------------------------------------------------------------------

:- export(ground/1).
ground(X) :-
    cyclic_term(X), !,
    term_variables_cy(X, []). % fail early as soon as we get one variable
ground(X) :-
    term_typing:ground(X).
    
% ---------------------------------------------------------------------------

:- export(numbervars/3).
numbervars(X, N0, N) :-
    cyclic_term(X), !,
    term_variables_cy(X, Vars),
    write:numbervars(Vars, N0, N).
numbervars(X, N0, N) :-
    write:numbervars(X, N0, N).

% ---------------------------------------------------------------------------

:- use_package(hiord).
:- use_module(library(cyclic_terms)).
:- use_module(library(aggregates)).

:- export(findall/3).
:- meta_predicate findall(?,goal,?).
findall(Tmpl, Goal, Xs) :-
    aggregates:findall(Tmpl2, uncycle(Goal,Tmpl,Tmpl2), Xs0),
    recycle_sols(Xs0, Xs).

:- meta_predicate uncycle(goal,?,?).
uncycle(G,Tmpl,Tmpl2) :-
    call(G),
    ( cyclic_term(Tmpl) -> Tmpl2 = cy(Tmpl1), uncycle_term(Tmpl, Tmpl1)
    ; Tmpl2 = nocy(Tmpl)
    ).

recycle_sols([], []).
recycle_sols([X|Xs], [Y|Ys]) :-
    recycle_sol(X, Y),
    recycle_sols(Xs, Ys).

recycle_sol(nocy(X), X).
recycle_sol(cy(X0), X) :- recycle_term(X0, X).

