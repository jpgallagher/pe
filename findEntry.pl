:- module(findEntry, [main/1, findEntry/1], [assertions, isomodes, doccomments, dynamic]).

%! \title Simple program loader
%
%  \module
%    Load clauses in `my_clause/3` (keeps a unique identifer for each
%    clause). Drops any `:- _` declaration.

:- use_module(library(operators)).
:- use_module(engine(runtime_control), [set_prolog_flag/2]).

:- use_module(library(streams)).
:- use_module(library(read)).
:- use_module(library(write)).


main([F]) :-
	findEntry(F).
	
findEntry(F) :-
	enable_assrt_syntax,
    open(F,read,S),
    lookForEntryDirective(S),
    close(S).

lookForEntryDirective(S) :-
    read(S,C),
    ( C == end_of_file ->
        write(user_output,'No entry directive found')
    ;
    ( entryDir(C,Entry) ->
        write(user_output,Entry)
    ; 
      lookForEntryDirective(S)
    )).

entryDir((:- entry P/N:_), Entry):- 
	functor(Entry,P,N),
	numbervars(Entry,0,_),
	!.
entryDir((:- entry(P/N)), Entry):- 
	functor(Entry,P,N),
	numbervars(Entry,0,_).

% added to handle standard operators in files with assertions

enable_assrt_syntax :-
    % flags for hiord
    set_prolog_flag(read_hiord, on),
    % operators for assertions
    op(975, xfx,(=>)),
    op(978, xfx,(::)),
    op(1150, fx,(decl)),
    op(1150,xfx,(decl)),
    op(1150, fx,(pred)),
    op(1150,xfx,(pred)),
    op(1150, fx,(func)),
    op(1150,xfx,(func)),
    op(1150, fx,(prop)),
    op(1150,xfx,(prop)),
    op(1150, fx,(modedef)),
    op(1150,xfx,(modedef)),
    op(1150, fx,(calls)),
    op(1150,xfx,(calls)),
    op(1150, fx,(success)),
    op(1150,xfx,(success)),
    op(1150, fx,(test)),
    op(1150,xfx,(test)),
    op(1150, fx,(texec)),
    op(1150,xfx,(texec)),
    op(1150, fx,(comp)),
    op(1150,xfx,(comp)),
    op(1150, fx,(entry)),
    op(1150,xfx,(entry)),
    op(1150, fx,(exit)),
    op(1150,xfx,(exit)),
    % operators for regtypes
    op(1150, fx,(regtype)),
    op(1150,xfx,(regtype)).