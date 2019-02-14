:- module(backEdges,_,[dynamic]).

:- use_module(chclibs(program_loader)).
:- use_module(chclibs(builtins)).
:- use_module(chclibs(common)).
:- use_module(library(lists)).
:- use_module(library(write)).
:- use_module(library(read)).
:- use_module(library(streams)).
:- use_module(library(aggregates)).
:- use_module(library(read_from_string), [read_from_atom/2]).

:- include(chclibs(get_options)).

:- data flag/1.

main(ArgV) :-
    backEdges:get_options(ArgV,Options,_),
    backEdges:setOptions(Options,File,Entry,OutS),
    backEdges(File,Entry,Bs,Es,Vs),
    write(OutS,Bs),
	write(OutS,'.'),
	nl(OutS),
	write(OutS,Es),
	write(OutS,'.'),
	nl(OutS),
	write(OutS,Vs),
	write(OutS,'.'),
	nl(OutS),
	close(OutS).
    
backEdges(File,Entry,Bs,Es,Vs) :-
	load_file(File),
	functor(Entry,P,N),
	dependency_graph(Es,Vs),
	findBackEdges(Es,[P/N],[],_,[],Bs,[]).

recognised_option('-prg',  program(R),[R]).
recognised_option('-o',    outputFile(R),[R]).
recognised_option('-entry',entry(Q),[Q]).

	
setOptions(Options,File,Goal,OutS) :-
	(member(program(File),Options); 
			write(user_output,'No input file given.'),
			nl(user_output), 
			fail),
	(member(entry(Q),Options), convertQueryString(Q,Goal); 
			write(user_output,'No goal given or invalid goal.'),
			nl(user_output), 
			fail),
	(member(outputFile(OutFile),Options), open(OutFile,write,OutS); 
			OutS=user_output).
			

convertQueryString(Q,Q1) :-
	read_from_atom(Q,Q1).

	
findBackEdges(Es,[P|Ps],M0,M3,Anc,Bs0,Bs3) :-
	successors(Es,P,Ss),
	getBackEdges(Ss,P,Anc,Bs0,Bs1),
	marking(Ss,M0,M1,Ss1),
	findBackEdges(Es,Ss1,[P|M1],M2,[P|Anc],Bs1,Bs2),
	findBackEdges(Es,Ps,M2,M3,Anc,Bs2,Bs3).
findBackEdges(_,[],M,M,_,Bs,Bs).

extractBackPreds([(_-P)|Bs],Ps1) :-
	extractBackPreds(Bs,Ps),
	insertElement(Ps,P,Ps1).
extractBackPreds([],[]).

insertElement(Ps,P,Ps) :-
	member(P,Ps),
	!.
insertElement(Ps,P,[P|Ps]).

successors(Es,P/N,Ss) :-
	setof(Q/M, 
			member(P/N-Q/M,Es),
			Ss),
	!.
successors(_,_,[]).


getBackEdges([],_,_,Bs,Bs).
getBackEdges([Q|Qs],P,Anc,[P-Q|Bs0],Bs1) :-
	member(Q,[P|Anc]),
	!,
	getBackEdges(Qs,P,Anc,Bs0,Bs1).
getBackEdges([_|Qs],P,Anc,Bs0,Bs1) :-
	getBackEdges(Qs,P,Anc,Bs0,Bs1).

marking([],M,M,[]).
marking([Q|Qs],M0,M1,Ss) :-
	member(Q,M0),
	!,
	marking(Qs,M0,M1,Ss).
marking([Q|Qs],M0,M1,[Q|Ss]) :-
	marking(Qs,M0,M1,Ss).
			
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicate dependency graph
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dependency_graph(Es,Vs) :-
	(setof(P/N-Q/M, [H,Bs,B,C,BC]^(
			my_clause(H,Bs,C),
			functor(H,P,N),
			member(B,Bs),
			\+ constraint(B,BC),
			functor(B,Q,M)
			),
			Es) -> true; Es = []),
	(setof(A, [X,Y,H,B,C,P,N]^(
			(member(X-Y,Es),
			(A=X; A=Y);
			my_clause(H,B,C),
			functor(H,P,N),
			A = P/N)
			),
			Vs) -> true; Vs = []).