% Translate PE-d CHCs to CFG form.  
% Optionally use the original file before PE for variable names 
% Option -init S.  S is the initial node of the CFG

:- module(chc2cfg,_,[]).

:- use_module(chclibs(common)).
:- use_module(library(terms_vars)).
:- use_module(chclibs(program_loader)).
:- include(chclibs(get_options)).
:- include(chclibs(messages)).
:- use_module(library(write)).
:- use_module(library(read)).
:- use_module(library(dynamic)).
:- use_module(library(lists)).
:- use_module(library(read_from_string), [read_from_atom/2]).




:- data flag/1.

:- dynamic(trcount/1).
:- dynamic(initNode/1).


	
main(ArgV) :-
	chc2cfg:cleanup,
    chc2cfg:get_options(ArgV,Options,_),
    chc2cfg:setOptions(Options,File,CfgFile,OutS),
    load_file(File),
	showTransitions(CfgFile,OutS),
	close(OutS).	

recognised_option('-prg',  program(R),[R]).
recognised_option('-o',    outputFile(R),[R]).
recognised_option('-init', initNode(N),[N]).
recognised_option('-cfg',  cfgFile(R),[R]).

	
setOptions(Options,File,CfgFile,OutS) :-
	(member(program(File),Options); 
			write(user_output,'No input file given.'),
			nl(user_output), 
			fail),
	(member(outputFile(OutFile),Options), open(OutFile,write,OutS); 
			OutS=user_output),
	(member(initNode(N),Options), convertQueryString(N,N1),functor(N1,Init,_),assert(initNode(Init));
			write(user_output,'No initial node given.'),
			nl(user_output), 
			fail),
	(member(cfgFile(CfgFile),Options); 
			CfgFile=File).
			
convertQueryString(Q,Q1) :-
	read_from_atom(Q,Q1).
			
cleanup :-
	retractall(my_clause(_,_,_)),
	retractall(trcount(_)),
	retractall(initNode(_)).
	
showTransitions(CfgFile,S) :-
	getStateVars(CfgFile, Vs,Vs1),
	write(S,'{'),
	nlIndent(S),
	write(S, 'vars: '),
	write(S, Vs),
	write(S,','),
	nlIndent(S),
	write(S, 'pvars: '),
	write(S, Vs1),
	write(S,','),
	nlIndent(S),
	write(S,'initnode: '),
	initNode(Init),
	write(S,Init),
	write(S,','),
	nlIndent(S),
	write(S, 'transitions: ['),
	nl(S),
	assert(trcount(0)),
	showTransitions2(S,Vs,Vs1),
	write(S,']'),
	nl(S),
	write(S,'}'),
	nl(S).
	
nlIndent(S) :-
	nl(S),
	write(S,'     ').
	
nlIndent2(S) :-
	nl(S),
	write(S,'         ').
	
showGlobals(_NVersions,_S).
	
showTransitions2(S,Vs,Vs1) :-
	%initNode(Init),
	my_clause(Source,B,_),
	separate_constraints(B,Cs,[Target]),
	%functor(Source,F,_),
	%F \== Init,
	nlIndent2(S),
	write(S,'{'),
	Source =.. [P1|Vs],
	Target =.. [P2|Vs1],
	% rename local variables 
	localvars(Cs),
	write(S,'source: '),
	writeq(S,P1),
	write(S,','),
	nlIndent2(S),
	write(S,'target: '),
	writeq(S,P2),
	write(S,','),
	nlIndent2(S),
	retract(trcount(T)),
	write(S, 'name: t'),
	write(S,T),
	write(S,','),
	nlIndent2(S),
	T1 is T+1,
	assert(trcount(T1)),
	write(S, 'constraints: '),
	write(S,'['),
	nl(S),
	writeConstraints(S,Cs),
	nlIndent2(S),
	write(S,']'),
	write(S,'},'),
	nlIndent2(S),
	fail.
showTransitions2(_,_,_).

writeConstraints(S,[C]) :-
	!,
	write(S,'               '),
	write(S,C).
writeConstraints(S,[C|Cs]) :-
	write(S,'               '),
	write(S,C),
	write(S,','),
	nl(S),
	writeConstraints(S,Cs).
	
getStateVars(File,Vs,Vs1):-
	open(File,read,S),
	getTransitionClause(S,C,Ws),
	close(S),
	C = (H :- B),
	conj2List(B,Bs),
	separate_constraints(Bs,_,[B1]), 
	H =.. [_|Vs],
	B1 =.. [_|Vs1],
	unifyNames(Ws).
	
getTransitionClause(S,C,Ws) :-
	read_term(S,C1,[variable_names(Ws1)]),	
	initNode(Init),
	ignoreStartClauses(C1,Init,S,C,Ws1,Ws).

ignoreStartClauses(C1,Init,S,C,_,Ws):-
	C1 = (H :- _),
	functor(H,Init,_),
	!,
	read_term(S,C2,[variable_names(Ws2)]),
	ignoreStartClauses(C2,Init,S,C,Ws2,Ws).
ignoreStartClauses(C,_,_,C,Ws,Ws).


	
unifyNames([]).
unifyNames([VX='$VAR'(X)|Ns]) :-
	(atom_concat('Var',X,VX) -> true; VX=X),
	unifyNames(Ns).
	

/*	
getInitNode(Init) :-
	my_clause(H,B,_),
	functor(H,F,_),
	F == startpoint,
	!,
	separate_constraints(B,_,[B1]),
	functor(B1,Init,_).
*/

localvars(Cs1) :-
	varset(Cs1,Vs),
	renameLocals(Vs,0,'Local').
	
renameLocals([],_,_).
renameLocals(['$VAR'(L)|Vs],K,Loc) :-
	atom_number(AK,K),
	atom_concat(Loc,AK,L),
	K1 is K+1,
	renameLocals(Vs,K1,Loc).
