% Specialise a program wrt to a goal. Deterministic unfolding

:- module(peunf_det,_,[dynamic]).

:- use_module(library(streams)).
:- use_module(library(write)).
:- use_module(library(read)).
:- use_module(library(lists)).
:- use_module(library(aggregates)).
:- use_module(chclibs(builtins)).
:- use_module(chclibs(setops)).
:- use_module(chclibs(canonical)).
:- use_module(chclibs(common)).
:- use_module(library(terms_vars)).
:- use_module(library(terms_check)).
:- use_module(chclibs(program_loader)).
:- include(chclibs(get_options)).
:- include(chclibs(messages)).
:- use_module(chclibs(balanced_tree)).
:- use_module(library(read_from_string), [read_from_atom/2]).
:- use_module(library(cyclic_terms)).
:- use_module(cyclic_terms_extra).

:- use_module(engine(hiord_rt)).


:- data flag/1.

:- dynamic(peClause/1).



go(F,Q) :-
	peunf_det:main(['-prg',F, '-entry', Q]).
	
main(ArgV) :-
	peunf_det:cleanup,
    peunf_det:get_options(ArgV,Options,_),
    peunf_det:setOptions(Options,File,Goal,OutS),
	load_file(File),
	functor(Goal,P,N),
	canonical(Goal),
	cyclic_terms_extra:findall(C,(melt(Goal,Goal1),my_clause(Goal1,_,C)),Cs),
	findBackEdges(Cs,[],_Ps,[],Bs,[]),
	extractBackPreds(Bs,BPs),
	pe([Goal],BPs,[version(P/N,Goal)],AllVersions), 
	numberVersions(AllVersions,P/N,1,NVersions),
	showVersionClauses(NVersions,OutS),
	close(OutS).	


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

cleanup :-
	retractall(peClause(_)).

pe([A|Gs],BPs,Versions0,Versions2) :-
	cyclic_melt(A,H),
	resultants(H,BPs,Cls),
	versionClauses(Cls,A,VCls),
	storeVersionClauses(VCls),
	newVersions(VCls,Versions0,Versions1,NewGs,Gs),
	pe(NewGs,BPs,Versions1,Versions2).
pe([],_,Vs,Vs).

resultants(A,BPs,Cls) :-
	cyclic_terms_extra:findall((A :- R),(
			my_clause(A,B,_),
			unfoldForward(B,BPs,R),
			cyclic_terms_extra:numbervars((A,R),0,_)),
		Cls).

unfoldForward([A=B|Bs],BPs,R) :-
	!,
	A=B,
	unfoldForward(Bs,BPs,R).	
unfoldForward([B|Bs],BPs,[B|R]) :-
	builtin(B),
	!,
	unfoldForward(Bs,BPs,R).
unfoldForward([B|Bs],BPs,R) :-
	determinate(B,[_]), 	% matches 1 clause
	%\+ member(C,BPs),
	!,
	my_clause(B,Body,_),
	evalGuard(Body,Body1),
	unfoldForward(Body1,BPs,R1),
	unfoldForward(Bs,BPs,R2),
	append(R1,R2,R).
unfoldForward([B|Bs],BPs,[B|R]) :-
	unfoldForward(Bs,BPs,R).
unfoldForward([],_,[]).

determinate(B,Cs) :-
	cyclic_terms_extra:findall(C,(my_clause(B,Bs,C),evalGuard(Bs,_)),Cs),
	length(Cs,N),
	N=<1.
	
evalGuard([X=Y|Bs],Bs1) :-
	!,
	X=Y,
	evalGuard(Bs,Bs1).
evalGuard([X==Y|Bs],Bs1) :-
	cyclic_terms_extra:ground(X),
	cyclic_terms_extra:ground(Y),
	!,
	X==Y,
	evalGuard(Bs,Bs1).
evalGuard([X\==Y|Bs],Bs1) :-
	cyclic_terms_extra:ground(X),
	cyclic_terms_extra:ground(Y),
	!,
	X\==Y,
	evalGuard(Bs,Bs1).
evalGuard([B|Bs],[B|Bs1]) :-
	evalGuard(Bs,Bs1).
evalGuard([],[]).

versionClauses([],_,[]).
versionClauses([(A :- Bs)|Cls],Goal,[(atom(A,Goal) :- VBs)|VCls]) :-
	bodyVersions(Bs,VBs),
	versionClauses(Cls,Goal,VCls).

bodyVersions([],[]).
bodyVersions([B|Bs],[atom(B,B1)|Bs1]) :-
	abstractVersion(B,B1),
	bodyVersions(Bs,Bs1).

abstractVersion(B,A) :-
	cyclic_melt(B,A),
	cyclic_terms_extra:numbervars(A,0,_).	

newVersions([(_ :- Bs)|VCls],Versions0,Versions2,Gs0,Gs2) :-
	collectVersions(Bs,Versions0,Versions1,Gs0,Gs1),
	newVersions(VCls,Versions1,Versions2,Gs1,Gs2).
newVersions([],Vs,Vs,Gs,Gs).

collectVersions([atom(A,Goal)|Bs],Vs0,Vs1,Gs0,Gs1) :-
	functor(A,P,N),
	member(version(P/N,Goal),Vs0),
	!,
	collectVersions(Bs,Vs0,Vs1,Gs0,Gs1).
collectVersions([atom(A,Goal)|Bs],Vs0,Vs1,Gs0,Gs1) :-
	functor(A,P,N),
	%write_cyclic(A),nl,
	collectVersions(Bs,[version(P/N,Goal)|Vs0],Vs1,Gs0,[Goal|Gs1]).
collectVersions([],Vs,Vs,Gs,Gs).

storeVersionClauses([]).
storeVersionClauses([(H :- Bs)|VCls]) :-
	(cyclic_term((H,Bs)) -> 
		uncycle_term((H,Bs),C), assert(peClause(cyclic(C)));
		assert(peClause((H,Bs)))),
	storeVersionClauses(VCls).

write_cyclic(Goal) :-
	cyclic_term(Goal),
	!,
	uncycle_term(Goal,T),
	write(T).
write_cyclic(Goal) :-
	write(Goal).
showVersionClauses(NVersions,S) :-
	peClause(C),
	(C=cyclic(C1) -> 
		recycle_term(C1,(H,Bs));
		C=(H,Bs)),
	atomRename(H,NVersions,A),
	bodyRename(Bs,NVersions,Bs1),
	list2conj(Bs1,Body),
	writeq(S,A),
	write(S,' :- '),
	writeq(S, Body),
	write(S,'.'),
	nl(S),
	fail.
showVersionClauses(_,_).


atomRename(atom(A,_),_,A) :-
	builtin(A),
	!.
atomRename(atom(A,Goal),NVersions,A2) :-
	functor(A,P,N),
	member(nversion(P/N,Goal,P1),NVersions),
	cyclic_melt(Goal,A1),
	cyclic_terms_extra:varset(A1,Zs),
	A2=..[P1|Zs],
	A=A1,
	!.
atomRename(atom(A,Ids),_,_) :-
	write('Cannot find version '),
	write(atom(A,Ids)),
	nl,
	fail.
	
bodyRename([],_,[]).
bodyRename([B|Bs],NVersions,[B1|Bs1]) :-
	atomRename(B,NVersions,B1),
	bodyRename(Bs,NVersions,Bs1).


numberVersions([version(P/N,Goal)|AllVersions],P/N,K,[nversion(P/N,Goal,P)|NVersions]) :-
	!, % initial goal not renamed
	numberVersions(AllVersions,P/N,K,NVersions).
numberVersions([version(Q/M,Goal)|AllVersions],P/N,K,[nversion(Q/M,Goal,Q)|NVersions]) :-
	functor(A,Q,M),
	builtin(A),
	!, % builtins not renamed
	numberVersions(AllVersions,P/N,K,NVersions).
numberVersions([version(Q/M,Goal)|AllVs],P/N,K,[nversion(Q/M,Goal,QK)|NVersions]) :-
	name(K,NK),
	name(Q,QN),
	append(QN,[95,95|NK],QKN),
	name(QK,QKN),
	K1 is K+1,
	numberVersions(AllVs,P/N,K1,NVersions).
numberVersions([],_,_,[]).

list2conj([A],A) :-
	!.
list2conj([],true) :-
    !.
list2conj([A|As],(A,As1)) :-
	list2conj(As,As1).

% Cyclic melt, applies 

cyclic_melt(T,MT) :-
	cyclic_term(T),
	!,
	uncycle_term(T,U),
	cmelt(U,UM),
	recycle_term(UM,MT).
cyclic_melt(T,MT) :-
	melt(T,MT).
	

cmelt(X,Y) :-
    cmelt1(X,Y,_Assoclist),
    !.

cmelt1(X,X,_) :-
	var(X),
	!.
cmelt1('$VAR'(N),Y,S) :-
    !,
    assoc('$VAR'(N),Y,S).
cmelt1(X,X,_) :-
    atomic(X),
    !.
cmelt1(X,Y,S) :-
    functor(X,F,N),
    functor(Y,F,N),
    cmeltargs(1,N,X,Y,S).


cmeltargs(I,N,_,_,_) :-
    I > N,
    !.
cmeltargs(I,N,X,Y,S) :-
    arg(I,X,Xi),
    cmelt1(Xi,Yi,S),
    arg(I,Y,Yi),
    I1 is I+1,
    cmeltargs(I1,N,X,Y,S).


assoc(X,Y,[assoc(X,Y)|_]) :-
    !.
assoc(X,Y,[_|S]) :-
    assoc(X,Y,S).

%! melteach(+Xs,-Ys): apply `melt/2` for each element
cmelteach([],[]).
cmelteach([X|Xs],[Y|Ys]) :-
    cmelt(X,Y),
    cmelteach(Xs,Ys).
    
% Dependency graph

findBackEdges([P|Ps],M0,M3,Anc,Bs0,Bs3) :-
	successors(P,Ss),
	getBackEdges(Ss,P,Anc,Bs0,Bs1),
	marking(Ss,M0,M1,Ss1),
	findBackEdges(Ss1,[P|M1],M2,[P|Anc],Bs1,Bs2),
	findBackEdges(Ps,[P|M2],M3,Anc,Bs2,Bs3).
findBackEdges([],M,M,_,Bs,Bs).

extractBackPreds([(_-P)|Bs],Ps1) :-
	extractBackPreds(Bs,Ps),
	insertElement(Ps,P,Ps1).
extractBackPreds([],[]).

insertElement(Ps,P,Ps) :-
	member(P,Ps),
	!.
insertElement(Ps,P,[P|Ps]).

successors(C,Ss) :-
	setof(C1, [H,B,B1]^(
			my_clause(H,Bs,C),
			bodyPred(Bs,B),
			my_clause(B,B1,C1)),
			Ss),
	!.
successors(_,[]).

bodyPred([B|_],B) :-
	hasDef(B).
bodyPred([B|_],B) :-
	\+ constraint(B,_),
	\+ builtin(B).
bodyPred([_|Bs],B) :-
	bodyPred(Bs,B).

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
	
hasDef(B) :-
	my_clause(B,_,_),
	!.