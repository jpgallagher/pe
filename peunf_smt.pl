% Specialise a program wrt to a goal and a set of properties

%:- module(peunf_smt,[main/1],[]).
:- module(peunf_smt,_,[]).

:- use_module(library(write)).
:- use_module(library(read)).
:- use_module(library(dynamic)).
:- use_module(library(lists)).
:- use_module(library(aggregates)).
:- use_module(chclibs(builtins)).
:- use_module(chclibs(setops)).
:- use_module(chclibs(canonical)).
:- use_module(chclibs(common)).
:- use_module(chclibs(linearize)).
:- use_module(library(terms_vars)).
:- use_module(chclibs(yices2_sat)).
:- use_module(ciao_yices(ciao_yices_2)).
:- use_module(chclibs(ppl_ops)).
:- use_module(chclibs(timer_ciao)).
:- use_module(chclibs(program_loader)).
:- include(chclibs(get_options)).
:- include(chclibs(messages)).
:- use_module(library(read_from_string), [read_from_atom/2]).


:- data flag/1.

:- dynamic(prop/2).
:- dynamic(peClause/3).
:- dynamic(preserveInitPred/0).



go(F,Q,Props) :-
	peunf_smt:main(['-prg',F, '-entry', Q, '-props',Props]).
	
main(ArgV) :-
	peunf_smt:cleanup,
    peunf_smt:get_options(ArgV,Options,_),
    peunf_smt:setOptions(Options,File,Goal,OutS),
	load_file(File),
	functor(Goal,P,N),
	findBackEdges([P/N],[],Ps,[],Bs,[]),
	extractBackPreds(Bs,BPs),
	unfoldablePreds(Ps,BPs,Us),
	start_time,
	yices_init,
	start_ppl,
	proplist(L,_),
	pe([(Goal:-[])],L,Us,[version(P/N,[])],AllVersions), % assume that the initial goal is unconstrained
	numberVersions(AllVersions,P/N,1,NVersions),
	%end_time(user_output),
	showVersionClauses(NVersions,L,OutS),
	close(OutS),
	yices_exit,
	end_ppl.	


recognised_option('-prg',  program(R),[R]).
recognised_option('-o',    outputFile(R),[R]).
recognised_option('-props',propFile(R),[R]).
recognised_option('-entry',entry(Q),[Q]).
recognised_option('-init', preserveInitPred,[]).

	
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
			OutS=user_output),
	(member(propFile(PFile),Options), readPropFile(PFile); 
			true),
	(member(preserveInitPred,Options), assert(preserveInitPred); 
			true).
			

convertQueryString(Q,Q1) :-
	read_from_atom(Q,Q1).


cleanup :-
	retractall(prop(_,_)),
	retractall(peClause(_,_,_)),
	retractall(preserveInitPred).
	
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

successors(P/N,Ss) :-
	setof(Q/M, [H,C,B]^(
			functor(H,P,N),
			my_clause(H,B,C),
			bodyPred(B,Q/M)),
			Ss),
	!.
successors(_,[]).

bodyPred([B|_],P/N) :-
	hasDef(B),
	functor(B,P,N).
bodyPred([B|_],P/N) :-
	\+ constraint(B,_),
	\+ builtin(B),
	functor(B,P,N).
bodyPred([_|Bs],Q) :-
	bodyPred(Bs,Q).

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
	
detPred(P/N) :-
	functor(A,P,N),
	findall(C,my_clause(A,_,C),[_]).	
	

unfoldablePreds([],_,[]).
unfoldablePreds([P|Ps],BPs,[P|Us]) :-
	\+ member(P,BPs),
	detPred(P),
	(preserveInitPred -> \+ isInitPred(P); true),
	!,
	unfoldablePreds(Ps,BPs,Us).
unfoldablePreds([_|Ps],BPs,Us) :-
	unfoldablePreds(Ps,BPs,Us).
	
hasDef(B) :-
	my_clause(B,_,_),
	!.

isInitPred(P/_) :-
	atom_concat(init,_,P).
	
pe([(A :- Ids)|Gs],L,Us,Versions0,Versions2) :-
	versionConstraints(A,Ids,L,Cs),
	resultants(A,Cs,Us,Cls),
	versionClauses(Cls,Ids,L,VCls),
	storeVersionClauses(VCls),
	%showPeClauses,
	newVersions(VCls,Versions0,Versions1,NewGs,Gs),
	pe(NewGs,L,Us,Versions1,Versions2).
pe([],_,_,Vs,Vs).

versionConstraints(_,[init(Cs)],_,Cs) :-
	!.
versionConstraints(A,Ids,L,Cs) :-
	functor(A,F,N),
	getIds(F/N,L,HIs),
	selectIds(Ids,HIs,Hs1),
	intersectionConstraints(Hs1,Cs).
	
resultants(A,Cs,Us,Cls) :-
	functor(A,P,N),
	functor(A1,P,N),
	findall(((A1 :- R),Trans),(
		my_clause(A1,B,C),
		traceTerm(B,Ts),
		unfoldForward(B,Us,R,Ts,Qs),
		Trace =.. [C|Ts],
		ftaTransition(Trace,A1,Qs,Trans),	% generate FTA transition but not used for now
		%write(Trans),nl,
		true),
		Cls0),
	feasibleClauses(Cls0,P/N, Cs,Cls).
	
traceTerm(B,Trace) :-
	separate_constraints(B,_,Bs),
	length(Bs,N),
	length(Trace,N).
	
ftaTransition(Trace,A,Qs,(L :- Qs)) :-
	A =.. [P|_],
	L =.. [P,Trace],
	numbervars(Trace,0,_).
	
unfoldForward([B|Bs],Us,[B1|R],Trs,Qs) :-
	constraint(B,B1),
	!,
	unfoldForward(Bs,Us,R,Trs,Qs).
unfoldForward([B|Bs],Us,R,[Tr|Trs],Qs) :-
	functor(B,P,N),
	member(P/N,Us),
	!,
	my_clause(B,Body,C1),
	traceTerm(Body,T1),
	unfoldForward(Body,Us,R1,T1,Qs1),
	Tr =.. [C1|T1],
	unfoldForward(Bs,Us,R2,Trs,Qs2),
	append(Qs1,Qs2,Qs),
	append(R1,R2,R).
unfoldForward([B|Bs],Us,[B|R],[T|Ts],[Q|Qs]) :-
	B =..[P|_],
	Q =.. [P,T],
	unfoldForward(Bs,Us,R,Ts,Qs).
unfoldForward([],_,[],[],[]).
	
feasibleClauses([],_,_,[]).
feasibleClauses([((A :-B),Trans)|Cls0],P/N,Cs,[((A :- Cs3,NLCs,Bs),Trans)|Cls]) :-
	separate_constraints(B,Cs1,Bs),
	linearConstraints(Cs1,LCs,NLCs),
	functor(H,P,N),
	numbervars(H,0,_),
	melt((H,Cs),(A,HCs)),
	append(HCs,LCs,Cs2),
	numbervars((A,B),0,_),
	satisfiable(Cs2),
	!,
	simplify(Cs2,Cs3),	% needed to remove trivial constraints
	feasibleClauses(Cls0,P/N,Cs,Cls).
feasibleClauses([_|Cls0],P/N,Cs,Cls) :-	
	feasibleClauses(Cls0,P/N,Cs,Cls).
	
linearConstraints([],[],[]).
linearConstraints([C|Cs],[C|LCs],NLCs) :-
	linear_constraint(C),
	!,
	linearConstraints(Cs,LCs,NLCs).
linearConstraints([C|Cs],LCs,[C|NLCs]) :-
	linearConstraints(Cs,LCs,NLCs).

versionClauses([],_,_,[]).
versionClauses([((A :- Cs,NLCs,Bs),_)|Cls],Ids,L,[(atom(A,Ids) :- Cs,NLCs,VBs)|VCls]) :-
	bodyVersions(Bs,Cs,L,VBs),
	versionClauses(Cls,Ids,L,VCls).

bodyVersions([],_,_,[]).
bodyVersions([B|Bs],Cs,L,[atom(B,Ids)|Bs1]) :-
	abstractVersion(B,Cs,L,Ids),
	bodyVersions(Bs,Cs,L,Bs1).
		
abstractVersion(B,Cs,L,Ids) :-
	melt((B,Cs),(B1,Cs1)),
	varset(B1,Xs),
	numbervars((Xs,Cs1),0,_),
	predicate_abstract(B1,Cs1,L,Ids).
	
newVersions([(_ :- _,_,Bs)|VCls],Versions0,Versions2,Gs0,Gs2) :-
	collectVersions(Bs,Versions0,Versions1,Gs0,Gs1),
	newVersions(VCls,Versions1,Versions2,Gs1,Gs2).
newVersions([],Vs,Vs,Gs,Gs).

collectVersions([atom(A,Ids)|Bs],Vs0,Vs1,Gs0,Gs1) :-
	functor(A,P,N),
	member(version(P/N,Ids),Vs0),
	!,
	collectVersions(Bs,Vs0,Vs1,Gs0,Gs1).
collectVersions([atom(A,Ids)|Bs],Vs0,Vs1,Gs0,Gs1) :-
	functor(A,P,N),
	collectVersions(Bs,[version(P/N,Ids)|Vs0],Vs1,Gs0,[(A:-Ids)|Gs1]).
collectVersions([],Vs,Vs,Gs,Gs).

storeVersionClauses([]).
storeVersionClauses([(atom(A,Ids) :- Cs,NLCs,Bs)|VCls]) :-
	append(Cs,NLCs,Cs1),
	assert(peClause(atom(A,Ids),Cs1,Bs)),
	storeVersionClauses(VCls).

readPropFile(PFile) :-
	open(PFile,read,S),
	read(S,C),
	readPropFacts(S,C),
	close(S).
	
readPropFacts(_,end_of_file) :-
	!.
readPropFacts(S,(H:-C)) :-
	assert(prop(H,C)),
	read(S,C1),
	readPropFacts(S,C1).
	
proplist(L,K) :-
	findall((A,C),(
		prop(A,C),
		numbervars((A,C),0,_)),
	Ps),
	makePropList(Ps,0,K,[],L).
	
makePropList([],K,K,L,L).
makePropList([(A,C)|Ps],K0,K2,L0,L2) :-
	functor(A,P,N),
	N>0,
	!,
	addProperty(P/N,C,K0,K1,L0,L1),
	makePropList(Ps,K1,K2,L1,L2).
makePropList([_|Ps],K0,K1,L0,L1) :-
	makePropList(Ps,K0,K1,L0,L1).

selectIds([Id|Ids],[C-Id|Hs],[C|Hs1]) :-
	!,
	selectIds(Ids,Hs,Hs1).
selectIds([Id|Ids],[_-Id1|Hs],Hs1) :-
	Id @> Id1,
	!,
	selectIds([Id|Ids],Hs,Hs1).
selectIds([_|Ids],Hs,Hs1) :-
	selectIds(Ids,Hs,Hs1).
selectIds([],_,[]).
	
intersectionConstraints([],[]).
intersectionConstraints([C|Cs],Ds) :-
	intersectionConstraints(Cs,Ds1),
	append(C,Ds1,Ds).

predicate_abstract(Head,F,L,Ids) :-
	functor(Head,P,N),
	predAbstract(P/N,F,L,Ids).

predAbstract(P/N,F,Props,Ids) :-
	member(pred(P/N,LPs),Props),
	!,
	selectProps(LPs,F,Ids).
predAbstract(_,_,_,[]).

selectProps([],_,[]).
selectProps([C1-Id|LPs],F,[Id|Ids]) :-
	peunf_smt:contains(C1,F),
	!,
	selectProps(LPs,F,Ids).
selectProps([_|LPs],F,Ids) :-
	selectProps(LPs,F,Ids).	
	
satisfiable(Cs) :-
	melt(Cs,MCs),
	varset(MCs,Vs),
	linearConstraints(MCs,LCs,_),
	numbervars(Vs,0,_),
	yices_vars(Vs,real,Ws),
	yices_sat(LCs,Ws).
	
showVersionClauses(NVersions,L,S) :-
	writeVersions(S, NVersions, L),
	nl(S),
	showVersionClauses2(NVersions,S).
	

writeVersions(S,[nversion(Q/M,Ids,P1)|Vs],L) :-
	getIds(Q/M,L,HIs),
	selectIds(Ids,HIs,Hs1),
	intersectionConstraints(Hs1,Cs),
	functor(A,Q,M),
	A =.. [_|Xs],
	A1 =.. [P1|Xs],
	numbervars((A1,Cs),0,_),
	write(S,'% '),
	write(S, Ids),
	write(S, ': '),
	write(S,(A1 :- Cs)),
	nl(S),
	writeVersions(S,Vs,L).
writeVersions(_,[],_).
		
showVersionClauses2(NVersions,S) :-
	peClause(H,Cs,Bs),
	atomRename(H,NVersions,A),
	bodyRename(Bs,NVersions,Bs1),
	append(Cs,Bs1,B),
	list2conj(B,Body),
	writeq(S,A),
	write(S,' :- '),
	writeq(S, Body),
	write(S,'.'),
	nl(S),
	fail.
showVersionClauses2(_,_).


atomRename(atom(A,Ids),NVersions,A1) :-
	functor(A,P,N),
	A =.. [P|Xs],
	member(nversion(P/N,Ids,P1),NVersions),
	A1 =.. [P1|Xs],
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
	
showPeClauses :-
	peClause(H,Cs,Bs),
	write((H :- Cs,Bs)),
	write('.'),
	nl,
	fail.
showPeClauses.

getIds(P/N,L,HIs) :-
	member(pred(P/N,HIs), L),
	!.
getIds(_,_,[]).

addProperty(P/N,C,K0,K1,[pred(P/N,Props)|Ps0],[pred(P/N,Props1)|Ps0]) :-
	!,
	addPredProps(Props,C,K0,K1,Props1).
addProperty(P/N,C,K0,K1,[PredProps|Ps0],[PredProps|Ps1]) :-
	addProperty(P/N,C,K0,K1,Ps0,Ps1).
addProperty(P/N,C,K0,K1,[],[pred(P/N,[C-K0])]) :-
	K1 is K0+1.

addPredProps([],C,K0,K1,[C-K0]) :-
	K1 is K0+1.
addPredProps([C-Id|Props],Cp,K0,K1,[C-Id|Props1]) :-
	addPredProps(Props,Cp,K0,K1,Props1).
	
	
contains(C1,C2) :-
	melt([C2,neg(C1)],E),
	varset(E,Vs),
	numbervars(Vs,0,_),
	yices_vars(Vs,real,Ws),
	yices_unsat(E,Ws).
	
simplify(Cs,Cs1) :-
	makePolyhedron(Cs,H),
	getConstraint(H,Cs1).

numberVersions([version(P/N,[])|AllVersions],P/N,K,[nversion(P/N,[],P)|NVersions]) :-
	!, % initial goal not renamed
	numberVersions(AllVersions,P/N,K,NVersions).
numberVersions([version(Q/M,Ids)|AllVersions],P/N,K,[nversion(Q/M,Ids,QK)|NVersions]) :-
	name(K,NK),
	name(Q,QN),
	append(QN,[95, 95,95|NK],QKN),
	name(QK,QKN),
	K1 is K+1,
	numberVersions(AllVersions,P/N,K1,NVersions).
numberVersions([],_,_,[]).

list2conj([A],A) :-
	!.
list2conj([],true) :-
    !.
list2conj([A|As],(A,As1)) :-
	list2conj(As,As1).

