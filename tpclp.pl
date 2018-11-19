:- module(tpclp,_).

:- use_module(chclibs(builtins)).
:- use_module(chclibs(setops)).
:- use_module(chclibs(canonical)).
:- use_module(chclibs(linearize)).
:- use_module(chclibs(common)).
:- use_module(chclibs(ppl_ops)).

:- use_module(library(terms_vars)).
:- use_module(library(sort)).
:- use_module(library(ppl)).
:- use_module(library(lists)).
:- use_module(chclibs(timer_ciao)).

:- use_module(chclibs(load_simple)).

:- dynamic(flag/2).
:- dynamic(fact/3).
:- dynamic(operatorCount/1).
:- dynamic(findCEx/0).



	
	
main(ArgV) :- 
	cleanup,
	%write('Starting analysis'),nl,
	get_options(ArgV,Options,_),
	(member(programO(File),Options); 
			write(user_output,'No input file given.'),nl(user_output)),
	(member(outputFile(OutFile),Options), open(OutFile,write,OutS); 
			OutS=user_output),
	(member(findCEx,Options) -> assert(findCEx); true),
	
	start_time,
	write(File),nl,
	load_file(File),	
	ppl_initialize,
	%ppl_version(Pv),
	%write('PPL version used: '),write(Pv),nl,
	assert(flag(first,0)),
	assert(operatorCount(0)),
	% Compute the model and display it
	iterate,
	%write('Analysis Succeeded'),
	nl,
	end_time(user_output),
	%nl(OutS), showallfacts(OutS), nl(OutS),
	close(OutS),
	ppl_finalize,
	write('Safe'),nl,
	halt(1).

% get_options/3 provided by Michael Leuschel
get_options([],[],[]).
get_options([X|T],Options,Args) :-
   (recognised_option(X,Opt,Values) ->
	  ( append(Values, Rest, T),
	    RT = Rest,
	    Options = [Opt|OT], Args = AT
	  )
   ;
	  (
	    Options = OT,	Args = [X|AT],
	    RT = T
	  )
   ),
   get_options(RT,OT,AT).

recognised_option('-prg',programO(R),[R]).
recognised_option('-o',outputFile(R),[R]).
recognised_option('-cex',findCEx,[]). % termination when counterexample found

cleanup :-
	retractall(fact(_,_,_)),
	retractall(flag(_,_)),
	retractall(operatorCount(_)),
	retractall(findCEx).


iterate :-
	operator,
	fail.
iterate :-
	retract(operatorCount(K)),
	K1 is K+1,
	assert(operatorCount(K1)),
	retractflags(K),
	!,
	%write(K),nl,
	%showallfacts(user_output),
	iterate.
iterate.

operator:-
	operatorCount(K),
	my_clause(Head,B,_),
	(changed(B);flag(first,0)),
	prove(B,Cs,Ds,K),
	Head =.. [_|Xs],
	linearize(Cs,CsLin),
	append(CsLin,Ds,CsDs),
	varset((Head,CsDs),Ys),
	dummyCList(Ys,DCL),
	append(CsDs,DCL,CsL),
	numbervars((Head:-CsL),0,_),
	satisfiable(CsL,H1),
	setdiff(Ys,Xs,Zs),
	project(H1,Zs,Hp),
	checkCounterExample(Head),
	record(Head,Hp,K).

checkCounterExample(Head) :-
	\+ foundCEx(Head).
	
foundCEx(Head) :-	
	findCEx,
	isFalsePred(Head),
	write('Unsafe'),nl,
	halt(0).
	
isFalsePred(false) :-
	!.
isFalsePred(false_ans).


changed([B|_]) :-
	\+ isConstraint(B),
	functor(B,P,N),
	operatorCount(K),
	K1 is K-1,
	flag(P/N,K1),
	!.
changed([_|Bs]):-
	changed(Bs).
	
isConstraint(B) :-
	constraint(B,_).

record(Head,H,K):-
	cond_assert(Head,H,K).
	
	
cond_assert(Head,H,K):-
	\+ alreadyAsserted(Head,H),
	removeSubsumed(Head,H),
	assert(fact(Head,H,K)),
	%write(fact(Head,H,K)),nl,
	raise_flag(Head,K).
	
alreadyAsserted(Head,H) :-
	fact(Head,H1,_), 
	contains(H1,H).
	
removeSubsumed(Head,H) :-
	fact(Head,H1,_),
	contains(H,H1),
	retract(fact(Head,H1,_)),
	fail.
removeSubsumed(_,_).
	
	
retractflags(K) :-
	K1 is K-1,
	(flag(_,K1);flag(first,0)),
	retractall(flag(_,K1)),
	retractall(flag(first,0)).

raise_flag(Head,K):-
	functor(Head,P,N),
	(flag(P/N,K)->true
	;  assert(flag(P/N,K))
	).
	
prove(B,Cs,Ds,K) :-
	K1 is K-1,
	separate_constraints(B,Cs,Bs),
	prove_K(Bs,Bs1,CK,K1),  % use at least one K1-fact
	prove_any(Bs1,Ds1),
	append(CK,Ds1,Ds).

prove_K([],[],[],_) :-
	!.
prove_K(Bs,Bs1,CK,K1) :-
	prove_K_fact(Bs,Bs1,CK,K1).
	
prove_K_fact([B|Bs],Bs,CK,K1):-
	getKfact(B,CK,K1).
prove_K_fact([B|Bs],[B|Cs],CK,K1):-
	prove_K_fact(Bs,Cs,CK,K1).
	
prove_any([],[]).
prove_any([true],[]).
prove_any([B|Bs],Ds):-
	getanyfact(B,CsOld),
	prove_any(Bs,Ds1),
	append(CsOld,Ds1,Ds).
	
getKfact(B,Cs1,K) :-
	functor(B,F,N),
	functor(B1,F,N),
	fact(B1,H,K),	
	ppl_Polyhedron_get_minimized_constraints(H,Cs2),
	melt((B1,Cs2),(B,Cs1)).
	
getanyfact(B,Cs1) :-
	functor(B,F,N),
	functor(B1,F,N),
	fact(B1,H,_),	
	ppl_Polyhedron_get_minimized_constraints(H,Cs2),
	melt((B1,Cs2),(B,Cs1)).

showallfacts(S) :-
	fact(F,H,_),
	ppl_Polyhedron_get_minimized_constraints(H,C),
	write(S,F), write(S,' :- '), write(S,C),nl(S),
	fail.
showallfacts(_).


