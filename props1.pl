% Generate properties for PE using propagation back to the loop head
:- module(props1,_).

:- use_module(chclibs(setops)).
:- use_module(chclibs(canonical)).
:- use_module(chclibs(linearize)).
:- use_module(chclibs(scc)).
:- use_module(library(terms_vars)).
:- use_module(library(ppl)).
:- use_module(library(lists)).
:- use_module(chclibs(program_loader)).
:- use_module(chclibs(ppl_ops)).
:- use_module(chclibs(common)).
:- use_module(backEdges).

%:- include(chclibs(get_options)).

%:- data flag/1.

:- dynamic(prop/2).


main(ArgV) :-
	cleanup,
	get_options(ArgV,Options,_),
	props1:setOptions(Options,File,Entry,OutS),
	backEdges(File,Entry,Bs,Es,Vs),
	write(Bs),nl,
	setdiff(Es,Bs,Es1), 	% remove the back edges
	scc_graph(Es1,Vs,SCCs),
	write(SCCs),nl,
	start_ppl,
	makeAllProps(SCCs,Bs),
	end_ppl,
	showallprops(OutS),
	close(OutS).
	
	
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



cleanup :-
	retractall(prop(_,_)),
	retractall(my_clause(_,_,_)).
	
makeAllProps([(_,[P/N])|SCCs],Es) :-
	makeProps(P/N,Es),
	makeAllProps(SCCs,Es).
makeAllProps([],_).
	
makeProps(P/N,Es) :-
	findClause(P/N,A,Cs,Bs),
	numbervars((A,Cs,Bs),0,_),
	projectBodyProp(Bs,Es,A,Cs),
	fail.
makeProps(_,_).
	
findClause(P/N,A,Cs,Bs) :-
	functor(A,P,N),
	my_clause(A,B,_),
	separate_constraints(B,Cs,Bs).

projectBodyProp([B|Bs],Es,A,Cs) :-
	\+ isBackEdge(A,B,Es),
	!,	
	melt((A,B,Cs),(A1,B1,Cs1)),
	getProp(B1,Cs2),
	append(Cs1,Cs2,Cs3),
	projectProp(Cs3,A1),
	projectBodyProp(Bs,Es,A,Cs).
projectBodyProp([_|Bs],Es,A,Cs) :-
	projectBodyProp(Bs,Es,A,Cs).
projectBodyProp([],_Es,A,Cs) :-
	melt((A,Cs),(A1,Cs1)),
	projectProp(Cs1,A1).
	
projectProp(Cs2,A) :-
	varset((A :- Cs2),Ys),
	A =.. [_|Xs],
	numbervars((A :- Cs2),0,_),
	satisfiable(Cs2,H1),
	length(Ys,N),
	ppl_Polyhedron_add_space_dimensions_and_embed(H1,N),
	setdiff(Ys,Xs,Zs),
	project(H1,Zs,Hp),
	getConstraint(Hp,XCs),
	%checkAssert(prop(A,XCs)).
	assertProps(XCs,A).
	
assertProps([],A) :-
	!,
	checkAssert(prop(A,[])).
assertProps([C|Cs],A) :-
	checkAssert(prop(A,[C])),
	(Cs=[] -> true; assertProps(Cs,A)).

	
getProp(B,Cs) :-
	prop(B,Cs).
	
checkAssert(P) :-
	existingProp(P),
	!.
checkAssert(P) :-
	melt(P,Prop),
    assert(Prop).
	
existingProp(prop(B,C)) :-
	prop(B,C).


showallprops(S) :-
	prop(F,C),
	numbervars((F,C),0,_),
	writeq(S,F), 
	write(S,' :- '), 
	write(S,C),
	write(S,'.'),
	nl(S),
	fail.
showallprops(_).

isBackEdge(A,B,Es) :-
	functor(A,P,N),
	functor(B,Q,M),
	member(P/N-Q/M, Es).