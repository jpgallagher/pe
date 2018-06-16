:- module(counterExample,_).

:- use_module(chclibs(common)).
:- use_module(chclibs(linearize)).
:- use_module(chclibs(program_loader)).
:- use_module(chclibs(yices2_sat)).
:- use_module(ciao_yices(ciao_yices_2)).
:- use_module(library(terms_vars)).
:- include(chclibs(get_options)).

:- data flag/1.

:- dynamic(qa/0).
:- dynamic(type/1).

main(ArgV) :-
	counterExample:cleanup,
    counterExample:get_options(ArgV,Options,_),
    counterExample:setOptions(Options,File,CexS),
	unsafe(File,CexS).
	
unsafe(F,S) :-
	read(S,C),
	existsCex(S,C,Cex),
	close(S),
	checkCounterExample(Cex,F).
	
existsCex(_,end_of_file,no) :-
	!.
existsCex(_,safe,no) :-
	!.
existsCex(_,(cex(Cex)),Cex) :-
	!.
existsCex(_,(counterexample(Cex)),Cex) :-
	!.
existsCex(S,_,Cex) :-
	read(S,C1),
	existsCex(S,C1,Cex).
	
checkCounterExample(no,_) :-
	!,
	%write('Program is safe'),
	%nl,
	halt(1).
checkCounterExample(Cex,F) :-
	load_file(F),
	(checkTrace([false],[],[Cex]);
	 checkTrace([false_ans],[],[Cex])
	),
	!,
	yices_exit,
	%write('Program is unsafe'),
	%nl,
	halt(0).
checkCounterExample(_,_) :-
	yices_exit,
	%write('Program might be unsafe'),
	%nl,
	%nl,
	halt(2).
	
checkTrace(A,_,Trace) :-
	andTreeConstraint(A,Trace,Cs),
	feasible(Cs).

andTreeConstraint([],[],[]) :-
	!.
andTreeConstraint([B|Bs],[T|Ts],Cs) :-
	!,
	andTreeConstraint(B,T,Cs1),
	andTreeConstraint(Bs,Ts,Cs2),
	append(Cs1,Cs2,Cs).
andTreeConstraint(A,Trace,Cs) :-
	Trace =.. [C|Ts],
	my_clause(A,B,C),
	separate_constraints(B,Cs1,Bs),
	(qa -> Ts=[_|Ts1]; Ts=Ts1),	% if qa flag, ignore the first trace-term argument
	andTreeConstraint(Bs,Ts1,Cs2),
	append(Cs1,Cs2,Cs).
	
feasible(Cs) :-
	varset(Cs,Vs),
	linearConstraints(Cs,LCs,_),
	numbervars(Vs,0,_),
	yices_init,
	type(T),
	yices_vars(Vs,T,Ws),
	yices_sat(LCs,Ws).
	
linearConstraints([],[],[]).
linearConstraints([C|Cs],[C|LCs],NLCs) :-
	linear_constraint(C),
	!,
	linearConstraints(Cs,LCs,NLCs).
linearConstraints([C|Cs],LCs,[C|NLCs]) :-
	linearConstraints(Cs,LCs,NLCs).
	
recognised_option('-prg',  program(R),[R]).
recognised_option('-cex',  cexFile(R),[R]).
recognised_option('-qa',  qa,[]).
recognised_option('-type',  type(T),[T]).

	
setOptions(Options,File,CexS) :-
	(member(program(File),Options) -> true; 
			write(user_output,'No input file given.'),
			nl(user_output), 
			fail),
	(member(cexFile(OutFile),Options) -> open(OutFile,read,CexS); 
			open("traceterm.out",read,CexS)),
	(member(qa,Options) -> assert(qa); true),
	(member(type(T),Options), member(T,[int,real]) -> assert(type(T)); assert(type(real))).
	
cleanup :-
	retractall(qa),
	retractall(type(_)).
	