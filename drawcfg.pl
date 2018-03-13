:- module(drawcfg,_).

:- use_module(library(lists)).
:- use_module(chclibs(load_simple)).
:- use_module(chclibs(linearize)).
:- use_module(chclibs(common)).




go(F) :-
	main(['-prg',F]).
	
main(ArgV) :-
	cleanup,
	setOptions(ArgV,File,OutS),
	load_file(File),
	drawCfg(OutS),
	close(OutS).
	
drawCfg(S) :-
	write(S, 'digraph {'),
	nl(S),
	writeNodes(S),
	write(S,'}'),
	nl(S).
	

writeNodes(S) :-
	my_clause(A,B,_),
	functor(A,P,N),
	separate_constraints(B,_,Bs),
	(Bs = [] -> 
		Q/M=true/0; 
		member(A1,Bs), functor(A1,Q,M)),
	writeGraphNode(Q/M,S),
	write(S,' -> '),
	writeGraphNode(P/N,S),
	nl(S),
	fail.
writeNodes(_).
	
writeGraphNode(N,S) :-
	write(S,'"'),
	write(S,N),
	write(S,'"').
	
setOptions(ArgV,File,OutS) :-
	get_options(ArgV,Options,_),
	(member(programO(File),Options); 
			write(user_output,'No input file given.'),nl(user_output),fail),
	(member(outputFile(OutFile),Options), open(OutFile,write,OutS); 
				open('cfg_graph.txt',write,OutS)).

	
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

recognised_option('-prg',  programO(R),[R]).
recognised_option('-o',    outputFile(R),[R]).

cleanup :-
	retractall(my_clause(_,_,_)).
		

