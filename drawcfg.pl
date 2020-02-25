:- module(drawcfg,_).

:- use_module(library(lists)).
:- use_module(chclibs(load_simple)).
:- use_module(chclibs(linearize)).
:- use_module(chclibs(common)).


:- use_module(library(operators)).
:- use_module(engine(runtime_control), [set_prolog_flag/2]).


go(F) :-
	main(['-prg',F]).
	
main(ArgV) :-
	cleanup,
	setOptions(ArgV,File,OutS),
	enable_assrt_syntax,
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
	writeGraphNode(P/N,S),
	write(S,' -> '),
	writeGraphNode(Q/M,S),
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

