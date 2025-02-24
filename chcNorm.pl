:- module(chcNorm,_).

:- use_module(library(lists)).
:- use_module(library(terms_vars)).



:- dynamic(intDomain/0).

main([FIn,FOut,'-int']) :-
	cleanup,
	assert(intDomain),
	open(FIn,read,S1),
	open(FOut,write,S2),
	read(S1,C),
	normaliseClauses(C,S1,S2,Ds,[],0,_),
	(intDomain -> integerTransCls(Ds,Ds1); Ds1=Ds),
	addBoolClauses(Ds1,S2),
	addNeqClauses(S2),
	close(S1),
	close(S2).
	
main([FIn,FOut]) :-
	cleanup,
	open(FIn,read,S1),
	open(FOut,write,S2),
	read(S1,C),
	normaliseClauses(C,S1,S2,Ds,[],0,_),
	(intDomain -> integerTransCls(Ds,Ds1); Ds1=Ds),
	addBoolClauses(Ds1,S2),
	addNeqClauses(S2),
	close(S1),
	close(S2).
	
cleanup :-
	retractall(intDomain).
	
normaliseClauses(end_of_file,_,_,Ds,Ds,K,K) :-
	!.
normaliseClauses(C,S1,S2,Ds0,Ds2,K0,K2) :-
	normaliseClause(C,C1,Ds0,Ds1,K0,K1),
	numbervars(C1,0,_),
	writeClause(C1,S2),
	read(S1,C2),
	normaliseClauses(C2,S1,S2,Ds1,Ds2,K1,K2).
	
%  assume H is already normalised, i.e. p(X1,...,Xn), n >= 0, X1,...,Xn distinct variables

normaliseClause((H :- B), (H :- B4), Ds0,Ds1,K0,K1) :-
	!,
	H =.. [_|Xs],
	removeBooleanVars(B,B1,Xs),
	peBoolExprs(B1,B2,Ds0,Ds1,K0,K1),
	trueFalseSubst(B2,B3),
	(intDomain -> integerTrans(B3,B4); B3=B4).
normaliseClause(H, (H :- true),Ds,Ds,K,K).


removeBooleanVars(B,B,_) :-
	var(B),
	!.
removeBooleanVars((B,Bs),Bs3,Xs) :-
	!,
	simplifyIff(B,B1,Xs),
	removeBooleanVars(Bs,Bs2,Xs),
	conjunct(B1,Bs2,Bs3).
removeBooleanVars(B,B,_).
	
simplifyIff(B,true,Xs) :-
	nonvar(B),
	B = iff(X,Y),
	var(X),
	\+ vmemb(X,Xs),
	!,
	X=Y.
simplifyIff(B,B,_).

trueFalseSubst(X,X) :-
	var(X),
	!.
trueFalseSubst((B,Bs),(B1,Bs1)) :-
	!,
	trueFalseSubst(B,B1),
	trueFalseSubst(Bs,Bs1).
trueFalseSubst(B,B1) :-
	B =.. [P|Xs],
	substTrueFalseList(Xs,Ys),
	B1 =.. [P|Ys].
	
substTrueFalse(X,X) :-
	var(X),
	!.
substTrueFalse(true,1) :-
	!.
substTrueFalse(false,0) :-
	!.
substTrueFalse(T1,T2) :-
	T1 =.. [P|Xs],
	substTrueFalseList(Xs,Ys),
	T2 =.. [P|Ys].
	
substTrueFalseList([],[]).
substTrueFalseList([X|Xs],[Y|Ys]) :-
	substTrueFalse(X,Y),
	substTrueFalseList(Xs,Ys).


peBoolExprs(B,B,Ds,Ds,K,K) :-
	var(B),
	!.
peBoolExprs((B,Bs),Bs3,Ds0,Ds2,K0,K2) :-
	!,
	peBoolExpr(B,B1,Ds0,Ds1,K0,K1),
	peBoolExprs(Bs,Bs2,Ds1,Ds2,K1,K2),
	conjunct(B1,Bs2,Bs3).
peBoolExprs(B,B1,Ds0,Ds1,K0,K1) :-
	peBoolExpr(B,B1,Ds0,Ds1,K0,K1).

peBoolExpr(B,B,Ds,Ds,K,K) :-
	var(B),
	!.
peBoolExpr(Or,B1,Ds0,Ds2,K0,K2) :-
	Or =.. [or|Ts],
	!,
	peBoolExprList(Ts,Ts1,Ds0,Ds1,K0,K1),
	varset(Ts,Xs),
	newPred(or,OrK,K1,K2),
	B1 =.. [OrK|Xs],
	makeOrClauses(B1,Ts1,Ds1,Ds2).
peBoolExpr(And,B1,Ds0,Ds2,K0,K2) :-
	And =.. [and|Ts],
	!,
	peBoolExprList(Ts,Ts1,Ds0,Ds1,K0,K1),
	varset(Ts,Xs),
	newPred(and,AndK,K1,K2),
	B1 =.. [AndK|Xs],
	makeAndClauses(B1,Ts1,Ds1,Ds2).
peBoolExpr(if(B,Then,Else),BIf,Ds0,Ds2,K0,K2) :-
	!,
	peBoolExprList([B,not(B),Then,Else],[B1,NotB1,Then1,Else1],Ds0,Ds1,K0,K1),
	varset(if(B,Then,Else),Xs),
	newPred(if,IfK,K1,K2),
	BIf =.. [IfK|Xs],
	makeIfClauses(BIf,B1,NotB1,Then1,Else1,Ds1,Ds2).
peBoolExpr(not(B),B1,Ds0,Ds1,K0,K1) :-
	%nonvar(B),
	!,
	nnf(B,NotB),
	peBoolExpr(NotB,B1,Ds0,Ds1,K0,K1).
peBoolExpr(iff(B1,B2),BIff,Ds0,Ds2,K0,K2) :-
	!,
	peBoolExprList([B1,B2,not(B1),not(B2)],[B3,B4,B5,B6],Ds0,Ds1,K0,K1),
	varset(iff(B1,B2),Xs),
	newPred(iff,IffK,K1,K2),
	BIff =.. [IffK|Xs],
	makeIffClauses(BIff,B3,B4,B5,B6,Ds1,Ds2).
peBoolExpr(X=V,B1,Ds0,Ds1,K0,K1) :-
	nonvar(V),
	V = if(B,Then,Else),
	!,
	peBoolExpr(if(B,(X=Then),(X=Else)),B1,Ds0,Ds1,K0,K1).
peBoolExpr(B,B,Ds,Ds,K,K).

peBoolExprList([],[],Ds,Ds,K,K).
peBoolExprList([B|Bs],[B1|Bs1],Ds0,Ds2,K0,K2) :-
	peBoolExpr(B,B1,Ds0,Ds1,K0,K1),
	peBoolExprList(Bs,Bs1,Ds1,Ds2,K1,K2).
	
nnf(X,X=0) :-
	var(X),
	!.
nnf(true,false) :-
	!.
nnf(false,true) :-
	!.
nnf(not(X),X) :-
	!.
nnf(And,Or) :-
	And =.. [and|Xs],
	!,
	nnfList(Xs,Ys),
	Or =.. [or|Ys].
nnf(Or,And) :-
	Or =.. [or|Xs],
	!,
	nnfList(Xs,Ys),
	And =.. [and|Ys].
nnf(If,If1) :-
	If =.. [if,B,Then,Else],
	!,
	nnfList([B,Then,Else],[B1,Then1,Else1]),
	If1 =.. [if,B1,Then1,Else1].
nnf(X >= Y, X < Y) :-
	!.
nnf(X =< Y, X > Y) :-
	!.
nnf(X > Y, X =< Y) :-
	!.
nnf(X < Y, X >= Y) :-
	!.
nnf(X = Y, neq(X,Y)) :-
	!.
nnf(neq(X,Y), X = Y) :-
	!.

nnfList([],[]).
nnfList([X|Xs],[Y|Ys]) :-
	nnf(X,Y),
	nnfList(Xs,Ys).
	
makeOrClauses(H,[T|Ts],[(H :- T1)|Ds0],Ds1) :-
	makeArg(T,T1),
	makeOrClauses(H,Ts,Ds0,Ds1).
makeOrClauses(_,[],Ds,Ds). 

makeAndClauses(H,Ts,[(H :- Body)|Ds0],Ds0) :-
	makeArgList(Ts,Ts1),
	list2Conj(Ts1,Body).
makeAndClauses(_,[],Ds,Ds).

makeArg(X,X=1) :-
	var(X),
	!.
makeArg(not(X),X=0) :-
	var(X),
	!.
makeArg(true, 1=1) :-
	!.
makeArg(false, 0=1) :-
	!.
makeArg(iff(X,Y),X=Y) :-
	!.
makeArg(X,X).

makeArgList([],[]).
makeArgList([X|Xs],[Y|Ys]) :-
	makeArg(X,Y),
	makeArgList(Xs,Ys).


makeIfClauses(H,B,NotB,Then,Else,[(H :- (B1,Then1)),(H :- (NotB1,Else1))|Ds0],Ds0) :-
	makeArgList([B,Then,NotB,Else],[B1,Then1,NotB1,Else1]).

makeIffClauses(H,B1,B2,B3,B4,[(H :- A1,A2),(H :- A3,A4)|Ds0],Ds0) :-
	makeArgList([B1,B2,B3,B4],[A1,A2,A3,A4]).
	
integerTrans(X,Y) :-
	integerTransform(X,Y1,Bs,[]),
	list2Conj(Bs,Bs1),
	conjunct(Y1,Bs1,Y).

integerTransform(X,X,Bs,Bs) :-
	var(X),
	!.
integerTransform(X<Y,X+1=<Y,Bs,Bs) :-
	!.
integerTransform(X>Y,X>=Y+1,Bs,Bs) :-
	!.
integerTransform((A mod B), R, [R>=0, R=<B-1, A=_K*B+R|Bs],Bs) :-
	!.
integerTransform(div(A,B), K, [R>=0, R=<B-1, A=K*B+R|Bs],Bs) :-
	!.
integerTransform(T,T1,Ds0,Ds1) :-
	T =.. [P|Xs],
	integerTransformList(Xs,Ys,Ds0,Ds1),
	T1 =.. [P|Ys].
	
integerTransformList([],[],Ds,Ds).
integerTransformList([X|Xs],[Y|Ys],Ds0,Ds2) :-
	integerTransform(X,Y,Ds0,Ds1),
	integerTransformList(Xs,Ys,Ds1,Ds2).

integerTransCls([],[]).
integerTransCls([(H:-B)|Cs],[(H1:-B2)|Cs1]) :-
	melt((H:-B),(H1:-B1)),
	integerTrans(B1,B2),
	numbervars((H1:-B2),0,_),
	integerTransCls(Cs,Cs1).
%--------------

conjunct(true,Bs,Bs) :-
	!.
conjunct(Bs,true,Bs) :-
	!.
conjunct((E,Es),Bs,(E,Bs1)) :-
	!,
	conjunct(Es,Bs,Bs1).
conjunct(B,Bs,(B,Bs)).
	
vmemb(X,[Y|_]) :-
	X == Y,
	!.
vmemb(X,[_|Xs]) :-
	vmemb(X,Xs).
	
writeClause((H :- B), S) :-
	writeq(S,H),
	write(S,' :-'),
	nl(S),
	writeBody(B,S),
	write(S,'.'),
	nl(S).
	
writeBody((B,Bs),S) :-
	!,
	write(S,'    '),
	writeq(S,B),
	write(S,','),
	nl(S),
	writeBody(Bs,S).
writeBody(B,S) :-
	write(S,'    '),
	writeq(S,B).
	
addBoolClauses(Ds,S) :-
	writeClauses(Ds,S).
	
writeClauses([C|Cs],S) :-
	writeClause(C,S),
	writeClauses(Cs,S).
writeClauses([],_).

newPred(P,PK,K0,K1) :-
	name(P,PN),
	name(K0,NK0),
	append(PN,NK0,PKN),
	name(PK,PKN),
	K1 is K0+1.
	
list2Conj([A], (A)):- !.
list2Conj([A|R], (A,R1)):- !,
	list2Conj(R, R1).
list2Conj([], (true)). % meaning true


addNeqClauses(S) :-
	numbervars((X,Y),0,_),
	Cls = [
			(neq(X, Y) :- X < Y),  (neq(X, Y) :- X > Y)
			],
	(intDomain -> integerTransCls(Cls,Cls1); Cls1=Cls),
	writeClauses(Cls1,S).

%! melt(+X,-Y): Obtain the free variable term representation `Y` from
%    ground representation `X`. E.g.,
%
%        ?- melt(a('$VAR'(1),'$VAR'(2),'$VAR'(1)),X).
%       
%        X = a(_A,_,_A) ? 
melt(X,Y) :-
	melt1(X,Y,_Assoclist),
	!.

melt1(X,Y,S) :-
	variable(X),
	!,
	assoc(X,Y,S).
melt1(X,X,_) :-
	atomic(X),
	!.
melt1(X,Y,S) :-
	functor(X,F,N),
	functor(Y,F,N),
	meltargs(1,N,X,Y,S).

meltargs(I,N,_,_,_) :-
	I > N,
	!.
meltargs(I,N,X,Y,S) :-
	arg(I,X,Xi),
	melt1(Xi,Yi,S),
	arg(I,Y,Yi),
	I1 is I+1,
	meltargs(I1,N,X,Y,S).


assoc(X,Y,[assoc(X,Y)|_]) :-
	!.
assoc(X,Y,[_|S]) :-
	assoc(X,Y,S).

%! variable(+X): `X` is a variable in ground representation
variable('$VAR'(_)).	