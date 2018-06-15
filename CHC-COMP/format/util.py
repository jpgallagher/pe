from __future__ import print_function
from z3 import *
import sys
#import z3


def eprint(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)


class Exc(Exception):
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return repr(self.value)


def is_pred_app(expr):
    return (
        not z3.is_var(expr) and
        z3.is_expr(expr) and
        expr.decl().kind() == z3.Z3_OP_UNINTERPRETED
    )


def make_and(exprs):
    if len(exprs) > 1:
        return z3.And(exprs)
    elif len(exprs) == 1:
        return exprs[0]
    else:
        return z3.BoolVal(True)


def foreach_expr(fn, expr, *args, **kwargs):
    """Applies a given function to each sub-expression of the input expr"""
    _foreach_expr(fn, expr, z3.AstMap(), *args, **kwargs)


def _foreach_expr(fn, expr, visited, *args, **kwargs):
    """Helper function for foreach_expr"""
    if expr in visited:
        return

    visited[expr] = expr

    fn(expr, *args, **kwargs)

    for k in expr.children():
        _foreach_expr(fn, k, visited, *args, **kwargs)


def find_vars(ast):
    """Returns the set of all variables in a given expression"""
    vars = list()

    def add(expr):
        if z3.is_var(expr):
            vars.append(expr)

    foreach_expr(add, ast)

    return sorted(vars, lambda x, y: z3.get_var_index(x) - z3.get_var_index(y))


def quote_symbol_if_needed(symbol):
    for char in symbol:
        if not char.isalnum() or char not in [
            '~' '!' '@' '$' '%' '^' '&' '*' '_' '-' '+' '=' '<' '>' '.' '?' '/'
        ]:
            return '|{}|'.format(symbol)
    return symbol


def write_pred_decl(decl, writer):
    symbol = quote_symbol_if_needed(decl.name())
    writer.write("(declare-fun {} (".format(symbol))
    for n in range(0, decl.arity()):
        writer.write((" {}".format(decl.domain(n).sexpr())))
    writer.write(" ) {})".format(decl.range().sexpr()))

def translate_infix(ous, n, chs, subst,bindings):
    ous.write("(")
    translate_term(ous, chs[0],subst,bindings)
    i = 1
    while i < len(chs):
	ous.write(" %s " % n)
	translate_term(ous, chs[i],subst,bindings)
	i += 1
    ous.write(")")
    
def visitor(e, seen, seen2):
   if e in seen:
		if not is_const(e):
			seen2[e] = True
		return
   seen[e] = True
   yield e
   if is_app(e):
       for ch in e.children():
           for e in visitor(ch, seen, seen2):
               yield e
       return
   if is_quantifier(e):
       for e in visitor(e.body(), seen, seen2):
           yield e
       return
       
def termvar(p,subst):
	for e in subst:
		if eq(e,p):
			return True
	return False 

def translate_term(ous, p, subst, bindings):
    #print(p)
    if is_var(p):
	ous.write("X%d" % get_var_index(p))
    elif p in bindings:
	ous.write("Y_"+str(bindings[p]))
    elif is_const(p) and termvar(p,subst):
	ous.write("X_"+p.sexpr())
    elif is_add(p):
	translate_infix(ous, "+", p.children(),subst,bindings)
    elif is_sub(p):
	translate_infix(ous, "-", p.children(),subst,bindings)
    elif is_mul(p):
	translate_infix(ous, "*", p.children(),subst,bindings)
    elif is_div(p):
	translate_infix(ous, "/", p.children(),subst,bindings)
    elif is_eq(p):
    	chs = p.children()
    	if is_bool(chs[0]):
		ous.write("'iff'")
		ous.write("(")
		i = 0
		while i < len(chs):
			translate_term(ous, chs[i], subst,bindings)
			if i + 1 < len(chs):
				ous.write(", ")
			i += 1
		ous.write(")")
	else:	
		translate_infix(ous, "=", p.children(),subst,bindings)
    elif is_le(p):
	translate_infix(ous, "=<", p.children(),subst,bindings)
    elif is_ge(p):
	translate_infix(ous, ">=", p.children(),subst,bindings)
    elif is_lt(p):
	translate_infix(ous, "<", p.children(),subst,bindings)
    elif is_gt(p):                               
	translate_infix(ous, ">", p.children(),subst,bindings)
    elif is_quantifier(p):
	assert False
    elif is_false(p):
	ous.write("false")
    elif is_true(p):
	ous.write("true")
    elif is_rational_value(p):
	ous.write("%s" % p)
    elif is_int_value(p):
	ous.write("%s" % p)
    elif p is None:
	ous.write("false")
    else:
	ous.write("'"+p.decl().name()+"'")
	chs = p.children()
	if len(chs) == 0:
	    return
	ous.write("(")
	i = 0
	while i < len(chs):
	    translate_term(ous, chs[i], subst,bindings)
	    if i + 1 < len(chs):
		ous.write(", ")
	    i += 1
	ous.write(")")		  
	
def flatten_and(conjs):
    i = 0
    result = []
    while i < len(conjs):
	if is_and(conjs[i]):
	    conjs += conjs[i].children()
	else:
	    result += [conjs[i]]
	i += 1
    return result
    
def write_implication_smt2(implication, pref, false_subst, subst, writer):
    assert implication.decl().kind() == z3.Z3_OP_IMPLIES
    assert len(implication.children()) == 2

    #writer.write("{}(=>\n".format(pref))

    tail = implication.children()[0]
    head = implication.children()[1]
    
    if z3.is_false(head) and false_subst is not None:
        writer.write(false_subst)
    else:
    	translate_term(writer, head, subst, {})
        
    writer.write(":-\n".format(pref))

    assert tail.decl().kind() == z3.Z3_OP_AND
    seen = {}
    seen2 = {}
    i = 0
    for e in visitor(implication,seen,seen2):
    	i = i+1
    tail = [tail]
    tail = flatten_and(tail)
    bindings = {}
    k = 0
    for e in seen2:
    	bindings[e] = k
    	k = k+1
    for e in seen2:
    	if is_bool(e):
    		writer.write(format(pref)+"iff(Y_"+str(bindings[e])+", ")
    	else:
    		writer.write(format(pref)+"Y_"+str(bindings[e])+" = (")
    	idx = bindings[e]
    	del bindings[e]
    	translate_term(writer, e, subst, bindings)
    	if is_bool(e):
    		writer.write("),\n")
    	else:
    		writer.write("),\n")
    	bindings[e] = idx
    #print(bindings)
    i = 0
    while i < len(tail):
	writer.write("    ")
        translate_term(writer, tail[i], subst, bindings)
	if i + 1 < len(tail):
	    writer.write(",\n")
	i += 1
    if len(tail) == 0:
	writer.write("    true")
    writer.write(".\n")
    

def write_clause_smt2(forall, pref, writer):
    if not (z3.is_quantifier(forall) and forall.is_forall()):
        raise Exception(
            "Illegal clause for write_clause_smt2: {}".format(forall.decl())
        )

    implication = forall.body()
    subst = list(reversed([
        z3.Const(
            forall.var_name(n), forall.var_sort(n)
        ) for n in range(0, forall.num_vars())
    ]))
    implication = z3.substitute_vars(implication, * subst)
    #print(subst)
    #for s in subst:
    #	print(s.sort())
    write_implication_smt2(implication, pref + '  ', None, subst, writer)

    #writer.write("\n{})".format(pref))
    
def write_clause_prolog(forall, pref, writer):
    if not (z3.is_quantifier(forall) and forall.is_forall()):
        raise Exception(
            "Illegal clause for write_clause_smt2: {}".format(forall.decl())
        )

    #writer.write("{}(forall (".format(pref))

    #for idx in range(0, forall.num_vars()):
    #    writer.write(" ({} {})".format(
    #        forall.var_name(idx), forall.var_sort(idx).sexpr()
    #    ))

    #writer.write(" ) \n".format(pref))

    implication = forall.body()
    subst = list(reversed([
        z3.Const(
            forall.var_name(n), forall.var_sort(n)
        ) for n in range(0, forall.num_vars())
    ]))

    implication = z3.substitute_vars(implication, * subst)

    write_implication_smt2(implication, pref + '  ', None, subst, writer)

    #writer.write("\n{})".format(pref))


def write_clauses_smt2(declarations, clauses, writer):
    #writer.write('(set-logic HORN)\n\n')
    #for decl in declarations:
    #    write_pred_decl(decl, writer)
    #    writer.write('\n')
    #writer.write('\n')
    for clause in clauses:
        #writer.write('(assert\n')
        write_clause_smt2(clause, '  ', writer)
        #writer.write('\n)\n')
        #writer.write('.')
    writer.write('\n')
    #writer.write('(check-sat)\n')
    #writer.write('(exit)\n')


def write_clauses_datalog(declarations, clauses, writer):
    reserved_exit_point = 'reserved_exit_point'
    writer.write(
        '(declare-rel {} ())\n\n'.format(reserved_exit_point)
    )

    for decl in declarations:
        symbol = quote_symbol_if_needed(decl.name())
        writer.write(
            '(declare-rel {} ('.format(symbol)
        )
        for n in range(0, decl.arity()):
            writer.write((" {}".format(decl.domain(n))))
        writer.write(' ))\n')

    known_vars = set([])
    implications = []
    writer.write('\n')

    for clause in clauses:
        if not (z3.is_quantifier(clause) and clause.is_forall()):
            raise Exception(
                "Illegal clause for write_clause_smt2: {}".format(
                    clause.decl()
                )
            )

        for idx in range(0, clause.num_vars()):
            if (clause.var_name(idx), clause.var_sort(idx)) not in known_vars:
                known_vars.add(
                    (clause.var_name(idx), clause.var_sort(idx))
                )
                writer.write(
                    "(declare-var {} {})\n".format(
                        clause.var_name(idx), clause.var_sort(idx).sexpr()
                    )
                )

        implication = clause.body()
        subst = list(reversed([
            z3.Const(
                clause.var_name(n), clause.var_sort(n)
            ) for n in range(0, clause.num_vars())
        ]))

        implications.append(
            z3.substitute_vars(implication, * subst)
        )

    writer.write('\n')

    for implication in implications:
        writer.write('(rule\n')
        write_implication_smt2(implication, '  ', reserved_exit_point, subst, writer)
        writer.write('\n)\n')

    writer.write(
        '\n\n(query {})\n\n'.format(reserved_exit_point)
    )

