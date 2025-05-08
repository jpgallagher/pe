# PE: Partial evaluation for Constrained Horn clauses (pure Prolog)

PE is a tool for partially evaluating Constrained Horn clauses.

## Programming 

PE is written in Ciao and is interfaced with Parma Polyhedra
library and Yices SMT solver for handling constraints.  

## Requirements

[Ciao](https://github.com/ciao-lang/ciao) 1.16 or newer (installed
from git repository with `./ciao-boot.sh local-install`)

The following dependendencies (including third-party code) need to be
installed automatically:

* [Ciao bindings](https://github.com/ciao-lang/ciao_ppl) for [Parma Polyhedra Library](https://bugseng.com/products/ppl/) (`ciao get ciao_ppl`)
* [Ciao bindings](https://github.com/jfmc/ciao_yices) for [Yices SMT solver](https://yices.csl.sri.com/) (`ciao get github.com/jfmc/ciao_yices`)
* [CHCLibs](https://github.com/bishoksan/chclibs) (`ciao get github.com/bishoksan/chclibs`)


Note:  Installing RAHFT (`ciao get github.com/bishoksan/RAHFT`) includes installation of all the above dependencies, and the Yices and PPL libraries.

* [dot](https://www.graphviz.org) for drawing control flow graphs.

## Partial evaluation

PE is carried out with respect to a fixed set of properties.  Properties are generated using the props.pl or props1.pl programs

* See shell script pe.sh

## Control Flow Refinement

PE is used as the central component in a control-flow refinement algorithm.

* See shell script cfr.sh

## Verification of safety properties

PE is used in an iterative algorithm for checking safety of a set of CHCs.

* See shell script safe.sh

## Constraint specialisation

(Does not use PE).  Strengthens constraints in a set of CHCs with respect to an entry goal.
adding invariants to the clause bodies.

* See shell script spec.sh

