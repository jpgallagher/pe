# PE: Partial evaluation for Constrained Horn clauses (pure Prolog)

PE is a tool for partially evaluating Constrained Horn clauses.

## Programming 

PE is written in Ciao and is interfaced with Parma polyhedra
library and Yices SMT solver for handling constraints.  

## Requirements

[Ciao](https://github.com/ciao-lang/ciao) 1.16 or newer (installed
from git repository with `./ciao-boot.sh local-install`)

The following dependendencies (including third-party code) need to be
installed automatically:

* [Ciao bindings](https://github.com/ciao-lang/ciao_ppl) for [Parma Polyhedra Library](https://bugseng.com/products/ppl/) (`ciao get ciao_ppl`)
* [Ciao bindings](https://github.com/jfmc/ciao_yices) for [Yices SMT solver](https://yices.csl.sri.com/) (`ciao get github.com/jfmc/ciao_yices`)
* [CHCLibs](https://github.com/bishoksan/chclibs) (`ciao get github.com/bishoksan/chclibs`)
* [dot](www.graphviz.org) for drawing control flow graphs
