#!/bin/sh

# $1 = input file
# $2 = entry goal

# Control flow refinement. 

PE="."


# constraint specialisation
function spec() {
   local infile=$1
   local query=$2
   local outfile=$3
   #echo "Performing query transformation"
   chclibs-qa $infile -query "$2" -o $resultdir/$f.qa.pl
   #echo "Computing widening thresholds"
   chclibs-thresholds1 -prg $resultdir/$f.qa.pl -a -o wut.props
   #$PE/props -prg "$resultdir/$f.qa.pl" -l 1 -o wut.props
   
   #echo "Computing convex polyhedron approximation of QA clauses"
   chclibs-cpascc -prg $resultdir/$f.qa.pl -cex "traceterm.out"  -withwut -wfunc h79 -o "$resultdir/$f.qa.cha.pl"
   #echo "Specialise clauses"
   chclibs-insertProps -prg $infile -props $resultdir/$f.qa.cha.pl -o $outfile
}

# partial evaluation with prop-based abstraction
function pe() {
   local infile="$1"
   local sourcefile="$2"
   local query="$3"
   $PE/props2 -prg "$infile" -entry "$query" -o "$resultdir/$f.props"
   $PE/peunf_smt_2 -prg "$sourcefile" -entry "$query" -props "$resultdir/$f.props" -o "$resultdir/$f.pe.pl" -neg
}

# draw control flow graph
function drawgraph() {
	local infile="$1"
	local outfile="$2"
	$PE/drawcfg -prg "$infile" -o "$resultdir/cfg.txt"
    dot -Tjpg -o "$outfile" "$resultdir/cfg.txt"
    rm "$resultdir/cfg.txt"
}

#=================

resultdir=$1_output
f=`basename $1`
f=${f%.pl} # remove .pl extension

if (test ! -d $resultdir) then
        mkdir $resultdir
fi

# Main script - 
# draw original cfg
drawgraph "$1" "$resultdir/cfg.jpg"
# Constraint specialisation
spec "$1" "$2" "$resultdir/$f.sp.pl"
# Property-based partial evaluation
pe "$resultdir/$f.sp.pl" "$1" "$2"
# Generate new control flow graph
drawgraph "$resultdir/$f.pe.pl" "$resultdir/cfg.pe.jpg"
rm -f "$resultdir/$f.qa.pl" "$resultdir/$f.sp.pl" "$resultdir/$f.qa.cha.pl"
#rm -f "$resultdir/$f.props" 

 
