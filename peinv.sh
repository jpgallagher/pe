#!/bin/sh

# $1 = input file
# $2 = entry goal

# pe with invariants added first

LIB="/Users/jpg/ciao/build/bin"
PE="/Users/jpg/Research/LP/clptools/predabs/pe"


# constraint specialisation
function spec() {
   local infile=$1
   local query=$2
   local outfile=$3
   #echo "Performing query transformation"
   $LIB/qa $infile -query "$2" -ans -o $resultdir/$f.qa.pl
   #echo "Computing widening thresholds"
   $LIB/thresholds1 -prg $resultdir/$f.qa.pl -a -o wut.props
   #$PE/props -prg "$resultdir/$f.qa.pl" -l 1 -o wut.props
   
   #echo "Computing convex polyhedron approximation of QA clauses"
   $LIB/cpascc -prg $resultdir/$f.qa.pl -cex "traceterm.out"  -withwut -wfunc h79 -o $resultdir/$f.qa.cha.pl
   #echo "Specialise clauses"
   $LIB/insertProps -prg $infile -props $resultdir/$f.qa.cha.pl -o $outfile
}

function pe() {
   local infile=$1
   local query=$1
   $PE/props1 -prg "$1" -entry "$2" -o "$resultdir/$f.props"
   $PE/peunf_smt_2 -prg "$1" -entry "$2" -props "$resultdir/$f.props" -o "$resultdir/$f.pe.pl" -neg
}

function drawgraph() {
	local infile=$1
	$PE/drawcfg -prg "$infile" -o "$resultdir/cfg.txt"
    dot -Tjpg -o "$resultdir/cfg$k.jpg" "$resultdir/cfg.txt"
    rm "$resultdir/cfg.txt"
}

#=================

draw=0
while getopts "d" flag
do
   case $flag in
   d) draw=1
      shift
   ;;
   *)
   ;;
   esac
done

resultdir=$1_output
f=`basename $1`
f=${f%.pl} # remove .pl extension

if (test ! -d $resultdir) then
        mkdir $resultdir
fi


spec "$1" "$2" "$resultdir/$f.sp.pl"
pe "$resultdir/$f.sp.pl" "$2"
drawgraph "$resultdir/$f.pe.pl"

 