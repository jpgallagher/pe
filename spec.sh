#!/bin/sh

# $1 = input file
# $2 = entry goal

# Constraint specialisation

PE="."


# constraint specialisation
function spec() {
   local infile=$1
   local query=$2
   local outfile=$3
   #echo "Performing query transformation"
   chclibs-qa $infile -query "$2" -ans -o $resultdir/$f.qa.pl
   #echo "Computing widening thresholds"
   chclibs-thresholds1 -prg $resultdir/$f.qa.pl -a -o wut.props
   #$PE/props -prg "$resultdir/$f.qa.pl" -l 1 -o wut.props
   
   #echo "Computing convex polyhedron approximation of QA clauses"
   chclibs-cpascc -prg $resultdir/$f.qa.pl -cex "traceterm.out"  -withwut -wfunc h79 -o $resultdir/$f.qa.cha.pl
   #echo "Specialise clauses"
   chclibs-insertProps -prg $infile -props $resultdir/$f.qa.cha.pl -o $outfile
}



#=================

resultdir=$1_output
f=`basename $1`
f=${f%.pl} # remove .pl extension

if (test ! -d $resultdir) then
        mkdir $resultdir
fi


spec "$1" "$2" "$resultdir/$f.sp.pl"

#rm -f $resultdir/$f.qa.pl $resultdir/$f.qa.cha.pl wut.props 
 