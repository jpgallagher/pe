#!/bin/sh

# $1 = input file
# $2 = entry goal
# $3 = choice for widening thresholds
#      1 = narrowing with abstraction
#      2 = concrete narrowing
#      3 = loop head projection
#      4 = loop head projection, individual atomic constraints
#      Default if omitted, no threshold constraints

# Constraint specialisation

# Example usage:
# ./spec.sh myfile.pl  "main(_,_,_,_)" 1
# Results are found in the directory myfile.pl_output

PE="."


# constraint specialisation
function spec() {
   local infile=$1
   local query=$2
   local outfile=$3
   local threshold=$4
   #echo "Performing query transformation"
   chclibs-qa $infile -query "$2" -ans -o $resultdir/$f.qa.pl
   #echo "Computing widening thresholds"
   case "$threshold" in
       1) chclibs-thresholds1 -prg $resultdir/$f.qa.pl -a -o $resultdir/wut.props;
	   ;;
       2) chclibs-thresholds1 -prg $resultdir/$f.qa.pl -o $resultdir/wut.props; 
	   ;;
       3) $PE/props -prg "$resultdir/$f.qa.pl" -l 1 -o $resultdir/wut.props;
	   ;;
	   4) $PE/props -prg "$resultdir/$f.qa.pl" -l 2 -o $resultdir/wut.props;
       ;;
       * ) echo "" > $resultdir/wut.props; 
       ;;
   esac
   
   #echo "Computing convex polyhedron approximation of QA clauses"
   chclibs-cpascc -prg $resultdir/$f.qa.pl -delaywidening 0 narrowiterations 0 -threshold $resultdir/wut.props -cex "traceterm.out"  -withwut -wfunc h79 -o $resultdir/$f.qa.cha.pl
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


spec "$1" "$2" "$resultdir/$f.sp.pl" "$3"

#rm -f $resultdir/$f.qa.pl $resultdir/$f.qa.cha.pl wut.props 
 
