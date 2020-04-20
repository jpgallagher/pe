#!/bin/sh

# Check safety of CHC clauses

# $1 = input file
# $2 = max number of iterations (default 5)

PE="/Users/jpg/Research/LP/clptools/predabs/pe"


# constraint specialisation
function spec() {
   local infile=$1
   local outfile=$2
   #echo "Performing query transformation"
   chclibs-qa $infile -query false -ans -o $resultdir/$f.qa.pl
   #echo "Computing widening thresholds"
   #$LIB/thresholds1 -prg $resultdir/$f.qa.pl -a -o wut.props
   $PE/props -prg "$resultdir/$f.qa.pl" -l 1 -o wut.props
   
   #echo "Computing convex polyhedron approximation of QA clauses"
   chclibs-cpascc -prg $resultdir/$f.qa.pl -cex "traceterm.out" -v -withwut -wfunc h79 -o $resultdir/$f.qa.cha.pl
   #echo "Specialise clauses"
   chclibs-insertProps -prg $infile -props $resultdir/$f.qa.cha.pl -o $outfile
}

function checksafe() {
    local file=$1
    $PE/counterExample -prg $file -cex "traceterm.out" -qa
    retval=$? 
    # return the result from counterExample1
    if [[ $retval -eq 0 ]]; then
    	echo "UNSAFE" 
    elif [[ $retval -eq 1 ]]; then
    	echo "SAFE"
    elif [[ $retval -eq 2 ]]; then
		echo "UNKNOWN"
    fi
    return $retval
}

function pe() {
    local file=$1
    local outfile=$2
    $PE/props -prg "$file" -l 3 -o "$resultdir/$f.props"
    $PE/peunf_smt_2 -prg "$file" -entry false -props "$resultdir/$f.props" -o "$resultdir/$f.pe.pl" 
}

#=================

resultfile="/Users/jpg/Desktop/results.txt"
resultdir=$1_output
f=`basename $1`
f=${f%.pl} # remove .pl extension

if (test ! -d $resultdir) then
        mkdir $resultdir
fi

echo "Removal of redundant arguments"
chclibs-raf $1 false $resultdir/$f.raf.pl
k=$2
# set default iteration count to 5
if (test -z "$k") then
 k=5
fi
i=1
prog=$resultdir/$f.raf.pl
terminate=0
until [[ $k -eq 0 || $terminate -eq 1 ]];
do
   echo "Iteration" $i
   echo "Specialisation"
   spec "$prog" "$resultdir/$f.sp.pl"
   echo "Checking safety"
   checksafe "$prog"
   ret=$?
   if [ $ret -eq 0 -o $ret -eq 1 ]; then
		terminate=1
   else
		k=`expr $k \- 1`
		i=`expr $i \+ 1`
		echo "Partial evaluation"
		pe "$resultdir/$f.sp.pl" "$resultdir/$f.pe.pl"
		prog="$resultdir/$f.pe.pl"
		
   fi
done 


if [[ $ret -eq 0 ]]; then
    	echo "UNSAFE" >> "$resultfile"
    elif [[ $ret -eq 1 ]]; then
    	echo "SAFE" >> "$resultfile"
    elif [[ $ret -eq 2 ]]; then
		echo "UNKNOWN" >> "$resultfile"
fi

rm widenpoints wut.props traceterm.out


