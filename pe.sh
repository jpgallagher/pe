#!/bin/sh
# read -d flag if present for drawing cfp
# $1 - file
# $2 - initial goal
# $3 - property level  

# If $3 not present, then run all property generators

PE="/Users/jpg/Research/LP/clptools/predabs/pe"

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

k=$3
if [[ $k -eq 0 ]]; then
  k=4
  until [[ $k -eq 0 ]];
  do

   # Use props property generator k = 1..4
   $PE/props -prg "$1" -l $k -o "$resultdir/$f$k.props"
   $PE/peunf_smt_2 -prg "$1" -entry "$2" -props "$resultdir/$f$k.props" -o "$resultdir/$f$k.pe.pl" 
  
   if [[ $draw -eq 1 ]]; then
     $PE/drawcfg -prg "$resultdir/$f$k.pe.pl" -o "$resultdir/cfg.txt"
     dot -Tjpg -o "$resultdir/cfg$k.jpg" "$resultdir/cfg.txt"
     rm "$resultdir/cfg.txt"
   fi
   k=`expr $k \- 1`
  done
   # Finally use props1 property generator
   $PE/props1 -prg "$1" -entry "$2" -o "$resultdir/$f.props"
   $PE/peunf_smt_2 -prg "$1" -entry "$2" -props "$resultdir/$f.props" -o "$resultdir/$f.pe.pl" 
   k=""
   if [[ $draw -eq 1 ]]; then
     $PE/drawcfg -prg "$resultdir/$f.pe.pl" -o "$resultdir/cfg.txt"
     dot -Tjpg -o "$resultdir/cfg$k.jpg" "$resultdir/cfg.txt"
     rm "$resultdir/cfg.txt"
   fi
else
   # Use the provided value of k
   $PE/props -prg "$1" -l $k -o "$resultdir/$f$k.props"
   $PE/peunf_smt_2 -prg "$1" -entry "$2" -props "$resultdir/$f$k.props" -o "$resultdir/$f$k.pe.pl" 
  
   if [[ $draw -eq 1 ]]; then
     $PE/drawcfg -prg "$resultdir/$f$k.pe.pl" -o "$resultdir/cfg.txt"
     dot -Tjpg -o "$resultdir/cfg$k.jpg" "$resultdir/cfg.txt"
     rm "$resultdir/cfg.txt"
   fi
fi



