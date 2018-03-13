#!/bin/sh
# read -d flag if present for drawing cfp
# $1 - file
# $2 - initial goal

BACKWARDS="/Users/jpg/Research/LP/clptools/predabs/backwards-analysis/src"

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

$BACKWARDS/props -prg "$1" -o "$resultdir/$f.props"
$BACKWARDS/peunf -prg "$1" -entry "$2" -props "$resultdir/$f.props" -o "$resultdir/$f.pe.pl" 

if [[ $draw -eq 1 ]]; then
  $BACKWARDS/drawcfg -prg "$resultdir/$f.pe.pl" -o "$resultdir/cfg.txt"
  dot -Tjpg -o "$resultdir/cfg.jpg" "$resultdir/cfg.txt"
  rm "$resultdir/cfg.txt"
fi
