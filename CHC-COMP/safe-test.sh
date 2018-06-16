#!/bin/sh

resultfile="/Users/jpg/Desktop/results.txt"
tlimit="1.0m"
echo "Run benchmarks in $1\n\n" >> "/Users/jpg/Desktop/results.txt"

shopt -s nullglob
for file in $1/*.{pl,horn}
do
   echo "`basename $file`" >> "$resultfile"
   START=$(date +%s)
   gtimeout "$tlimit" sh safe.sh $file 30
   case $? in
			"124") 
				echo "TIMEOUT "$tlimit"" >> "$resultfile"
				;;
			"137")
				echo "TIMEOUT "$tlimit"" >> "$resultfile"
				;;
			"1")
				echo "TERMINATED BEFORE TIMEOUT" >> "$resultfile"
				;;
			*)  END=$(date +%s)
   				DIFF=$(( $END - $START ))
				echo "TIME: "$DIFF" secs" >> "$resultfile"
				;;
		esac
   echo "================\n" >> "$resultfile"
done
shopt -u nullglob
rm  widenpoints wut.props traceterm.out
