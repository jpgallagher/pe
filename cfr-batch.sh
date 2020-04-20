#!/bin/sh

# Run control flow refinement on all .pl files in a directory
PE="."

for file in $1/*.pl
do
   entry=`echo $($PE/findEntry $file)`
   $PE/peinv.sh $file $entry
done
