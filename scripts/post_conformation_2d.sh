#! /bin/bash

if [ -z "$1" ]; then
    dirname=.
else
    dirname=$1
fi

rm ${dirname}/post_conformation.dat 

for F in ${dirname}/mcf_conformation*.out; do
    
    printf "%s" $F    
    perl  ~/MCF/mcf/scripts/conformation_2d.perl $F 'post_conformation.dat'
done
