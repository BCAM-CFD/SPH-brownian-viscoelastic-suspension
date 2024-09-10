#! /bin/bash

if [ -z "$1" ]; then
    dirname=.
else
    dirname=$1
fi

rm ${dirname}/punto_particles_no_colloid_v2.dat 

for F in ${dirname}/mcf_*particles*.out; do
    
    printf "%s" $F
    perl  ~/MCF/mcf/scripts/particles_2d_no_colloid_v2.perl $F 'punto_particles_no_colloid_v2.dat'
    printf "\n" >> ${dirname}/punto_particles_no_colloid_v2.dat

done
