#!/bin/bash

set +e
set +u

PREFIX=/scratch/prefix-mcf/

./configure --prefix=${PREFIX} FC=gfortran MPIFC=${PREFIX}/bin/mpif90 \
    LDFLAGS=-L${PREFIX}/lib/ FCFLAGS="-I${PREFIX}/include -pg -g -O2" \
    MAKEDEPF90=${PREFIX}/bin/makedepf90 

make -j 8
