#!/bin/bash

set +e
set +u

PREFIX=~/prefix-gfortran-mcf/

./configure --prefix=${PREFIX} FC=gfortran MPIFC=${PREFIX}/bin/mpif90 \
    LDFLAGS=-L${PREFIX}/lib/ FCFLAGS="-I${PREFIX}/include -g -O3" \
    MAKEDEPF90=${PREFIX}/bin/makedepf90 

make -j 8
