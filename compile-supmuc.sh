#! /bin/bash

set +e
set +u
source /etc/profile
source /etc/profile.d/modules.sh

module unload mpi.ibm
module load mpi.intel

module unload fortran
module load fortran/intel/14.0

fortrancomp=ifort
suffix=14.0
PREFIX=${HOME}/mcf-${fortrancomp}${suffix}

./configure --prefix=${PREFIX} FC=gfortran MPIFC=mpif90 \
    LDFLAGS=-L${PREFIX}/lib/ FCFLAGS="-I${PREFIX}/include -g -O2" \
    MAKEDEPF90=${PREFIX}/bin/makedepf90 

make -j 8
