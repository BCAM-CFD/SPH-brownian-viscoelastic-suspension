# SPH-brownian-viscoelastic-suspension
![alt text](https://github.com/BCAM-CFD/SPH-brownian-viscoelastic-suspension/blob/main/non-colloidal_suspension.gif)

Code to simulate Brownian and non-Brownian suspensions with  Newtonian and viscoelastic matrices

---------------------------- DESCRIPTION ------------------------

 Code to simulate Brownian and non-Brownian suspensions with
 Newtonian and viscoelastic matrices. Thermodynamically consistent
 lubrication forces between close particles are considered.
 Some references:
 - Adolfo  Vázquez-Quesada,  Marco  Ellero.  Numerical  simulations  of
   Brownian suspensions  using Smoothed Dissipative  Particle Dynamics:
   diffusion,  rheology and  microstructure.  Journal of  Non-Newtonian
   Fluid Mechanics, 317, 105044. 2023
 - Adolfo  Vázquez-Quesada;   Pep  Español;  Roger  I.   Tanner;  Marco
   Ellero.  Shear-thickening  of  a  non-colloidal  suspension  with  a
   viscoelastic matrix.  Journal of  Fluid Mechanics.  880, pp.  1070 -
   1094. 2019.
 - Bian, X.,  & Ellero, M.  (2014). A splitting integration  scheme for
   the SPH  simulation of  concentrated particle  suspensions. Computer
   Physics Communications, 185(1), 53-62.
 - Bian,   X.,  Litvinov,   S.,  Qian,   R.,  Ellero,   M.,  &   Adams,
   N. A.  (2012). Multiscale  modeling of  particle in  suspension with
   smoothed dissipative particle dynamics. Physics of Fluids, 24(1).

 This code is  based on the original MCF code  developed by Xin Bian.
 The  current version  has  been developed  in collaboration  between
 Marco Ellero,  leader of the  CFD Modelling and Simulation  group at
 BCAM (Basque Center  for Applied Mathematics) in  Bilbao, Spain, and
 Adolfo Vazquez-Quesada from  the Department of Fundamental Physics
 at UNED, in Madrid, Spain.

 Developers:
     Xin Bian.
     Adolfo Vazquez-Quesada.

 Contact: 
        a.vazquez-quesada@fisfun.uned.es, mellero@bcamath.org
-----------------------------------------------------------------

-------------------------- INSTALLATION -------------------------------

After download MCF/mcf/ folder,
I suppose you have already installed PPM library
in a proper path,
which needs to be given when you configure mcf client.
I also suppose you have installed makedepf90,
which is to check routines dependency of mcf/src/*.F90.

1) run script
 
  ./clean.sh

to clean all old files, which may be machine dependant.

2) run script

  ./bootstrap.sh

to make use of GNU autoconf + automake,
which generates configure script.

3) run configuration.

./configure --prefix=$HOME/MCF/mcf/mcf_install/ FC=ifort MPIFC=mpif90 LDFLAGS=-L$HOME/MCF/ppm/local/lib/ FCFLAGS=-I$HOME/MCF/ppm/local/include/ MAKEDEPF90=$HOME/MCF/ppm/local/bin/makedepf90

to generate Makefile, which is used to compile mcf code.

LDFLAGS: indicate ppm library object files.
FCFLAGS: indicate ppm library header files.


4) run compiler

  make -j 8

to compile mcf code,
-j 8 specify to use 8 processors to accelerate compiling.

5) run installation

  make install

to install mcf executable binary at ...../mcf/mcf_install/ folder.
-----------------------------------------------------------------

-------------- USE -----------------------------------------------

Three input files are required to launch a simulation. Examples of
these input files can be found in the 'mcf_config' directory. The
details of the inputs are explained within those files.
-------------------------------------------------------------------
