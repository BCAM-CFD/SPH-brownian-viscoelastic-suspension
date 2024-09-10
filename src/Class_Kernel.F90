      MODULE Class_Kernel
        !----------------------------------------------------
      	!  Class      : Kernel
	!----------------------------------------------------
      	!
      	!  Purpose    :
	!> \brief       Variables and corresponding operations
        !>              for kernel quantities.
      	!>	   	
        !>              The variable memebers are public 
        !
        !  Remarks    : Since there is a 'SAVE' after 'IMPLICIT NONE',
        !               all procedures using this Module sharing the
        !               same copy of this Module.
        !
      	!
      	!  References :
     	!
      	!  Revisions  : 0.1 03.03.2009, original version.
        !
	!----------------------------------------------------
        ! This code is  based on the original MCF code  developed by Xin Bian.
        ! The  current version  has  been developed  in collaboration  between
        ! Marco Ellero,  leader of the  CFD Modelling and Simulation  group at
        ! BCAM (Basque Center  for Applied Mathematics) in  Bilbao, Spain, and
        ! Adolfo Vazquez-Quesada from  the Department of Fundamental Physics
        ! at UNED, in Madrid, Spain.
        !
        ! Developers:
        !     Xin Bian.
        !     Adolfo Vazquez-Quesada.
        !
        ! Contact: a.vazquez-quesada@fisfun.uned.es
        !          mellero@bcamath.org
        !----------------------------------------------------
   
        USE mcf_header

        IMPLICIT NONE
        SAVE 

        !----------------------------------------------------
        !  num_dim     : number of dimension.
        !  kernel_type : type of kernel.
        !  cut_off     : cut off.
        !  h           : smoothing length.
        !----------------------------------------------------
        
        TYPE Kernel
           
           PRIVATE
           
           INTEGER              :: num_dim
           INTEGER              :: kernel_type
           REAL(MK)             :: h
           REAL(MK)             :: cut_off
           REAL(MK)             :: coef
           REAL(MK)             :: coef_grad
           
        END TYPE Kernel
        
        INTERFACE kernel_new
           MODULE PROCEDURE kernel_init
        END INTERFACE

        INTERFACE kernel_kernel
           MODULE PROCEDURE kernel_kernel_w
           MODULE PROCEDURE kernel_kernel_w_gradw
        END INTERFACE
        
        INTERFACE kernel_kernel_quintic_spline
           MODULE PROCEDURE kernel_kernel_quintic_spline_w
           MODULE PROCEDURE kernel_kernel_quintic_spline_w_gradw
        END INTERFACE
        
        INTERFACE kernel_kernel_Lucy
           MODULE PROCEDURE kernel_kernel_Lucy_w
           MODULE PROCEDURE kernel_kernel_Lucy_w_gradw
        END INTERFACE
        

      CONTAINS

#include "kernel_new.F90"
#include "kernel_finalize.F90"
#include "kernel_kernel.F90"
#include "kernel_get.F90"


      END MODULE Class_Kernel

