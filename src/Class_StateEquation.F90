      MODULE Class_StateEquation
        !----------------------------------------------------
      	!  Class      :	StateEquation
	!----------------------------------------------------
      	!
      	!  Purpose    :
	!> \brief       Variables and corresponding operations
        !>              for state equation quantities.
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
      	!  Revisions  : 0.1 03.03.2009
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
        USE Class_Tool
        
        IMPLICIT NONE
        SAVE
        
        TYPE StateEquation
           INTEGER              :: stateEquation_type
           REAL(MK)             :: c 
           REAL(MK)             :: p0
           REAL(MK)             :: rho_ref
           REAL(MK)             :: gamma
           
           TYPE(Tool)           :: tool                      
        END TYPE StateEquation
        
        INTERFACE stateEquation_new
           MODULE PROCEDURE stateEquation_init
        END INTERFACE
        
        INTERFACE stateEquation_compute_pressure
           MODULE PROCEDURE stateEquation_compute_pressure_scalar
           MODULE PROCEDURE stateEquation_compute_pressure_vector
        END INTERFACE
        
      CONTAINS
          
#include "stateEquation_new.F90"
#include "stateEquation_finalize.F90"
#include "stateEquation_compute_pressure.F90"
#include "stateEquation_get.F90"
#include "stateEquation_set.F90"

      END MODULE Class_StateEquation
