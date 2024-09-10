      MODULE Class_Control
        !----------------------------------------------------
      	! Class	      :	Control
	!----------------------------------------------------
      	!
      	!  Purpose    :
	!> \brief       Variables and corresponding operations
        !>              for control infomation.
      	!>	   	
        !>              The variable memebers are public 
	! Remarks    :
      	!
      	! References :
     	!
      	! Revisions  : 0.2 04.03.2010, including relax_run. 
        !
        !              0.1 03.03.2009, original version.
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
        
        TYPE Control
           PRIVATE
           
           CHARACTER(LEN=MAX_CHAR)    :: job_name
           CHARACTER(LEN=MAX_CHAR)    :: job_submit_date
           CHARACTER(LEN=10)          :: job_execute_date
           CHARACTER(LEN=12)          :: job_execute_time
           CHARACTER(LEN=7)           :: job_execute_zone
           INTEGER, DIMENSION(10)     :: job_execute_values
           REAL(MK)                   :: job_time_start
           INTEGER                    :: debug_flag
           LOGICAL                    :: relax_run
           LOGICAL                    :: colloid_relax
           LOGICAL                    :: read_external
           INTEGER                    :: kernel_type
           LOGICAL                    :: symmetry
           INTEGER                    :: rhs_density_type
           LOGICAL                    :: dynamic_density_ref
           INTEGER                    :: stateEquation_type
           LOGICAL                    :: Newtonian
           LOGICAL                    :: Brownian
           INTEGER                    :: random_seed
           INTEGER                    :: rhs_force_type
           LOGICAL                    :: pp_interact_cc
           LOGICAL                    :: pp_interact_cw
           INTEGER                    :: cc_lub_type
           INTEGER                    :: cc_repul_type
           INTEGER                    :: cc_magnet_type
           INTEGER                    :: cw_lub_type
           INTEGER                    :: cw_repul_type
           LOGICAL                    :: stress_tensor
           LOGICAL                    :: p_energy
           LOGICAL                    :: flow_v_fixed
           INTEGER                    :: integrate_type
           INTEGER                    :: integrate_colloid_type
           INTEGER                    :: integrate_colloid_RK
           INTEGER                    :: integrate_colloid_AB
           INTEGER                    :: adaptive_dt
           INTEGER                    :: write_output
           INTEGER                    :: write_restart
           
           TYPE(Tool)                 :: tool

        END TYPE Control
        
        
        INTERFACE control_new
           MODULE PROCEDURE control_init_default
        END INTERFACE
        
      CONTAINS
        
#include "control_new.F90"
#include "control_finalize.F90"
#include "control_check_parameters.F90"
#include "control_get.F90"
#include "control_set.F90"
        
          
      END MODULE Class_Control
      
