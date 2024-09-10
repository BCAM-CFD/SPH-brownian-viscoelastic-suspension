      MODULE Class_Tool
        !----------------------------------------------------
      	!       Class_Tool
      	!----------------------------------------------------
        !
      	!  Purpos     : auxiliary functions for global usage.
      	!
      	!  Remarks    :
      	!
      	!  References :
      	!
      	!  Version    : V 0.1. 03.03.2009, original version.
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
        
        TYPE Tool
           INTEGER      :: flag
        END TYPE Tool
        
        
        INTERFACE tool_new
           MODULE PROCEDURE tool_init           
        END INTERFACE
        
        INTERFACE tool_print_msg
           MODULE PROCEDURE tool_print_msg_b
           MODULE PROCEDURE tool_print_msg_f
           MODULE PROCEDURE tool_print_msg_f2
           MODULE PROCEDURE tool_print_msg_ff
           MODULE PROCEDURE tool_print_msg_fs
           MODULE PROCEDURE tool_print_msg_i
           MODULE PROCEDURE tool_print_msg_i2
           MODULE PROCEDURE tool_print_msg_ii
           MODULE PROCEDURE tool_print_msg_is
           MODULE PROCEDURE tool_print_msg_s
           MODULE PROCEDURE tool_print_msg_s2
           MODULE PROCEDURE tool_print_msg_si
        END INTERFACE tool_print_msg

      CONTAINS
        
#include "tool_new.F90"
#include "tool_uppercase.F90"
#include "tool_cross_product.F90"
#include "tool_rotation_matrix.F90"
#include "tool_rotation_vector.F90"
#include "tool_solve_linear_equations.F90"
#include "tool_L1_norm.F90"
#include "tool_L2_norm.F90"
#include "tool_Linfty_norm.F90"
#include "tool_print_msg.F90"

      END MODULE Class_Tool
