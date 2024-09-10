      SUBROUTINE kernel_kernel_w(this, rij, w, stat_info)
        !----------------------------------------------------
        ! Subroutine  :  kernel_kenerl
        !----------------------------------------------------
        !
        ! Purpose     : Return value of a kernel.
        !
        ! Reference   :
        !
        ! Remark      :
        !
        ! Revisions   : V0.1 03.03.2009, original version.
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
        
        TYPE(Kernel), INTENT(IN)        :: this
        REAL(MK), INTENT(IN)            :: rij
        REAL(MK), INTENT(OUT)           :: w
        INTEGER, INTENT(OUT)            :: stat_info


        INTEGER                         :: stat_info_sub

        stat_info = 0
        stat_info_sub = 0
        
        SELECT CASE(this%kernel_type)
           
        CASE (1)           
           CALL kernel_kernel_quintic_spline_w(this, &
                rij,w,stat_info_sub)
           
        CASE (2)           
           CALL kernel_kernel_Lucy_w(this, &
                rij,w,stat_info_sub)
        END SELECT
        
        
      END SUBROUTINE kernel_kernel_w
      
      
      SUBROUTINE kernel_kernel_w_gradw(this, rij, w, gradw,stat_info)
        !----------------------------------------------------
        ! Subroutine  :  kernel_kenerl_w_gradw
        !----------------------------------------------------
        !
        ! Purpose     : Return value and its derivative of 
        !               a kernel.
        !
        ! Reference   :
        !
        ! Remark      :
        !
        ! Revisions   : V0.1 03.03.2009, original version.
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
        
        TYPE(Kernel), INTENT(IN)        :: this
        REAL(MK), INTENT(IN)            :: rij
        REAL(MK), INTENT(OUT)           :: w
        REAL(MK), INTENT(OUT)           :: gradw
        INTEGER, INTENT(OUT)            :: stat_info


        INTEGER                         :: stat_info_sub

        stat_info = 0
        stat_info_sub = 0
        
        SELECT CASE(this%kernel_type)
           
        CASE (1)           
           CALL kernel_kernel_quintic_spline_w_gradw(this, &
                rij,w,gradw,stat_info_sub)
           
        CASE (2)           
           CALL kernel_kernel_Lucy_w_gradw(this, &
                rij,w,gradw,stat_info_sub)
        END SELECT
        
        
      END SUBROUTINE kernel_kernel_w_gradw

#include "kernel_kernel_quintic_spline.F90"
#include "kernel_kernel_Lucy.F90"
      
      
      
      
     
