!----------------------------------------------------
! Subroutine  : debug_get* routines.
!----------------------------------------------------
!
! Revisions   : V0.1 16.11.2010, original version.
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

      INTEGER FUNCTION debug_get_flag(this,stat_info)
        
        TYPE(Debug), INTENT(IN)         :: this
        INTEGER, INTENT(OUT)            :: stat_info
        
        stat_info = 0
        
        debug_get_flag = this%flag
        
        RETURN
      END FUNCTION debug_get_flag
      

      REAL(MK) FUNCTION debug_get_time(this,stat_info)
        TYPE(Debug), INTENT(IN)         :: this
        INTEGER, INTENT(OUT)            :: stat_info
        
        REAL(MK)                        :: time

        stat_info = this%flag
        stat_info = 0
        
        CALL ppm_time(time,stat_info)
        

        debug_get_time = time
        
        RETURN
      END FUNCTION debug_get_time
