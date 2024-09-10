      SUBROUTINE colloid_compute_dt_f(this,stat_info)
        !----------------------------------------------------
        ! Subroutine  : colloid_compute_dt_f
        !----------------------------------------------------
        !
        ! Purpose     : Adapt dt according to the new
        !               maximum accerlation of colloids,
        !               compute dt.
        !      
        ! Reference   : Morris et al. JCP 1997.
        !
        ! Remark      :
     
        !
        ! Revisions   : V0.1 13.10.20101, original version.
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

        !----------------------------------------------------
        ! Arguments
        !----------------------------------------------------
        
        TYPE(Colloid), INTENT(INOUT)    :: this
        INTEGER, INTENT(OUT)            :: stat_info
        
        !----------------------------------------------------
        ! Initialization of variables.
        !----------------------------------------------------
        
        stat_info     = 0
        
        !----------------------------------------------------
        ! Calculate dt according to maximum acceleration 
        ! constraints.
        !----------------------------------------------------
        
        IF( this%fa_max > 0.0_MK ) THEN
           
           !this%dt_f = 0.25_MK * SQRT(this%h / this%fa_max)
           !this%dt_f = 0.5_MK * SQRT(this%h / this%fa_max)
           !this%dt_f = 1.0_MK * SQRT(this%h / this%fa_max)
           this%dt_f = this%adapt_t_coef * SQRT(this%h / this%fa_max)
           
        ELSE
           
           this%dt_f = -1.0_MK
           
        END IF
        
        RETURN
        
      END SUBROUTINE colloid_compute_dt_f
      
      
