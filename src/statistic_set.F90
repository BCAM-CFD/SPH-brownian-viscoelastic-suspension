!------------------------------------------------------------
! All the public "set" subroutines of Class statistic,
! which return member variables of statistic.
!
! Reference   :
!
! Remark      :
!
! Revisions   :  V0.1 15.03.2010, original version.
!
!------------------------------------------------------------
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
!------------------------------------------------------------      

      SUBROUTINE statistic_set_rho_min(this,d_rho_min,stat_info)
        
        TYPE(Statistic), INTENT(OUT)    :: this
        REAL(MK), INTENT(IN)            :: d_rho_min
        INTEGER, INTENT(OUT)            :: stat_info 
        
        stat_info = 0
        
        this%rho_min = d_rho_min
        
        RETURN
        
      END SUBROUTINE statistic_set_rho_min
      

      SUBROUTINE statistic_set_rho_max(this,d_rho_max,stat_info)
        
        TYPE(Statistic), INTENT(OUT)    :: this
        REAL(MK), INTENT(IN)            :: d_rho_max
        INTEGER, INTENT(OUT)            :: stat_info 
        
        stat_info = 0
        
        this%rho_max = d_rho_max
        
        RETURN        
        
      END SUBROUTINE statistic_set_rho_max
      

      SUBROUTINE statistic_set_colloid_implicit_pair_sweep_adaptive(this,d_adapt,stat_info)
        
        TYPE(Statistic), INTENT(OUT)    :: this
        LOGICAL, INTENT(IN)             :: d_adapt
        INTEGER, INTENT(OUT)            :: stat_info 
        
        stat_info = 0
        
        this%colloid_implicit_pair_sweep_adaptive = d_adapt
        
        RETURN        
        
      END SUBROUTINE statistic_set_colloid_implicit_pair_sweep_adaptive

      
      SUBROUTINE statistic_set_colloid_implicit_pair_num_sweep(this,d_num,stat_info)
        
        TYPE(Statistic), INTENT(OUT)    :: this
        INTEGER, INTENT(IN)             :: d_num
        INTEGER, INTENT(OUT)            :: stat_info 
        
        stat_info = 0
        
        this%colloid_implicit_pair_num_sweep = d_num
        
        RETURN        
        
      END SUBROUTINE statistic_set_colloid_implicit_pair_num_sweep
      
      
      SUBROUTINE statistic_set_colloid_implicit_pair_sweep_error(this,d_error,stat_info)
        
        TYPE(Statistic), INTENT(OUT)    :: this
        REAL(MK), INTENT(IN)            :: d_error
        INTEGER, INTENT(OUT)            :: stat_info 
        
        stat_info = 0
        
        this%colloid_implicit_pair_sweep_error = d_error
        
        RETURN        
        
      END SUBROUTINE statistic_set_colloid_implicit_pair_sweep_error
      

