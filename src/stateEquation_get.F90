!------------------------------------------------------------
! All the public "get" subroutines of Class StateEquation,
! which return member variables of StateEquation.
!
! Reference   :
!
! Remark      :
!
! Revisions   :  V0.1 03.03.2009, original version.
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

      REAL(MK) FUNCTION stateEquation_get_c(this,stat_info)

        TYPE(StateEquation), INTENT(IN) :: this
        INTEGER, INTENT(OUT)            :: stat_info

        stat_info = 0
        stateEquation_get_c = this%c

        RETURN
        
      END FUNCTION stateEquation_get_c

      
      REAL(MK) FUNCTION stateEquation_get_p0(this,stat_info)

        TYPE(StateEquation), INTENT(IN) :: this
        INTEGER, INTENT(OUT)            :: stat_info

        stat_info = 0
        stateEquation_get_p0 = this%p0

        RETURN
        
      END FUNCTION stateEquation_get_p0

      
      REAL(MK) FUNCTION stateEquation_get_rho_ref(this,stat_info)
        
        TYPE(StateEquation), INTENT(IN) :: this
        INTEGER, INTENT(OUT)            :: stat_info
        
        stat_info = 0
        stateEquation_get_rho_ref = this%rho_ref

        RETURN
        
      END FUNCTION stateEquation_get_rho_ref

      
      REAL(MK) FUNCTION stateEquation_get_gamma(this,stat_info)
        
        TYPE(StateEquation), INTENT(IN) :: this
        INTEGER, INTENT(OUT)            :: stat_info
        
        stat_info = 0
        stateEquation_get_gamma = this%gamma

        RETURN
        
      END FUNCTION stateEquation_get_gamma
