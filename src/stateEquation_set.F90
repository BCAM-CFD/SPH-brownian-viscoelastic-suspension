!------------------------------------------------------------
! All the public "set" subroutines of Class StateEquation,
! which return member variables of StateEquation.
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

      SUBROUTINE stateEquation_set_rho_ref(this,d_rho_ref,stat_info)
        
        TYPE(StateEquation),INTENT(OUT) :: this
        REAL(MK), INTENT(IN)            :: d_rho_ref
        INTEGER, INTENT(OUT)            :: stat_info
        
        stat_info = 0
        
        this%rho_ref = d_rho_ref
        
        RETURN
        
      END SUBROUTINE stateEquation_set_rho_ref
