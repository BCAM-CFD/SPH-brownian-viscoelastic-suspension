      SUBROUTINE colloid_noslip(this,xf,xc,vf,vc,sid_c,stat_info)
        !----------------------------------------------------
        ! Subroutine  : colloid_noslip
        !----------------------------------------------------
        !
        ! Purpose     : Return an artificial velocity for a 
        !               numerical particle inside a colloid
        !               object, in order to get no slip 
        !               condition on the surface of a colloid.
        !
        !               It will use accordingly, different
        !               ways of implementing no slip
        !               boundary condition.
        !
        !  Revision   : V0.1  01.03.2009, original version.
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
        ! Arguments :
        !
        ! Input 
        !
        ! this  : object of a colloid.
        ! xf    : position of a fluid particle.
        ! xc    : position of a colloid boundary particle.
        ! vf    : velocity of a fluid particle.
        ! sid_c : species ID of a colloid boundary particle.
        !
        ! Output
        !
        ! vc    : extrapolated velocity for the colloid
        !         boundary particle.
        ! stat_info : status of the routine.
        !----------------------------------------------------
        
        TYPE(Colloid), INTENT(IN)               :: this
        REAL(MK), DIMENSION(:), INTENT(IN)      :: xf
        REAL(MK), DIMENSION(:), INTENT(IN)      :: xc
        !****** Modified by Adolfo ******
!        REAL(MK), DIMENSION(:), INTENT(IN)      :: vf
        REAL(MK), DIMENSION(:), INTENT(INOUT)      :: vf
        !********************************
        REAL(MK), DIMENSION(:), INTENT(OUT)     :: vc
        INTEGER, INTENT(IN)                     :: sid_c        
        INTEGER, INTENT(OUT)                    :: stat_info 
        
        !----------------------------------------------------
        ! Local variables.
        !----------------------------------------------------
        
        INTEGER                                 :: stat_info_sub

        !----------------------------------------------------
        ! Initialization of variables.
        !----------------------------------------------------
        
        stat_info     = 0
        stat_info_sub = 0
        
        vc(:) = 0.0_MK
        
        !----------------------------------------------------
        ! Decide which no slip type to choose.
        !----------------------------------------------------
        
        SELECT CASE( this%noslip_type )
           
        CASE ( mcf_no_slip_frozen )
           
           CALL colloid_noslip_frozen(this,xc,vc,sid_c,stat_info_sub)
           
           IF( stat_info_sub /= 0 ) THEN
              PRINT *, "colloid_noslip : ", &
                   "Freezing boundary particle has problem !"
              stat_info = -1
              GOTO 9999
           END IF
           
        CASE ( mcf_no_slip_Morris )

           CALL colloid_noslip_Morris(this,xf,xc,vf,vc,sid_c,&
                stat_info_sub)
           
           IF( stat_info_sub /= 0 ) THEN
              PRINT *, "colloid_noslip : ", &
                   "Morris boundary particle has problem ! "
              stat_info = -1
              GOTO 9999
           END IF
           
        CASE (3)
           
           CALL colloid_noslip_Zhu(this,xf,xc,vf,vc,sid_c,&
                stat_info_sub)
           
           IF( stat_info_sub /= 0 ) THEN
              PRINT *, "colloid_noslip : ", &
                   "Zhu boundary particle has problem ! "
              stat_info = -1
              GOTO 9999
           END IF
           
        END SELECT
        
9999    CONTINUE
        
        RETURN
        
      END SUBROUTINE colloid_noslip
      
#include "colloid_noslip_frozen.F90"
#include "colloid_noslip_Morris.F90"
#include "colloid_noslip_Zhu.F90"

