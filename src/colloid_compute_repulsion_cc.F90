      SUBROUTINE colloid_compute_repulsion_cc(this,&
           x_ip,x_jp,sid_ip,sid_jp,F_ij,stat_info)
        !----------------------------------------------------
        ! Subroutine  : colloid_compute_repulsion_cc
        !----------------------------------------------------
        !
        ! Purpose     : The gap between near contacting
        !               colloid can be too small and
        !               tend to overlap.
        !               Compute a extra repulsive force
        !               which prevents them to overlap
        !               or become too close.
        !
        ! Routines    :
        !
        ! References  : 1) Brady and Bossis,
        !                  J. Fluid Mech. vol. 155,
        !                  pp. 105-129.1985
        !               2) Ball and Melrose, 
        !                  Adv. Colloid Interface Sci.
        !                  59 19-30, 1995.
        !               3) Dratler and Schowalter,
        !                  J. Fluid Mech.
        !                  vol. 325, pp 53-77. 1996.
        !               4) Sierou and Brady,
        !                  J. Rheol. 46(5), 1031-1056, 2002.
        !               5) Sing et al, 2009, SI[S3]
        !                  PNAS
        !                
        !              
        !
        ! Remarks     : V0.5 19.06.2013, add in Lennard-Jones
        !               potential for repulsion.
        !
        !               V0.4 18.5.2012, shift 2nd type
        !               repulsive force down to have a zero
        !               value at 5*cut off, i.e., 5*hn.
        !
        !               V0.3 6.3.2012, change the second
        !               repulive force,assuming radius
        !               of colloid is universally one.
        !             
        !               V0.2 16.11.2010, one pair version.
        !
        !               V0.1 15.10 2010, original version,
        !               loop over all pairs
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
        
        TYPE(Colloid), INTENT(IN)               :: this
        REAL(MK), DIMENSION(:),INTENT(IN)       :: x_ip
        REAL(MK), DIMENSION(:),INTENT(IN)       :: x_jp
        INTEGER, INTENT(IN)                     :: sid_ip
        INTEGER, INTENT(IN)                     :: sid_jp
        REAL(MK), DIMENSION(:),INTENT(OUT)      :: F_ij
        INTEGER, INTENT(OUT)                    :: stat_info
        
        !----------------------------------------------------
        ! Local variables.
        !----------------------------------------------------
        
        INTEGER                         :: dim,num
        REAL(MK)                        :: sigma, sm,sn, F0
        REAL(MK)                        :: a, aa, r, s
        REAL(MK)                        :: F
        REAL(MK), DIMENSION(3)          :: R12
        
        !----------------------------------------------------
        ! Initialization of variables.
        !----------------------------------------------------
        
        stat_info     = 0
        
        dim = this%num_dim
        num = this%num_colloid
        
        F_ij(1:dim) = 0.0_MK
        F  = 0.0_MK
        
        sigma = this%cc_repul_sigma
        sn    = this%cc_repul_cut_off
        sm    = this%cc_repul_cut_on
        F0    = this%cc_repul_F0
        
        !----------------------------------------------------
        ! Calculate the gap.
        !----------------------------------------------------

        R12(1:dim) = x_ip(1:dim) - x_jp(1:dim)
        
        r = SQRT(DOT_PRODUCT(R12(1:dim), R12(1:dim)))
        a  = this%radius(1,sid_ip)
        aa = this%radius(1,sid_ip) + this%radius(1,sid_jp)
        
        s = r - aa
        
        !----------------------------------------------------
        ! If gap smaller than repulsion cut off.
        !----------------------------------------------------
        
        IF ( s < 5.0_MK * sn ) THEN
           
           SELECT CASE (this%cc_repul_type)
              
           CASE (mcf_cc_repul_type_Hookean)
              !----------------------------------------------
              ! For linear spring force, it has clear zero 
              ! at hn.
              !----------------------------------------------
              
              IF ( s < sn ) THEN
                 
                 !-------------------------------------------
                 ! If gap smaller than minimal allowed gap, 
                 ! set it to minimum.
                 !-------------------------------------------
                 
                 IF ( s < sm ) THEN
                    
                    s = sm
                    
                 END IF
                 
                 F = F0 - F0*s/sn
                 
              END IF ! h < hn
              
           CASE (mcf_cc_repul_type_DLVO)
              !----------------------------------------------
              ! For DLVO force, it does not have clear zero 
              ! at hn. But at 5*hn, its value is smaller
              ! than F0/100.
              !----------------------------------------------
              
              !----------------------------------------------
              ! If gap smaller than minimal allowed gap, 
              ! set it to minimum.
              !----------------------------------------------
              
              IF ( s < sm ) THEN
                 
                 s = sm
                 
              END IF
              
              F = F0 / sn * EXP(-s/sn) /(1.0_MK-EXP(-s/sn)) - &
                   F0 / sn * EXP(-5.0_MK) /(1.0_MK-EXP(-5.0_MK))
              
           CASE (mcf_cc_repul_type_LJ)
              
              IF ( s < sn ) THEN
                 
                 !-------------------------------------------
                 ! If gap smaller than minimal allowed gap, 
                 ! set it to minimum.
                 !-------------------------------------------
                 
                 IF ( s < sm ) THEN
                    
                    s = sm
                    
                 END IF

                 F = F0/s * ( (sigma*a/s)**12-(sigma*a/s)**6 )
                 
              END IF
              
           CASE DEFAULT
              
              PRINT *, __FILE__, __LINE__, &
                   "no such repulsive force!"
              stat_info = -1
              GOTO 9999
              
           END SELECT
           
           R12(1:dim) = R12(1:dim) / r        
           F_ij(1:dim) = F * R12(1:dim)
           
        END IF ! h < 5*hn
        
9999    CONTINUE
        
        RETURN          
        
      END SUBROUTINE colloid_compute_repulsion_cc
      
      
      !***********************************************
      SUBROUTINE colloid_compute_spring_cc(this,&
           x_ip,x_jp,sid_ip,sid_jp,F_ij,stat_info)
        !----------------------------------------------------
        ! Quadratic potential between colloids
        !----------------------------------------------------
    
        !----------------------------------------------------
        ! Arguments
        !----------------------------------------------------
        
        TYPE(Colloid), INTENT(IN)               :: this
        REAL(MK), DIMENSION(:),INTENT(IN)       :: x_ip
        REAL(MK), DIMENSION(:),INTENT(IN)       :: x_jp
        INTEGER, INTENT(IN)                     :: sid_ip
        INTEGER, INTENT(IN)                     :: sid_jp
        REAL(MK), DIMENSION(:),INTENT(OUT)      :: F_ij
        INTEGER, INTENT(OUT)                    :: stat_info
        
        !----------------------------------------------------
        ! Local variables.
        !----------------------------------------------------
        
        INTEGER                         :: dim,num
        REAL(MK)                        :: quad_k, quad_r0
        REAL(MK)                        :: a, aa, r, s
        REAL(MK)                        :: F
        REAL(MK), DIMENSION(3)          :: R12
        
        !----------------------------------------------------
        ! Initialization of variables.
        !----------------------------------------------------
        
        stat_info     = 0
        
        dim = this%num_dim
        num = this%num_colloid
        
        F_ij(1:dim) = 0.0_MK
        F  = 0.0_MK
        
        quad_k = this%cc_quad_k
        quad_r0 = this%cc_quad_r0
        
        !----------------------------------------------------
        ! Calculate the gap.
        !----------------------------------------------------

        R12(1:dim) = x_ip(1:dim) - x_jp(1:dim)
        
        r = SQRT(DOT_PRODUCT(R12(1:dim), R12(1:dim)))
        F = -quad_k * (r - quad_r0)
        R12(1:dim) = R12(1:dim) / r        
        F_ij(1:dim) = F * R12(1:dim)
        
9999    CONTINUE
        
        RETURN          
        
      END SUBROUTINE colloid_compute_spring_cc
!*******************************************************      
      
      
