      SUBROUTINE colloid_compute_magnetism_cc(this,&
           x_ip,x_jp,sid_ip,sid_jp,F_ij,stat_info)
        !----------------------------------------------------
        ! Subroutine  : colloid_compute_magnetism_cc
        !----------------------------------------------------
        !
        ! Purpose     : 
        !
        ! Routines    :
        !
        ! References  : Sing et al. PNAS 2009, Supporting info
        !               [S8].
        !
        ! Remarks     : V0.1 18.06 2013, original version,
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
        
        TYPE(Colloid), INTENT(INOUT)            :: this
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
        REAL(MK)                        :: a, aa, r, F0, cut_off, cut_on, s
        REAL(MK)                        :: vc, mu, cof
        REAL(MK)                        :: c1, c2, c3, c4
        REAL(MK), DIMENSION(3)          :: r12
        REAL(MK), DIMENSION(3)          :: mom1,mom2
        
        !----------------------------------------------------
        ! Initialization of variables.
        !----------------------------------------------------
        
        stat_info = 0
        
        dim = this%num_dim
        num = this%num_colloid
        
        a   = this%radius(1,sid_ip)
        aa  = 2.0_MK*a
        !mu  = this%cc_magnet_mu
        !vc  = 4.0_MK*mcf_pi*a**3*this%cc_magnet_f/3.0_MK
        !cof =  vc * this%cc_magnet_chi/ mu
        !mom1(1:dim) = cof * this%cc_magnet_B(1:dim)
        F0 = this%cc_magnet_F0
        mom1(1:dim) = this%cc_magnet_mom(1:dim)
        mom2(1:dim) = mom1(1:dim)
        cut_off     = this%cc_magnet_cut_off
        cut_on      = this%cc_magnet_cut_on
        
        F_ij(1:dim) = 0.0_MK
        
        
        !----------------------------------------------------
        ! Calculate the magnetic force 
        !----------------------------------------------------

        r12(1:dim) = x_ip(1:dim) - x_jp(1:dim)
        r = SQRT(DOT_PRODUCT(r12(1:dim), r12(1:dim)))
        
        s = r - aa
        
        IF ( s <= cut_off ) THEN
           
           IF ( s <= cut_on ) THEN
              
              r12(1:dim) = r12(1:dim) * (aa+cut_on)/ r
              r = aa + cut_on
              
           END IF

           r12(1:dim) = r12(1:dim)/a
           r          = r/a
           
           c1 = DOT_PRODUCT(mom1(1:dim), r12(1:dim))/r
           c2 = DOT_PRODUCT(mom2(1:dim), r12(1:dim))/r
           c3 =  DOT_PRODUCT(mom1(1:dim), mom2(1:dim))
           
           F_ij(1:dim) = F0 / r**4 * ( c1 * mom2(1:dim) + c2*mom1(1:dim) &
                - (5.0_MK*c1*c2-c3)* r12(1:dim) / r )
           
        END IF
        
9999    CONTINUE
        
        RETURN          
        
      END SUBROUTINE colloid_compute_magnetism_cc
      
      
      
