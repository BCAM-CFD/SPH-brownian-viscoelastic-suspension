      SUBROUTINE colloid_compute_repulsion_cw(this,&
           x,sid,F,FB,stat_info)
        !----------------------------------------------------
        ! Subroutine  : colloid_compute_repulsion_cw
        !----------------------------------------------------
        !
        ! Purpose     : The gap between near contacting
        !               colloid and wall can be too small
        !               and they tend to overlap.
        !               Compute an extra repulsive force
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
        !              
        !
        ! Remarks     : V0.4 18.5.2012, shift second type
        !               repulsive force down to have a zero
        !               value at 5*cut off, i.e., 5*hn.
        !
        !               V0.3 6.3.2012, change the second
        !               repulsive force, assuming radius
        !               of colloid is universally one.
        !
        !               V0.2 19.11.2010, one pair version.
        !
        !               V0.1 5.11 2010, original version,
        !               for all pairs.
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
        ! Arguments:
        !
        ! input:
        !   x: location of colloid
        ! sid: species id of colloid
        !
        ! output:
        ! F : force on colloid
        ! FB: force on wall boundary
        !----------------------------------------------------
        
        TYPE(Colloid), INTENT(IN)               :: this
        REAL(MK), DIMENSION(:),INTENT(IN)       :: x
        INTEGER, INTENT(IN)                     :: sid
        REAL(MK), DIMENSION(:),INTENT(OUT)      :: F
        REAL(MK), DIMENSION(:,:),INTENT(OUT)    :: FB
        INTEGER, INTENT(OUT)                    :: stat_info
        
        !----------------------------------------------------
        ! Local variables.
        !
        ! s_drag   : drag on this process
        ! t_drag   : total drag on all processes
        ! s_torque : torque on this process
        ! t_torque : total torque on all processes        
        !----------------------------------------------------
        
        INTEGER                                  :: dim,num
        INTEGER                                  :: j
        REAL(MK)                                 :: a,sn,sm,s1,s2
        REAL(MK)                                 :: F0,Ft
        
        !----------------------------------------------------
        ! Initialization of variables.
        !----------------------------------------------------
        
        stat_info     = 0
        
        dim = this%num_dim
        num = dim * 2
        
        IF ( SIZE(F,1) /= dim ) THEN
           PRINT *, "colloid_compute_repulsion_cw : ", &
                "input F dimension does not match !"
           stat_info = -1
           GOTO 9999
        END IF
        
        IF ( SIZE(FB,1) /= dim ) THEN
           PRINT *, "colloid_compute_repulsion_cw : ", &
                "input FB dimension does not match !"
           stat_info = -1
           GOTO 9999
        END IF

        IF ( SIZE(FB,2) /= num ) THEN
           PRINT *, "colloid_compute_repulsion_cw : ", &
                "input FB number does not match !"
           stat_info = -1
           GOTO 9999
        END IF
        
        
        F(:)    = 0.0_MK
        FB(:,:) = 0.0_MK
        
        sn = this%cw_repul_cut_off
        sm = this%cw_repul_cut_on
        F0 = this%cw_repul_F0
        a  = this%radius(1,sid)
        Ft = 0.0_MK

        !----------------------------------------------------
        ! Loop each dimension of solid wall.
        !----------------------------------------------------
        
        DO j = 1, dim
           
           IF ( this%bcdef(2*j-1) == ppm_param_bcdef_wall_solid ) THEN
              
              !---------------------------------------------
              ! Calculate distance of the colloid to
              ! each wall.
              !----------------------------------------------
              
              s1 = x(j)-this%min_phys(j)-a
              s2 = this%max_phys(j)-x(j)-a
              
              !----------------------------------------------
              ! distance to down wall smaller than 5*hn
              !----------------------------------------------
              
              IF ( s1 < 5.0_MK * sn ) THEN
                 
                 SELECT CASE (this%cw_repul_type)
                    
                 CASE (mcf_cw_repul_type_Hookean)
                    !----------------------------------------
                    ! For linear spring force, it is
                    ! zero at hn and beyond.
                    !----------------------------------------
                    
                    IF ( s1 < sn ) THEN
                       
                       !-------------------------------------
                       ! If gap smaller than minimal 
                       ! allowed gap, set it to minimum.
                       !-------------------------------------
                       
                       IF ( s1 < sm ) THEN
                          
                          s1 = sm
                          
                       END IF
                       
                       Ft = F0 - F0*s1/sn
                       
                    END IF
                    
                 CASE (mcf_cw_repul_type_DLVO)
                    
                    !----------------------------------------
                    ! For DLVO force, it is not zero at hn
                    ! and less than 1/100 beyond 5*hn.
                    !----------------------------------------
                       
                    !----------------------------------------
                    ! If gap smaller than minimal 
                    ! allowed gap, set it to minimum.
                    !----------------------------------------
                    
                    IF ( s1 < sm ) THEN
                       
                       s1 = sm
                       
                    END IF
                    
                    Ft = &
                         F0 / sn * EXP(-s1/sn) /(1.0_MK-EXP(-s1/sn)) - &
                         F0 / sn * EXP(-5.0_MK) /(1.0_MK-EXP(-5.0_MK))
                    
                 CASE (mcf_cw_repul_type_LJ)
                    !----------------------------------------
                    ! For Lennard-Jones potential
                    !----------------------------------------
                    
                    IF ( s1 < sn ) THEN
                       
                       !-------------------------------------
                       ! If gap smaller than minimal 
                       ! allowed gap, set it to minimum.
                       !-------------------------------------
                       
                       IF ( s1 < sm ) THEN
                          
                          s1 = sm
                          
                       END IF
                       
                       Ft = F0 / s1 * ((0.1_MK*a/s1)**12- (0.1_MK*a/s1)**6)
                       
                    END IF
                    
                 CASE DEFAULT
                    
                    PRINT *, __FILE__, __LINE__, &
                         "no such repulsive force!"
                    stat_info = -1
                    GOTO 9999
                    
                 END SELECT ! cw_repul_type
                 
                 F(j) = F(j) +  Ft
                 
                 !-------------------------------------------
                 ! Collect force on boundary
                 !-------------------------------------------
                 
                 FB(j,2*j-1) = -Ft
                 
              END IF ! h1 < 5.0*hn
              
              
              IF ( s2 < 5.0_MK * sn ) THEN
                 
                 SELECT CASE (this%cw_repul_type)
                    
                 CASE (mcf_cw_repul_type_Hookean)
                    
                    !----------------------------------------
                    ! For linear spring force, it is
                    ! zero at hn and beyond.
                    !----------------------------------------
                    
                    IF ( s2 < sn ) THEN
                       
                       !-------------------------------------
                       ! If gap smaller than minimal 
                       ! allowed gap, set it to minimum.
                       !-------------------------------------
                       
                       IF ( s2 < sm ) THEN
                          
                          s2 = sm
                          
                       END IF
                       
                       Ft = F0 - F0*s2/sn
                       
                    END IF
                    
                 CASE (mcf_cw_repul_type_DLVO)
                    
                    !----------------------------------------
                    ! For DLVO force, it is not zero at hn
                    ! and less than 1/100 beyond 5*hn.
                    !----------------------------------------
                    
                    !----------------------------------------
                    ! If gap smaller than minimal 
                    ! allowed gap, set it to minimum.
                    !----------------------------------------
                    
                    IF ( s2 < sm ) THEN
                       
                       s2 = sm
                       
                    END IF
                    
                    Ft = &
                         F0 / sn * EXP(-s2/sn) /(1.0_MK-EXP(-s2/sn)) - &
                         F0 / sn * EXP(-5.0_MK) /(1.0_MK-EXP(-5.0_MK))
                    

                 CASE (mcf_cw_repul_type_LJ)

                    !----------------------------------------
                    ! For Lennard-Jones potential
                    !----------------------------------------
                    
                    IF ( s2 < sn ) THEN
                       
                       !-------------------------------------
                       ! If gap smaller than minimal 
                       ! allowed gap, set it to minimum.
                       !-------------------------------------
                       
                       IF ( s2 < sm ) THEN
                          
                          s2 = sm
                          
                       END IF
                       
                       Ft = F0 / s2 * ((0.1_MK*a/s2)**12- (0.1_MK*a/s2)**6)
                       
                    END IF
                 CASE DEFAULT
                    
                    PRINT *, __FILE__, __LINE__, &
                         "no such repulsive force!"
                    stat_info = -1
                    GOTO 9999
                    
                 END SELECT
                 
                 F(j) = F(j) - Ft
                 
                 !-------------------------------------------
                 ! Collect force on boundary
                 !-------------------------------------------
                 
                 FB(j,2*j) = Ft
                 
              END IF ! s2 < sn
              
           END IF ! bcdef(2j-1)=wall_solid
           
        END DO ! j = 1 , dim
        
        
9999    CONTINUE
        
        
        RETURN
        
      END SUBROUTINE colloid_compute_repulsion_cw
      
      
      
