      SUBROUTINE colloid_compute_lubrication_cw(this,&
           x,v,sid,F,FB,stat_info)
        !----------------------------------------------------
        ! Subroutine  : colloid_compute_lubrication_cw
        !----------------------------------------------------
        !
        ! Purpose     : When the resolution is finite, fluid 
        !               in the gap between near contacting
        !               colloid and wall can not be resolved. 
        !               We use lubrication theory to
        !               compute the lubrication force.
        !
        ! Routines    :
        !
        ! References  : (2D : Jeffrey and Onishi 1981)
        !
        !
        ! Remarks     :  V0.2 19.11.2010, one pair version.
        !                
        !                V0.1 03.11 2010, lubricaiont between
        !                a cylinder and single wall is 
        !                introduced for all pairs.
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
        REAL(MK), DIMENSION(:),INTENT(IN)       :: x
        REAL(MK), DIMENSION(:),INTENT(IN)       :: v
        INTEGER, INTENT(IN)                     :: sid
        REAL(MK), DIMENSION(:),INTENT(OUT)      :: F
        REAL(MK), DIMENSION(:,:),INTENT(OUT)    :: FB

        INTEGER, INTENT(OUT)                    :: stat_info
        
        !----------------------------------------------------
        ! Local variables.
        !----------------------------------------------------
        
        INTEGER                                 :: dim,num
        INTEGER                                 :: i,j
        REAL(MK)                                :: hn,hm,h
        REAL(MK)                                :: Ft
        
        !----------------------------------------------------
        ! Initialization of variables.
        !----------------------------------------------------
        
        stat_info     = 0
        
        dim = this%num_dim
        num = 2*dim
        
        IF ( SIZE(F,1) /= dim ) THEN
           PRINT *, "colloid_compute_lubrication_cw : ", &
                "input F dimension does not match !"
           stat_info = -1
           GOTO 9999
        END IF

        IF ( SIZE(FB,1) /= dim ) THEN
           PRINT *, "colloid_compute_lubrication_cw : ", &
                "input FB dimension does not match !"
           stat_info = -1
           GOTO 9999
        END IF

        IF ( SIZE(FB,2) /= num ) THEN
           PRINT *, "colloid_compute_lubrication_cw : ", &
                "input FB number does not match !"
           stat_info = -1
           GOTO 9999
        END IF
  
        F(:)    = 0.0_MK
        FB(:,:) = 0.0_MK
    
        hn  = this%cw_lub_cut_off
        hm  = this%cw_lub_cut_on
        
        IF ( dim == 2  ) THEN
           
           !-------------------------------------------------
           ! Loop each dimension for solid wall
           !-------------------------------------------------
           
           DO j = 1, num
              
              IF ( this%bcdef(j) == ppm_param_bcdef_wall_solid ) THEN
                 
                 i = INT((j+1)/2)
              
                 IF ( MOD(j,2) == 1 ) THEN
                    
                    !----------------------------------------
                    ! Check lower boundary for wall solid.
                    !----------------------------------------
                    
                    h = x(i)-this%min_phys(i)-this%radius(1,sid)
                    
                 ELSE
                    
                    !----------------------------------------
                    ! Check upper boundary for wall solid.
                    !----------------------------------------
                    
                    h = this%max_phys(i)-x(i)-this%radius(1,sid)
                    
                 END IF
                 
                 IF ( h < hn  ) THEN
                    
                    IF ( h < hm ) THEN
                       h = hm
                    END IF
                    
                    Ft = 12.0_MK * mcf_pi * this%eta * v(i) * &
                         ( (2.0*h/this%radius(1,sid))**(-1.5_MK) - &
                         (2.0*hn/this%radius(1,sid))**(-1.5_MK) )
                    
                    F(i) = F(i) - Ft
                    
                    !----------------------------------------
                    ! Collect force on boundary
                    !----------------------------------------
                    
                    FB(i,j) = Ft
                    
                 END IF ! h < hn
                 
              END IF ! bcdef(j)
              
           END DO ! j =1, num
           
        END IF ! dim = 2
        
9999    CONTINUE
        
        RETURN          
        
      END SUBROUTINE colloid_compute_lubrication_cw
      
      
      
