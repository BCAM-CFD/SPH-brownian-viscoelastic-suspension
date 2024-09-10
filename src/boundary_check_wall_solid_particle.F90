      SUBROUTINE boundary_check_wall_solid_particle(this,&
           p_x,l_w,p_sid,stat_info)
        !----------------------------------------------------
        ! Subroutine  : boundary_check_wall_solid_particle
        !----------------------------------------------------
        !
        ! Purpose     : Check if p_x is a solid wall boundary 
        !               particle. 
        !       
        !
        ! Reference   :
        !
        ! Remark      :
        !
        ! Revisions   : V0.1 14.12 2009, original version.
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
        ! dx        : initial distance between particles.
        ! p_x       : position.
        ! l_w       : indicate inside wall or not.
        ! p_sid     : species ID.
        ! stat_info : status of this routine.
        !----------------------------------------------------
        
        TYPE(Boundary), INTENT(INOUT)           :: this
        REAL(MK), DIMENSION(:), INTENT(IN)      :: p_x
        LOGICAL, INTENT(OUT)                    :: l_w
        INTEGER, INTENT(OUT)                    :: p_sid
        INTEGER, INTENT(OUT)                    :: stat_info
        
        
        !----------------------------------------------------
        ! Local variables
        !----------------------------------------------------
        
        INTEGER                                 :: num_dim
        INTEGER                                 :: i
        
        !----------------------------------------------------
        ! Initialization of variables.
        !----------------------------------------------------
        
        stat_info = 0
        l_w       = .FALSE.
        num_dim   = this%num_dim
        
        !----------------------------------------------------
        ! Loop over each boundary direction.
        !----------------------------------------------------
        
        DO i = 1, num_dim
           
           IF ( this%bcdef(2*i-1) == &
                   ppm_param_bcdef_wall_solid ) THEN
              
              IF ( p_x(i) <= this%min_phys(i) .AND. &
                   p_x(i) >= this%min_phys_t(i) ) THEN
                 
                 
                 l_w = .TRUE.
                 p_sid = 1-2*i
                 
                 EXIT
                 
              END IF ! bcdef

           END IF

           IF ( this%bcdef(2*i) == &
                ppm_param_bcdef_wall_solid ) THEN
              
#ifdef __WALL_FLUCTUATE
              IF ( p_x(i) >= this%max_phys(i)-0.3*ABS(cos(p_x(1))) .AND. &
                   p_x(i) < this%max_phys_t(i) ) THEN
#else              
                 IF ( p_x(i) >= this%max_phys(i) .AND. &
                      p_x(i) < this%max_phys_t(i) ) THEN
#endif         
                    
                    l_w = .TRUE.
                    p_sid = -2*i
                    
                    EXIT
                    
                 END IF ! p_x
              
           END IF ! bcdef
           
        END DO ! i
        
        
9999    CONTINUE
        
        RETURN
        
      END SUBROUTINE boundary_check_wall_solid_particle
      
