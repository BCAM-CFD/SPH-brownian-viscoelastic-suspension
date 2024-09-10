      SUBROUTINE particles_integrate_position(this,&
           num,dt,accuracy_order,stat_info)
        !----------------------------------------------------
        ! Subroutine  : particles_integrate_position
        !----------------------------------------------------
        !
        ! Purpose     : Integrate position of particles
        !               with required accuracy.
        !
        ! Reference   :
        !
        ! Remark      : colloidal boundary particle may rotate,
        !               needs to be done seperately.
        !               boundary condition boundary particles 
        !               can be done here.
        !
        !
        ! Revision    : V0.3 24.08.2010, integrate position of
        !               particles, except colloidal boundary 
        !               particles.
        !
        !               V0.2  08.07.2009,
        !               check again the work flow and supply
        !               with more comments.
        !
        !               V0.1  01.04.2009, original version.
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
        !  Arguments
        !
        !  this           : an object of Particles Class.
        !  num            : number of particles needed to be updated,
        !                   i.e. first num particles in this%x 
        !                   are operated.
        !  dt             : time step.
        !  accuracy_order : accuracy required.
        !  stat_info      : return flag of status.
        !----------------------------------------------------
        
        TYPE(Particles), INTENT(INOUT)          :: this
        INTEGER, INTENT(IN)                     :: num
        REAL(MK),INTENT(IN)                     :: dt
        INTEGER, INTENT(IN)                     :: accuracy_order
        INTEGER, INTENT(OUT)                    :: stat_info
        
        !----------------------------------------------------
        ! Local variables
	!----------------------------------------------------

        INTEGER                                 :: stat_info_sub
        INTEGER                                 :: dim, i
        
        !----------------------------------------------------
        ! Initialization of variables.
        !----------------------------------------------------
        
        stat_info     = 0
        stat_info_sub = 0
        
        dim           = &
             physics_get_num_dim(this%phys,stat_info_sub)

#ifdef __PARTICLES_POSITION_FIXED
        
#else      
        !----------------------------------------------------
        ! Select different accuracy oder
        ! 1 only velocity contribution.
        ! 2 velocity + acceleration contribution.
        !----------------------------------------------------
       
        SELECT CASE (accuracy_order)
           
        CASE (1)
           
           DO i = 1, num
              
              IF ( this%id(this%sid_idx,i) == mcf_particle_type_fluid ) THEN
                 
                 this%x(1:dim,i) = &
                      this%x(1:dim,i) + &
                      this%v(1:dim,i) * dt
                 
              END IF
              
           END DO
           
        CASE (2)
           
           DO i = 1, num
              
              IF ( this%id(this%sid_idx,i) == mcf_particle_type_fluid ) THEN
                 
                 this%x(1:dim,i) = &
                      this%x(1:dim,i) + &
                      this%v(1:dim,i) * dt +&
                      0.5_MK * this%f(1:dim,i) * dt**2
                 
              END IF
              
           END DO
           
           !-------------------------------------------------
           ! Other integration not available.
           !-------------------------------------------------
           
        CASE DEFAULT
           PRINT *, "particles_integrate_position : ", &
                "Order of accuracy not available !"
           stat_info = -1
           GOTO 9999
           
        END SELECT ! accuracy_order
     
#endif
        
9999    CONTINUE      
        
        RETURN
        
      END SUBROUTINE particles_integrate_position
      
