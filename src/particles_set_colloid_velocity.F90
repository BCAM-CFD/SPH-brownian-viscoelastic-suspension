      SUBROUTINE particles_set_colloid_velocity(this,stat_info)
        !----------------------------------------------------
        ! Subroutine  : particles_set_colloid_velocity
        !----------------------------------------------------
        !
        ! Purpose     : Set colloidal boundary particles
        !               velocity to its instaneous velocity.
        !
        ! Refernece   :
        !
        ! Remark      :
        !
        ! Revision    : V0.1 14.10.2010, original version.
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
        ! this       : an object of Particles Class.
        ! stat_info  : return flag of status.
        !----------------------------------------------------
        
        TYPE(Particles), INTENT(INOUT)          :: this
        INTEGER, INTENT(OUT)                    :: stat_info
        
        
	!----------------------------------------------------
        ! Local variables start here :
	!----------------------------------------------------
        
        INTEGER                                 :: stat_info_sub
      
        INTEGER                                 :: dim
        INTEGER                                 :: num_colloid
        TYPE(Colloid), POINTER                  :: colloids
        INTEGER                                 :: i,ip,sid
        REAL(MK), DIMENSION(3)                  :: v_p
        
        !----------------------------------------------------
        ! Initialization of variables.
        !----------------------------------------------------
        
        stat_info     = 0
        stat_info_sub = 0
        
        dim         = &
             physics_get_num_dim(this%phys,stat_info_sub)
        num_colloid = &
             physics_get_num_colloid(this%phys,stat_info_sub)
        NULLIFY(colloids)
        
        
        IF ( num_colloid > 0 ) THEN
           
           CALL physics_get_colloid(this%phys,colloids,stat_info_sub)
           
           !-------------------------------------------------
           ! Loop over all colloidal boundary particles and
           ! assign them instaneous velocity according to
           ! their host colloids.
           !-------------------------------------------------
           
           DO i =1, this%num_part_colloid
              
              !----------------------------------------------
              ! Get index of this boundary particle
              ! and its species ID.
              !----------------------------------------------
              
              ip  = this%part_colloid_list(1,i)
              sid = this%part_colloid_list(2,i)
              
              CALL colloid_particle_velocity(colloids,&
                   this%x(1:dim,ip),sid,v_p(1:dim),stat_info_sub)
              
              IF ( stat_info_sub /= 0 ) THEN
                 PRINT *, "particles_set_colloid_velocity : ", &
                      "Calculating colloid particle velocity failed !"
                 stat_info = -1
                 GOTO 9999
              END IF
              
              this%v(1:dim,ip) = v_p(1:dim)
              
           END DO ! i = 1, num_part_colloid
           
        END IF ! num_colloid > 0
        
        
9999    CONTINUE
        
        RETURN
        
      END SUBROUTINE particles_set_colloid_velocity
      
      
