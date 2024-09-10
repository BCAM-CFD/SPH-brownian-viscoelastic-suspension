      SUBROUTINE particles_compute_colloid_absolute_position(this,&
           stat_info)
        !----------------------------------------------------
        ! Subroutine  : particles_compute_colloid_absolute_position
        !----------------------------------------------------
        !
        ! Purpose     : Compute the colloid boundary particle's
        !               absolute position after the colloid center
        !               translated.
        !               
        !
        ! Reference   :
        !
        ! Remark      : Colloid are modelled as rigid body.
        !
        ! Revision    : V0.1  17.11.2011, original version.
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
        !
        ! this           : an object of Particles Class.
        ! stat_info      : return flag of status.
        !----------------------------------------------------
        
        TYPE(Particles), INTENT(INOUT)          :: this
        INTEGER, INTENT(OUT)                    :: stat_info
        
        !----------------------------------------------------
        ! Local variables
	!----------------------------------------------------

        INTEGER                                 :: stat_info_sub
        INTEGER                                 :: dim, i
        INTEGER                                 :: ip, sid
        TYPE(Colloid),POINTER                   :: colloids
        LOGICAL                                 :: translate, rotate
        REAL(MK), POINTER, DIMENSION(:,:)       :: coll_x
        
        !----------------------------------------------------
        ! Initialization of variables.
        !----------------------------------------------------
        
        stat_info     = 0
        stat_info_sub = 0
        rotate        = .FALSE.
        
        dim = physics_get_num_dim(this%phys,stat_info_sub)
        
        NULLIFY(colloids)
        NULLIFY(coll_x)
        
        CALL physics_get_colloid(this%phys,colloids,stat_info_sub)
        translate    = &
             colloid_get_translate(colloids,stat_info_sub)
        rotate       = &
             colloid_get_rotate(colloids,stat_info_sub)
        
        CALL colloid_get_x(colloids,coll_x,stat_info_sub)
        
#ifdef __PARTICLES_POSITION_FIXED
#else
        
        IF ( translate .OR. rotate ) THEN
           
           !-------------------------------------------------
           ! Loop over all colloid boundary particles.
           !-------------------------------------------------
           
           DO i = 1, this%num_part_colloid
              
              !----------------------------------------------
              ! Get index of this boundary particle
              ! and its species ID.
              !----------------------------------------------
              
              ip  = this%part_colloid_list(1,i)
              sid = this%part_colloid_list(2,i)
              
              this%x(1:dim,ip) = this%x(1:dim,ip) + coll_x(1:dim,sid)
              
           END DO ! i =1, num_part_colloid
           
        END IF ! translate OR rotate
        
#endif        
        
9999    CONTINUE

        IF ( ASSOCIATED(coll_x) ) THEN
           DEALLOCATE(coll_x)
        END IF
        
        RETURN
        
      END SUBROUTINE particles_compute_colloid_absolute_position
      
