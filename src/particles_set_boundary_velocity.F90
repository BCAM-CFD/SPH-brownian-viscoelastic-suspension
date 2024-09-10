      SUBROUTINE particles_set_boundary_velocity(this,stat_info)
        !----------------------------------------------------
        ! Subroutine  : particles_set_boundary_velocity
        !----------------------------------------------------
        !
        ! Purpose     : Set boundary condition boundary 
        !               particles velocity,
        !               to have correct boundary conditions.
        !
        ! Remarks     : 
        !                
        !
        ! Revisions   :V0.1 22.11 2010, original version.
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
        ! Argumetns :
      	!----------------------------------------------------
        
        TYPE(Particles),INTENT(INOUT)   :: this
        INTEGER,INTENT(OUT)             :: stat_info
        
        !----------------------------------------------------
        ! Physics, boundary parameters :
        !----------------------------------------------------

        INTEGER                         :: dim
        TYPE(Boundary), POINTER         :: tboundary
        INTEGER                         :: num_wall_solid
        REAL(MK),DIMENSION(:,:),POINTER :: shear_v        
        INTEGER                         :: i,ip,sid
        
        INTEGER                         :: stat_info_sub

        !----------------------------------------------------
        ! Initialization of variables.
      	!----------------------------------------------------
        
        stat_info     = 0
        stat_info_sub = 0

        NULLIFY(tboundary)        
        NULLIFY(shear_v)
        
        !----------------------------------------------------
        ! Physics_parameters :
        !----------------------------------------------------
        
        dim = this%num_dim
        
        !----------------------------------------------------
        ! Boundary parameters :
        !----------------------------------------------------
        
        CALL physics_get_boundary(this%phys,tboundary,stat_info_sub)
        num_wall_solid = &
             boundary_get_num_wall_solid(tboundary,stat_info_sub)
        
        IF ( num_wall_solid > 0 ) THEN
           
           CALL boundary_get_shear_v(tboundary,shear_v,stat_info_sub)
        
           !-------------------------------------------------
           ! Assign particles which constitute solid walls 
           ! with solid walls' v.
           !-------------------------------------------------
           
           !PRINT *, "shear_v: ", shear_v(1:2,1:4)

           DO i =1, this%num_part_wall_solid_real
              
              ip  = this%part_wall_solid_real_list(1,i)
              sid = this%part_wall_solid_real_list(2,i)
              sid = ABS(sid)
              this%v(1:dim,ip) = shear_v(1:dim,sid)
              
           END DO
           
        END IF
        
9999    CONTINUE
        
        !----------------------------------------------------
        ! Release all dynamics memories.
        !----------------------------------------------------
        
        IF(ASSOCIATED(shear_v)) THEN
           DEALLOCATE(shear_v)
        END IF
        
        RETURN
        
      END SUBROUTINE particles_set_boundary_velocity
      
      
