      SUBROUTINE colloid_set_flow_developed(this,stat_info)
        !----------------------------------------------------
        ! Subroutine  : colloid_set_flow_developed
        !----------------------------------------------------
        !
        ! Purpose     : Set colloidal particles velocity to
        !               the develop velocity. 
        !
        ! Remarks     : Currently, shear flow is available.
        !                
        !
        ! Revisions   :V0.1 9.12 2010, original version.
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
        
        TYPE(Colloid),INTENT(INOUT)     :: this
        INTEGER,INTENT(OUT)             :: stat_info
        
        !----------------------------------------------------
        ! Physics, boundary parameters :
        !----------------------------------------------------
        INTEGER                         :: stat_info_sub
        INTEGER                         :: dim, i,j
        REAL(MK),DIMENSION(3)           :: min_phys
        REAL(MK),DIMENSION(3)           :: max_phys
        REAL(MK),DIMENSION(3)           :: length
        INTEGER,DIMENSION(6)            :: bcdef
        INTEGER                         :: num_wall_solid
        REAL(MK),DIMENSION(:,:),POINTER :: shear_v        

        !----------------------------------------------------
        ! Initialization of variables.
      	!----------------------------------------------------
        
        stat_info     = 0
        stat_info_sub = 0

        NULLIFY(shear_v)

        !----------------------------------------------------
        ! Physics_parameters :
        !----------------------------------------------------
        
        dim = this%num_dim
        min_phys(1:dim) = this%min_phys(1:dim)
        max_phys(1:dim) = this%max_phys(1:dim)
        length(1:dim) = max_phys(1:dim) - min_phys(1:dim)
        
        !----------------------------------------------------
        ! Boundary parameters :
        !----------------------------------------------------
        bcdef(1:2*dim)= this%bcdef(1:2*dim)
        num_wall_solid = &
             boundary_get_num_wall_solid(this%boundary,stat_info_sub)
        
        IF ( num_wall_solid > 0 ) THEN
           
           CALL boundary_get_shear_v(this%boundary,shear_v,stat_info_sub)

           DO i = 1, dim
              
              IF ( bcdef(2*i) == ppm_param_bcdef_wall_solid ) THEN
                 
                 DO j = 1, this%num_colloid
                    
                    this%v(1:dim,j,1) = &
                         (shear_v(1:dim,2*i) - shear_v(1:dim,2*i-1)) * &
                         ( this%x(i,j) - min_phys(i)) / &
                         length(i) + shear_v(1:dim,2*i-1)
                    
                 END DO ! j = 1 , num_colloid
                 
              END IF ! wall_solid
              
           END DO ! i = 1, dim
           
        END IF ! num_wall_solid > 0
        
        
9999    CONTINUE
        
        !----------------------------------------------------
        ! Release all dynamics memories.
        !----------------------------------------------------
        
        IF(ASSOCIATED(shear_v)) THEN
           DEALLOCATE(shear_v)
        END IF

        RETURN
        
      END SUBROUTINE colloid_set_flow_developed
      
