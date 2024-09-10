      SUBROUTINE colloid_compute_magnetism_rotation_matrix(this,stat_info)
        !----------------------------------------------------
        ! Subroutine  : colloid_compute_magnetism_rotation_matrix
        !----------------------------------------------------
        !
        ! Purpose     : Using current rotaiton vector to 
        !               compute current rotation matrix.

        !
        ! Referecen   : Chen et al. 
        !               Physics of Fluids, 18, 103605, 2006.
        !
        ! Revision    : V.01  26.06.2013
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
        
        TYPE(Colloid), INTENT(OUT)      :: this
        INTEGER, INTENT(OUT)            :: stat_info
        
        INTEGER                         :: stat_info_sub
        REAL(MK), DIMENSION(3,3)        :: rot_matrix
        
        !----------------------------------------------------
        ! Initialization of variables.
        !----------------------------------------------------
        
        stat_info     = 0
        stat_info_sub = 0        
        
        rot_matrix(:,:) = 0.0_MK
        
        CALL tool_rotation_matrix(this%tool,&
             3,this%cc_magnet_rot_vector(1:3),this%cc_magnet_rot_vector(4),&
             rot_matrix(1:3,1:3),stat_info_sub)
        
        IF ( stat_info_sub /= 0 ) THEN
           PRINT *, __FILE__, __LINE__, &
                "Using tool_rotation_matrix failed! "
           stat_info = -1
           GOTO 9999                 
        END IF
        
        this%cc_magnet_rot_matrix(:,:) = rot_matrix(:,:)
        
        
9999    CONTINUE
        
        RETURN
        
      END SUBROUTINE colloid_compute_magnetism_rotation_matrix
      
