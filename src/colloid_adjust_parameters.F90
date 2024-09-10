      SUBROUTINE colloid_adjust_parameters(this,stat_info)
        !----------------------------------------------------
        ! Subroutine  : colloid_adjust_parameters
        !----------------------------------------------------
        !
        ! Purpose     : Adjust colloid parameters resonably.
        !
        ! Reference   :
        !
        ! Remark      :
        !
        ! Revisions   :  V0.1 21.05.2010, original version.
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
        
        TYPE(colloid), INTENT(INOUT)    :: this
        INTEGER, INTENT(OUT)            :: stat_info
        
        REAL(MK)                        :: cut_off
        
        !----------------------------------------------------
        ! Initialization of variables.
        !----------------------------------------------------
        
        stat_info = 0
        
        !----------------------------------------------------
        ! If colloids do not translate,
        ! we set translation velocity to be zero.
        !----------------------------------------------------
        
        IF ( .NOT. this%translate ) THEN
              
           this%v(:,:,:) = 0.0_MK
           
        END IF
           
        !----------------------------------------------------
        ! If colloids do not rotate,
        ! we set rotation velocity to be zero.
        !----------------------------------------------------
      
        IF ( .NOT. this%rotate ) THEN
              
           this%omega(:,:,:) = 0.0_MK
           
        END IF

        !----------------------------------------------------
        ! If colloids do not translate or rotate,
        ! we set sub time step to be zero.
        !----------------------------------------------------
        
        IF ( (.NOT. this%translate) .AND. &
             (.NOT. this%rotate ) ) THEN
           
           this%sub_time_step = 0
           
        END IF
        
        !----------------------------------------------------
        ! Set h to threshold gap, i.e., cc_lub_cut_off
        !----------------------------------------------------
        
        cut_off = 1.0e6_MK
        
        IF ( this%cc_lub_type > 0 .AND. &
             this%cc_lub_cut_off < cut_off ) THEN
           
           cut_off = this%cc_lub_cut_off
           
        END IF
        
        IF ( this%cc_repul_type > 0 .AND. &
             this%cc_repul_cut_off < cut_off) THEN
           
           cut_off = this%cc_repul_cut_off
           
        END IF
        
        IF ( this%cc_magnet_type > 0 .AND. &
             this%cc_magnet_cut_off < cut_off) THEN
           
           cut_off = this%cc_magnet_cut_off
           
        END IF
        
        this%h = cut_off / 3.0_MK
        
        RETURN
        
      END SUBROUTINE  colloid_adjust_parameters
      
