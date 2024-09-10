      SUBROUTINE colloid_compute_magnetism_rotation_vector(this, dt, stat_info)
        !----------------------------------------------------
        ! Subroutine  : colloid_compute_magnetism_rotation_vector
        !----------------------------------------------------
        !
        ! Purpose     : Compute the rotation vector
        !
        ! Remark      : 
        !
        ! Reference  : Chen et. al. 2006, physics of fluids
        !              wikipedia
        !
        ! Revision   : V0.1  26.06.2013, original.
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
        
        TYPE(Colloid), INTENT(INOUT)    :: this
        REAL(MK), INTENT(IN)            :: dt
        INTEGER, INTENT(OUT)            :: stat_info
        
        REAL(MK)                        :: len, freq, phi
        
        !----------------------------------------------------
        ! Initialization of variables.
        !----------------------------------------------------
        
        stat_info = 0
        freq      = this%cc_magnet_rot_freq
        
        this%cc_magnet_rot_vector(1:3) = &
             this%cc_magnet_acc_rot_vector(1:3)
        
        !----------------------------------------------
        ! Normalize roation vector at this time step.
        !----------------------------------------------
        
        len = SQRT(DOT_PRODUCT(this%cc_magnet_rot_vector(1:3),&
             this%cc_magnet_rot_vector(1:3)))
        
        IF ( len < mcf_machine_zero ) THEN
           
           this%cc_magnet_rot_vector(1:3) = 0.0_MK
           this%cc_magnet_rot_vector(2)   = 1.0_MK
           this%cc_magnet_rot_vector(4)   = 0.0_MK
           
        ELSE
           
           this%cc_magnet_rot_vector(1:3) = &
                this%cc_magnet_rot_vector(1:3) / len
           this%cc_magnet_rot_vector(4)   = &
                2.0_MK * mcf_pi * freq * dt
           
        END IF
        
        RETURN
        
      END SUBROUTINE colloid_compute_magnetism_rotation_vector
      
