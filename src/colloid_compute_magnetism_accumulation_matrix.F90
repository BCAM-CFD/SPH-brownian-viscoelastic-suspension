      SUBROUTINE colloid_compute_magnetism_accumulation_matrix(this,stat_info)
        !----------------------------------------------------
        ! Subroutine  : colloid_compute_magnetism_accumulation_matrix
        !----------------------------------------------------
        !
        ! Purpose     : Using current rotaiton matrix A to 
        !               compute accumulative rotation matrix B
        !               for colloids at this time step, i.e.,
        !               B = A * B.
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
        
        TYPE(Colloid), INTENT(INOUT)    :: this
        INTEGER, INTENT(OUT)            :: stat_info
        
        INTEGER                         :: i
        
        !----------------------------------------------------
        ! Initialization of variables.
        !----------------------------------------------------
        
        stat_info     = 0
        
        this%cc_magnet_acc_rot_matrix(:,:) = &
             MATMUL(this%cc_magnet_rot_matrix(:,:), &
             this%cc_magnet_acc_rot_matrix(:,:) )
        
        
9999    CONTINUE
        
        RETURN
        
      END SUBROUTINE colloid_compute_magnetism_accumulation_matrix
      
