      SUBROUTINE colloid_init_magnetism_accumulation_matrix(this,stat_info)
        !----------------------------------------------------
        ! Subroutine  : colloid_init_magnetism_accumulation_matrix
        !----------------------------------------------------
        !
        ! Purpose     : Using current accumulative rotaiton 
        !               vector to compute
        !               accumulative rotation matrix for colloid.
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
        
        INTEGER                         :: i,dim,stat_info_sub
        REAL(MK), DIMENSION(3,3)        :: rot_matrix
        
        
        !----------------------------------------------------
        ! Initialization of variables.
        !----------------------------------------------------
        
        stat_info     = 0
        stat_info_sub = 0
        dim           = this%num_dim        
        
        rot_matrix(:,:) = 0.0_MK
        
        CALL tool_rotation_matrix(this%tool,&
             dim,this%cc_magnet_acc_rot_vector(1:3),&
             this%cc_magnet_acc_rot_vector(4),&
             rot_matrix(1:3,1:3),stat_info_sub)
           
        this%cc_magnet_acc_rot_matrix(1:3,1:3) = rot_matrix(1:3,1:3)            
        
           
9999    CONTINUE
        
        RETURN
        
      END SUBROUTINE colloid_init_magnetism_accumulation_matrix
      
