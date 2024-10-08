      SUBROUTINE particles_reset_v(this,stat_info)
        !----------------------------------------------------
        ! Subroutine  : particles_reset_v
        !----------------------------------------------------
        !
        ! Purpose     : Reset particles with
        !               zero velocity.
        !
        ! Refernece   :
        !
        ! Remark      : 
        !
        !              
        !
        ! Revision    : V0.1 11.02 2010, original version.
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
        ! this       : an object of Particles Class.
        ! stat_info  : return flag of status.
        !----------------------------------------------------
        
        TYPE(Particles), INTENT(INOUT)          :: this
        INTEGER, INTENT(OUT)                    :: stat_info
        
        !----------------------------------------------------
        ! Local variables
	!----------------------------------------------------
        
        INTEGER                                 :: stat_info_sub
        
        INTEGER                                 :: num_dim
        INTEGER                                 :: num_part
        REAL(MK), DIMENSION(:,:),POINTER        :: v
        
        !----------------------------------------------------
        ! Initialization of variables.
        !----------------------------------------------------
        
        stat_info     = 0
        stat_info_sub = 0
        
        NULLIFY(v)
        
        num_dim  = this%num_dim
        num_part = this%num_part_real
        
        ALLOCATE(v(num_dim,num_part))
        
        
        v(:,:) = 0.0_MK
        
        CALL particles_set_v(this,v,num_part,stat_info_sub)
        
        
        
9999    CONTINUE
        
        IF(ASSOCIATED(v)) THEN
           DEALLOCATE(v)
        END IF
        
        RETURN
        
      END SUBROUTINE particles_reset_v
      

      SUBROUTINE particles_reset_f(this,stat_info)
        !----------------------------------------------------
        ! Subroutine  : particles_reset_f
        !----------------------------------------------------
        !
        ! Purpose     : Reset particles with
        !               zero force.
        !
        ! Refernece   :
        !
        ! Remark      : 
        !
        !              
        !
        ! Revision    : V0.1 11.02 2010, original version.
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
        ! this       : an object of Particles Class.
        ! stat_info  : return flag of status.
        !----------------------------------------------------
        
        TYPE(Particles), INTENT(INOUT)          :: this
        INTEGER, INTENT(OUT)                    :: stat_info
        
        !----------------------------------------------------
        ! Local variables
	!----------------------------------------------------
        
        INTEGER                                 :: stat_info_sub
        
        INTEGER                                 :: num_dim
        INTEGER                                 :: num_part
        REAL(MK), DIMENSION(:,:),POINTER        :: f
        
        !----------------------------------------------------
        ! Initialization of variables.
        !----------------------------------------------------
        
        stat_info     = 0
        stat_info_sub = 0
        
        NULLIFY(f)
        
        num_dim  = this%num_dim
        num_part = this%num_part_real
        
        ALLOCATE(f(num_dim,num_part))
        

        f(:,:) = 0.0_MK
        
        CALL particles_set_f(this,f,num_part,stat_info_sub)
        
        
9999    CONTINUE
        
        IF(ASSOCIATED(f)) THEN
           DEALLOCATE(f)
        END IF
        
        RETURN
        
      END SUBROUTINE particles_reset_f
      
