      SUBROUTINE particles_integrate_eval(this,&
           num,dt,lambda,stat_info)
        !----------------------------------------------------
        ! Subroutine  : particles_integrate_eval
        !----------------------------------------------------
        !
        ! Purpose     : Integrate egenvalues of particles
        !                with required accuracy, in case of
        !               egenvector dynamics.
        !
        ! Reference   :
        !
        ! Remark      :
        !
        ! Revision    :  V0.1 04.08.2009, original version.
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
        !  Arguments
        !
        !  this       : an object of Particles Class.
        !  num        : number of particles needed to be updated,
        !               i.e. first num particles in this%x 
        !               are operated.
        !  dt         : time step.
        !  lambda     : coefficient required.
        !  stat_info  : return flag of status.
        !----------------------------------------------------
        
        TYPE(Particles), INTENT(INOUT)          :: this
        INTEGER, INTENT(IN)                     :: num
        REAL(MK), INTENT(IN)                    :: dt
        REAL(MK), INTENT(IN)                    :: lambda
        INTEGER, INTENT(OUT)                    :: stat_info
        
        !----------------------------------------------------
        ! Local variables
	!----------------------------------------------------
        
        INTEGER                                 :: stat_info_sub
        INTEGER                                 :: dim
        
        !----------------------------------------------------
        ! Initialization of variables.
        !----------------------------------------------------
        
        stat_info     = 0
        stat_info_sub = 0
        
        
        IF( num > this%num_part_real) THEN
           PRINT *, "particles_integrate_eval : ", &
                "num > num_part_real, wrong !"
           stat_info = -1
           GOTO 9999      
        END IF
        
        dim = physics_get_num_dim(this%phys,stat_info_sub)
        
        !----------------------------------------------------
        ! Integrate egenvalues.
        !----------------------------------------------------
        
        this%eval(1:dim,1:num) = &
             this%eval(1:dim,1:num) + &
             this%aeval(1:dim,1:num) * dt * lambda
        
        
9999    CONTINUE      
        
        RETURN
        
      END SUBROUTINE particles_integrate_eval
      
      
