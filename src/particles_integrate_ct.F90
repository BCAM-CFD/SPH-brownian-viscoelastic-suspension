      SUBROUTINE particles_integrate_ct(this,&
           num,dt,lambda,stat_info)
        !----------------------------------------------------
        ! Subroutine  : particles_integrate_ct
        !----------------------------------------------------
        !
        ! Purpose     : Integrate the conformation tensor of
        !               particles with required accuracy.
        !
        ! Reference   :
        !
        ! Remark      :
        !
        ! Revision    :  V0.1  29.07.2009, original version.
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
        ! this        : an object of Particles Class.
        ! num         : number of particles updated,
        !               i.e. first num particles in this%x 
        !               are operated.
        ! dt          : time step.
        ! lambda      : coefficient required.
        ! stat_info   : return flag of status.
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
        
        dim = physics_get_num_dim(this%phys,stat_info_sub)
        
        dim = dim**2
        
        !----------------------------------------------------
        ! Update conformation tensor.
        !----------------------------------------------------
        
        this%ct(1:dim,1:num) = &
             this%ct(1:dim,1:num) + &
             this%act(1:dim,1:num) * dt * lambda
        
        
9999    CONTINUE      
        
        RETURN
        
      END SUBROUTINE particles_integrate_ct
      
      
