      SUBROUTINE particles_find_density_extreme(this, &
           comm,MPI_PREC,stat_info)
        !----------------------------------------------------
        ! Subroutine  : particles_find_density_extreme
        !----------------------------------------------------
        !
        ! Purpose     : Find the minimal and maximal
        !               density.
        !               
        !
        ! Routines    :
        !
        ! References  :
        !
        ! Remarks     :
        !
        ! Revisions   : V0.1 15.03.2010, original version.
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
        !----------------------------------------------------
        
        TYPE(Particles), INTENT(INOUT)          :: this
        INTEGER, INTENT(IN)                     :: comm
        INTEGER, INTENT(IN)                     :: MPI_PREC
        INTEGER, INTENT(OUT)                    :: stat_info
        
        !----------------------------------------------------
        ! Local variables.
        !----------------------------------------------------
        
        INTEGER                                 :: stat_info_sub
        INTEGER                                 :: rhs_density_type
        REAL(MK)                                :: rho_min
        REAL(MK)                                :: rho_max
        
        !----------------------------------------------------
        ! Initialization of variables.
        !----------------------------------------------------
        
        stat_info     = 0
        stat_info_sub = 0
        
        rhs_density_type = &
             control_get_rhs_density_type(this%ctrl,stat_info_sub)
        
        SELECT CASE(rhs_density_type)
           
        CASE (1)
           this%rho_min = &
                MINVAL(this%rho(1:this%num_part_real))
           this%rho_max = &
                MAXVAL(this%rho(1:this%num_part_real))
        CASE (2)
           this%rho_min = &
                MINVAL(this%rho(1:this%num_part_real)* &
                this%m(1:this%num_part_real))
           this%rho_max = &
                MAXVAL(this%rho(1:this%num_part_real)* &
                this%m(1:this%num_part_real))
        END SELECT
        
        
#ifdef __MPI
        
        !----------------------------------------------------
        ! In context of MPI, pick up the minimum and
        ! maximum from all processes,
        ! then broadcast to all processes.
        !----------------------------------------------------

        CALL MPI_ALLREDUCE (this%rho_min,rho_min, &
             1,MPI_PREC,MPI_MIN,comm,stat_info_sub)
        
        CALL MPI_ALLREDUCE (this%rho_max,rho_max, &
             1,MPI_PREC,MPI_MAX,comm,stat_info_sub)
        
        this%rho_min = rho_min
        this%rho_max = rho_max
        
#endif
        
        RETURN
        
      END SUBROUTINE particles_find_density_extreme
      
      
      
