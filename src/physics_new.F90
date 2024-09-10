      SUBROUTINE physics_init(this,d_ctrl,stat_info)
        !----------------------------------------------------
        ! Subroutine  :  physics_init
        !----------------------------------------------------
        !
        ! Purpose     : Construtor of Class Physics.
        !
        ! Reference   :
        !
        ! Remark      :
        !
        ! Revisions   : V0.1 15.07.2009, original version.
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
        
        TYPE(Physics),INTENT(OUT)               :: this
        TYPE(Control), INTENT(IN),TARGET        :: d_ctrl
        INTEGER,INTENT(OUT)                     :: stat_info
        
        !----------------------------------------------------
        ! Init values are for flow around cylinder 
        ! with resolution 50*50
        ! dx(1:2) = 2.0e-3.
        ! This will be overritten, 
        ! if the physics.config file presents.
        !----------------------------------------------------
        
        INTEGER                                 :: stat_info_sub
        
        !----------------------------------------------------
        ! Initialization of variables.
        !----------------------------------------------------
        
        stat_info     = 0
        stat_info_sub = 0
        
        NULLIFY(this%ctrl)
        this%ctrl => d_ctrl
        
        this%num_species       = 2
        this%num_dim           = 2

        NULLIFY(this%min_phys)
        ALLOCATE(this%min_phys(2))
        this%min_phys(1:2) = 0.0_MK

        NULLIFY(this%max_phys)
        ALLOCATE(this%max_phys(2))
        this%max_phys(1)       = 1.0e-1_MK
        this%max_phys(2)       = 1.0e-1_MK
        
        NULLIFY(this%min_phys_t)
        ALLOCATE(this%min_phys_t(2))
        this%min_phys_t(1:2)   = this%min_phys(1:2)
        
        NULLIFY(this%max_phys_t)
        ALLOCATE(this%max_phys_t(2))
        this%max_phys_t(1:2)   = this%max_phys(1:2)
        
        this%lattice_type      = 1

        NULLIFY(this%num_part_dim)
        ALLOCATE(this%num_part_dim(2))
        
        this%num_part_dim(1)   = 50
        this%num_part_dim(2)   = 50
        
        NULLIFY(this%num_part_dim_t)
        ALLOCATE(this%num_part_dim_t(2))
        
        this%num_part_dim_t(1)   = 50
        this%num_part_dim_t(2)   = 50
        
        this%num_part_tot      = 2500
        
        NULLIFY(this%dx)
        ALLOCATE(this%dx(2))
        this%dx(1:2)           = &
             (this%max_phys(1:2)-this%min_phys(1:2)) / &
             this%num_part_dim(1:2)
        
        this%cut_off       = 9.6e-3_MK
        this%h             = 3.2e-3_MK
        this%dt            = -1.0_MK
        this%dt_c          = -1.0_MK
        this%dt_nu         = -1.0_MK
        this%fa_max        = -1.0_MK
        this%dt_f          = -1.0_MK
        this%step_start    = -1
        this%step_end      = -1
        this%step_current  = 0
        this%time_start    = 0.0_MK
        this%time_end      = 500.0_MK
        this%time_current  = 0.0_MK
        this%rho           = 1.0e+3_MK
        this%eta           = 1.0e-1_MK
        this%eta_coef      = 4.0_MK
        this%ksai          = 0.0_MK
        this%kt            = 0.0_MK
        this%c             = 2.0e-2_MK
        this%rho_ref       = this%rho*0.9_MK
        this%gamma         = 7.0_MK
        
        this%relax_type     = 0
        this%dt_relax       = 0.0_MK
        this%step_relax     = 0
        this%time_relax     = 0.0_MK
        this%disorder_level = 0.2_MK
        this%kt_relax       = 0.0_MK
        this%c_relax        = 0.0_MK

        this%tau  = 0.0_MK
	this%tau_sm = 0.0_MK
	this%k_sm = 0.0_MK
        this%n_p  = 0.0_MK
        this%kt_p = 0.0_MK
        this%eigen_dynamics    = .FALSE.
        NULLIFY(this%eval)
        this%eval_tolerance   = 0.0_MK        
        ALLOCATE(this%eval(4))
        NULLIFY(this%evec)
        ALLOCATE(this%evec(4,4))
        this%evec_normalize   = .FALSE.
        this%evec_tolerance   = 0.0_MK
        
        this%body_force_type = 1
        NULLIFY(this%body_force)
        ALLOCATE(this%body_force(2))
        this%body_force(1)    = 5.0e-5_MK
        this%body_force(2)    = 0.0_MK
        NULLIFY(this%body_force_d)
        ALLOCATE(this%body_force_d(2))      
        this%body_force_d(1:2)= 0.0_MK
        this%flow_direction   = 1
        this%flow_width       = 0.1_MK
        this%flow_v           = 0.0_MK
        this%flow_adjust_freq = 0
      
        this%num_colloid   = 0
        NULLIFY(this%colloids)
        
        NULLIFY(this%bcdef)
        ALLOCATE(this%bcdef(4))
        this%bcdef(1:4)       = ppm_param_bcdef_periodic
        
        NULLIFY(this%boundary)
        
        CALL tool_new(this%tool,stat_info_sub)
        
        RETURN
          
      END SUBROUTINE physics_init
     
      
      SUBROUTINE physics_display_parameters(this,stat_info)
        !----------------------------------------------------
        ! Subroutine  : physics_display parameters
        !----------------------------------------------------
        !
        ! Purpose     : To display physics parameters.
        !
        ! Reference   :
        !
        ! Remark      :
        !
        ! Revisions   : V0.1 15.07.2009, original version.
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

        TYPE(Physics),INTENT(IN)      :: this
        INTEGER,INTENT(OUT)           :: stat_info
        
        
        LOGICAL                       :: relax_run
        LOGICAL                       :: flow_v_fixed
        LOGICAL                       :: Newtonian
        
        INTEGER                       :: j,num_dim
        INTEGER                       :: stat_info_sub
          
        stat_info     = 0
        stat_info_sub = 0
        
        relax_run    = &
             control_get_relax_run(this%ctrl,stat_info_sub)
        flow_v_fixed = &
             control_get_flow_v_fixed(this%ctrl,stat_info_sub)      
        Newtonian = &
             control_get_Newtonian(this%ctrl,stat_info_sub)
        num_dim   = this%num_dim
        
        PRINT *, '============================================================'
        PRINT *, '              Physics  parameters'
        PRINT *, '====================Start==================================='
        
        CALL tool_print_msg(this%tool, &
             "num_species", this%num_species, stat_info_sub)
        CALL tool_print_msg(this%tool, &
             "num_dim", this%num_dim, stat_info_sub)
        CALL tool_print_msg(this%tool, &
             "min_phys", this%min_phys(1:num_dim), stat_info_sub)
        CALL tool_print_msg(this%tool, &
             "max_phys", this%max_phys(1:num_dim), stat_info_sub)
        CALL tool_print_msg(this%tool, &
             "min_phys_t", this%min_phys_t(1:num_dim), stat_info_sub)
        CALL tool_print_msg(this%tool, &
             "max_phys_t", this%max_phys_t(1:num_dim), stat_info_sub)
        
        IF( this%num_dim ==2 ) THEN
           
           SELECT CASE (this%lattice_type)
           CASE (1)
              CALL tool_print_msg(this%tool, &
                   "lattice type", "square lattice", stat_info_sub)
           CASE (2)
              CALL tool_print_msg(this%tool, &
                   "lattice type", "staggered lattice", stat_info_sub)
           CASE (3)
              CALL tool_print_msg(this%tool, &
                   "lattice type", "hexagonal lattice", stat_info_sub)
           END SELECT
           
        ELSE IF( this%num_dim == 3 ) THEN
           
           SELECT CASE (this%lattice_type)
           CASE (1)
              CALL tool_print_msg(this%tool, &
                   "lattice type", "simple cubic lattice", stat_info_sub)
           CASE (2)
              CALL tool_print_msg(this%tool, &
                   "lattice type", "body center lattice", stat_info_sub)
           CASE (3)
              CALL tool_print_msg(this%tool, &
                   "lattice type", "face center lattice", stat_info_sub)
           END SELECT
           
        END IF
        
        CALL tool_print_msg(this%tool, "num_part_dim", &
             this%num_part_dim(1:num_dim), stat_info_sub)
        CALL tool_print_msg(this%tool, "num_part_dim_t", &
             this%num_part_dim_t(1:num_dim), stat_info_sub)
        CALL tool_print_msg(this%tool, "num_part_tot", &
             this%num_part_tot, stat_info_sub)
        CALL tool_print_msg(this%tool, "dx", &
             this%dx(1:num_dim), stat_info_sub)
        CALL tool_print_msg(this%tool,&
             "cut_off", this%cut_off, stat_info_sub)
        CALL tool_print_msg(this%tool,&
             "smoothing length", this%h, stat_info_sub)
        CALL tool_print_msg(this%tool,&
             "dt", this%dt, stat_info_sub)
        CALL tool_print_msg(this%tool,&
             "dt_c", this%dt_c, stat_info_sub)
        CALL tool_print_msg(this%tool,&
             "dt_nu", this%dt_nu, stat_info_sub)
        CALL tool_print_msg(this%tool,&
             "dt_f", this%dt_f, stat_info_sub)
        CALL tool_print_msg(this%tool,&
             "step_start", this%step_start, stat_info_sub)
        CALL tool_print_msg(this%tool,&
             "step_end", this%step_end, stat_info_sub)
        CALL tool_print_msg(this%tool,&
             "time_start", this%time_start, stat_info_sub)
        CALL tool_print_msg(this%tool,&
             "time_end", this%time_end, stat_info_sub)
        CALL tool_print_msg(this%tool,&
             "rho", this%rho, stat_info_sub)
        CALL tool_print_msg(this%tool,&
             "eta", this%eta, stat_info_sub)
        CALL tool_print_msg(this%tool,&
             "eta_coef", this%eta_coef, stat_info_sub)
        CALL tool_print_msg(this%tool,&
             "ksai", this%ksai, stat_info_sub)
        CALL tool_print_msg(this%tool,&
             "kt", this%kt, stat_info_sub)
        CALL tool_print_msg(this%tool,&
             "c", this%c, stat_info_sub)
        CALL tool_print_msg(this%tool,&
             "rho_ref", this%rho_ref, stat_info_sub)
        CALL tool_print_msg(this%tool,&
             "gamma", this%gamma, stat_info_sub)
        CALL tool_print_msg(this%tool,&
             "tau_sm", this%tau_sm, stat_info_sub)
        CALL tool_print_msg(this%tool,&
             "k_sm", this%k_sm, stat_info_sub)
   
        IF ( relax_run ) THEN

           CALL tool_print_msg(this%tool,&
             "relax_type", this%relax_type, stat_info_sub)
           CALL tool_print_msg(this%tool,&
             "dt_relax", this%dt_relax, stat_info_sub)
           CALL tool_print_msg(this%tool,&
             "dt_c_relax", this%dt_c_relax, stat_info_sub)
           CALL tool_print_msg(this%tool,&
             "step_relax", this%step_relax, stat_info_sub)
           CALL tool_print_msg(this%tool,&
             "time_relax", this%time_relax, stat_info_sub)
           CALL tool_print_msg(this%tool,&
             "disorder_level", this%disorder_level, stat_info_sub)
           CALL tool_print_msg(this%tool,&
             "kt_relax", this%kt_relax, stat_info_sub)
           CALL tool_print_msg(this%tool,&
             "c_relax", this%c_relax, stat_info_sub)
           
        END IF
        
        IF (.NOT. Newtonian ) THEN
           
           CALL tool_print_msg(this%tool,&
                "tau", this%tau, stat_info_sub)
           CALL tool_print_msg(this%tool,&
                "n_p", this%n_p, stat_info_sub)
           CALL tool_print_msg(this%tool,&
                "kt_p", this%kt_p, stat_info_sub)
           CALL tool_print_msg(this%tool,&
                "eigen_dynamics", this%eigen_dynamics, stat_info_sub)
           IF( this%eigen_dynamics) THEN
              PRINT *, "eigenvalues      : "
              PRINT *, this%eval(1:num_dim)
              CALL tool_print_msg(this%tool,&
                   "eval_tolerance", this%eval_tolerance, stat_info_sub)
              PRINT *, "eigenvectors     : "
              DO j = 1, num_dim
                 PRINT *, this%evec(1:num_dim,j)
              END DO
              CALL tool_print_msg(this%tool,&
                   "evec_normalize", this%evec_normalize, stat_info_sub)
              IF ( this%evec_normalize ) THEN
                 CALL tool_print_msg(this%tool,&
                      "evec_tolerance", this%evec_tolerance, stat_info_sub)
              END IF
           END IF
           
        END IF
        
        CALL tool_print_msg(this%tool, "body_force_type", &
             this%body_force_type, stat_info_sub)
        CALL tool_print_msg(this%tool, "body_force", &
             this%body_force(1:num_dim), stat_info_sub)
        
        IF ( flow_v_fixed ) THEN
           
           CALL tool_print_msg(this%tool, "body_force_d", &
                this%body_force_d(1:num_dim), stat_info_sub)           
           CALL tool_print_msg(this%tool, "flow_directioin", &
                this%flow_direction, stat_info_sub)
           CALL tool_print_msg(this%tool, "flow_width", &
                this%flow_width, stat_info_sub)
           CALL tool_print_msg(this%tool, "flow_v", &
                this%flow_v, stat_info_sub)
           CALL tool_print_msg(this%tool, "flow_adjust_freq", &
                this%flow_adjust_freq, stat_info_sub)
           
        END IF
        
        CALL tool_print_msg(this%tool, "num_colloid", &
             this%num_colloid,stat_info_sub)
        
        IF (this%num_colloid > 0) THEN
           
           CALL colloid_display_parameters(this%colloids,stat_info_sub)
           
        END IF
        
        CALL boundary_display_parameters(this%boundary,stat_info_sub)
  
        PRINT *, '=====================END===================================='
        PRINT *, '              Physics  parameters'
        PRINT *, '============================================================'
        
        RETURN
        
      END SUBROUTINE physics_display_parameters
      
      
