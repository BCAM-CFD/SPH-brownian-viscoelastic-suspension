      SUBROUTINE control_init_default(this,stat_info)
        !----------------------------------------------------
        !  Subroutine   :  control_init
        !----------------------------------------------------
        !
        !  Purpose      : Construtor of Class Control.
        !
        !  Reference    :
        !
        !  Remark       :
        !
        !  Revisions    : V0.1 15.07.2009, original version.
        !
        !----------------------------------------------------
        ! This code is  based on the original MCF code  developed by Xin Bian.
        ! The  current version  has  been developed  in collaboration  between
        ! Marco Ellero,  leader of the  CFD Modelling and Simulation  group at
        ! BCAM (Basque Center  for Applied Mathematics) in  Bilbao, Spain, and
        ! Adolfo Vázquez-Quesada from  the Department of Fundamental Physics
        ! at UNED, in Madrid, Spain.
        !
        ! Developers:
        !     Xin Bian.
        !     Adolfo Vázquez-Quesada.
        !
        ! Contact: a.vazquez-quesada@fisfun.uned.es
        !          mellero@bcamath.org                
        !----------------------------------------------------
        
        TYPE(Control),INTENT(OUT)       :: this
        INTEGER,INTENT(OUT)             :: stat_info
        
        INTEGER                         :: stat_info_sub

        !----------------------------------------------------
        ! Init values are for flow around cylinder
        ! using
        ! quintic spline
        ! non-symmetry force calculation
        ! Morris's formulation of density and force
        ! Euler integration.
        !----------------------------------------------------
        
        stat_info     = 0
        stat_info_sub = 0
        
        this%job_name            = ""
        this%job_submit_date     = ""
        
        !----------------------------------------------------
        ! For default, give current time of running 
        ! as submit date. It should be overwritten by 
        ! the parameter in ctrl.mcf file.
        !----------------------------------------------------
        CALL DATE_AND_TIME(this%job_submit_date)
        this%job_execute_date     = ""
        this%job_execute_time     = ""
        this%job_execute_zone     = ""
        CALL DATE_AND_TIME(this%job_execute_date, &
             this%job_execute_time, this%job_execute_zone,&
             this%job_execute_values)
        !----------------------------------------------------
        ! Job start time is set to be zero.
        !----------------------------------------------------
        !CALL CPU_TIME(this%job_time_start)
        this%job_time_start      = 0.0_MK

        this%debug_flag          = 1
        this%relax_run           = .FALSE.
        this%colloid_relax       = .FALSE.
        this%read_external       = .FALSE.
        this%kernel_type         = 1
        this%symmetry            = .FALSE.
        this%rhs_density_type    = 1
        this%dynamic_density_ref = .FALSE.
        this%stateEquation_type  = 1
        this%Newtonian           = .TRUE.
        this%Brownian            = .FALSE.
        this%random_seed         = -1        
        this%rhs_force_type      = 2
        this%pp_interact_cc      = .FALSE.
        this%pp_interact_cw      = .FALSE.
        this%cc_lub_type         = 0
        this%cc_repul_type       = 0
        this%cc_magnet_type      = 0
        this%cw_lub_type         = 0
        this%cw_repul_type       = 0
        this%stress_tensor       = .FALSE.
        this%p_energy            = .FALSE.
        this%flow_v_fixed        = .FALSE.
        this%integrate_type      = 1
        this%integrate_colloid_type = 1
        this%integrate_colloid_RK = 1
        this%integrate_colloid_AB = 1
        this%adaptive_dt         = 0
        this%write_output        = 1
        this%write_restart       = 0

        CALL tool_new(this%tool,stat_info_sub)

        RETURN
          
      END SUBROUTINE control_init_default
     
      
      SUBROUTINE control_display_parameters(this,stat_info)
        !----------------------------------------------------
        !  Subroutine   :  control_init
        !----------------------------------------------------
        !
        !  Purpose      : To display control parameters
        !
        !  Reference    :
        !
        !  Remark       :
        !
        !  Revisions    : V0.1 15.07.2009, original version.
        !
        !----------------------------------------------------
        ! This code is  based on the original MCF code  developed by Xin Bian.
        ! The  current version  has  been developed  in collaboration  between
        ! Marco Ellero,  leader of the  CFD Modelling and Simulation  group at
        ! BCAM (Basque Center  for Applied Mathematics) in  Bilbao, Spain, and
        ! Adolfo Vázquez-Quesada from  the Department of Fundamental Physics
        ! at UNED, in Madrid, Spain.
        !
        ! Developers:
        !     Xin Bian.
        !     Adolfo Vázquez-Quesada.
        !
        ! Contact: a.vazquez-quesada@fisfun.uned.es
        !          mellero@bcamath.org                
        !----------------------------------------------------
        
        TYPE(Control),INTENT(IN)        :: this
        INTEGER,INTENT(OUT)             :: stat_info
        CHARACTER(100)                  :: string_display
        CHARACTER(10)                   :: string_format

        INTEGER                         :: stat_info_sub
        !----------------------------------------------------
        ! Initialization
        !----------------------------------------------------
        
        stat_info     = 0
        stat_info_sub = 0
        
        !----------------------------------------------------
        ! Printing onto standard output device
        !----------------------------------------------------
        
        PRINT *, '============================================================'
        PRINT *, '              Control  parameters'
        PRINT *, '====================Start==================================='
        
        CALL tool_print_msg(this%tool,&
             "job_name", &
             this%job_name(1:LEN_TRIM(this%job_name)), &
             stat_info_sub)
        CALL tool_print_msg(this%tool,&
             "job_submit_date", &
             this%job_submit_date(1:LEN_TRIM(this%job_submit_date)), &
             stat_info_sub)
        CALL tool_print_msg(this%tool,&
             "job_execute_date", &
             this%job_execute_date(1:4)//"_"//&
             this%job_execute_date(5:6)//"_"//&
             this%job_execute_date(7:8), stat_info_sub)
        CALL tool_print_msg(this%tool,&
             "job_execute_time", &
             this%job_execute_time(1:2)//":"//&
             this%job_execute_time(3:4)//":"//&
             this%job_execute_time(5:10), stat_info_sub)       
        CALL tool_print_msg(this%tool,&
             "job_execute_zone ", &
             this%job_execute_zone(1:LEN_TRIM(this%job_execute_zone)),&
             stat_info_sub)
        !CALL tool_print_msg(this%tool,&
        !     "job_execute_values     : ", &
        !     this%job_execute_values(1:8),&
        !     stat_info_sub)
        CALL tool_print_msg(this%tool,&
             "job_time_start", &
             this%job_time_start, stat_info_sub)
        CALL tool_print_msg(this%tool,&
             "debug_flag", &
             this%debug_flag, stat_info_sub)
        CALL tool_print_msg(this%tool,&
             "relax_run", &
             this%relax_run,stat_info_sub)

        IF ( this%relax_run ) THEN
           
           CALL tool_print_msg(this%tool,&
                "colloid relax",&
                this%colloid_relax,stat_info_sub)
           
        END IF
        
        CALL tool_print_msg(this%tool,&
             "read external", &
             this%read_external,stat_info_sub)
        
        SELECT CASE(this%kernel_type)
           
        CASE (1)
           
           CALL tool_print_msg(this%tool,&
                "kernel_type", &
                "Quintic Spline", stat_info_sub)
           
        CASE (2)
           
           CALL tool_print_msg(this%tool,&
                "kernel_type", &
                "Lucy kernel", stat_info_sub)
           
        CASE DEFAULT
           
           CALL tool_print_msg(this%tool,&
                "kernel_type", this%kernel_type, &
                "not available!", stat_info_sub)
           
           stat_info = -1
           GOTO 9999
           
        END SELECT ! kernel_type
        
        CALL tool_print_msg(this%tool,&
             "inter-symmetry",&
             this%symmetry, stat_info_sub)
        
        SELECT CASE(this%rhs_density_type)
           
        CASE (1)
           
           CALL tool_print_msg(this%tool,&
                "rhs_density_type", &
                "Summation of mass density", stat_info_sub)
           
        CASE (2)
           
           CALL tool_print_msg(this%tool,&
                "rhs_density_type", &
                "Summation of number density", stat_info_sub)
           
        CASE DEFAULT
           
           CALL tool_print_msg(this%tool,&
                "rhs_density_type", this%rhs_density_type, &
                "not available!", stat_info_sub)
           stat_info = -1
           GOTO 9999
           
        END SELECT ! rhs_density_type
        
        CALL tool_print_msg(this%tool,&
             "dynamic_density_ref", &
             this%dynamic_density_ref, stat_info_sub)
        
        SELECT CASE(this%stateEquation_type)
           
        CASE (1)
           
           CALL tool_print_msg(this%tool,&
                "stateEquation_type", &
                "Morris et al., 1997", stat_info_sub)
           
        CASE (2)
           
           CALL tool_print_msg(this%tool,&
                "stateEquation_type", &
                "Batchelor. 1967", stat_info_sub)
           
        CASE DEFAULT

           CALL tool_print_msg(this%tool,&
                "stateEquation_type", this%stateEquation_type, &
                "not available!", stat_info_sub)
           
        END SELECT
        
        CALL tool_print_msg(this%tool,&
             "Newtonian fluid", &
             this%Newtonian, stat_info_sub)
        
        CALL tool_print_msg(this%tool,&
             "Brownian motion", &
             this%Brownian, stat_info_sub)
        
        CALL tool_print_msg(this%tool,&
             "random seed", &
             this%random_seed, stat_info_sub)
        
        SELECT CASE(this%rhs_force_type)
           
        CASE (1)
           
           CALL tool_print_msg(this%tool,&
                "rhs_force_type", &
                "Morris et al., J. Comput. Phys. 1997", &
                stat_info_sub)
           
        CASE (2)

           CALL tool_print_msg(this%tool,&
                "rhs_force_type", &
                "Espanol and Revenga, Phys. Rev. E 2003", &
                stat_info_sub)
           
        CASE (3)
           
           CALL tool_print_msg(this%tool,&
                "rhs_force_type", &
                "Hu and Adams, J. Comput. Phys. 2006", &
                stat_info_sub)
           
        CASE (4)
           
           CALL tool_print_msg(this%tool,&
                "rhs_force_type", &
                "Hu and Adams, Phys. Fluids 2006", &
                stat_info_sub)
           
        CASE DEFAULT

           CALL tool_print_msg(this%tool,&
                "rhs_force_type", this%rhs_force_type, &
                "not available!", stat_info_sub)
           
        END SELECT ! rhs_force_type
        

        CALL tool_print_msg(this%tool,&
             "pp_interact_cc", &
             this%pp_interact_cc, stat_info_sub)
        
        CALL tool_print_msg(this%tool,&
             "pp_interact_cw", &
             this%pp_interact_cw, stat_info_sub)
        
        CALL tool_print_msg(this%tool,&
             "cc_lub_type", &
             this%cc_lub_type, stat_info_sub)
        
        CALL tool_print_msg(this%tool,&
             "cc_repul_type", &
             this%cc_repul_type, stat_info_sub)

        CALL tool_print_msg(this%tool,&
             "cc_magnet_type", &
             this%cc_magnet_type, stat_info_sub)
        
        CALL tool_print_msg(this%tool,&
             "cw_lub_type", &
             this%cw_lub_type, stat_info_sub)
        
        CALL tool_print_msg(this%tool,&
             "cw_repul_type", &
             this%cw_repul_type, stat_info_sub)
        
        CALL tool_print_msg(this%tool,&
             "stress tensor", &
             this%stress_tensor, stat_info_sub)
        
        CALL tool_print_msg(this%tool,&
             "potential energy", &
             this%p_energy, stat_info_sub)
        
        CALL tool_print_msg(this%tool,&
             "flow velocity fixed", &
             this%flow_v_fixed, stat_info_sub)
        
        SELECT CASE(this%integrate_type)
           
        CASE (1)

           CALL tool_print_msg(this%tool,&
                "integrate_type", &
                "explicit Euler", stat_info_sub)
        CASE (2)
           
           CALL tool_print_msg(this%tool,&
                "integrate_type", &
                "modified velocity Verlet", stat_info_sub)
           
        CASE (3)
           
           CALL tool_print_msg(this%tool,&
                "integrate_type", &
                "predictor-corrector 2nd order", stat_info_sub)
           
        CASE DEFAULT
           
           CALL tool_print_msg(this%tool,&
                "integrate_type", this%integrate_type, &
                " not available!", stat_info_sub)
           
        END SELECT ! integrate_type
        
        
        SELECT CASE(this%integrate_colloid_type)
           
        CASE (-2)
           
           CALL tool_print_msg(this%tool,&
                "integrate_colloid_type", &
                "implicit velocity for pairwise colloids", &
                stat_info_sub)
           
        CASE (-1)
           
           CALL tool_print_msg(this%tool,&
                "integrate_colloid_type", &
                "implicit velocity for all colloids", &
                stat_info_sub)
           
        CASE (1)
           
           CALL tool_print_msg(this%tool,&
                "integrate_colloid_type", &
                "single step method", stat_info_sub)
           
           SELECT CASE(this%integrate_colloid_RK)
              
           CASE (1:4)
              
              CALL tool_print_msg(this%tool,&
                   "integrate_colloid_RK", &
                   "Runge-Kutta order", &
                   this%integrate_colloid_RK, stat_info_sub)
              
           CASE DEFAULT
              
              CALL tool_print_msg(this%tool,&
                   "integrate_colloid_RK", &
                   this%integrate_colloid_RK, &
                   " not available!", stat_info_sub)
              
              stat_info = -1
              GOTO 9999
              
           END SELECT ! integrate_colloid_RK
           
        CASE (2)
           
           CALL tool_print_msg(this%tool,&
                "integrate_colloid_type", &
                "multiple steps method", stat_info_sub)
           
           SELECT CASE(this%integrate_colloid_AB)
              
           CASE (1:5)
              
              CALL tool_print_msg(this%tool,&
                   "integrate_colloid_AB", &
                   "Adams-Bashforth order", &
                   this%integrate_colloid_AB, stat_info_sub)
              
           CASE DEFAULT
              
              CALL tool_print_msg(this%tool,&
                   "integrate_colloid_AB", &
                   this%integrate_colloid_AB, &
                   " not available!", stat_info_sub)
              
              stat_info = -1
              GOTO 9999
              
           END SELECT ! integrate_colloid_AB
           
        CASE DEFAULT
           
           CALL tool_print_msg(this%tool,&
                "integrate_colloid_type", &
                this%integrate_colloid_type, &
                " not available!", stat_info_sub)
           stat_info = -1
           GOTO 9999
           
        END SELECT ! integrate_colloid_TYPE
        
        CALL tool_print_msg(this%tool,&
             "adaptive dt", this%adaptive_dt, stat_info_sub)
        CALL tool_print_msg(this%tool,&
             "write output", this%write_output, stat_info_sub)
        CALL tool_print_msg(this%tool,&
             "write restart", this%write_restart, stat_info_sub)
        
        PRINT *, '=====================END===================================='
        PRINT *, '              Control  parameters'
        PRINT *, '============================================================'
  
9999    CONTINUE
        
        RETURN
        
      END SUBROUTINE control_display_parameters
