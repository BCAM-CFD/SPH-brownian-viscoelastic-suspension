      SUBROUTINE io_init_default(this,stat_info)
        !----------------------------------------------------
        ! Subroutine  : io_init_default
        !----------------------------------------------------
        !
        ! Purpose     : Default construtor of IO Class.
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

        !----------------------------------------------------
        ! Arguments
        !----------------------------------------------------

        TYPE(IO),INTENT(OUT)            :: this
        INTEGER,INTENT(OUT)             :: stat_info
        
        INTEGER                         :: stat_info_sub
        
        stat_info     = 0
        stat_info_sub = 0
        
        this%ctrl_file = TRIM("ctrl.mcf")
        this%ctrl_unit = 20
        
        this%physics_config_file = TRIM("physics_config.mcf")
        this%physics_config_unit = 21
        
        this%io_config_file = TRIM("io_config.mcf")
        this%io_config_unit = 22
        
        this%write_output = 1
        
        this%read_particles_file  = TRIM("mcf_restart_particles.dat")
        this%read_particles_unit  = 23
        this%read_particles_fmt   =  TRIM("FORMATTED")

        this%read_conformation_file  = TRIM("mcf_restart_conformation.dat")
        this%read_conformation_unit  = 24
        this%read_conformation_fmt   =  TRIM("FORMATTED")
        
        this%output_particles_relax_file  = TRIM("mcf_init_particles")
        this%output_particles_relax_unit  = 30
        this%output_particles_relax_fmt   = TRIM("FORMATTED")
        this%output_particles_relax_freq_step  = 100

        this%output_particles_file  = TRIM("mcf_particles")
        this%output_particles_unit  = 31
        this%output_particles_fmt   = TRIM("FORMATTED")
        this%output_particles_freq_step  = 100
        this%output_particles_freq_time  = 0.1_MK
        this%output_particles_freq_time_num = 0

        this%output_conformation_file  = TRIM("mcf_conformation")
        this%output_conformation_unit  = 32
        this%output_conformation_fmt   = TRIM("FORMATTED")
        this%output_conformation_freq_step  = 100
        this%output_conformation_freq_time  = 0.1_MK
        this%output_conformation_freq_time_num = 0

        this%colloid_file  = TRIM("mcf_colloid")
        this%colloid_unit  = 100
        this%colloid_fmt   = TRIM("FORMATTED")
        this%colloid_freq_step  = 100
        this%colloid_freq_time  = 0.1_MK
        this%colloid_freq_time_num = 0
        
        this%statistic_relax_file       = TRIM("mcf_init_statistic.dat")
        this%statistic_relax_unit       = 40
        this%statistic_relax_fmt        = TRIM("FORMATTED")
        this%statistic_relax_freq_step  = 1

        this%statistic_file  = TRIM("mcf_statistic.dat")
        this%statistic_unit  = 41
        this%statistic_fmt   = TRIM("FORMATTED")
        this%statistic_freq_step  = 100
        this%statistic_freq_time  = 0.1_MK
        this%statistic_freq_time_num = 0

        this%boundary_file  = TRIM("mcf_boundary.dat")
        this%boundary_unit  = 50
        this%boundary_fmt   = TRIM("FORMATTED")
        this%boundary_freq_step  = 100
        this%boundary_freq_time  = 0.1_MK
        this%boundary_freq_time_num = 0
        
        this%write_restart = 0
        
        this%restart_particles_relax_file  = TRIM("mcf_init_restart_particles")
        this%restart_particles_relax_unit  = 90
        this%restart_particles_relax_fmt   =  TRIM("FORMATTED")
        
        this%restart_physics_file    = TRIM("mcf_restart_physics")
        this%restart_physics_unit    = 91
        this%restart_physics_fmt     =  TRIM("FORMATTED")
        
        this%restart_particles_file  = TRIM("mcf_restart_particles")
        this%restart_particles_unit  = 92
        this%restart_particles_fmt   =  TRIM("FORMATTED")
        
        this%restart_conformation_file  = TRIM("mcf_restart_conformation")
        this%restart_conformation_unit  = 93
        this%restart_conformation_fmt   =  TRIM("FORMATTED")
        
        this%restart_freq_step       = 100
        this%restart_freq_time       = 0.1_MK
        this%restart_freq_time_wall  = 48.0_MK
        this%restart_freq_time_num   = 1

        this%write_particles    = .FALSE.
        this%write_conformation = .FALSE.
        this%write_colloid      = .FALSE.
        this%write_statistic    = .FALSE.
        this%write_boundary     = .FALSE.        
        this%write_restart_physics      = .FALSE.
        this%write_restart_particles    = .FALSE.
        this%write_restart_conformation = .FALSE.

        CALL tool_new(this%tool,stat_info_sub)
        
        
        RETURN          
        
      END SUBROUTINE io_init_default
      
      !**** Changed by Adolfo *****
!      SUBROUTINE io_init(this,d_ctrl,d_ctrl_file,d_phys,stat_info)
      SUBROUTINE io_init(this,d_ctrl,d_ctrl_file,d_phys,d_random,stat_info)
        !************************
        
        TYPE(IO), INTENT(OUT)                  :: this
        TYPE(Control), INTENT(INOUT),TARGET    :: d_ctrl
        CHARACTER(LEN=*), INTENT(IN)           :: d_ctrl_file
        TYPE(Physics), INTENT(INOUT),TARGET    :: d_phys
        !******* Added by Adolfo ********
        TYPE(Random), INTENT(IN)            :: d_random
        !********************************
        INTEGER,INTENT(OUT)                    :: stat_info
        
        INTEGER                                :: stat_info_sub
        
        stat_info     = 0
        stat_info_sub = 0
        
        !------------------------------------------
        ! The name of control file is by default 
        ! "ctrl.mcf", if not given as parameter.
        ! The other two files which are physics 
        ! config and io config files, named in
        ! in "ctrl.mcf".
        !--------------------------------------------------------------

        !--------------------------------------------------------------
        ! The files' Units of file handling are all pre-defined, fixed.
        !--------------------------------------------------------------

        NULLIFY(this%ctrl)
        this%ctrl => d_ctrl
        
        NULLIFY(this%phys)
        this%phys => d_phys
     
        IF ( LEN_TRIM(d_ctrl_file) < 0 ) THEN
           PRINT *, "io_init : ", &
                "The given ctrl file name is empty !"
           stat_info = -1
           GOTO 9999
        END IF
        
        this%ctrl_file = TRIM(ADJUSTL(d_ctrl_file))
        
        this%ctrl_unit = 20
        
        this%physics_config_file = TRIM("physics_config.mcf")
        this%physics_config_unit = 21
        
        this%io_config_file = TRIM("io_config.mcf")
        this%io_config_unit = 22      
        
        this%read_particles_file  = TRIM("mcf_restart_particles.dat")
        this%read_particles_unit  = 23
        this%read_particles_fmt   =  TRIM("FORMATTED")
        
        this%read_conformation_file  = TRIM("mcf_restart_conformation.dat")
        this%read_conformation_unit  = 24
        this%read_conformation_fmt   =  TRIM("FORMATTED")
        
        this%output_particles_relax_file  = TRIM("mcf_init_particles")
        this%output_particles_relax_unit  = 30
        this%output_particles_relax_fmt   =  TRIM("FORMATTED")
        this%output_particles_relax_freq_step  = 100
        
        this%output_particles_file  = TRIM("mcf_particles")
        this%output_particles_unit  = 31
        this%output_particles_fmt   =  TRIM("FORMATTED")
        this%output_particles_freq_step  = 100
        this%output_particles_freq_time  = 0.1_MK
        this%output_particles_freq_time_num = 0

        this%output_conformation_file  = TRIM("mcf_conformation")
        this%output_conformation_unit  = 32
        this%output_conformation_fmt   =  TRIM("FORMATTED")
        this%output_conformation_freq_step  = 100
        this%output_conformation_freq_time  = 0.1_MK
        this%output_conformation_freq_time_num = 0

        this%colloid_file  = TRIM("mcf_colloid")
        this%colloid_unit  = 100
        this%colloid_fmt   =  TRIM("FORMATTED")
        this%colloid_freq_step  = 1
        this%colloid_freq_time  = 0.1_MK
        this%colloid_freq_time_num = 0

        this%statistic_relax_file  =  TRIM("mcf_init_statistic.dat")
        this%statistic_relax_unit  = 40
        this%statistic_relax_fmt   =  TRIM("FORMATTED")
        this%statistic_relax_freq_step  = 1
        
        this%statistic_file  = TRIM("mcf_statistic.dat")
        this%statistic_unit  = 41
        this%statistic_fmt   =  TRIM("FORMATTED")
        this%statistic_freq_step  = 1
        this%statistic_freq_time  = 0.1_MK
        this%statistic_freq_time_num = 0

        this%boundary_file  =  TRIM("mcf_boundary.dat")
        this%boundary_unit  = 50
        this%boundary_fmt   =  TRIM("FORMATTED")
        this%boundary_freq_step  = 1
        this%boundary_freq_time  = 0.1_MK
        this%boundary_freq_time_num = 0

        this%restart_particles_relax_file  = TRIM("mcf_init_restart_particles")
        this%restart_particles_relax_unit  = 90
        this%restart_particles_relax_fmt   =  TRIM("FORMATTED")

        this%restart_physics_file    = TRIM("mcf_restart_physics")
        this%restart_physics_unit    = 91
        this%restart_physics_fmt     =  TRIM("FORMATTED")
        
        this%restart_particles_file  = TRIM("mcf_restart_particles")
        this%restart_particles_unit  = 92
        this%restart_particles_fmt   =  TRIM("FORMATTED")
        
        this%restart_conformation_file  = TRIM("mcf_restart_conformation")
        this%restart_conformation_unit  = 93
        this%restart_conformation_fmt   =  TRIM("FORMATTED")
        
        this%restart_freq_step  = 100
        this%restart_freq_time  = 0.1_MK
        this%restart_freq_time_wall = 48.0_MK
        this%restart_freq_time_num   = 1

        CALL tool_new(this%tool,stat_info_sub)        
        
        CALL io_read_ctrl(this,d_ctrl,stat_info_sub)
        IF( stat_info_sub /= 0 ) THEN
           PRINT *, "io_init : ", &
                "Reading control file has problem !"
           stat_info = -1
           GOTO 9999
        END IF
        
        !********** Changed by Adolfo *********
        CALL io_read_physics_config(this,d_phys,d_random, stat_info_sub)
!        CALL io_read_physics_config(this,d_phys, stat_info_sub)
        !**************************************
        IF( stat_info_sub /= 0 ) THEN
           PRINT *, "io_init : ", &
                "Reading physics config file has problem !"
           stat_info = -1
           GOTO 9999
        END IF
        
        CALL io_read_io_config(this,stat_info_sub)
        IF( stat_info_sub /= 0 ) THEN
           PRINT *, "io_init : ", &
                "Reading io config file has problem !"
           stat_info = -1
           GOTO 9999
        END IF

        this%write_output  = &
             control_get_write_output(this%ctrl,stat_info_sub)
        this%write_restart = &
             control_get_write_restart(this%ctrl,stat_info_sub)
        this%write_particles    = .FALSE.
        this%write_conformation = .FALSE.
        this%write_colloid      = .FALSE.
        this%write_statistic    = .FALSE.
        this%write_boundary     = .FALSE.        
        this%write_restart_physics      = .FALSE.
        this%write_restart_particles    = .FALSE.
        this%write_restart_conformation = .FALSE.
        
        CALL tool_new(this%tool,stat_info_sub)

9999    CONTINUE
        
        RETURN
        
      END SUBROUTINE io_init
      
      
      SUBROUTINE io_display_parameters(this,stat_info)
        
        
        TYPE(IO),INTENT(IN)           :: this
        INTEGER,INTENT(OUT)           :: stat_info
        
        LOGICAL                       :: read_external
        LOGICAL                       :: relax_run
        LOGICAL                       :: Newtonian
        
        INTEGER                       :: num_species
        TYPE(Boundary), POINTER       :: tboundary
        INTEGER                       :: num_shear
        INTEGER                       :: num_colloid

        INTEGER                       :: stat_info_sub
        
        stat_info     = 0
        stat_info_sub = 0
        NULLIFY(tboundary)
        
        read_external = &
             control_get_read_external(this%ctrl,stat_info_sub)
        relax_run     = &
             control_get_relax_run(this%ctrl,stat_info_sub)  
        Newtonian     = &
             control_get_Newtonian(this%ctrl,stat_info_sub)
        num_species   = &
             physics_get_num_species(this%phys,stat_info_sub)
        
        CALL physics_get_boundary(this%phys,tboundary,stat_info_sub)
        
        num_shear    = &
             boundary_get_num_shear(tboundary,stat_info_sub)
        
        num_colloid  = &
             physics_get_num_colloid(this%phys,stat_info_sub)
        
        PRINT *, '============================================================'
        PRINT *, '              IO  parameters'
        PRINT *, '====================Start==================================='
     
        CALL tool_print_msg(this%tool,&
             "ctrl_file", &
             this%ctrl_file(1:LEN_TRIM(this%ctrl_file)), &
             stat_info_sub)  
        CALL tool_print_msg(this%tool,&
             "ctrl_unit", this%ctrl_unit, stat_info_sub)
        CALL tool_print_msg(this%tool,&
             "physics_config_file",TRIM(this%physics_config_file), &
             stat_info_sub)
        CALL tool_print_msg(this%tool,&
             "physics_config_unit", this%physics_config_unit, &
             stat_info_sub)
        CALL tool_print_msg(this%tool,&
             "io_config_file", TRIM(this%io_config_file), &
             stat_info_sub)
        CALL tool_print_msg(this%tool,&
             "io_config_unit", this%io_config_unit, &
             stat_info_sub)
        
        IF( read_external ) THEN
           
           CALL tool_print_msg(this%tool,"read_particle_file", &
                TRIM(this%read_particles_file), stat_info_sub)
           CALL tool_print_msg(this%tool, "read_particles_unit", &
                this%read_particles_unit, stat_info_sub)
           CALL tool_print_msg(this%tool, "read_particles_format", &
                TRIM(this%read_particles_fmt), stat_info_sub)
           
           IF ( .NOT. Newtonian ) THEN
              
              CALL tool_print_msg(this%tool, "read_conformation_file", &
                   TRIM(this%read_conformation_file), stat_info_sub)
              CALL tool_print_msg(this%tool, "read_conformation_unit", &
                   this%read_conformation_unit, stat_info_sub)
              CALL tool_print_msg(this%tool, "read_conformation_format", &
                   TRIM(this%read_conformation_fmt), stat_info_sub)
              
           END IF
           
        END IF
        
        CALL tool_print_msg(this%tool, "write output", &
             this%write_output, stat_info_sub)
        
        IF ( this%write_output > 0 .AND. &
             this%write_output < 3 ) THEN
           
           IF( relax_run ) THEN
              CALL tool_print_msg(this%tool, "output_particles_relax_file", &
                   TRIM(this%output_particles_relax_file),  "****.out", &
                   stat_info_sub)
              CALL tool_print_msg(this%tool, "output_particles_relax_unit", &
                   this%output_particles_relax_unit, stat_info_sub)
              CALL tool_print_msg(this%tool, "output_particles_relax_format", &
                   TRIM(this%output_particles_relax_fmt), stat_info_sub)
              CALL tool_print_msg(this%tool, "output_particles_relax_freq_step", &
                   this%output_particles_relax_freq_step, stat_info_sub)
           END IF
           CALL tool_print_msg(this%tool, "output_particles_file", &
                TRIM(this%output_particles_file), "****.out", &
                stat_info_sub)
           CALL tool_print_msg(this%tool, "output_particles_unit", &
                this%output_particles_unit, stat_info_sub)
           CALL tool_print_msg(this%tool, "output_particles_format", &
                TRIM(this%output_particles_fmt), stat_info_sub)
           
           SELECT CASE ( this%write_output )
           CASE (1)
              CALL tool_print_msg(this%tool, "output_particles_freq_step", &
                   this%output_particles_freq_step, stat_info_sub)
           CASE (2)
              CALL tool_print_msg(this%tool, "output_particles_freq_time", &
                   this%output_particles_freq_time, stat_info_sub)
           END SELECT
           
           IF ( .NOT. Newtonian ) THEN
              CALL tool_print_msg(this%tool, "output_conformation_file", &
                   TRIM(this%output_conformation_file), "****.out", &
                   stat_info_sub)
              CALL tool_print_msg(this%tool, "output_conformation_unit", &
                   this%output_conformation_unit, stat_info_sub)
              CALL tool_print_msg(this%tool, "output_conformation_format", &
                   TRIM(this%output_conformation_fmt), stat_info_sub)
              SELECT CASE ( this%write_output )
              CASE (1)
                 CALL tool_print_msg(this%tool, "output_conformation_freq_step", &
                      this%output_conformation_freq_step, stat_info_sub)
              CASE (2)
                 CALL tool_print_msg(this%tool, "output_conformation_freq_time", &
                      this%output_conformation_freq_time, stat_info_sub)
              END SELECT
           END IF
           
           IF ( num_species > 1 .AND. num_colloid > 0 )THEN
              CALL tool_print_msg(this%tool, "colloid_file", &
                   TRIM(this%colloid_file), "**.dat", stat_info_sub)
              CALL tool_print_msg(this%tool, "colloid_unit", &
                   this%colloid_unit, stat_info_sub)
              CALL tool_print_msg(this%tool, "colloid_format", &
                   TRIM(this%colloid_fmt), stat_info_sub)
              SELECT CASE ( this%write_output )
              CASE (1)
                 CALL tool_print_msg(this%tool, "colloid_freq_step", &
                      this%colloid_freq_step, stat_info_sub)
              CASE (2)
                 CALL tool_print_msg(this%tool, "colloid_freq_time", &
                      this%colloid_freq_time, stat_info_sub)
              END SELECT
           END IF
           
           IF( relax_run ) THEN
              CALL tool_print_msg(this%tool, "statistic_relax_file", &
                   TRIM(this%statistic_relax_file), stat_info_sub)
              CALL tool_print_msg(this%tool, "statistic_relax_unit", &
                   this%statistic_relax_unit, stat_info_sub)
              CALL tool_print_msg(this%tool, "statistic_relax_format", &
                   TRIM(this%statistic_relax_fmt), stat_info_sub)
              CALL tool_print_msg(this%tool, "statistic_relax_freq_step", &
                   this%statistic_relax_freq_step, stat_info_sub)
           END IF
           
           CALL tool_print_msg(this%tool, "statistic_file", &
                TRIM(this%statistic_file), stat_info_sub)
           CALL tool_print_msg(this%tool, "statistic_unit", &
                this%statistic_unit, stat_info_sub)
           CALL tool_print_msg(this%tool, "statistic_format", &
                TRIM(this%statistic_fmt), stat_info_sub)
           SELECT CASE ( this%write_output )
           CASE (1)
              CALL tool_print_msg(this%tool, "statistic_freq_step", &
                   this%statistic_freq_step, stat_info_sub)
           CASE (2)
              CALL tool_print_msg(this%tool, "statistic_freq_time", &
                   this%statistic_freq_time, stat_info_sub)
           END SELECT
           
           IF ( num_shear > 0 )THEN
              CALL tool_print_msg(this%tool, "boundary_file", &
                   TRIM(this%boundary_file), stat_info_sub)
              CALL tool_print_msg(this%tool, "boundary_unit", &
                   this%boundary_unit, stat_info_sub)
              CALL tool_print_msg(this%tool, "boundary_format", &
                   TRIM(this%boundary_fmt), stat_info_sub)
              SELECT CASE ( this%write_output )
              CASE (1)
                 CALL tool_print_msg(this%tool, "boundary_freq_step", &
                      this%boundary_freq_step, stat_info_sub)
              CASE (2)
                 CALL tool_print_msg(this%tool, "boundary_freq_time", &
                      this%boundary_freq_time, stat_info_sub)
              END SELECT
           END IF
           
           IF( relax_run ) THEN
              
              IF( LEN(TRIM(this%restart_particles_relax_file)) > 0 ) THEN
                 CALL tool_print_msg(this%tool, "restart_particles_relax_file", &
                      TRIM(this%restart_particles_relax_file),&
                      "****.dat", stat_info_sub)
              ELSE
                 CALL tool_print_msg(this%tool, "restart_particles_relax_file", &
                      "", stat_info_sub)
              END IF
              CALL tool_print_msg(this%tool, "restart_particles_relax_unit", &
                   this%restart_particles_relax_unit, stat_info_sub)
              CALL tool_print_msg(this%tool, "restart_particles_relax_format", &
                   TRIM(this%restart_particles_relax_fmt), stat_info_sub)
              
           END IF
           
        END IF ! write_output
        
        
        IF ( this%write_restart > 0 ) THEN
           
           IF(LEN(TRIM(this%restart_physics_file)) > 0) THEN
              CALL tool_print_msg(this%tool, "restart_physics_file", &
                   TRIM(this%restart_physics_file),&
                   "****.dat", stat_info_sub)
           ELSE
              CALL tool_print_msg(this%tool, "restart_physics_file", &
                   "", stat_info_sub)
           END IF
           CALL tool_print_msg(this%tool, "restart_physics_unit", &
                this%restart_physics_unit, stat_info_sub)
           CALL tool_print_msg(this%tool, "restart_physics_format", &
                TRIM(this%restart_physics_fmt), stat_info_sub)
              
           IF(LEN(TRIM(this%restart_particles_file)) > 0) THEN
              CALL tool_print_msg(this%tool, "restart_particles_file", &
                   TRIM(this%restart_particles_file),&
                   "****.dat", stat_info_sub)
           ELSE
              CALL tool_print_msg(this%tool, "restart_particles_file", &
                   "", stat_info_sub)
           END IF
           
           CALL tool_print_msg(this%tool, "restart_particles_unit", &
                this%restart_particles_unit, stat_info_sub)
           CALL tool_print_msg(this%tool, "restart_particles_format", &
                TRIM(this%restart_particles_fmt), stat_info_sub)
           
           IF ( .NOT. Newtonian ) THEN
              
              IF(LEN(TRIM(this%restart_conformation_file)) > 0) THEN
                 CALL tool_print_msg(this%tool, "restart_conformation_file", &
                      TRIM(this%restart_conformation_file),&
                      "****.dat", stat_info_sub)
              ELSE
                 CALL tool_print_msg(this%tool, "restart_conformation_file", &
                      "", stat_info_sub)
              END IF
              CALL tool_print_msg(this%tool, "restart_conformation_unit", &
                   this%restart_conformation_unit, stat_info_sub)
              CALL tool_print_msg(this%tool, "restart_conformation_format", &
                   TRIM(this%restart_conformation_fmt), stat_info_sub)
              
           END IF
                 
           SELECT CASE ( this%write_restart ) 
           CASE (1)
              CALL tool_print_msg(this%tool, "restart_freq_step", &
                   this%restart_freq_step, stat_info_sub)
           CASE (2)
              CALL tool_print_msg(this%tool, "restart_freq_time", &
                   this%restart_freq_time, stat_info_sub)
           CASE (3)
              CALL tool_print_msg(this%tool, "restart_freq_time_wall", &
                   this%restart_freq_time_wall, " hours", stat_info_sub)
           CASE DEFAULT
              CALL tool_print_msg(this%tool, "restart_freq_time_wall", &
                   "no such way of writting restart files!", stat_info_sub)              
           END SELECT
           
        END IF ! write_restart
        
            
        PRINT *, '=====================END===================================='
        PRINT *, '              IO  parameters'
        PRINT *, '============================================================'
        
        
        RETURN
        
      END SUBROUTINE io_display_parameters
      
      
