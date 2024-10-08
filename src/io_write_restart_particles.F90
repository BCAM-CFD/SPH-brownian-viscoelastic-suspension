      SUBROUTINE io_write_restart_particles(this,&
           rank,step,d_particles,num_part,stat_info)
        !----------------------------------------------------
        ! Subroutine  :  io_write_particle
        !----------------------------------------------------
        !
        ! Purpose     :  Writing particles into  restart
        !                files, which are used for future.
        !
        ! Reference   :
        !
        ! Remark      :
        !
        ! Revisions   : V0.1 01.03.2009, original version.
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
      	! Modules
      	!----------------------------------------------------
        
        USE ppm_module_user_io
        
      	!----------------------------------------------------
      	! Arguments     
      	!----------------------------------------------------
        
        TYPE(IO),INTENT(IN)                     :: this
        INTEGER, INTENT(IN)                     :: rank
        INTEGER, INTENT(IN)                     :: step
        TYPE(Particles), INTENT(IN)             :: d_particles
        INTEGER, INTENT(IN)                     :: num_part
        INTEGER,INTENT(OUT)                     :: stat_info
        
      	!----------------------------------------------------
      	! Local variables 
      	!----------------------------------------------------
        
        INTEGER                                 :: stat_info_sub
        
        REAL(MK),DIMENSION(:,:),POINTER         :: x
        REAL(MK),DIMENSION(:,:),POINTER         :: v
        REAL(MK),DIMENSION(:),POINTER           :: rho
        REAL(MK),DIMENSION(:),POINTER           :: m
        INTEGER,DIMENSION(:,:),POINTER          :: id
       
        CHARACTER(LEN=MAX_CHAR)                 :: file_name
        INTEGER                                 :: ifmt,output_unit
        
        INTEGER                                 :: num_x,num_v,num_id
                
        INTEGER                                 :: data_dim
        REAL(MK),DIMENSION(:,:), POINTER        :: output
        
        INTEGER					:: i,j
        
        INTEGER                                 :: num_part_ppm
        CHARACTER(LEN=MAX_CHAR)                 :: cbuf
        INTEGER					:: clen

#ifdef __DEBUG
        INTEGER                                 :: debug_flag
        INTEGER                                 :: debug_threshold
        REAL(MK)                                :: time_routine_start

#endif

	!----------------------------------------------------
      	! Initialization of variables.
      	!----------------------------------------------------
        
        stat_info     = 0
        stat_info_sub = 0
        NULLIFY(x)
        NULLIFY(v)
        NULLIFY(rho)
        NULLIFY(m)
        NULLIFY(id)
        NULLIFY(output)
        
#ifdef __DEBUG
        debug_threshold = 3
        debug_flag = debug_get_flag(global_debug,stat_info_sub)
        IF( debug_flag > 2 .OR. debug_flag > debug_threshold) THEN
           CALL debug_substart(global_debug,rank,&
                "io_write_particles",&
                time_routine_start,stat_info_sub)
        END IF
#endif
        
        
        CALL particles_get_x(d_particles,x,num_part,stat_info)
        CALL particles_get_v(d_particles,v,num_part,stat_info)
        CALL particles_get_rho(d_particles,rho,num_part,stat_info)
        CALL particles_get_m(d_particles,m,num_part,stat_info)
        CALL particles_get_id(d_particles,id,num_part,stat_info)
        
      	!----------------------------------------------------
      	! Define the output file name for this time step.
      	!----------------------------------------------------
        
        WRITE(file_name,'(A,I8.8,A)') &
             TRIM(this%restart_particles_file),step,'.dat'
        
      	!----------------------------------------------------
      	! Define Format of output file.
      	!----------------------------------------------------
        
        IF (this%restart_particles_fmt(1:1) .EQ. 'f' .OR. &
             this%restart_particles_fmt(1:1) .EQ. 'F') THEN
           
           ifmt = ppm_param_io_ascii
           
        ELSE
           
           ifmt = ppm_param_io_binary
           
        ENDIF
        
        output_unit = this%restart_particles_unit
        
        !----------------------------------------------------
        ! Allocate memory for output.
        !----------------------------------------------------
        
        num_x  = SIZE(x,1)
        num_v  = SIZE(v,1)
        num_id = size(id,1)
        
        !----------------------------------------------------
        ! Position, velocity, density, mass, IDs.
        !----------------------------------------------------
        
        data_dim = num_x + num_v + 2 + num_id
        
        ALLOCATE(output(data_dim,num_part),STAT=stat_info_sub)
        
        
        IF (stat_info_sub /=0) THEN
           PRINT *, &
                'io_write_restart_particles : ',&
                'Allocation of buffer failed !'
           stat_info = -1
           GOTO 9999
        END IF
        
        !----------------------------------------------------
        ! Copy the data into output.
        !----------------------------------------------------
        
        DO j = 1, num_part
           
           DO i = 1, num_x
              
              output(i,j)  = x(i,j)
              
           END DO
           
           DO i = 1, num_v
              
              output(num_x+i,j) = v(i,j)
              
           END DO
           
           output(num_x+num_v+1,j) = rho(j)
           output(num_x+num_v+2,j) = m(j)
           
           DO i = 1, num_id          
              output(num_x+num_v+ 2+i,j) = id(i,j)
           END DO
           
        END DO
        
        !----------------------------------------------------
        ! Open ppm I/O unit for centralized I/O.
        !----------------------------------------------------
        
        CALL ppm_io_open(output_unit,file_name,&
             ppm_param_io_write, ppm_param_io_replace, &
             ifmt,ppm_param_io_centralized,stat_info_sub)
        
        IF (stat_info_sub /= 0) THEN
           PRINT *,&
                'io_write_restart_particles : ', &
                'Failed to open unit !'
           stat_info = -1
           GOTO 9999
        END IF
        
        num_part_ppm = num_part
        CALL ppm_io(output_unit,num_part_ppm,&
             ppm_param_io_write,ppm_param_io_sum,STAT=stat_info_sub)
        
        
        WRITE(cbuf,'(A1,I2,A6)') '(', data_dim ,'E16.8)'
        clen = LEN_TRIM(cbuf)         
        
        CALL ppm_io(output_unit,output,&
             ppm_param_io_write,ppm_param_io_concat,&
             IOFMT=cbuf(1:clen), STAT=stat_info_sub)
        
        
        IF (stat_info_sub /= 0) THEN
           PRINT *, &
                'io_write_restart_particles : ',&
                'Writing xp failed'
           stat_info = -1	   
           GOTO 9999
           
        END IF
        
        !----------------------------------------------------
        ! Close file.
        !----------------------------------------------------
        
        CALL ppm_io_close(output_unit,stat_info_sub)
        
        
        !----------------------------------------------------
        ! Print to confirm.
        !----------------------------------------------------
        
        IF (rank == 0) THEN
           
           WRITE(cbuf,'(2A)') &
                'Restart particles written to ',&
                TRIM(file_name)
           
           PRINT *, "***", TRIM(cbuf)
           
        END IF
        
        !----------------------------------------------------
        ! Return .
        !----------------------------------------------------
        
        
9999    CONTINUE
        
        
        IF (ASSOCIATED(x)) THEN 
           DEALLOCATE(x)
        END IF
        IF (ASSOCIATED(v)) THEN 
           DEALLOCATE(v)
        END IF
        IF (ASSOCIATED(rho)) THEN 
           DEALLOCATE(rho)
        END IF
        IF (ASSOCIATED(m)) THEN 
           DEALLOCATE(m)
        END IF
        IF (ASSOCIATED(id)) THEN 
           DEALLOCATE(id)
        END IF
        IF (ASSOCIATED(output)) THEN 
           DEALLOCATE(output)
        END IF
        
#ifdef __DEBUG
        IF( debug_flag > 2 .OR. debug_flag > debug_threshold) THEN
           CALL debug_substop(global_debug,rank,&
                "io_write_restart_particles",& 
                time_routine_start,stat_info_sub)
        END IF
#endif
        
        
        RETURN	
        
      END SUBROUTINE io_write_restart_particles
      
