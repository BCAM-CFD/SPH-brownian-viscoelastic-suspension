      SUBROUTINE io_write_particles(this,rank,step,&
           d_particles,num_part,stat_info)
        !----------------------------------------------------
        ! Subroutine  : io_write_particle
        !----------------------------------------------------
        !
        ! Purpose     : Writing particles' quantities into
        !               files, which is centralized.
        !               x, v, mass/number density, mass,
        !               species ID(if more than one species),
        !               particle ID.
        !
        ! Reference   :
        !
        ! Remark      :
        !
        ! Revisions   : V0.2 09.07.2009, 
        !               check again the work flow is correct
        !               and supply with more comments for code.
        !  
        !               V0.1 01.02.2009, original version.
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
        ! this           : an object of Marching Class.
        ! rank           : rank of process.
        ! step           : index of current step.
        ! d_particles    : an object of Particles Class
        ! num_part       : first num_part needed to be written.
        ! stat_info      : return flag of status.
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
        LOGICAL                                 :: read_external
        LOGICAL                                 :: Brownian
        LOGICAL                                 :: stress_tensor
        LOGICAL                                 :: p_energy
        
        REAL(MK), DIMENSION(:,:), POINTER       :: x
        REAL(MK), DIMENSION(:,:), POINTER       :: v
        REAL(MK), DIMENSION(:),   POINTER       :: rho
        REAL(MK), DIMENSION(:),   POINTER       :: m
        !********** Added by Adolfo *************
        REAL(MK), DIMENSION(:),   POINTER       :: spec
        !****************************************
        INTEGER,  DIMENSION(:,:), POINTER       :: id

#ifdef __IO_PARTICLES_FORCE_TOTAL
        REAL(MK), DIMENSION(:,:), POINTER       :: f
#endif
#ifdef __IO_PARTICLES_FORCE_SEPARATE
        REAL(MK), DIMENSION(:,:), POINTER       :: fp
        REAL(MK), DIMENSION(:,:), POINTER       :: fv
        REAL(MK), DIMENSION(:,:), POINTER       :: fr
#endif
!#ifdef __IO_PARTICLES_STRESS_TOTAL
        REAL(MK), DIMENSION(:,:), POINTER       :: s
!#endif
#ifdef __IO_PARTICLES_STRESS_SEPARATE
        REAL(MK), DIMENSION(:,:), POINTER       :: sp
        REAL(MK), DIMENSION(:,:), POINTER       :: sv
        REAL(MK), DIMENSION(:,:), POINTER       :: sr
#endif
        REAL(MK), DIMENSION(:),   POINTER       :: u
        
        CHARACTER(LEN=MAX_CHAR)                 :: file_name
        INTEGER                                 :: ifmt,output_unit
        
        INTEGER                                 :: num_x
        INTEGER                                 :: num_v
        INTEGER                                 :: num_id
#ifdef __IO_PARTICLES_FORCE_TOTAL
        INTEGER                                 :: num_f
#endif
#ifdef __IO_PARTICLES_FORCE_SEPARATE
        INTEGER                                 :: num_fp
        INTEGER                                 :: num_fv
        INTEGER                                 :: num_fr
#endif

        
        INTEGER                                 :: num_s, i
        INTEGER                                 :: data_dim
        INTEGER                                 :: current_dim
        REAL(MK), DIMENSION(:,:), POINTER       :: output
        
        CHARACTER(LEN=MAX_CHAR)                 :: cbuf
        INTEGER					:: clen

#ifdef __DEBUG
        INTEGER                                 :: debug_flag
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
        !****** Added by Adolfo *******
        NULLIFY(spec)
        !******************************
        NULLIFY(id)
#ifdef __IO_PARTICLES_FORCE_TOTAL
        NULLIFY(f)
#endif
#ifdef __IO_PARTICLES_FORCE_SEPARATE
        NULLIFY(fp)
        NULLIFY(fv)
        NULLIFY(fr)
#endif
!#ifdef __IO_PARTICLES_STRESS_TOTAL
        NULLIFY(s)
!#endif
#ifdef __IO_PARTICLES_STRESS_SEPARATE
        NULLIFY(sp)
        NULLIFY(sv)
        NULLIFY(sr)
#endif
        NULLIFY(u)
        NULLIFY(output)
        
#ifdef __DEBUG
        debug_flag = debug_get_flag(global_debug,stat_info_sub)
        IF( debug_flag == 2 ) THEN
           CALL debug_substart(global_debug,rank,"io_write_particles",&
                time_routine_start,stat_info_sub)
        END IF
#endif
        
	!----------------------------------------------------
        ! Get parameters.
	!----------------------------------------------------
        
        read_external   = &
             control_get_read_external(this%ctrl,stat_info_sub)
        Brownian        = &
             control_get_Brownian(this%ctrl,stat_info_sub)
        stress_tensor   = &
             control_get_stress_tensor(this%ctrl,stat_info_sub) 
        p_energy        = &
             control_get_p_energy(this%ctrl,stat_info_sub)
        
        CALL particles_get_x(d_particles,x,num_part,stat_info)
        CALL particles_get_v(d_particles,v,num_part,stat_info)
        CALL particles_get_rho(d_particles,rho,num_part,stat_info)
        CALL particles_get_m(d_particles,m,num_part,stat_info)
        !********* Added by Adolfo ********
        CALL particles_get_spec(d_particles,spec,num_part,stat_info)
        !**********************************
        
#ifdef __IO_PARTICLES_FORCE_TOTAL
        CALL particles_get_f(d_particles,f,num_part,stat_info)
#endif
#ifdef __IO_PARTICLES_FORCE_SEPARATE
        CALL particles_get_fp(d_particles,fp,num_part,stat_info)
        CALL particles_get_fv(d_particles,fv,num_part,stat_info)
        IF ( Brownian ) THEN
           CALL particles_get_fr(d_particles,fr,num_part,stat_info)
        END IF
#endif


        IF ( stress_tensor ) THEN
           
!#ifdef __IO_PARTICLES_STRESS_TOTAL
           CALL particles_get_s(d_particles,s,num_part,stat_info)
!#endif
           
#ifdef __IO_PARTICLES_STRESS_SEPARATE
           CALL particles_get_sp(d_particles,sp,num_part,stat_info)
           CALL particles_get_sv(d_particles,sv,num_part,stat_info)
           IF ( Brownian ) THEN
              CALL particles_get_sr(d_particles,sr,num_part,stat_info)
           END IF
#endif
        END IF ! stress tensor
        
        CALL particles_get_id(d_particles,id,num_part,stat_info)
        
        IF( p_energy ) THEN
           CALL particles_get_u(d_particles,u,num_part,stat_info)
        END IF
        
        !----------------------------------------------------
      	! Define the output file name for this time step.
        ! If we have read particles from file,
        ! the output should include "_r" to distingusih.
      	!----------------------------------------------------
        
      	!----------------------------------------------------
      	! Define format of output file.
      	!----------------------------------------------------
        
        IF( read_external ) THEN
           
           WRITE(file_name,'(2A,I8.8,A)') &
                TRIM(this%output_particles_file),'_r',step,'.out'
           
        ELSE
           
           WRITE(file_name,'(A,I8.8,A)') &
                TRIM(this%output_particles_file),step,'.out'
           
        END IF
        
        IF (this%output_particles_fmt(1:1) .EQ. 'f' .OR. &
             this%output_particles_fmt(1:1) .EQ. 'F') THEN
           
           ifmt = ppm_param_io_ascii
           
        ELSE
           
           ifmt = ppm_param_io_binary
           
        END IF
        
        output_unit = this%output_particles_unit
        
        !----------------------------------------------------
        ! Allocate memory for output.
        !----------------------------------------------------
        
        num_x  = SIZE(x,1)
        num_v  = SIZE(v,1)
        
        num_id = 2
        
        !----------------------------------------------------
        ! Allocate memory for output data.
        ! x,y(,z), vx,vy(,vz), rho/d, m, pid,sid;
        ! force, stress tensor, potential energy.
        !----------------------------------------------------
        
        !******** Changed by Adolfo ********
!        data_dim = num_x + num_v + 1 + 1 + num_id
        data_dim = num_x + num_v + 1 + 1 + num_id + 1
        !***********************************
        
#ifdef __IO_PARTICLES_FORCE_TOTAL
        num_f = SIZE(f,1)
        data_dim = data_dim + num_f
#endif
#ifdef __IO_PARTICLES_FORCE_SEPARATE
        num_fp = SIZE(fp,1)
        data_dim = data_dim + num_fp
        num_fv = SIZE(fv,1)
        data_dim = data_dim + num_fv
        IF ( Brownian ) THEN
           num_fr = SIZE(fr,1)
           data_dim = data_dim + num_fr
        END IF
#endif
        
        IF ( stress_tensor ) THEN
           
           num_s    = SIZE(s,1)
!#ifdef __IO_PARTICLES_STRESS_TOTAL           
           data_dim = data_dim + num_s
!#endif
#ifdef __IO_PARTICLES_STRESS_SEPARATE
           data_dim = data_dim + num_s
           data_dim = data_dim + num_s
           IF ( Brownian ) THEN
              data_dim = data_dim + num_s
           END IF
#endif
        END IF ! stress_tensor
        
        IF (p_energy )  THEN
           data_dim = data_dim + 1
        END IF
        
        ALLOCATE(output(data_dim,num_part),STAT=stat_info_sub)
        
        IF (stat_info_sub /=0) THEN          
           PRINT *, 'io_write_particles : ',&
                'Allocating buffer for output failed !'
           stat_info = -1
           GOTO 9999          
        END IF
        
        !----------------------------------------------------
        ! Copy the data into output data structure.
        !----------------------------------------------------
        output(1:num_x,1:num_part) = x(1:num_x,1:num_part)
        
        current_dim = num_x
        
        output(current_dim+1:current_dim+num_v,1:num_part) = &
             v(1:num_v,1:num_part)

        current_dim = current_dim + num_v
        
        output(current_dim+1,1:num_part) = rho(1:num_part)
        output(current_dim+2,1:num_part) = m(1:num_part)
        
        current_dim = current_dim + 2
           
        output(current_dim+1:current_dim+num_id,1:num_part) = &
             id(1:num_id,1:num_part)
        
        current_dim = current_dim + num_id

        !*********** Added by Adolfo ***********
        output(current_dim+1,1:num_part) = spec(1:num_part)
        
        current_dim = current_dim + 1
        !***************************************
        
#ifdef __IO_PARTICLES_FORCE_TOTAL
        output(current_dim+1:current_dim+num_f,1:num_part) = &
             f(1:num_f,1:num_part)
        current_dim = current_dim + num_f
#endif
#ifdef __IO_PARTICLES_FORCE_SEPARATE
        output(current_dim+1:current_dim+num_fp,1:num_part) = &
             fp(1:num_fp,1:num_part)
        current_dim = current_dim + num_fp
        output(current_dim+1:current_dim+num_fv,1:num_part) = &
             fv(1:num_fv,1:num_part)
        current_dim = current_dim + num_fv
        IF ( Brownian ) THEN
           output(current_dim+1:current_dim+num_fr,1:num_part) = &
                fr(1:num_fr,1:num_part)
           current_dim = current_dim + num_fr
        END IF
#endif

        IF ( stress_tensor ) THEN
           
!#ifdef __IO_PARTICLES_STRESS_TOTAL
! currently for 2D         
           DO i=1, 4
              output(current_dim+i,1:num_part) = &
                   s(i,1:num_part)      
           END DO
           current_dim = current_dim + 4
!#endif
#ifdef __IO_PARTICLES_STRESS_SEPARATE
           output(current_dim+1,1:num_part) = &
                sp(2,1:num_part)
           output(current_dim+2,1:num_part) = &
             sp(3,1:num_part)      
        current_dim = current_dim + 2
        output(current_dim+1,1:num_part) = &
             sv(2,1:num_part)
        output(current_dim+2,1:num_part) = &
             sv(3,1:num_part)      
        current_dim = current_dim + 2
        IF ( Brownian ) THEN
           output(current_dim+1,1:num_part) = &
                sr(2,1:num_part)
           output(current_dim+2,1:num_part) = &
                sr(3,1:num_part)      
           current_dim = current_dim + 2
        END IF
#endif
     END IF ! stress_tensor
        
        IF( p_energy ) THEN
           
           output(current_dim+1,1:num_part) = u(1:num_part)
           current_dim = current_dim + 1
           
        END IF
        
        !----------------------------------------------------
        ! Open ppm I/O unit for centralized I/O.
        !----------------------------------------------------
        
        CALL ppm_io_open(output_unit,file_name,&
             ppm_param_io_write, ppm_param_io_replace, &
             ifmt,ppm_param_io_centralized,stat_info_sub)
        
        IF (stat_info_sub /= 0) THEN          
           PRINT *, 'io_write_particles : ', &
                'Failed to open unit !'          
           stat_info = -1
           GOTO 9999          
        END IF
        
        !************** Changed by Adolfo ******************
!        WRITE(cbuf,'(A1,I2,A6)') '(', data_dim ,'E16.8)'
!        WRITE(cbuf,'(A1,I2,A6)') '(', data_dim ,'E12.4)'
        cbuf = '(8E12.4, E16.8, 2E12.4)'
        !***************************************************
        clen = LEN_TRIM(cbuf)         

        !******* Changed by Adolfo to avoid some runtime problem 
        !        in enhpc **********
!!$        DO I = 1, num_part
!!$           CALL ppm_io(output_unit,output(:,I),&
!!$                ppm_param_io_write,&
!!$                ppm_param_io_concat,IOFMT=cbuf(1:clen), &
!!$                STAT=stat_info_sub)
!!$        ENDDO

        CALL ppm_io(output_unit,output,&
             ppm_param_io_write,&
             ppm_param_io_concat,IOFMT=cbuf(1:clen), &
             STAT=stat_info_sub)
        !*************************************
        
        IF ( stat_info_sub /= 0 ) THEN          
           PRINT *, 'io_write_particles : ',&
                'Writing particles failed'	   
           stat_info = -1
           GOTO 9999
        END IF
        
        
        !----------------------------------------------------
        ! Close file.
        !----------------------------------------------------
        
        CALL ppm_io_close(output_unit,stat_info_sub)
        
        !----------------------------------------------------
        ! Print out for user to confirm.
        !----------------------------------------------------
        
        IF (rank == 0) THEN
           
           WRITE(cbuf,'(2A)') 'Particles written to ',&
                TRIM(file_name)
           PRINT *, TRIM(cbuf)
           
        END IF
        
        !----------------------------------------------------
        ! Return.
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

        !************** Added by Adolfo ********
        IF (ASSOCIATED(spec)) THEN
           DEALLOCATE(spec)
        END IF
        !***************************************
        
        IF (ASSOCIATED(id)) THEN
           DEALLOCATE(id)
        END IF
        
#ifdef __IO_PARTICLES_FORCE_TOTAL
        IF (ASSOCIATED(f)) THEN
           DEALLOCATE(f)
        END IF
#endif

#ifdef __IO_PARTICLES_FORCE_SEPARATE
        IF (ASSOCIATED(fp)) THEN
           DEALLOCATE(fp)
        END IF
        IF (ASSOCIATED(fv)) THEN
           DEALLOCATE(fv)
        END IF
        IF (ASSOCIATED(fr)) THEN
           DEALLOCATE(fr)
        END IF
#endif
        
#ifdef __IO_PARTICLES_STRESS_TOTAL
        IF (ASSOCIATED(s)) THEN
           DEALLOCATE(s)
        END IF
#endif

#ifdef __IO_PARTICLES_STRESS_SEPARATE
        IF (ASSOCIATED(sp)) THEN
           DEALLOCATE(sp)
        END IF
        
        IF (ASSOCIATED(sv)) THEN
           DEALLOCATE(sv)
        END IF
        
        IF (ASSOCIATED(sr)) THEN
           DEALLOCATE(sr)
        END IF
#endif
        
        IF (ASSOCIATED(u) ) THEN
           DEALLOCATE(u)
        END IF
        
        IF (ASSOCIATED(output)) THEN
           DEALLOCATE(output)
        END IF
        
        
#ifdef __DEBUG
        IF( debug_flag == 2 ) THEN
           CALL debug_substop(global_debug,rank,&
                "io_write_particles",&
                time_routine_start,stat_info_sub)
        END IF
#endif
        RETURN	
        
        
      END SUBROUTINE io_write_particles   
     
