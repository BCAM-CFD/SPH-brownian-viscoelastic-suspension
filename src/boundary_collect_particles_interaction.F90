!******* Changed by Adolfo *******
!!$SUBROUTINE boundary_collect_particles_interaction(this,&
!!$     comm,MPI_PREC,drag,drag_p,drag_v,drag_r,stat_info)
SUBROUTINE boundary_collect_particles_interaction(this,&
     comm,MPI_PREC,drag, &
     total_force_top, total_force_bottom, &
     drag_p,drag_v,drag_r, stat_info)
!**********************************
        !----------------------------------------------------
        ! Subroutine  : boundary_collect_particles_interaction
        !----------------------------------------------------
        !
        ! Purpose     : Collect total drag/force exerted
        !               on boundaries from all processes.
        !                
        !
        ! Routines    :
        !
        ! References  :
        !
        ! Remarks     :
        !
        ! Revisions   : V0.1, 23.10.2009, original version.
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
        ! Arguments :
        !----------------------------------------------------
        
        TYPE(Boundary), INTENT(OUT)             :: this
        INTEGER, INTENT(IN)                     :: comm
        INTEGER, INTENT(IN)                     :: MPI_PREC
        REAL(MK),DIMENSION(:,:),INTENT(IN)      :: drag
        !********* Added by Adolfo ********
        REAL(MK),DIMENSION(:)  :: total_force_top
        REAL(MK),DIMENSION(:)  :: total_force_bottom        
        !**********************************
        REAL(MK),DIMENSION(:,:),INTENT(IN),OPTIONAL:: drag_p
        REAL(MK),DIMENSION(:,:),INTENT(IN),OPTIONAL:: drag_v
        REAL(MK),DIMENSION(:,:),INTENT(IN),OPTIONAL:: drag_r
        INTEGER, INTENT(OUT)                    :: stat_info
        
        !----------------------------------------------------
        ! Local variables start here :
        !----------------------------------------------------
        
        INTEGER                                 :: stat_info_sub
        INTEGER                                 :: dim
        INTEGER                                 :: num
        REAL(MK), DIMENSION(:,:), POINTER       :: t_drag
        REAL(MK), DIMENSION(:,:), POINTER       :: t_drag_p
        REAL(MK), DIMENSION(:,:), POINTER       :: t_drag_v
        REAL(MK), DIMENSION(:,:), POINTER       :: t_drag_r
        
        !----------------------------------------------------
        ! Initialization of variables.
        !----------------------------------------------------
        
        stat_info        = 0
        stat_info_sub    = 0
        
        dim  = this%num_dim
        num  = 2 * dim

        IF ( SIZE(drag,1) /= dim ) THEN
           PRINT *, "boundary_collect_particles_interaction : ", &
                "drag dimension does not match ! "
           stat_info = -1
           GOTO 9999
        END IF
        
        IF ( SIZE(drag,2) /= num ) THEN
           PRINT *, "boundary_collect_particles_interaction : ", &
                "drag number does not match ! "
           stat_info = -1
           GOTO 9999
        END IF
        
        this%drag(:,:) = 0.0_MK        
        NULLIFY(t_drag)
        
        NULLIFY(t_drag_p)
        NULLIFY(t_drag_v)
        NULLIFY(t_drag_r)
        
#ifdef __WALL_FORCE_SEPARATE
        this%drag_p(:,:) = 0.0_MK
        this%drag_v(:,:) = 0.0_MK
        this%drag_r(:,:) = 0.0_MK
#endif
        
#ifdef __MPI
        
        !----------------------------------------------------
        ! In context of MPI, sum up from all processes,
        ! then broadcast to all processes.
        !----------------------------------------------------
        
        ALLOCATE(t_drag(dim,num))
        t_drag(:,:) = 0.0_MK
        
        CALL MPI_ALLREDUCE (drag(:,:), t_drag(:,:),&
             SIZE(drag),MPI_PREC,MPI_SUM,comm,stat_info_sub)
     
        IF ( stat_info_sub /= 0 ) THEN
           PRINT *, "boundary_collect_particles_interaction : ", &
                "MPI_ALLREDUCE() for total drag has problem !"
           stat_info = -1
           GOTO 9999
        END IF
        
        this%drag(1:dim,1:num) = t_drag(1:dim,1:num)
        !***** Added by Adolfo ****
        IF (dim == 2) THEN
           this%drag(1:dim, 3) = this%drag(1:dim, 3) + total_force_bottom(1:dim)
           this%drag(1:dim, 4) = this%drag(1:dim, 4) + total_force_top(1:dim)
        ELSE !-- dim = 3 --
           this%drag(1:dim, 5) = this%drag(1:dim, 5) + total_force_bottom(1:dim)
           this%drag(1:dim, 6) = this%drag(1:dim, 6) + total_force_top(1:dim)
        ENDIF
        !*************************        
        
#ifdef __WALL_FORCE_SEPARATE

        ALLOCATE(t_drag_p(dim,num))
        t_drag_p(:,:) = 0.0_MK        
        CALL MPI_ALLREDUCE (drag_p(:,:), t_drag_p(:,:),&
             SIZE(drag_p),MPI_PREC,MPI_SUM,comm,stat_info_sub)        
        this%drag_p(1:dim,1:num) = t_drag_p(1:dim,1:num)

        ALLOCATE(t_drag_v(dim,num))
        t_drag_v(:,:) = 0.0_MK        
        CALL MPI_ALLREDUCE (drag_v(:,:), t_drag_v(:,:),&
             SIZE(drag_v),MPI_PREC,MPI_SUM,comm,stat_info_sub)        
        this%drag_v(1:dim,1:num) = t_drag_v(1:dim,1:num)
        
        ALLOCATE(t_drag_r(dim,num))
        t_drag_r(:,:) = 0.0_MK        
        CALL MPI_ALLREDUCE (drag_r(:,:), t_drag_r(:,:),&
             SIZE(drag_r),MPI_PREC,MPI_SUM,comm,stat_info_sub)        
        this%drag_r(1:dim,1:num) = t_drag_r(1:dim,1:num)
        
#endif

#else
        
        this%drag(1:dim,1:num) = drag(1:dim,1:num)
        
#ifdef __WALL_FORCE_SEPARATE
        
        this%drag_p(1:dim,1:num) = drag_p(1:dim,1:num)
        this%drag_v(1:dim,1:num) = drag_v(1:dim,1:num)
        this%drag_r(1:dim,1:num) = drag_r(1:dim,1:num)
        
#endif

#endif
        
        
9999    CONTINUE
        
        IF(ASSOCIATED(t_drag)) THEN
           DEALLOCATE(t_drag)
        END IF
        
#ifdef __WALL_FORCE_SEPARATE
        IF(ASSOCIATED(t_drag_p)) THEN
           DEALLOCATE(t_drag_p)
        END IF
        IF(ASSOCIATED(t_drag_v)) THEN
           DEALLOCATE(t_drag_v)
        END IF
        IF(ASSOCIATED(t_drag_r)) THEN
           DEALLOCATE(t_drag_r)
        END IF
#endif
        
        RETURN          
        
      END SUBROUTINE boundary_collect_particles_interaction
      
      
