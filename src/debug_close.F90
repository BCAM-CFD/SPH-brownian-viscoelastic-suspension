      SUBROUTINE debug_close_time(this,rank,stat_info)
        !----------------------------------------------------
        ! Subroutine  : debug_close_time
        !----------------------------------------------------
        !
        ! Purpose     : Close time file for writting.
        !
        ! Input       : rank  : MPI rank
        !
        ! Output      : stat_info : return status
        !
        ! Routines    : ppm_time
        !
        ! Remarks     :
        !
        ! References  :
        !
        ! Revisions   : V0.1 16.11.2010, original version.
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
        
        TYPE(Debug), INTENT(IN)         :: this
        INTEGER, INTENT(IN)             :: rank
        INTEGER,  INTENT(OUT)           :: stat_info
        
        !----------------------------------------------------
        ! Local variables.
        !----------------------------------------------------
        
        INTEGER                         :: stat_info_sub
        LOGICAL                         :: lopened
        
        !----------------------------------------------------
        ! Initialization.
        !----------------------------------------------------
        
        stat_info     = 0
        stat_info_sub = 0

        IF ( rank /=0 ) THEN
           PRINT *, "debug_close_time : ", &
                "rank must be 0 !"
           stat_info = -1
           GOTO 9999
        END IF
        
        INQUIRE(UNIT=this%time_file_unit,OPENED=lopened)
        
        IF (lopened) THEN
           CLOSE(this%time_file_unit)
        END IF
        
9999    CONTINUE
        
        RETURN
      END SUBROUTINE debug_close_time
