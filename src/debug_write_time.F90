      SUBROUTINE debug_write_time(this,rank,time,stat_info)
        !----------------------------------------------------
        ! Subroutine  : debug_write_time
        !----------------------------------------------------
        !
        ! Purpose     : Wirte time file.
        !
        ! Input       : rank  : MPI rank
        !               time  : time data
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
        REAL(MK),DIMENSION(:),INTENT(IN):: time
        INTEGER,  INTENT(OUT)           :: stat_info
        
        !----------------------------------------------------
        ! Local variables.
        !----------------------------------------------------
        
        INTEGER                         :: stat_info_sub
        LOGICAL                         :: lopened
        INTEGER                         :: num
        CHARACTER(len=MAX_CHAR)	        :: form
        INTEGER                         :: iform
        CHARACTER(len=2*MAX_CHAR)	:: cbuf
      
        !----------------------------------------------------
        ! Initialization.
        !----------------------------------------------------
        
        stat_info     = 0
        stat_info_sub = 0

        IF ( rank /=0 ) THEN
           PRINT *, "debug_write_time : ", &
                "rank must be 0 !"
           stat_info = -1
           GOTO 9999
        END IF
        
        INQUIRE(UNIT=this%time_file_unit,OPENED=lopened)
        
        IF (lopened) THEN
        
           num = SIZE(time,1)
           
           WRITE(form,'(A1,I2,A6)') '(', num, 'E16.8)'
           iform = LEN_TRIM(form)
           
           WRITE(cbuf,form(1:iform)), time(1:num)

           WRITE(UNIT=this%time_file_unit,FMT='(A)', &
                IOSTAT=stat_info_sub) TRIM(cbuf)
      
        ELSE
           
           PRINT *, "debug_write_time : ", &
                TRIM(this%time_file), " not opened yet !"
           stat_info = -1
           GOTO 9999
           
        END IF
        
9999    CONTINUE
        
        RETURN
        
      END SUBROUTINE debug_write_time
