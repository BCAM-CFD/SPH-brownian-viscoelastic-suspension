      SUBROUTINE tool_uppercase(this,string,ilen,stat_info)
        !----------------------------------------------------
        !  Subroutine   :     tool_uppercase
        !----------------------------------------------------
        !
        !  Purpose      : Converts a character string to 
        !                 upper case letters.
        !
        !  Routines     :
        !
        !  Remarks      :
        !
        !  References   :
        !
        !  Revisions    : V0.1 03.03 2009 original version.
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
        
        TYPE(Tool), INTENT(IN)          :: this
        CHARACTER(LEN=*), INTENT(INOUT) :: string
        INTEGER, INTENT(IN)             :: ilen
        INTEGER, INTENT(OUT)            :: stat_info
        
        !----------------------------------------------------
        ! Local variables 
        !----------------------------------------------------
        
        INTEGER               :: i,j
        INTEGER               :: i1,i2,i3,iadd
        
        
        !----------------------------------------------------
        ! Initialise
        ! this is supposed to be used, otherwise,
        ! compiler complains that it is not used.
        !----------------------------------------------------
        
        stat_info = this%flag
        stat_info = 0
        
        !----------------------------------------------------
        ! Convert to upper case
        !----------------------------------------------------
        
        i1   = IACHAR('a') - 1
        i2   = IACHAR('z') + 1
        i3   = IACHAR('A')
        iadd = i3 - i1 - 1
        
        DO i=1,ilen
           j = IACHAR(string(i:i))
           IF (j.GT.i1.AND.j.LT.i2) THEN
              string(i:i) = CHAR(j+iadd)
           END IF
        END DO
        
        !----------------------------------------------------
        ! Return 
        !----------------------------------------------------
        
9999    CONTINUE
        
        RETURN
        
      END SUBROUTINE tool_uppercase
      
