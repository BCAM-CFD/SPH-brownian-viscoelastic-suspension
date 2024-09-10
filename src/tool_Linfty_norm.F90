      REAL(MK) FUNCTION tool_Linfty_norm(this,x,stat_info)
        !----------------------------------------------------
        ! Subroutine  : tool_Linfty_norm
        !----------------------------------------------------
        !
        ! Purpose     : Calculate Linfty norm of a vector.
        !
        ! Routines    :
        !
        ! Remarks     : 
        !
        ! References  :
        !
        ! Revisions   : V0.1 25.05 2012, original version.
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
        
        TYPE(Tool), INTENT(IN)                  :: this
        REAL(MK), DIMENSION(:),INTENT(IN)       :: x
        INTEGER, INTENT(OUT)                    :: stat_info
        
        INTEGER                                 :: num, i
        REAL(MK)                                :: norm
        
        !----------------------------------------------------
        ! Initialization
        !
        ! This is supposed to be used, otherwise,
        ! compiler complains that it is not used.
        !----------------------------------------------------
        
        stat_info = this%flag
        stat_info = 0
        
        
        num = SIZE(x,1)
        
        norm = ABS(x(1))
        
        DO i = 2, num
           
           IF ( ABS(x(i)) > norm ) THEN
              norm  =  ABS(x(i))
           END IF
           
        END DO
        
        tool_Linfty_norm = norm
        
        !----------------------------------------------------
        ! Return.
        !----------------------------------------------------
        
9999    CONTINUE
        
        RETURN
        
      END FUNCTION  tool_Linfty_norm
      
      
