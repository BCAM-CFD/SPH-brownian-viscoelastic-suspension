      SUBROUTINE control_finalize(this,stat_info)
        !----------------------------------------------------
        !  Subroutine   :  control_finalize
        !----------------------------------------------------
        !
        !  Purpose      : Destrutor of Class Control.
        !
        !  Reference    :
        !
        !  Remark       :
        !
        !  Revisions    : V0.1 01.03.2009, original version.
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
        
        TYPE(Control),INTENT(IN)              :: this
        INTEGER,INTENT(OUT)                   :: stat_info
        
        !------------------------
        ! Finalize
        !-----------------------
        
        stat_info = this%debug_flag
        stat_info = 0         

        PRINT *, "control_finalize : ", "Finished!"

        RETURN
        
      END SUBROUTINE control_finalize
     

