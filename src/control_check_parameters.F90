      LOGICAL FUNCTION control_check_parameters(this,stat_info)
        !----------------------------------------------------
        !  Subroutine   :  control_check_parameters
        !----------------------------------------------------
        !
        !  Purpose      :  Check if control parameters are
        !                  resonable.
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
        TYPE(Control), INTENT(INOUT)    :: this
        INTEGER, INTENT(OUT)            :: stat_info

        
        stat_info = 0
        
        control_check_parameters = .TRUE.

        IF ( (this%Brownian .EQV. .TRUE.) .AND. &
             ( this%symmetry .NEQV. .TRUE.) ) THEN
           
           control_check_parameters = .FALSE.
           PRINT *, "control_check_parameters : ", &
                "For Brownian solvent, do use symmetry interaction and &
                symmetry inter-processor communication !"
           GOTO 9999
           
        END IF

9999    CONTINUE  
        
        RETURN        
        
      END FUNCTION control_check_parameters
      
