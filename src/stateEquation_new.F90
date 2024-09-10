      SUBROUTINE stateEquation_init(this,stateEquation_type,&
           d_c,d_rho,d_rho_ref,d_gamma,stat_info)
        !----------------------------------------------------
        ! Subroutine  : stateEquation_init
        !----------------------------------------------------
        !
        ! Purpose     : Default construtor of StateEqaution 
        !               Class.
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
        
        TYPE(StateEquation), INTENT(OUT)        :: this
        INTEGER, INTENT(IN)                     :: stateEquation_type
        REAL(MK), INTENT(IN)                    :: d_c
        REAL(MK), INTENT(IN)                    :: d_rho
        REAL(MK), INTENT(IN)                    :: d_rho_ref
        REAL(MK), INTENT(IN)                    :: d_gamma
        INTEGER, INTENT(OUT)                    :: stat_info
        
        INTEGER                                 :: stat_info_sub
        
        
        stat_info     = 0
        stat_info_sub = 0

        this%stateEquation_type = stateEquation_type
        this%c        = d_c
        this%rho_ref  = d_rho_ref
        this%gamma    = d_gamma

        SELECT CASE (stateEquation_type)
           
        CASE(1)
           
           CALL stateEquation_init_Morris(this,stat_info_sub)
           
        CASE(2)
           
           CALL stateEquation_init_Batchelor(this, &
                d_c,d_rho,d_rho_ref,d_gamma,stat_info_sub)
           
        END SELECT
        
        CALL tool_new(this%tool,stat_info_sub)

9999    CONTINUE
        
        RETURN
        
      END SUBROUTINE stateEquation_init
      

      SUBROUTINE stateEquation_init_Morris(this,stat_info)
        
        TYPE(StateEquation), INTENT(IN)        :: this
        INTEGER, INTENT(OUT)                   :: stat_info
        
        stat_info = 0

        IF ( this%c == 0.0_MK ) THEN
           PRINT *, "stateEquation_init_Morris : ", &
                "No sound speed should be zero !"
           stat_info = -1
        END IF
        
        RETURN

      END SUBROUTINE stateEquation_init_Morris

      
      SUBROUTINE stateEquation_init_Batchelor(this,&
           d_c,d_rho,d_rho_ref,d_gamma,stat_info)
        
        TYPE(StateEquation), INTENT(OUT)        :: this
        REAL(MK), INTENT(IN)                    :: d_c
        REAL(MK), INTENT(IN)                    :: d_rho
        REAL(MK), INTENT(IN)                    :: d_rho_ref
        REAL(MK), INTENT(IN)                    :: d_gamma
        INTEGER, INTENT(OUT)                    :: stat_info
        
        stat_info = 0

        IF( d_rho_ref == 0.0_MK .OR. d_gamma == 0.0_MK) THEN
           PRINT *, "stateEquation_init_Batchelor : ", &
                "Neither rho_ref nor gamma can be zero !"
           stat_info = -1
           GOTO 9999
        END IF
        
        this%p0    = &
             d_c**2 * d_rho * (d_rho_ref/d_rho)**d_gamma / d_gamma
        
        
9999    CONTINUE
        
        RETURN
        
      END SUBROUTINE stateEquation_init_Batchelor


      SUBROUTINE stateEquation_display_parameters(this,stat_info)
        
        TYPE(StateEquation),INTENT(IN)        ::this
        INTEGER,INTENT(OUT)                   :: stat_info
        
        INTEGER                               :: stat_info_sub
        
        stat_info = 0
        stat_info_sub = 0
        
        PRINT *, '============================================================'
        PRINT *, '              StateEquation  parameters'
        PRINT *, '====================Start==================================='
    
        
        SELECT CASE(this%stateEquation_type) 
           
        CASE (1)
           
           CALL tool_print_msg(this%tool, "stateEquation_type", &
                "p = (c^2) * (rho^gamma-rho_ref)", &
                stat_info_sub)
           CALL tool_print_msg(this%tool, "c", &
                this%c, stat_info_sub)
           CALL tool_print_msg(this%tool, "rho_ref", &
                this%rho_ref, stat_info_sub)
           CALL tool_print_msg(this%tool, "gamma", &
                this%gamma, stat_info_sub)
           
        CASE (2) 
           
           CALL tool_print_msg(this%tool, "stateEquation_type", &
                "p = p0 * { (rho/rho_ref)^gamma - 1 }" , &
                stat_info_sub)
           CALL tool_print_msg(this%tool, "p0", &
                this%p0, stat_info_sub)
           CALL tool_print_msg(this%tool, "rho_ref", &
                this%rho_ref, stat_info_sub)
           CALL tool_print_msg(this%tool, "gamma", &
                this%gamma, stat_info_sub)
           
        END SELECT
        
        PRINT *, '=====================END===================================='
        PRINT *, '              StateEquation  parameters'
        PRINT *, '============================================================'
        
   
        RETURN          
        
      END SUBROUTINE stateEquation_display_parameters
