      SUBROUTINE tool_print_msg_b(this,name,val,stat_info)
        !----------------------------------------------------
      	! Subroutine  : tool_print_msg_b
      	!----------------------------------------------------
	!
      	! Purpose     : Printing a name and a Boolean value 
      	!	 	for displaying purpose.
      	!
      	! Input       : 
      	!               
      	!
      	!Input/output : 
      	!
      	! Output      : stat_info      (I) error status
      	!
      	! Routines    :
      	!
      	! Remarks     :
      	!
      	! References  :
      	!
      	! Revisions   : V0.1 01.08.2012, original version.
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
        CHARACTER(LEN=*) 	        :: name
        LOGICAL       	                :: val
        INTEGER          	        :: stat_info
       
      	!----------------------------------------------------
      	! Local variables 
      	!----------------------------------------------------
        
        CHARACTER(LEN=MAX_CHAR)	        :: suffix, msg
        INTEGER                         :: name_length
        INTEGER                         :: suffix_length, i
        
      	!----------------------------------------------------
      	! Initialize 
      	!----------------------------------------------------
        
        stat_info = 0
        
      	!----------------------------------------------------
        ! Get length of name and the value
      	!----------------------------------------------------
        
        name_length = LEN_TRIM(name)        
        suffix_length = NAME_DISPLAY_CHAR - name_length
        
        !----------------------------------------------------
        ! Suffix will be at least one space
        !----------------------------------------------------

        suffix = "-"
        DO i = 2, suffix_length
           suffix = TRIM(suffix) // "-"
        END DO
        
        !----------------------------------------------------
        ! Concatenate name and value with a separater ">:"
        !----------------------------------------------------
        
        msg = TRIM(name) // TRIM(suffix) // ">: "
        
        WRITE(msg, "(A,L15)") TRIM(msg), val
        
        PRINT *, TRIM(msg)
        
        
        !----------------------------------------------------
        ! Return 
      	!----------------------------------------------------
        
9999    CONTINUE
        
        RETURN
        
      END SUBROUTINE tool_print_msg_b
      
      
      SUBROUTINE tool_print_msg_f(this,name,val,stat_info)
        !----------------------------------------------------
      	! Subroutine  : tool_print_msg_f
      	!----------------------------------------------------
	!
      	! Purpose     : Printing a name and a float value
      	!	 	for displaying purpose
      	!
      	! Input       : 
      	!               
      	!
      	!Input/output : 
      	!
      	! Output      : stat_info      (I) error status
      	!
      	! Routines    :
      	!
      	! Remarks     :
      	!
      	! References  :
      	!
      	! Revisions   : V0.1 01.08.2012, original version.
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
        CHARACTER(LEN=*) 	        :: name
        REAL(MK)       	                :: val
        INTEGER          	        :: stat_info
       
      	!----------------------------------------------------
      	! Local variables 
      	!----------------------------------------------------
        
        CHARACTER(LEN=MAX_CHAR)	        :: suffix, msg
        INTEGER                         :: name_length
        INTEGER                         :: suffix_length, i
        
      	!----------------------------------------------------
      	! Initialize 
      	!----------------------------------------------------
        
        stat_info = 0
        
      	!----------------------------------------------------
        ! Get length of name and the value
      	!----------------------------------------------------
        
        name_length = LEN_TRIM(name)        
        suffix_length = NAME_DISPLAY_CHAR - name_length
        
        !----------------------------------------------------
        ! Suffix will be at least one space
        !----------------------------------------------------

        suffix = "-"
        DO i = 2, suffix_length
           suffix = TRIM(suffix) // "-"
        END DO
        
        !----------------------------------------------------
        ! Concatenate name and value with a separater ">:"
        !----------------------------------------------------
        
        msg = TRIM(name) // TRIM(suffix) // ">: "
        
        WRITE(msg, "(A,F15.6)") TRIM(msg), val
        
        PRINT *, TRIM(msg)

        
        !----------------------------------------------------
        ! Return 
      	!----------------------------------------------------
        
9999    CONTINUE
        
        RETURN
        
      END SUBROUTINE tool_print_msg_f


      SUBROUTINE tool_print_msg_f2(this,name,val1,val2,stat_info)
        !----------------------------------------------------
      	! Subroutine  : tool_print_msg_f2
      	!----------------------------------------------------
	!
      	! Purpose     : Printing a name and two float values
      	!	 	for displaying purpose
      	!
      	! Input       : 
      	!               
      	!
      	!Input/output : 
      	!
      	! Output      : stat_info      (I) error status
      	!
      	! Routines    :
      	!
      	! Remarks     :
      	!
      	! References  :
      	!
      	! Revisions   : V0.1 19.06.2013, original version.
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
        CHARACTER(LEN=*) 	        :: name
        REAL(MK)       	                :: val1
        REAL(MK)       	                :: val2
        INTEGER          	        :: stat_info
       
      	!----------------------------------------------------
      	! Local variables 
      	!----------------------------------------------------
        
        CHARACTER(LEN=MAX_CHAR)	        :: suffix, msg
        INTEGER                         :: name_length
        INTEGER                         :: suffix_length, i
        
      	!----------------------------------------------------
      	! Initialize 
      	!----------------------------------------------------
        
        stat_info = 0
        
      	!----------------------------------------------------
        ! Get length of name and the value
      	!----------------------------------------------------
        
        name_length = LEN_TRIM(name)        
        suffix_length = NAME_DISPLAY_CHAR - name_length
        
        !----------------------------------------------------
        ! Suffix will be at least one space
        !----------------------------------------------------

        suffix = "-"
        DO i = 2, suffix_length
           suffix = TRIM(suffix) // "-"
        END DO
        
        !----------------------------------------------------
        ! Concatenate name and value with a separater ">:"
        !----------------------------------------------------
        
        msg = TRIM(name) // TRIM(suffix) // ">: "
        
        WRITE(msg, "(A,2F15.6)") TRIM(msg), val1, val2
        
        PRINT *, TRIM(msg)

        
        !----------------------------------------------------
        ! Return 
      	!----------------------------------------------------
        
9999    CONTINUE
        
        RETURN
        
      END SUBROUTINE tool_print_msg_f2
      
      
      SUBROUTINE tool_print_msg_ff(this,name,val,stat_info)
        !----------------------------------------------------
      	! Subroutine  : tool_print_msg_ff
      	!----------------------------------------------------
	!
      	! Purpose     : Printing a name and 
        !               an array of float values for
        !               displaying purpose
      	!
      	! Input       : 
      	!               
      	!
      	!Input/output : 
      	!
      	! Output      : stat_info      (I) error status
      	!
      	! Routines    :
      	!
      	! Remarks     :
      	!
      	! References  :
      	!
      	! Revisions   : V0.1 19.06.2013, original version.
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
        CHARACTER(LEN=*) 	        :: name
        REAL(MK), DIMENSION(:)          :: val
        INTEGER          	        :: stat_info
       
      	!----------------------------------------------------
      	! Local variables 
      	!----------------------------------------------------
        
        CHARACTER(LEN=MAX_CHAR)	        :: suffix, msg
        CHARACTER(LEN=MAX_CHAR)	        :: form
        INTEGER                         :: name_length
        INTEGER                         :: suffix_length, i
        INTEGER                         :: num
        
      	!----------------------------------------------------
      	! Initialize 
      	!----------------------------------------------------
        
        stat_info = 0
        
      	!----------------------------------------------------
        ! Get length of name and the value
      	!----------------------------------------------------
        
        name_length = LEN_TRIM(name)        
        suffix_length = NAME_DISPLAY_CHAR - name_length
        
        !----------------------------------------------------
        ! Suffix will be at least one space
        !----------------------------------------------------

        suffix = "-"
        DO i = 2, suffix_length
           suffix = TRIM(suffix) // "-"
        END DO
        
        !----------------------------------------------------
        ! Concatenate name and value with a separater ">:"
        !----------------------------------------------------
        
        msg = TRIM(name) // TRIM(suffix) // ">: "
        
        num = SIZE(val,1)
        
        IF ( num < 10 ) THEN           
           WRITE(form, "(A3,I1,A6)") "(A,",  num , "F15.6)"
        ELSE
           WRITE(form, "(A3,I2,A6)") "(A,",  num , "F15.6)"
        END IF
                
        WRITE(msg, TRIM(form)) TRIM(msg), val(1:num)
        
        PRINT *, TRIM(msg)
        
        !----------------------------------------------------
        ! Return 
      	!----------------------------------------------------
        
9999    CONTINUE
        
        RETURN
        
      END SUBROUTINE tool_print_msg_ff

      
      SUBROUTINE tool_print_msg_fs(this,name,val1,val2,stat_info)
        !----------------------------------------------------
      	! Subroutine  : tool_print_msg_f
      	!----------------------------------------------------
	!
      	! Purpose     : Printing a name, a float value,
        !               and a string message
      	!	 	for displaying purpose
      	!
      	! Input       : 
      	!               
      	!
      	!Input/output : 
      	!
      	! Output      : stat_info      (I) error status
      	!
      	! Routines    :
      	!
      	! Remarks     :
      	!
      	! References  :
      	!
      	! Revisions   : V0.1 19.06.2013, original version.
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
        CHARACTER(LEN=*) 	        :: name
        REAL(MK)       	                :: val1
        CHARACTER(LEN=*) 	        :: val2
        INTEGER          	        :: stat_info
       
      	!----------------------------------------------------
      	! Local variables 
      	!----------------------------------------------------
        
        CHARACTER(LEN=MAX_CHAR)	        :: suffix, msg
        INTEGER                         :: name_length
        INTEGER                         :: suffix_length, i
        
      	!----------------------------------------------------
      	! Initialize 
      	!----------------------------------------------------
        
        stat_info = 0
        
      	!----------------------------------------------------
        ! Get length of name and the value
      	!----------------------------------------------------
        
        name_length = LEN_TRIM(name)        
        suffix_length = NAME_DISPLAY_CHAR - name_length
        
        !----------------------------------------------------
        ! Suffix will be at least one space
        !----------------------------------------------------
        
        suffix = "-"
        DO i = 2, suffix_length
           suffix = TRIM(suffix) // "-"
        END DO
        
        !----------------------------------------------------
        ! Concatenate name and value with a separater ">:"
        !----------------------------------------------------
        
        msg = TRIM(name) // TRIM(suffix) // ">: "
        
        WRITE(msg, "(A,F15.6,A)") TRIM(msg), val1, val2
        
        PRINT *, TRIM(msg)

        
        !----------------------------------------------------
        ! Return 
      	!----------------------------------------------------
        
9999    CONTINUE
        
        RETURN
        
      END SUBROUTINE tool_print_msg_fs


      SUBROUTINE tool_print_msg_i(this,name,val,stat_info)
        !----------------------------------------------------
      	! Subroutine  : tool_print_msg_i
      	!----------------------------------------------------
	!
      	! Purpose     : Printing a name and an integer value
      	!	 	for displaying purpose
      	!
      	! Input       : 
      	!               
      	!
      	!Input/output : 
      	!
      	! Output      : stat_info      (I) error status
      	!
      	! Routines    :
      	!
      	! Remarks     :
      	!
      	! References  :
      	!
      	! Revisions   : V0.1 01.08.2012, original version.
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
        CHARACTER(LEN=*) 	        :: name
        INTEGER           	        :: val
        INTEGER          	        :: stat_info
       
      	!----------------------------------------------------
      	! Local variables 
      	!----------------------------------------------------

        CHARACTER(LEN=MAX_CHAR)	        :: suffix, msg
        INTEGER                         :: name_length
        INTEGER                         :: suffix_length, i
        
      	!----------------------------------------------------
      	! Initialize 
      	!----------------------------------------------------
        
        stat_info = 0
        
      	!----------------------------------------------------
        ! Get length of name and the value
      	!----------------------------------------------------
        
        name_length = LEN_TRIM(name)
        suffix_length = NAME_DISPLAY_CHAR - name_length
        
        !----------------------------------------------------
        ! Suffix will be at least one space
        !----------------------------------------------------

        suffix = "-"
        DO i = 2, suffix_length
           suffix = TRIM(suffix) // "-"
        END DO
        
        !----------------------------------------------------
        ! Concatenate name and value with a separater ">:"
        !----------------------------------------------------
        
        msg = TRIM(name) // TRIM(suffix) // ">: "
        
        WRITE(msg, "(A,I15)") TRIM(msg), val

        PRINT *, TRIM(msg)

        
        !----------------------------------------------------
        ! Return 
      	!----------------------------------------------------
        
9999    CONTINUE
        
        RETURN
        
      END SUBROUTINE tool_print_msg_i

      
      SUBROUTINE tool_print_msg_i2(this,name,val1,val2,stat_info)
        !----------------------------------------------------
      	! Subroutine  : tool_print_msg_i2
      	!----------------------------------------------------
	!
      	! Purpose     : Printing a name and two integer values
      	!	 	for displaying purpose
      	!
      	! Input       : 
      	!               
      	!
      	!Input/output : 
      	!
      	! Output      : stat_info      (I) error status
      	!
      	! Routines    :
      	!
      	! Remarks     :
      	!
      	! References  :
      	!
      	! Revisions   : V0.1 19.06.2013, original version.
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
        CHARACTER(LEN=*) 	        :: name
        INTEGER           	        :: val1
        INTEGER           	        :: val2
        INTEGER          	        :: stat_info
       
      	!----------------------------------------------------
      	! Local variables 
      	!----------------------------------------------------

        CHARACTER(LEN=MAX_CHAR)	        :: suffix, msg
        INTEGER                         :: name_length
        INTEGER                         :: suffix_length, i
        
      	!----------------------------------------------------
      	! Initialize 
      	!----------------------------------------------------
        
        stat_info = 0
        
      	!----------------------------------------------------
        ! Get length of name and the value
      	!----------------------------------------------------
        
        name_length = LEN_TRIM(name)
        suffix_length = NAME_DISPLAY_CHAR - name_length
        
        !----------------------------------------------------
        ! Suffix will be at least one space
        !----------------------------------------------------

        suffix = "-"
        DO i = 2, suffix_length
           suffix = TRIM(suffix) // "-"
        END DO
        
        !----------------------------------------------------
        ! Concatenate name and value with a separater ">:"
        !----------------------------------------------------
        
        msg = TRIM(name) // TRIM(suffix) // ">: "
        
        WRITE(msg, "(A,2I15)") TRIM(msg), val1, val2

        PRINT *, TRIM(msg)

        
        !----------------------------------------------------
        ! Return 
      	!----------------------------------------------------
        
9999    CONTINUE
        
        RETURN
        
      END SUBROUTINE tool_print_msg_i2


      SUBROUTINE tool_print_msg_ii(this,name,val,stat_info)
        !----------------------------------------------------
      	! Subroutine  : tool_print_msg_ii
      	!----------------------------------------------------
	!
      	! Purpose     : Printing a name and an array of
        !               integer values
      	!	 	for displaying purpose
      	!
      	! Input       : 
      	!               
      	!
      	!Input/output : 
      	!
      	! Output      : stat_info      (I) error status
      	!
      	! Routines    :
      	!
      	! Remarks     :
      	!
      	! References  :
      	!
      	! Revisions   : V0.1 01.08.2012, original version.
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
        CHARACTER(LEN=*) 	        :: name
        INTEGER, DIMENSION(:)  	        :: val
        INTEGER          	        :: stat_info
       
      	!----------------------------------------------------
      	! Local variables 
      	!----------------------------------------------------

        CHARACTER(LEN=MAX_CHAR)	        :: suffix, msg
        CHARACTER(LEN=MAX_CHAR)	        :: form
        INTEGER                         :: name_length
        INTEGER                         :: suffix_length, i
        INTEGER                         :: num
        
      	!----------------------------------------------------
      	! Initialize 
      	!----------------------------------------------------
        
        stat_info = 0
        
      	!----------------------------------------------------
        ! Get length of name and the value
      	!----------------------------------------------------
        
        name_length = LEN_TRIM(name)
        suffix_length = NAME_DISPLAY_CHAR - name_length
        
        !----------------------------------------------------
        ! Suffix will be at least one space
        !----------------------------------------------------
        
        suffix = "-"
        DO i = 2, suffix_length
           suffix = TRIM(suffix) // "-"
        END DO
        
        !----------------------------------------------------
        ! Concatenate name and value with a separater ">:"
        !----------------------------------------------------
        
        msg = TRIM(name) // TRIM(suffix) // ">: "
        
        num = SIZE(val,1)
        
        IF ( num < 10 ) THEN
           WRITE(form, "(A3,I1,A4)") "(A,",  num , "I15)"
        ELSE
           WRITE(form, "(A3,I2,A4)") "(A,",  num , "I15)"
        END IF
        
        WRITE(msg, TRIM(form)) TRIM(msg), val

        PRINT *, TRIM(msg)

        
        !----------------------------------------------------
        ! Return 
      	!----------------------------------------------------
        
9999    CONTINUE
        
        RETURN
        
      END SUBROUTINE tool_print_msg_ii
      

      SUBROUTINE tool_print_msg_is(this,name,val1,val2,stat_info)
        !----------------------------------------------------
      	! Subroutine  : tool_print_msg_is
      	!----------------------------------------------------
	!
      	! Purpose     : Printing a name, an integer value
        !               and a string message for 
        !               displaying purpose.
      	!
      	! Input       : 
      	!               
      	!
      	!Input/output : 
      	!
      	! Output      : stat_info      (I) error status
      	!
      	! Routines    :
      	!
      	! Remarks     :
      	!
      	! References  :
      	!
      	! Revisions   : V0.1 01.08.2012, original version.
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
        CHARACTER(LEN=*) 	        :: name
        INTEGER           	        :: val1
        CHARACTER(LEN=*)       	        :: val2
        INTEGER          	        :: stat_info
       
      	!----------------------------------------------------
      	! Local variables 
      	!----------------------------------------------------

        CHARACTER(LEN=MAX_CHAR)	        :: suffix, msg
        INTEGER                         :: name_length
        INTEGER                         :: suffix_length, i
        
      	!----------------------------------------------------
      	! Initialize 
      	!----------------------------------------------------
        
        stat_info = 0
        
      	!----------------------------------------------------
        ! Get length of name and the value
      	!----------------------------------------------------
        
        name_length = LEN_TRIM(name)        
        suffix_length = NAME_DISPLAY_CHAR - name_length
        
        !----------------------------------------------------
        ! Suffix will be at least one space
        !----------------------------------------------------

        suffix = "-"
        DO i = 2, suffix_length
           suffix = TRIM(suffix) // "-"
        END DO
        
        !----------------------------------------------------
        ! Concatenate name and value with a separater ">:"
        !----------------------------------------------------
        
        msg = TRIM(name) // TRIM(suffix) // ">: "
        
        WRITE(msg, "(A,I8,A)") TRIM(msg), val1, TRIM(val2)

        PRINT *, TRIM(msg)
        
        
        !----------------------------------------------------
        ! Return 
      	!----------------------------------------------------
        
9999    CONTINUE
        
        RETURN
        
      END SUBROUTINE tool_print_msg_is

      
      SUBROUTINE tool_print_msg_s(this,name,val,stat_info)
        !----------------------------------------------------
      	! Subroutine  : tool_print_msg_s
      	!----------------------------------------------------
	!
      	! Purpose     : Printing a name and a string message 
      	!	 	for displaying purpose
      	!
      	! Input       : 
      	!               
      	!
      	!Input/output : 
      	!
      	! Output      : stat_info      (I) error status
      	!
      	! Routines    :
      	!
      	! Remarks     :
      	!
      	! References  :
      	!
      	! Revisions   : V0.1 01.08.2012, original version.
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
        CHARACTER(LEN=*) 	        :: name
        CHARACTER(LEN=*) 	        :: val
        INTEGER          	        :: stat_info
        
      	!----------------------------------------------------
      	! Local variables 
      	!----------------------------------------------------

        CHARACTER(LEN=MAX_CHAR)	        :: suffix, msg
        INTEGER                         :: name_length
        INTEGER                         :: suffix_length, i
        
      	!----------------------------------------------------
      	! Initialize 
      	!----------------------------------------------------
        
        stat_info = 0
        
      	!----------------------------------------------------
        ! Get length of name and the value
      	!----------------------------------------------------
        
        name_length = LEN_TRIM(name)        
        suffix_length = NAME_DISPLAY_CHAR - name_length
        
        !----------------------------------------------------
        ! Suffix will be at least one space
        !----------------------------------------------------

        suffix = "-"
        DO i = 2, suffix_length
           suffix = TRIM(suffix) // "-"
        END DO
        
        !----------------------------------------------------
        ! Concatenate name and value with a separater ">:"
        !----------------------------------------------------
        
        msg = TRIM(name) // TRIM(suffix) // ">: " // TRIM(val)
        
        PRINT *, TRIM(msg)

        
        !----------------------------------------------------
        ! Return 
      	!----------------------------------------------------
        
9999    CONTINUE
        
        RETURN
        
      END SUBROUTINE tool_print_msg_s
            

      SUBROUTINE tool_print_msg_s2(this,name,val1,val2,stat_info)
        !----------------------------------------------------
      	! Subroutine  : tool_print_msg_s2
      	!----------------------------------------------------
	!
      	! Purpose     : Printing a name and two string messages
      	!	 	for displaying purpose
      	!
      	! Input       : 
      	!               
      	!
      	!Input/output : 
      	!
      	! Output      : stat_info      (I) error status
      	!
      	! Routines    :
      	!
      	! Remarks     :
      	!
      	! References  :
      	!
      	! Revisions   : V0.1 19.06.2013, original version.
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
        CHARACTER(LEN=*) 	        :: name
        CHARACTER(LEN=*) 	        :: val1
        CHARACTER(LEN=*) 	        :: val2
        INTEGER          	        :: stat_info
        
      	!----------------------------------------------------
      	! Local variables 
      	!----------------------------------------------------

        CHARACTER(LEN=MAX_CHAR)	        :: suffix, msg
        INTEGER                         :: name_length
        INTEGER                         :: suffix_length, i
        
      	!----------------------------------------------------
      	! Initialize 
      	!----------------------------------------------------
        
        stat_info = 0
        
      	!----------------------------------------------------
        ! Get length of name and the value
      	!----------------------------------------------------
        
        name_length = LEN_TRIM(name)        
        suffix_length = NAME_DISPLAY_CHAR - name_length
        
        !----------------------------------------------------
        ! Suffix will be at least one space
        !----------------------------------------------------

        suffix = "-"
        DO i = 2, suffix_length
           suffix = TRIM(suffix) // "-"
        END DO
        
        !----------------------------------------------------
        ! Concatenate name and value with a separater ">:"
        !----------------------------------------------------
        
        msg = TRIM(name) // TRIM(suffix) // ">: " // TRIM(val1) // " " //TRIM(val2)
        
        PRINT *, TRIM(msg)

        
        !----------------------------------------------------
        ! Return 
      	!----------------------------------------------------
        
9999    CONTINUE
        
        RETURN
        
      END SUBROUTINE tool_print_msg_s2

      
      SUBROUTINE tool_print_msg_si(this,name,val1,val2,stat_info)
        !----------------------------------------------------
      	! Subroutine  : tool_print_msg_si
      	!----------------------------------------------------
	!
      	! Purpose     : Printing a name, a string message
        !               and an integer value for
        !               displaying purpose
      	!
      	! Input       : 
      	!               
      	!
      	!Input/output : 
      	!
      	! Output      : stat_info      (I) error status
      	!
      	! Routines    :
      	!
      	! Remarks     :
      	!
      	! References  :
      	!
      	! Revisions   : V0.1 01.08.2012, original version.
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
        CHARACTER(LEN=*) 	        :: name
        CHARACTER(LEN=*)       	        :: val1
        INTEGER           	        :: val2
        INTEGER          	        :: stat_info
       
      	!----------------------------------------------------
      	! Local variables 
      	!----------------------------------------------------

        CHARACTER(LEN=MAX_CHAR)	        :: suffix, msg
        INTEGER                         :: name_length
        INTEGER                         :: suffix_length, i
        
      	!----------------------------------------------------
      	! Initialize 
      	!----------------------------------------------------
        
        stat_info = 0
        
      	!----------------------------------------------------
        ! Get length of name and the value
      	!----------------------------------------------------
        
        name_length = LEN_TRIM(name)        
        suffix_length = NAME_DISPLAY_CHAR - name_length
        
        !----------------------------------------------------
        ! Suffix will be at least one space
        !----------------------------------------------------

        suffix = "-"
        DO i = 2, suffix_length
           suffix = TRIM(suffix) // "-"
        END DO
        
        !----------------------------------------------------
        ! Concatenate name and value with a separater ">:"
        !----------------------------------------------------
        
        msg = TRIM(name) // TRIM(suffix) // ">: " // TRIM(val1)
        
        WRITE(msg, "(A,I8)") TRIM(msg), val2
        
        PRINT *, TRIM(msg)
        
        
        !----------------------------------------------------
        ! Return 
      	!----------------------------------------------------
        
9999    CONTINUE
        
        RETURN
        
      END SUBROUTINE tool_print_msg_si

      
