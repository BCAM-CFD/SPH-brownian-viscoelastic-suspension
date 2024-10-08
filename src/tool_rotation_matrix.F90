      SUBROUTINE tool_rotation_matrix(this,dim,ax,phi,rm,stat_info)
        !----------------------------------------------------
        ! Subroutine  : tool_rotation_matrix
        !----------------------------------------------------
        !
        ! Purpose     : Calculate rotation matrix, given
        !               rotating axis unit vector I and 
        !               angle about I.
        !
        ! Routines    :
        !
        ! Remarks     : dim: specify problem dimension;
        !               ax : Rotating axis is always 3D;
        !               phi: rotate angle;
        !               rm : resultant rotation matrix.
        !
        ! References  : Chen et al. 
        !               Physics of Fluids 18, 103605, 2006
        !
        ! Revisions   : V0.1 23.08 2010, original version.
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
        INTEGER,INTENT(IN)                      :: dim
        !********* Changed by Adolfo ********
!        REAL(MK),DIMENSION(:),INTENT(IN)        :: ax
        REAL(MK),DIMENSION(:),INTENT(INOUT)        :: ax
        !**************************
        REAL(MK),INTENT(IN)                     :: phi
        REAL(MK),DIMENSION(:,:),INTENT(OUT)     :: rm
        INTEGER, INTENT(OUT)                    :: stat_info

        INTEGER                                 :: dim1
        REAL(MK)                                :: len,cp,sp,cp1
        
        !----------------------------------------------------
        ! Initialization
        !
        ! This is supposed to be used, otherwise,
        ! compiler complains that it is not used.
        !----------------------------------------------------
        
        stat_info = this%flag
        stat_info = 0
        
        IF ( dim /=2 .AND. dim /=3 ) THEN
           PRINT *, __FILE__, __LINE__, &
                "Dimension should be either 2 or 3 !"
           stat_info = -1
           GOTO 9999
        END IF
        
        dim1 = SIZE(ax,1)
        IF ( dim1 /= 3 ) THEN
           PRINT *,  __FILE__, __LINE__, &
                "rotation axis should be 3D !"
           stat_info = -1
           GOTO 9999
        END IF
        
        len = DOT_PRODUCT(ax(:), ax(:))
        IF ( len > 1.0_MK + mcf_machine_zero) THEN
           PRINT *,  __FILE__, __LINE__, &
                "rotation axis should be unit vector !"
           !**** Changed by Adolfo ***
           PRINT *,  __FILE__, __LINE__, &
                "rotation axis has been resized"
           ax(:) = ax(:) / SQRT(len)
!           stat_info = -1
!           GOTO 9999
           !**************************
        END IF
        
        dim1 = SIZE(rm,1)
        IF ( dim1 /= 3 ) THEN
           PRINT *,  __FILE__, __LINE__, &
                "dimension of rotation matrix does not match !"
           stat_info = -1
           GOTO 9999
        END IF
        
        rm(:,:) = 0.0_MK

        cp  = COS(phi)
        cp1 = 1.0_MK-cp
        sp  = SIN(phi)
        
        rm(1,1) = ax(1)**2   *cp1 + cp
        rm(1,2) = ax(1)*ax(2)*cp1 - ax(3) * sp
        rm(1,3) = ax(1)*ax(3)*cp1 + ax(2) * sp
                   
        rm(2,1) = ax(2)*ax(1)*cp1 + ax(3) * sp
        rm(2,2) = ax(2)**2   *cp1 + cp
        rm(2,3) = ax(2)*ax(3)*cp1 - ax(1) * sp
        
        rm(3,1) = ax(3)*ax(1)*cp1 - ax(2) * sp
        rm(3,2) = ax(3)*ax(2)*cp1 + ax(1) * sp
        rm(3,3) = ax(3)**2*cp1    + cp
        
        
        !----------------------------------------------------
        ! Return.
        !----------------------------------------------------
        
9999    CONTINUE
        
        RETURN
        
      END SUBROUTINE tool_rotation_matrix
      
      
