      SUBROUTINE colloid_particle_velocity(this,x,sid,v,stat_info)
        !----------------------------------------------------
        ! Subroutine  : colloid_particle_velocity
        !----------------------------------------------------
        ! Purpose     : Compute colloidal boundary particle's  
        !               instantaneous velocity according to 
        !               different boundary conditions.
        !
        ! Routines    :
        !
        ! References  :
        !
        ! Remarks     : Since colloid are modelled as rigid 
        !               bodies, all boundary particles 
        !               which consititute the colloid
        !               should get the same translation
        !               velocity vt from the colloid and 
        !               variant rotation velocity 
        !               vr = omega (cross product) rc,
        !               where omega is angular velocity of
        !               the colloid and rc is relative 
        !               position of the boundary particle to
        !               the center of the colloid.
        !
        ! Revisions   :  V0.1 14.10.2010, original version
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
        ! Arguments :
        !
        ! Input 
        !
        ! this  : object of a colloid.
        ! x     : position of a boundary particle.
        ! sid   : species ID of the boundary particle.
        !
        ! Output
        !
        ! v    :  velocity of the boundary particle
        !         
        ! stat_info : status of the routine.
        !----------------------------------------------------
        
        TYPE(Colloid), INTENT(IN)               :: this
        REAL(MK), DIMENSION(:), INTENT(IN)      :: x
        INTEGER, INTENT(IN)                     :: sid
        REAL(MK), DIMENSION(:), INTENT(OUT)     :: v
        INTEGER, INTENT(OUT)                    :: stat_info
        
        !----------------------------------------------------
        ! Local parameters:
        !----------------------------------------------------
        
        INTEGER                                 :: stat_info_sub
        INTEGER                                 :: dim
        REAL(MK), DIMENSION(3)                  :: x_image
        REAL(MK), DIMENSION(3)                  :: rx
        REAL(MK), DIMENSION(3)                  :: v_image
        REAL(MK), DIMENSION(3)                  :: v_rotate

        
        !----------------------------------------------------
        ! Initialization of variables.
        !----------------------------------------------------
        
        stat_info     = 0
        stat_info_sub = 0
        
        dim      = this%num_dim
        v(1:dim) = 0.0_MK
        
        
        !----------------------------------------------------
        ! If colloid can move, i.e., translate or rotate,
        ! return the nearest images of colloid center.
        !----------------------------------------------------
        
        IF ( this%translate .OR. this%rotate ) THEN
           
           CALL colloid_in_nearest_image(this, x(1:dim), sid, &
                x_image(1:dim), rx(1:dim), v_image(1:dim), stat_info_sub)
           
           IF ( stat_info_sub /= 0 ) THEN
              
              PRINT *, "colloid_particle_velocity : ", &
                   "Calling colloid_in_nearest_image failed !"
              stat_info = -1
              GOTO 9999
              
           END IF
           
           v(1:dim) = v_image(1:dim)
           
           !-------------------------------------------------
           ! Add up its instantaneous rotation velocity.
           !-------------------------------------------------
           
           v_rotate(1:dim) = 0.0_MK
           
           IF ( this%rotate ) THEN
              
              CALL tool_cross_product(this%tool,&
                   this%omega(1:3,sid,1),rx(1:3),&
                   v_rotate(1:3),stat_info_sub)
              
              v(1:dim) = v(1:dim) + v_rotate(1:dim)
              
           END IF ! rotate
           
        END IF ! translate or rotate
        
        
9999    CONTINUE
        
        RETURN
        
      END SUBROUTINE colloid_particle_velocity
      
