      SUBROUTINE colloid_nearest_image(this,x,sid, &
           x_image,rx,v_image,stat_info)
        !----------------------------------------------------
        ! Subroutine  : colloid_nearest_image
        !----------------------------------------------------
        ! Purpose     : Compute for a fluid particle, 
        !               a boundary particle or a point
        !               the nearest image of colloid center.
        !
        ! Routines    :
        !
        ! References  :
        !
        ! Remarks     : According to different boundary
        !               conditions, loop over all images 
        !               of colloid center.
        !
        ! Revisions   : V0.1 12.10.2010, original version
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
        ! x     : position of a point.
        ! sid   : species ID of a colloid.
        !
        ! Output
        !
        ! x_image : position of nearest image of the colloid
        !           center to the boundary particle.
        ! rx      : relative postion
        ! v_image : velocity of the nearest image.
        ! stat_info : status of the routine.
        !----------------------------------------------------
        
        TYPE(Colloid), INTENT(IN)               :: this
        REAL(MK), DIMENSION(:), INTENT(IN)      :: x
        INTEGER, INTENT(IN)                     :: sid
        REAL(MK), DIMENSION(:), INTENT(OUT)     :: x_image
        REAL(MK), DIMENSION(:), INTENT(OUT)     :: rx
        REAL(MK), DIMENSION(:), INTENT(OUT)     :: v_image
        INTEGER, INTENT(OUT)                    :: stat_info
        
        !----------------------------------------------------
        ! Local parameters
        !----------------------------------------------------
        
        INTEGER                                 :: stat_info_sub
        INTEGER                                 :: dim, i
        REAL(MK)                                :: xmin, xdist
        
        
        !----------------------------------------------------
        ! Initialization of variables.
        !----------------------------------------------------
        
        stat_info     = 0
        stat_info_sub = 0
        dim           = this%num_dim
        
        x_image(1:dim) = this%x_image(1:dim,sid,1)
        rx(1:dim)      = x(1:dim) - x_image(1:dim)
        v_image(1:dim) = this%v_image(1:dim,sid,1)
        xmin = SQRT(DOT_PRODUCT(rx(1:dim),rx(1:dim)))
        
        !----------------------------------------------------
        ! Calculate relative position of x to each image and
        ! find the minimal.
        !----------------------------------------------------
        
        DO i = 2, this%num_image
           
           rx(1:dim) = x(1:dim) - this%x_image(1:dim,sid,i)
           
           xdist = SQRT(DOT_PRODUCT(rx(1:dim),rx(1:dim)))
           
           IF ( xdist < xmin ) THEN

              x_image(1:dim) = this%x_image(1:dim,sid,i)
              v_image(1:dim) = this%v_image(1:dim,sid,i)
              xmin = xdist
              
           END IF
           
        END DO ! i = 2, num_image
        
        rx(1:dim) = x(1:dim) - x_image(1:dim)
        
        RETURN
        
      END SUBROUTINE colloid_nearest_image
      
      
