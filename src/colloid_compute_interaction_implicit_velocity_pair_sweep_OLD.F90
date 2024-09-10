!------- Changed by Adolfo ------------------
!!$      SUBROUTINE colloid_compute_interaction_implicit_velocity_pair_sweep(&
!!$           this, dt, num_sweep, stat_info)
      SUBROUTINE colloid_compute_interaction_implicit_velocity_pair_sweep(&
           this, dt, num_sweep, dWij, kt, stat_info)
!----------------------------------------------
        !----------------------------------------------------
        ! Subroutine  : colloid_compute_interaction_implicit_
        !               velocity_pair_sweep
        !----------------------------------------------------
        !
        ! Purpose     :   Update velocties from 
        !                 lubrication-correction force using 
        !                 implicit splitting scheme for 
        !                 pair-wise colloids with number of
        !                 sweeps.
        !         
        !
        ! Routines    :
        !
        ! References  : Shardlow T. SIAM J. Sci. Comput. 2003.
        !               Litvinov S. et al. J. Comput. Phys. 2010.
        !
        ! Remarks     :
        !
        ! Revisions   : V0.2 17.07.2012, extended version for 3D
        !
        !               V0.1 25.05.2012, original version for 2D
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
        
        TYPE(Colloid), INTENT(OUT)              :: this
        REAL(MK), INTENT(IN)                    :: dt
        INTEGER, INTENT(IN)                     :: num_sweep
        !*********** Added by Adolfo ***********
        REAL(MK), DIMENSION(:,:,:,:), INTENT(IN) :: dWij
        REAL(MK), INTENT(IN)                     :: kt
        !***************************************
        INTEGER, INTENT(OUT)                    :: stat_info
        
        !----------------------------------------------------
        ! Local physics variables:
        !
        ! dim   : dimension
        ! num   : number of total colloids
        !
        ! hn_l,hm_l : lubrication correction cut off and on.
        ! F0,F1     : lubrication correction parameters.
        !----------------------------------------------------
        
        INTEGER                                 :: stat_info_sub
        INTEGER                                 :: dim, num
        REAL(MK)                                :: hn_l, hm_l
	!*** Added by Adolfo ***
	REAL(MK)                                :: htang_l
        REAL(MK)                                :: hn_r, hm_r
!	REAL(MK)                                :: hr !-- Faltaria darle valor a hr --
!	REAL(MK)                                :: F0_repul
!	REAL(MK)                                :: F_repul
!	REAL(MK), DIMENSION(3)                  :: F_repul_vec
        REAL(MK) :: eta_total
        INTEGER :: k
        REAL(MK) :: dPij
        REAL(MK) :: trace
	!***********************
        REAL(MK)                                :: F0, F1
        
        !----------------------------------------------------
        ! Local numerical variables:
        !
        ! num_sweep   : number of sweeps for pair-wise implicit
        !               splitting scheme.
        !----------------------------------------------------
        INTEGER                                 :: sweep
        REAL(MK)                                :: dt_sweep
        
        !----------------------------------------------------
        ! ai,aj       : radius of colloid i,j.
        ! aa          : ai+aj.
        ! Aij         : lubrication correction coefficient for i,j.
        ! Bij         : repulsive force coefficient for i,j.
        !
        ! x_image     : position of closest image of colloid j to i.
        ! v_image     : velocity of closest image of colloid j to i.
        !----------------------------------------------------
        
        REAL(MK)                                :: ai, aj, aa
        REAL(MK)                                :: Aij
        REAL(MK)                                :: Bij
	REAL(MK)                                :: A3D !-- Added by Adolfo --
	REAL(MK)                                :: A3D_kt !-- Added by Adolfo --
	REAL(MK)                                :: B3D !-- Added by Adolfo --
	REAL(MK)                                :: C3D !-- Added by Adolfo --
	REAL(MK)                                :: aux !-- Added by Adolfo --
	REAL(MK)                                :: aux_kt !-- Added by Adolfo --
	REAL(MK)                                :: aux2 !-- Added by Adolfo --
        REAL(MK), DIMENSION(3)                  :: vij_new !-- Added by Adolfo --
        REAL(MK), DIMENSION(3)                  :: vij_old !-- Added by Adolfo --
        REAL(MK)                                :: eij_vij_old !-- Added by Adolfo --
        REAL(MK)                                :: fij !-- Added by Adolfo --
        REAL(MK)                                :: gij !-- Added by Adolfo --
        INTEGER                                 :: L !-- Added by Adolfo --
        REAL(MK), DIMENSION(:,:), ALLOCATABLE   :: dW_overline !-- Added by Adolfo --        
        REAL(MK)                                :: A_kt !-- Added by Adolfo --
        REAL(MK)                                :: B_kt !-- Added by Adolfo --
        REAL(MK), DIMENSION(:), ALLOCATABLE     :: dPij_vec !-- Added by Adolfo --        
        REAL(MK)                                :: Cij !-- Added by Adolfo --
        REAL(MK)                                :: Dij !-- Added by Adolfo --
        REAL(MK)                                :: dPij_eij !-- Added by Adolfo --        
	REAL(MK)                                :: beta
        REAL(MK), DIMENSION(3)                  :: vi_old, vj_old
        REAL(MK), DIMENSION(3)                  :: vi_new, vj_new
        
        REAL(MK), DIMENSION(3)                  :: x_image, v_image 
        REAL(MK), DIMENSION(3)                  :: rij, eij, vij
        REAL(MK)                                :: r, h
          
        INTEGER                                 :: i, j
        
        !----------------------------------------------------
        ! Initialization
        !----------------------------------------------------

        stat_info     = 0
        stat_info_sub = 0
        
        IF ( num_sweep < 1 ) THEN
           PRINT *, __FILE__,__LINE__,&
                "number of sweeps is wrong!"
           stat_info = -1
           GOTO 9999
        END IF

        !----------------------------------------------------
        ! set up physics parameters.
        !----------------------------------------------------
        
        dim   = this%num_dim
        num   = this%num_colloid
        
        hn_l  = this%cc_lub_cut_off
	!***** Added by Adolfo *******
        htang_l  = this%cc_lub_cut_off/4.0_MK
!        hn_r  = this%cc_repul_cut_off
!        hm_r  = this%cc_repul_cut_on
!        F0_repul = this%cc_repul_F0
	!*****************************
        hm_l  = this%cc_lub_cut_on
        
        IF ( dim == 2 ) THEN
           
           F0  = 3.0_MK*mcf_pi*SQRT(2.0_MK)/4.0_MK
           F1  = 231.0_MK*mcf_pi*SQRT(2.0_MK) / 80.0_MK
           
        ELSE IF ( dim == 3 ) THEN
           
        ELSE
           
           PRINT *, __FILE__, __LINE__, "dimension not available!"
           stat_info = -1
           GOTO 9999
           
        END IF
        
        !----------------------------------------------------
        ! set up parameters for implicit sweeps.
        !----------------------------------------------------
        
        dt_sweep  = dt / num_sweep
        
        !----------------------------------------------------
        ! For now, we assume that each radius is the same.
        !----------------------------------------------------
        
        ai = this%radius(1,1)
        aj = ai
        aa = ai+aj

	!-- Added by Adolfo -- !! Assuming equal radio for all of them -
	!-- Only for normal lubrication
	A3D = -6.0_MK * mcf_pi * this%eta * &
	      ( ai*aj/aa )**2.0_MK *dt_sweep
	 !-- For normal and tangential lubrication as Bertevas --
	beta = aj / ai
	B3D = -6.0_MK * mcf_pi * this%eta * ai * &
	    (1.0_MK + 7.0_MK * beta + beta * beta) / &
	    (5.0_MK * (1.0_MK + beta)**3.0_MK)*dt_sweep
	C3D = -6.0_MK * mcf_pi * this%eta * ai * &
	    (4.0_MK * beta * (2.0_MK + beta + &
	    2.0_MK * beta * beta)) / &
	    (15.0_MK * (1.0_MK + beta)**3.0_MK)*dt_sweep
	!-- End of added by Adolfo --
         

        !----------------------------------------------------
        ! Do the pairwise sweep num_sweep times to update
        ! velocities using only lubrcation correction forces.
        ! Time step at each sweep is dt_sweep=dt/num_sweep.
        !
        ! It is implicit.
        !
        !----------------------------------------------------
        
        IF ( this%cc_lub_type > mcf_cc_lub_type_no ) THEN
        
           DO sweep = 1, num_sweep
              
              !----------------------------------------------
              ! Implicit: dt_sweep.
              !          
              ! Loop over each colloid and its interaction with 
              ! other colloids.
              ! Build up left hand matrix and right hand vector
              ! for a system of linear equations of two collodis,
              ! and then update velocity using implicit scheme
              ! by dt_sweep.
              ! We solve this small matrix explicitly/analytically
              ! using maxima.
              !----------------------------------------------
              
              DO i = 1, num - 1
                 
                 !-------------------------------------------
                 ! Interaction between two different colloids.
                 !-------------------------------------------
            
                 DO j = i + 1, num
                    
                    vi_old(1:dim) = this%v(1:dim,i,1)
                    vj_old(1:dim) = this%v(1:dim,j,1)
                    
                    !----------------------------------------
                    ! Calculate the gap and unit vector joining
                    ! two colloids, images of colloids have to
                    ! be considered according to different
                    ! boundary conditions.
                    !----------------------------------------
                    
                    CALL colloid_nearest_image(this,&
                         this%x(1:dim,i),j, &
                         x_image(1:dim),rij(1:dim), &
                         v_image(1:dim),stat_info_sub)
                    
                    r  = SQRT(DOT_PRODUCT(rij(1:dim), rij(1:dim)))
                    h  = r - aa
                    eij(1:dim) = rij(1:dim) / r
                    
                    !----------------------------------------
                    ! If gap is smaller than hn_l, i.e., 
                    ! cut_off of lubrication correction,
                    ! it needs lubrication correction.
                    !----------------------------------------
                    
                    IF ( h < hn_l ) THEN
                       
                       !-------------------------------------
                       ! If gap is smaller than minimal allowed
                       ! gap, set it to the pre-set minimum.
                       !-------------------------------------
                 
                       IF ( h < hm_l ) THEN
                          
                          h = hm_l
                          
                       END IF
                       
!		       !******** Added by Adolfo *****
!                       IF ( h < hm_r ) THEN
!                          
!                          hr = hm_r
!
!			ELSE
!			  
!			  hr = h
!                          
!                       END IF
		       !******************************

                       IF ( dim == 2 )THEN
                          !----------------------------------
                          ! dt_sweep/2/m is coefficient of Aij.
                          ! Assuming now mi=mj
                          !----------------------------------
                          
                          Aij = -0.5_MK * this%eta * &
                               ( (aa/h)**1.5_MK  * (F0 + h*F1/aa) - &
                               (aa/hn_l)**1.5_MK * (F0 + hn_l*F1/aa) ) * &
                               dt_sweep / this%m(i)
                          
                          !----------------------------------
                          ! Solve 2*2 linear system analytically.
                          !----------------------------------
                       
#include "velocity_pair_backward_Euler_2d.inc"
                          
                       ELSE IF ( dim == 3 ) THEN

                          !----------------------------------
                          ! dt_sweep/2/m is coefficient of Aij.
                          ! Assuming now mi=mj
                          !----------------------------------
 
!-- Changed by Adolfo --               
!!$                          Aij = -6.0_MK * mcf_pi * this%eta * &
!!$                               ( ai*aj/aa )**2  * &
!!$                               ( 1.0_MK/h - 1.0_MK/hn_l )  * &
!!$                               dt_sweep / this%m(i)
		   
!                          !-- This is only normal lubrication
!                           Aij = A3D * &
!                                ( 1.0_MK/h - 1.0_MK/hn_l )  * &
!                                (1.0_MK / this%m(i) + &
!				1.0_MK / this%m(j))
!                           aux = Aij / (Aij - 1.0_MK)

                          !-- Normal and tangential lubrication as Bertevas --
			   aux  = (LOG(ai/h) - LOG(ai/hn_l))
			   aux2 = (1.0_MK / this%m(i) + 1.0_MK / this%m(j))
                           Aij  = (A3D * ( 1.0_MK/h - 1.0_MK/hn_l ) + &
				 B3D * aux) * aux2
		           IF (h < htang_l) THEN
                              aux  = (LOG(ai/h) - LOG(ai/htang_l))
		              Bij  = C3D * aux * aux2
			   ELSE 
			      Bij = 0.0_MK
			   ENDIF 

!			   !-- Repulsion force --
!			   IF (h < hn_r) THEN
!			      F_repul =  F0_repul / hn_r * &
!				  EXP(-hr/hn_r) / (1.0_MK-EXP(-hr/hn_r))
!		              F_repul_vec(1:dim) = F_repul * aux2 * dt_sweep * eij(1:dim)
!			   ELSE		 
!			      F_repul_vec(:) = 0.0_MK
!			   ENDIF

 !                         !-- Normal and tangential lubrication splitted separately --
!                           aux  = (LOG(ai/h) - LOG(ai/hn_l))
!                           aux2 = (1.0_MK / this%m(i) + 1.0_MK / this%m(j))
!                          Aij  = (A3D * ( 1.0_MK/h - 1.0_MK/hn_l ) + &
!                                B3D * aux) * aux2


                          !----------------------------------
                          ! Solve 3*3 linear system analytically.
                          !----------------------------------
                          
#include "velocity_pair_backward_Euler_3d.inc"
                          
                       END IF
                       
                       !-------------------------------------
                       ! update velocity of the interacting
                       ! pair immediately.
                       !-------------------------------------
                       
                       this%v(1:dim,i,1) = vi_new(1:dim)
                       this%v(1:dim,j,1) = vj_new(1:dim)
                       
                    END IF ! h < hn_l
                    
                 END DO ! j = i + 1, num
                 
              END DO ! i = 1, num - 1
              
           END DO ! sweep = 1, num_sweep
           
        END IF ! cc_lub_type > mcf_cc_lub_type_no
        
9999    CONTINUE
        
        RETURN          
        
      END SUBROUTINE colloid_compute_interaction_implicit_velocity_pair_sweep


