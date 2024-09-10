!----------------------------------------------------------------
! This contains the routines which calculate the force and 
! return two accelerations between two Newtonian particles, 
! accelerations from i.e. conservative(pressure) force 
! dissipative(viscous) force and random(thermal noise) force.
! Using Espanol & Revenga 2003 formulation.
!----------------------------------------------------------------

!******************* Modified by Adolfo **************
!!$      SUBROUTINE rhs_force_ff_Newtonian_Espanol(this,&
!!$           xi,xj,dij,vi,vj,numi,numj,pi,pj,&
!!$           mi,mj,w,gradw,fi,fj,auij,stat_info)
      SUBROUTINE rhs_force_ff_Newtonian_Espanol(this,&
           xi,xj,dij,vi,vj,numi,numj,pi,pj,&
           mi,mj,w,gradw,fi,fj,type_i, type_j, auij,stat_info)
        !***********************************************
        !----------------------------------------------------
        ! Subroutine : rhs_force_ff_Newtonian_Espanol
        !----------------------------------------------------
        !
        ! Purpose    : Implement the pressure, viscous and 
        !              thermal force from 
        !              Espanol and Revenga 2003
        !              Physical Review E paper.

        !
        ! Reference  : Hu and Adams 
        !              Journal of Computational Physics 213 
        !              844-861, 2006.
        !
        ! Remark     :
        !
        ! Revisions  : V0.1 30.02.2009
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
        ! 
        ! numi : number density of i
        ! numj : number density of j
        !----------------------------------------------------
        
        TYPE(Rhs), INTENT(INOUT)                :: this
        REAL(MK), DIMENSION(:), INTENT(IN)      :: xi
        REAL(MK), DIMENSION(:), INTENT(IN)      :: xj
        REAL(MK), INTENT(IN)                    :: dij
        REAL(MK), DIMENSION(:), INTENT(IN)      :: vi
        REAL(MK), DIMENSION(:), INTENT(IN)      :: vj
        REAL(MK), INTENT(IN)                    :: numi
        REAL(MK), INTENT(IN)                    :: numj
        REAL(MK), INTENT(IN)                    :: pi
        REAL(MK), INTENT(IN)                    :: pj
        REAL(MK), INTENT(IN)                    :: mi
        REAL(MK), INTENT(IN)                    :: mj
        REAL(MK), INTENT(IN)                    :: w
        REAL(MK), INTENT(IN)                    :: gradw
        REAL(MK), DIMENSION(:), INTENT(INOUT)   :: fi
        REAL(MK), DIMENSION(:), INTENT(INOUT)   :: fj
        !************* Added by Adolfo **********
        INTEGER, INTENT(IN)   :: type_i
        INTEGER, INTENT(IN)   :: type_j
        !****************************************
        REAL(MK), INTENT(OUT),OPTIONAL          :: auij
        INTEGER, INTENT(OUT)                    :: stat_info
        
        !----------------------------------------------------
        ! Local variables.
        !----------------------------------------------------
        
        INTEGER                         :: stat_info_sub
        INTEGER                         :: num_dim
        REAL(MK)                        :: dt
        REAL(MK)                        :: eta
!!$	!** Added by Adolfo **
!!$	REAL(MK)                        :: ksai
        ! ----- Now defined in function of a and b ----
!!$	!*********************
        REAL(MK)                        :: kt        
        REAL(MK), DIMENSION(3)          :: eij
        REAL(MK)                        :: ev
        REAL(MK), DIMENSION(3)          :: vij
        !******* Modified by Adolfo *******
        REAL(MK)                        :: f_c!,f_d1,f_d2
!!$        REAL(MK)                        :: f_c,f_d1,f_d2
        !*********************************
        REAL(MK), DIMENSION(3)          :: f_r
        REAL(MK), DIMENSION(3,3)        :: dW
        REAL(MK)                        :: trace,Zij,Aij,Bij
        REAL(MK), DIMENSION(3)          :: We
        INTEGER                         :: i,j         
        !*********** Added by Adolfo **********
        REAL(MK)                        :: a, b
        !**************************************
        
        !----------------------------------------------------
        ! Initialization of variables.
        !----------------------------------------------------
        
        stat_info     = 0
        stat_info_sub = 0
        
        fi(:) = 0.0_MK
        fj(:) = 0.0_MK
        
        num_dim = this%num_dim

        dt  = this%dt
!!$        eta     = physics_get_eta(this%phys,stat_info_sub)     
!!$	!**** Added by Adolfo *****
!!$        ksai     = physics_get_ksai(this%phys,stat_info_sub)     
!!$	!**************************
        !********** Added by Adolfo ********
        a = physics_get_a(this%phys,stat_info_sub)     
        b = physics_get_b(this%phys,stat_info_sub)     
        !***********************************
        kt      = this%kt

        !----------------------------------------------------
        ! Check if any denominator is non-positive.
        !----------------------------------------------------

        IF ( dij < mcf_machine_zero .OR. &
             mi < mcf_machine_zero .OR. &
             mj < mcf_machine_zero .OR. &
             numi < mcf_machine_zero .OR. &
             numj < mcf_machine_zero) THEN
           
           PRINT *,"xi,xj,dij : ",  xi,xj,dij
           PRINT *,"vi,vj : ", vi,vj
           PRINT *,"numi,numj : ", numi,numj
           PRINT *,"pi,pj : ", pi,pj
           PRINT *,"mi,mj : ", mi,mj
           PRINT *,"w, gradw : ", w, gradw
           
           PRINT *, "rhs_force_ff_Newtonian_Espanol : ",& 
                "Divided by non-positive !" 
           stat_info = -1
           GOTO 9999
           
        END IF
        
        
        !----------------------------------------------------
        ! Calculate normalized vector pointing from j to i.
        !----------------------------------------------------
        
        eij(1:num_dim) = (xi(1:num_dim)  - xj(1:num_dim)) / dij
        
        !----------------------------------------------------
        ! Calculate velocity vector pointing from j to i.
        !----------------------------------------------------
        
        vij(1:num_dim)  = vi(1:num_dim) - vj(1:num_dim)
        
        !----------------------------------------------------
        ! Dot product of normalized vector e and velocity v.
        !----------------------------------------------------
        
        ev = 0.0_MK
        DO i = 1, num_dim
           ev = ev + eij(i) * vij(i)  
        END DO
        
  	!----------------------------------------------------
	! Calculate the conservative force
	! per unit mass(from pressure).
  	!----------------------------------------------------
        
        f_c = ( pi/( numi**2.0_MK) + pj/(numj**2.0_MK)) * gradw
        
        fi(1:num_dim) = fi(1:num_dim) - f_c * eij(1:num_dim) / mi
        fj(1:num_dim) = fj(1:num_dim) + f_c * eij(1:num_dim) / mj
        
        !----------------------------------------------------
        ! Calculate potential energy, if needed.
        !----------------------------------------------------
      
        IF( PRESENT(auij)) THEN
           auij = 0.5_MK * f_c * ev / mi
        END IF
        
        !----------------------------------------------------
        ! Calculate the first part of viscous force
        ! per unit mass(from viscosity).
        !----------------------------------------------------
        
	!******* Changed by adolfo ****
!        f_d1 =  (2.0_MK - 1.0_MK/num_dim) * eta * &
!             gradw / numi / numj /  dij
!!$        f_c =  ((2.0_MK - 1.0_MK/num_dim) * eta - ksai) * &
!!$             gradw / numi / numj /  dij
        f_c =  a * gradw / numi / numj /  dij

        
        fi(1:num_dim) = fi(1:num_dim) + f_c  * vij(1:num_dim) / mi
        fj(1:num_dim) = fj(1:num_dim) - f_c  * vij(1:num_dim) / mj
        
    	!----------------------------------------------------
  	! Calculate the second part of viscous force
	! per unit mass
  	!----------------------------------------------------
        
	!********* Changed by adolfo ****
!        f_d2 = eta * (num_dim + 2) / num_dim * &
!             gradw * ev / numi / numj / dij
!!$        f_c = (eta * (num_dim + 2) / num_dim + 5.0_MK * ksai) * &
!!$             gradw * ev / numi / numj / dij
        f_c = b* gradw * ev / numi / numj / dij
        
        fi(1:num_dim) = fi(1:num_dim) + f_c * eij(1:num_dim) / mi 
        fj(1:num_dim) = fj(1:num_dim) - f_c * eij(1:num_dim) / mj
        
        !----------------------------------------------------
        ! Calculate random force
	! per unit mass (from thermal noise),
        ! if kt is above zero.
  	!----------------------------------------------------

        !***** Added by Adolfo ********
        !--- If it is not fluid-wall or wall-fluid ---
!!$        IF (.NOT.( ((type_i == 0) .AND. (type_j < 0)) .OR.  &
!!$             ((type_j == 0) .AND. (type_i < 0)) ) ) THEN
           !**********************************
           
           IF ( this%Brownian .AND. &
                kt >  mcf_machine_zero ) THEN

              !-------------------------------------------------
              ! Generate the 2*2(2D) or 3*3(3D) matrix
              ! dW with 4 or 9 random numbers.
              ! Calculate the trace.
              !-------------------------------------------------

              trace = 0.0_MK

              DO i=1, num_dim
                 DO j = 1, num_dim
                    dW(i,j) = random_random(this%random,stat_info_sub)
                 END DO
                 trace = trace + dw(i,i)
              END DO

              trace = trace / num_dim

              !-------------------------------------------------
              ! Make the matrix dW symmetric and traceless.
              !-------------------------------------------------

              DO i =1,num_dim
                 DO j = i+1,num_dim
                    dW(i,j) = (dW(i,j) + dW(j,i)) / 2.0_MK
                    dW(j,i) = dW(i,j)
                 END DO
                 dW(i,i) = dW(i,i) - trace
              END DO

              Zij = 4.0_MK*kt*gradW/dij/numi/numj
              !** Lo siguiente ya no hace falta **
!!$	   !******** Changed by Adolfo *******
!!$!           a   = 5.0_MK *eta/3.0_MK
!!$!           b   = (DFLOAT(num_dim) + 2.0_MK)*eta/ 3.0_MK
!!$           a   = 5.0_MK *eta/3.0_MK - ksai
!!$           b   = (DFLOAT(num_dim) + 2.0_MK)*eta/ 3.0_MK + 5.0_MK * ksai
!!$	   !**********************************
              Aij = SQRT(-Zij * a)
              !-- Added by Adolfo to avoid NaN's in Aij --
              IF (-Zij * a < 0) THEN !-- it should be a very small value --
                 Aij = 0.0_MK
              ENDIF
              !------------------------------------------
              Bij = SQRT(-Zij*DFLOAT(num_dim)/2.0_MK * &
                   (b+a*(2.0_MK/DFLOAT(num_dim) -1.0_MK)))

              !-------------------------------------------------
              ! Generate the vector by
              ! dot product of dW and eij.  
              !-------------------------------------------------

              DO i = 1, num_dim
                 We(i) = 0.0_MK
                 DO j = 1, num_dim
                    We(i) = We(i) + dW(i,j) * eij(j)
                 END DO
              END DO

              f_r(1:num_dim) = (Aij * We(1:num_dim) + Bij*trace*eij(1:num_dim)) / &
                   SQRT(dt)
              fi(1:num_dim) = fi(1:num_dim) + f_r(1:num_dim) / mi
              fj(1:num_dim) = fj(1:num_dim) - f_r(1:num_dim) / mj 

           END IF ! Brownian .AND. kT > 0
           !****** Added by Adolfo ******
!!$        END IF
        !*******************************
        
9999    CONTINUE
        
        RETURN
        
      END SUBROUTINE rhs_force_ff_Newtonian_Espanol
    
