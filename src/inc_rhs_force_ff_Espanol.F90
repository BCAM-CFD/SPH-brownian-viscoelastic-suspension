!!!$        !----------------------------------------------------
!!!$        ! Check if any denominator is non-positive.
!!!$        !----------------------------------------------------
!!!$
!!!$        IF ( dij < mcf_machine_zero .OR. &
!!!$             mi < mcf_machine_zero .OR. &
!!!$             mj < mcf_machine_zero .OR. &
!!!$             numi < mcf_machine_zero .OR. &
!!!$             numj < mcf_machine_zero) THEN
!!!$           
!!!$           PRINT *,"xi,xj,dij : ",  xi,xj,dij
!!!$           PRINT *,"vi,vj : ", vi,vj
!!!$           PRINT *,"numi,numj : ", numi,numj
!!!$           PRINT *,"pi,pj : ", pi,pj
!!!$           PRINT *,"mi,mj : ", mi,mj
!!!$           PRINT *,"w, gradw : ", w, gradw
!!!$           
!!!$           PRINT *, "rhs_force_ff_Newtonian_Espanol : ",& 
!!!$                "Divided by non-positive !" 
!!!$           stat_info = -1
!!!$           GOTO 9999
!!!$           
!!!$        END IF

        
        !----------------------------------------------------
        ! Calculate normalized vector pointing from j to i.
        !----------------------------------------------------
        eij(1:num_dim) = (this%x(1:num_dim,ip)  - this%x(1:num_dim,jp)) / dij
        
        !----------------------------------------------------
        ! Calculate velocity vector pointing from j to i.
        !----------------------------------------------------
        
        vij(1:num_dim)  = this%v(1:num_dim, ip) - this%v(1:num_dim,jp)
        
        !----------------------------------------------------
        ! Dot product of normalized vector e and velocity v.
        !----------------------------------------------------

        ev = 0.0_MK
        DO kk = 1, num_dim
           ev = ev + eij(kk) * vij(kk)  
        END DO
        
  	!----------------------------------------------------
	! Calculate the conservative force
	! per unit mass(from pressure).
  	!----------------------------------------------------
        
!        f_c = ( this%p(ip)/( this%rho(ip)**2.0_MK) + this%p(jp)/(this%rho(jp)**2.0_MK)) * gradw
        faux(1:num_dim) = ( this%p(ip)/this%rho(ip)**2.0_MK + this%p(jp)/this%rho(jp)**2.0_MK) * &
             gradw * eij(1:num_dim) / this%m(ip)
        
!!$        fip(1:num_dim) = fip(1:num_dim) - f_c * eij(1:num_dim) / this%m(ip)
!!$        fjp(1:num_dim) = fjp(1:num_dim) + f_c * eij(1:num_dim) / this%m(jp)

        fip(1:num_dim) = fip(1:num_dim) - faux(1:num_dim) 
!        fjp(1:num_dim) = fjp(1:num_dim) + faux(1:num_dim) 
         
        
        !----------------------------------------------------
        ! Calculate the viscous force
        ! per unit mass(from viscosity).
        !----------------------------------------------------
        
	!******* Changed by adolfo ****
!        f_d1 =  (2.0_MK - 1.0_MK/num_dim) * eta * &
!             gradw / numi / numj /  dij
!!!$        f_c =  ((2.0_MK - 1.0_MK/num_dim) * eta - ksai) * &
!!!$             gradw / numi / numj /  dij
!        f_c =  visc_a * gradw / (this%rho(ip) * this%rho(jp) * dij)
        faux(1:num_dim) = (visc_a * vij(1:num_dim) + visc_b * ev * eij(1:num_dim)) * &
             gradw / (this%rho(ip) * this%rho(jp) * dij * this%m(ip))
        

        
!        fip(1:num_dim) = fip(1:num_dim) + f_c  * vij(1:num_dim) / this%m(ip)
!        fjp(1:num_dim) = fjp(1:num_dim) - f_c  * vij(1:num_dim) / this%m(jp)

        fip(1:num_dim) = fip(1:num_dim) + faux(1:num_dim)
!        fjp(1:num_dim) = fjp(1:num_dim) - faux(1:num_dim)        
                
        
        !----------------------------------------------------
        ! Calculate random force
	! per unit mass (from thermal noise),
        ! if kt is above zero.
  	!----------------------------------------------------
        
!!!$        IF ( Brownian .AND. &
!!!$             kt >  mcf_machine_zero ) THEN
!!!$           
!!!$           !-------------------------------------------------
!!!$           ! Generate the 2*2(2D) or 3*3(3D) matrix
!!!$           ! dW with 4 or 9 random numbers.
!!!$           ! Calculate the trace.
!!!$           !-------------------------------------------------
!!!$           
!!!$           trace = 0.0_MK
!!!$           
!!!$           DO i=1, num_dim
!!!$              DO j = 1, num_dim
!!!$                 dW(i,j) = random_random(this%random,stat_info_sub)
!!!$              END DO
!!!$              trace = trace + dw(i,i)
!!!$           END DO
!!!$           
!!!$           trace = trace / num_dim
!!!$           
!!!$           !-------------------------------------------------
!!!$           ! Make the matrix dW symmetric and traceless.
!!!$           !-------------------------------------------------
!!!$           
!!!$           DO i =1,num_dim
!!!$              DO j = i+1,num_dim
!!!$                 dW(i,j) = (dW(i,j) + dW(j,i)) / 2.0_MK
!!!$                 dW(j,i) = dW(i,j)
!!!$              END DO
!!!$              dW(i,i) = dW(i,i) - trace
!!!$           END DO
!!!$           
!!!$           Zij = 4.0_MK*kt*gradW/dij/numi/numj
!	   !******** Changed by Adolfo *******
!           a   = 5.0_MK *eta/3.0_MK
!           b   = (DFLOAT(num_dim) + 2.0_MK)*eta/ 3.0_MK
!           a   = 5.0_MK *eta/3.0_MK - ksai
!           b   = (DFLOAT(num_dim) + 2.0_MK)*eta/ 3.0_MK + 5.0_MK * ksai
!	   !**********************************
!!!$           Aij = SQRT(-Zij * a)
!!!$           Bij = SQRT(-Zij*DFLOAT(num_dim)/2.0_MK * &
!!!$                (b+a*(2.0_MK/DFLOAT(num_dim) -1.0_MK)))
!!!$           
!!!$           !-------------------------------------------------
!!!$           ! Generate the vector by
!!!$           ! dot product of dW and eij.  
!!!$           !-------------------------------------------------
!!!$           
!!!$           DO i = 1, num_dim
!!!$              We(i) = 0.0_MK
!!!$              DO j = 1, num_dim
!!!$                 We(i) = We(i) + dW(i,j) * eij(j)
!!!$              END DO
!!!$           END DO
!!!$           
!!!$           f_r(1:num_dim) = (Aij * We(1:num_dim) + Bij*trace*eij(1:num_dim)) / &
!!!$                SQRT(dt)
!!!$           fi(1:num_dim) = fi(1:num_dim) + f_r(1:num_dim) / mi
!!!$           fj(1:num_dim) = fj(1:num_dim) - f_r(1:num_dim) / mj 
!!!$
!!!$        END IF ! Brownian .AND. kT > 0
