!!$        !----------------------------------------------------
!!$        ! Check if any denominator is non-positive.
!!$        !----------------------------------------------------
!!$        
!!$        IF ( dij < mcf_machine_zero .OR. &
!!$             mi < mcf_machine_zero .OR. &
!!$             mj < mcf_machine_zero .OR. &
!!$             numi < mcf_machine_zero .OR. &
!!$             numj < mcf_machine_zero ) THEN
!!$           
!!$           PRINT *,"xi,xj,dij : ",  xi,xj,dij
!!$           PRINT *,"vi,vj : ", vi,vj
!!$           PRINT *,"numi,numj : ", numi,numj
!!$           PRINT *,"pi,pj : ", pi,pj
!!$           PRINT *,"mi,mj : ", mi,mj
!!$           PRINT *,"w, gradw : ", w, gradw          
!!$           
!!$           stat_info = -1
!!$           GOTO 9999
!!$        END IF


        !********** Added by Adolfo *************
        !----------------------------------------------------
        ! Dot product of normalized vector e and velocity v.
        !----------------------------------------------------
        ev = 0.0_MK
        DO kk = 1, num_dim
           ev = ev + eij(kk) * vij(kk)  
        END DO
        !*****************************************

        
        !----------------------------------------------------
	! Calculate the conservative force
	! per unit mass(from pressure).
  	!----------------------------------------------------
        
        f_c_vec(1:num_dim) = 0.0_MK

        DO kk = 1, num_dim
           DO ll = 1, num_dim
              f_c_vec(kk) = f_c_vec(kk) + &
                   this%pt(ll,kk,ip) * eij(ll) / (rho_ip*rho_ip) +  &
                   this%pt(ll,kk,jp) * eij(ll) /( rho_jp*rho_jp)              
           END DO
        END DO
        

        fip(1:num_dim) = fip(1:num_dim) - &
             f_c_vec(1:num_dim) * gradw / this%m(ip)
!!$        fj(1:num_dim) = fj(1:num_dim) + &
!!$             f_c(1:num_dim) * gradw / mj
        
        !----------------------------------------------------
        ! Calculate the dissipative force
	! per unit mass(from viscosity).
  	!----------------------------------------------------
        
!!$        !----------------- Commented by Adolfo ---------------
!!$        f_d =  eta * (1.0_MK/numi**2.0_MK + &
!!$             1.0_MK/numj**2.0_MK) * gradw / dij
!!$        
!!$        fi(1:num_dim) = fi(1:num_dim) + &
!!$             f_d  * vij(1:num_dim) / mi
!!$        fj(1:num_dim) = fj(1:num_dim) - &
!!$             f_d  * vij(1:num_dim) / mj
!!$        !------------------------------------------------------
	!******* Added by adolfo ****
!        f_d =  a * gradw / numi / numj /  dij
!        f_d =  visc_a * gradw / (rho_ip * rho_jp * dij)
         faux(1:num_dim) =  (visc_a * vij(1:num_dim) + visc_b * ev * eij(1:num_dim)) * gradw / (rho_ip * rho_jp * dij * this%m(ip))        

!        fip(1:num_dim) = fip(1:num_dim) + f_d  * vij(1:num_dim) / this%m(ip)
         !        fj(1:num_dim) = fj(1:num_dim) - f_d  * vij(1:num_dim) / mj

         fip(1:num_dim) = fip(1:num_dim) + faux(1:num_dim)

!        f_d = b* gradw * ev / numi / numj / dij
!        f_d = visc_b* gradw * ev / (rho_ip * rho_jp * dij)
        
!        fip(1:num_dim) = fip(1:num_dim) + f_d * eij(1:num_dim) / this%m(ip) 
!        fj(1:num_dim) = fj(1:num_dim) - f_d * eij(1:num_dim) / mj
        !*****************************

!!$        !----------------------------------------------------
!!$        ! Calculate random force
!!$	! per unit mass(from thermal noise),
!!$        ! if kt is above zero.
!!$  	!----------------------------------------------------
!!$        
!!$        IF ( this%Brownian .AND. &
!!$             kt >  mcf_machine_zero ) THEN
!!$           
!!$           !-------------------------------------------------
!!$           ! Generate the 2*2(2D) or 
!!$           ! 3*3(3D) matrix
!!$           ! dW with 4 or 9 random numbers.
!!$           ! Calculate the trace.
!!$           !-------------------------------------------------
!!$           
!!$           trace = 0.0_MK
!!$           
!!$           DO i=1, num_dim
!!$              DO j = 1, num_dim
!!$                 dW(i,j) = random_random(this%random,stat_info_sub)
!!$              END DO
!!$              trace = trace + dw(i,i)
!!$           END DO
!!$           
!!$           trace = trace / num_dim
!!$           
!!$           !-------------------------------------------------
!!$           ! Make the matrix dW symmetric 
!!$           ! and traceless.
!!$           !-------------------------------------------------
!!$           
!!$           DO i =1,num_dim
!!$              DO j = i+1,num_dim
!!$                 dW(i,j) = (dW(i,j) + dW(j,i)) / 2.0_MK
!!$                 dW(j,i) = dW(i,j)
!!$              END DO
!!$              dW(i,i) = dW(i,i) - trace
!!$           END DO
!!$
!!$           Zij = 4.0_MK*kt*gradW/dij/numi/numj
!!$!           a   = 5.0_MK *eta/3.0_MK
!!$!           b   = (DFLOAT(num_dim) + 2.0_MK)*eta/ 3.0_MK
!!$           Aij = SQRT(-Zij * a)
!!$           Bij = SQRT(-Zij*DFLOAT(num_dim)/2.0_MK * &
!!$                (b+a*(2.0_MK/DFLOAT(num_dim) -1.0_MK)))
!!$           
!!$           !-------------------------------------------------
!!$           ! Generate the vector by
!!$           ! dot product of dW and eij.  
!!$           !-------------------------------------------------
!!$           
!!$           DO i = 1, num_dim
!!$              We(i) = 0.0_MK
!!$              DO j = 1, num_dim
!!$                 We(i) = We(i) + dW(i,j) * eij(j)
!!$              END DO
!!$           END DO
!!$
!!$           f_r(1:num_dim) = (Aij * We(1:num_dim) + Bij*trace*eij(1:num_dim)) / &
!!$                SQRT(dt)
!!$           fi(1:num_dim) = fi(1:num_dim) + f_r(1:num_dim) / mi
!!$           fj(1:num_dim) = fj(1:num_dim) - f_r(1:num_dim) / mj 
!!$           
!!$        END IF ! Brownian and kt > 0
