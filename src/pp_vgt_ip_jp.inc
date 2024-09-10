  !----------------------------------------------------------
  ! Particle-particle_velocity_gradient_tensor_fluid_fluid_ip
  !
  ! Velocity gradient tensor calculation of particle ip.
  !
  ! Here we handle currently only number density 
  ! as in Espanol&Revenga Phys. Rev. E 2003 or 
  !       Hu&Adams J. Comput.Phys. 2006 or
  !       Hu&Adams Phys.Fluids 2006  formulations.
  !----------------------------------------------------------

  DO b = 1, num_dim  ! ---, row direction     
     DO a =1, num_dim ! |,  column direction     
	vgt_aux(a,b) = -vij(a) * eij(b) * gradw 
        this%vgt(a+num_dim*(b-1),ip) = &
             this%vgt(a+num_dim*(b-1),ip) + vgt_aux(a,b) / rho_ip
        this%vgt(a+num_dim*(b-1),jp) = &
             this%vgt(a+num_dim*(b-1),jp) + vgt_aux(a,b) / rho_jp

     END DO    
  END DO


  