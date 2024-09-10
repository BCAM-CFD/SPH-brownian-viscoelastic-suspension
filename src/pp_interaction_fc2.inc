	!-------- Added by Adolfo ------
  !----------------------------------------------------------
  ! particle_particle_interaction_fluid_colloid
  !
  ! ip is fluid particle, 
  ! jp is colloid boundary particle. 
  !
  ! Substitute jp's velocity with an artificial
  ! velocity for no slip condition,
  ! pressure of jp with Neumann condition.
  !
  ! If Non-Newtonian fluids, 
  ! calculate velocity gradient tensor.
  ! pressure tensor of jp with Neumann condition.
  !
  !
  ! f_c interaction takes place the same way
  ! as f_f interaction.
  !----------------------------------------------------------
  
  
  v_ip(1:num_dim) = this%v(1:num_dim,ip)
  v_jp(1:num_dim) = this%v(1:num_dim,jp)

  !----------------------------------------------------------
  ! Morris no slip conditin for colloid.
  !----------------------------------------------------------
  
  IF ( coll_noslip == 2 ) THEN
     
     CALL colloid_noslip(colloids, &
          this%x(1:num_dim,ip),this%x(1:num_dim,jp), &
          this%v(1:num_dim,ip),v_jp(1:num_dim), &
          this%id(this%sid_idx,jp),&
          stat_info_sub)
     
     IF( stat_info_sub /=0 ) THEN
        PRINT *, "pp_interaction_fc : ",&
             "Colloid no slip jp:", jp," has problem !"
        stat_info = -1
        GOTO 9999
     END IF
     
  END IF  ! coll_noslip
  
  
     
     rho_ip = this%rho(ip)
     rho_jp = this%rho(ip)
     
#include "pp_vgt_ip.inc"
     
     IF ( symmetry ) THEN
        
#include "pp_vgt_jp.inc"
        
     END IF
     
