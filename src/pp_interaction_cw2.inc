	!-- Added by Adolfo ---
  !------------------------------------------------
  ! particle-particle_interaction_colloid_wall
  !
  ! ip is colloidal boundary particle, 
  ! jp is wall boundary particle. 
  !
  !
  ! ip and jp's pressure are from equation of state.
  !
  ! If fluid is non-Newtonian,
  ! not implemented.
  !
  !
  ! c_w interaction takes place the same way
  ! as f_f interaction.
  !------------------------------------------------

  
  v_ip(1:num_dim) = this%v(1:num_dim, ip)
  v_jp(1:num_dim) = this%v(1:num_dim, jp)
  
  
     rho_ip = this%rho(ip)
     rho_jp = this%rho(jp)
     
#include "pp_vgt_ip.inc"
     
     IF ( symmetry ) THEN
        
#include "pp_vgt_jp.inc"
        
     END IF
