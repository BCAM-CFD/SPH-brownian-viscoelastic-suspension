	!----- Added by Adolfo -----
  !----------------------------------------------------------
  ! particle-particle_interaction_colloid_colloid
  !
  ! ip is colloid boundary particle, 
  ! jp is colloid boundary particle. 
  !
  ! Note that ip and jp are not from the same colloid.
  !
  ! ip and jp's pressure are from equation of state.
  !
  ! If Non-Newtonian fluids, 
  ! not implemented.
  !
  !
  ! c_c interaction takes place the same way
  ! as f_f interaction.
  !----------------------------------------------------------

  
  v_ip(1:num_dim) = this%v(1:num_dim,ip)
  v_jp(1:num_dim) = this%v(1:num_dim,jp)
  
  
     rho_ip = this%rho(ip)
     rho_jp = this%rho(jp)
     
#include "pp_vgt_ip.inc"
     
     IF ( symmetry ) THEN
        
#include "pp_vgt_jp.inc"
        
     END IF
