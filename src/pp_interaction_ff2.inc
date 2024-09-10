	!---- File included by Adolfo -----
  !------------------------------------------------
  ! particle_particle_interaction_fluid_fluid
  !
  ! Remark:
  ! 
  ! In case of non-Newtonian fluid,
  ! we calculate velocity gradient tensor;
  ! For force calculation, we use pressure tensor.
  !------------------------------------------------
  
 
  v_ip(1:num_dim) = this%v(1:num_dim,ip)
  v_jp(1:num_dim) = this%v(1:num_dim,jp)
  
  rho_ip = this%rho(ip)
  rho_jp = this%rho(jp)
     
#include "pp_vgt_ip.inc"
     
     IF ( symmetry ) THEN
        
#include "pp_vgt_jp.inc"
        
     END IF


