	!------- File Added by Adolfo ---------
  !----------------------------------------------------------
  ! particle_particle_interaction_fluid_wall
  !
  ! ip is fluid particle, 
  ! jp is wall boundary particle. 
  !
  ! In case of solid wall boundary,
  ! substitute jp's velocity with an artificial 
  ! velocity for no slip condition,
  ! pressure of jp with Neumann condition.
  !
  ! If non-Newtonian fluids,
  ! calculate velocity gradient tensor.
  ! pressure tensor of jp with Neumann condition.
  !
  !
  ! f_w interaction(force) takes place the same way
  ! as f_f interaction.
  !----------------------------------------------------------
  

  v_ip(1:num_dim) = this%v(1:num_dim,ip)
  v_jp(1:num_dim) = this%v(1:num_dim,jp)

  IF ( num_wall_solid > 0 ) THEN
     
     !-------------------------------------------------------
     ! wall boundary using solid particles with
     ! Morris no slip boundary condition.
     !-------------------------------------------------------
     
     IF ( wall_noslip == 2 ) THEN
        
        CALL boundary_noslip(tboundary, &
             this%x(1:num_dim,ip),this%x(1:num_dim,jp), &
             this%v(1:num_dim,ip),v_jp(1:num_dim), &
             this%id(this%sid_idx,jp), &
             stat_info_sub)
        
        IF( stat_info_sub /=0 ) THEN
           PRINT *, "pp_interaction_fw : ", &
                "Wall no slip of jp has problem !"
           stat_info = -1
           GOTO 9999
        END IF
        
     END IF ! wall_noslip
     
  END IF ! num_wall_solid > 0

  
  
     
     rho_ip = this%rho(ip)
     rho_jp = this%rho(ip)
     
#include "pp_vgt_ip.inc"
     
     IF ( symmetry ) THEN
        
#include "pp_vgt_jp.inc"
        
     END IF

