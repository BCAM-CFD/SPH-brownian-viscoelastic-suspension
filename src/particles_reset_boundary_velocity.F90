      SUBROUTINE particles_reset_boundary_velocity(this,stat_info)
        !----------------------------------------------------
        ! Subroutine  : particles_reset_boundary_velocity
        !----------------------------------------------------
        !
        ! Purpose     : Assign solid wall particles with
        !               walls' velocity.
        !
        ! Refernece   :
        !
        ! Remark      : 
        !
        !               A For periodic boundary particles,
        !               we do nothing.
        !
        !               B Symmetry, Wall using symmetry or
        !               Lees-Edwards boundary.
        !
        !               For symmetry boundary particles, 
        !               wall using symmetry/mirror boundary
        !               particles, or Lees-Edwards boundary 
        !               paritcles, they are all created by
        !               PPM. They are outside of physical 
        !               domain and all of them are treated
        !               completely as ghost particles, we 
        !               don't need to integrate their 
        !               position at all. Therefore,
        !               We do nothing for their velocities.
        !
        !               C  Wall boundary using solid 
        !               particles.
        !
        !               Since solid wall are modeled using 
        !               solid particles inside the total 
        !               physical domain, i.e., there is no
        !               relative motion among solid wall 
        !               boundary particles if they are
        !               inside the same wall and PPM treat
        !               the solid wall particles as inside
        !               physical domain.
        !
        !               Moreover, the solid wall boundary 
        !               particles are created by client
        !               MCF, thus we have to integrate 
        !               their positions every step.
        !
        !               However, during the force calculation,
        !               the solid wall boundary particles 
        !               could have been given artificial 
        !               velocities (if Morris et al. 1997 
        !               noslip is used) to ensure noslip on 
        !               the wall, therefore here we have to 
        !               assign them with the real velocities
        !               of walls again.
        !
        !
        !
        ! Revision    : V0.2 02.12 2009, check and supply
        !               with more comments.
        !
        !               V0.1 02.10 2009, original version.
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
        ! this       : an object of Particles Class.
        ! stat_info  : return flag of status.
        !----------------------------------------------------
        
        TYPE(Particles), INTENT(INOUT)          :: this
        INTEGER, INTENT(OUT)                    :: stat_info
        
        !----------------------------------------------------
        ! Local variables
	!----------------------------------------------------
        
        INTEGER                                 :: dim
        INTEGER                                 :: i,ip
        
        !----------------------------------------------------
        ! Initialization of variables.
        !----------------------------------------------------
        
        stat_info = 0
        dim       = this%num_dim
        
        !----------------------------------------------------
        ! Assign particles which constitute solid walls 
        ! with solid velocity zero.
        !----------------------------------------------------
        
        DO i =1, this%num_part_wall_solid_real
           
           ip  = this%part_wall_solid_real_list(1,i)
           
           this%v(1:dim,ip) = 0.0_MK
           
        END DO
        
        
9999    CONTINUE
        
        
        RETURN
        
      END SUBROUTINE particles_reset_boundary_velocity
      
      
      
