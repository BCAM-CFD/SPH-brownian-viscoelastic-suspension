      SUBROUTINE particles_compute_interaction(this,stat_info)
        !----------------------------------------------------
        ! Subroutine  : particles_compute_interaction
        !----------------------------------------------------
        !
        ! Purpose     : Computing interaction for 2D or 3D, 
        !               using symmetric or non-symmetric 
        !               inter-process communication.
        !
        !               Interaction includes :
        !               force;
        !               velocity gradient tensor in case of
        !               non-Newtonian Oldroyd-B model.
        !                  
        !
        ! Routines    : pp_interaction.inc
        !
        !
        ! References  : Sbalzarini et al.
        !               2006 Journal of Computational Physics.
        !
        ! Remarks     :  
        !
        ! Revisions   : V0.7 22.04.2010, changes in the 
        !               density of the wall particles in 
        !               the case of non newtonian
        !               fluids. For every pairwise f-w, the 
        !               density of the wall particle is 
        !               the same than the density of the
        !               fluid particle. I think this should
        !               be implemented also for the particles
        !               of the colloids, and also for the 
        !               newtonian case. (Adolfo Vazquez-Quesada)
        !
        !               V0.6 03.12 2009, move the allocation
        !               and recording boundary particles
        !               to particles_set_boundary_IDt() and
        !               particles_compute_density().
        !
        !               V0.5 09.10 2009, merge 2D,3D, 
        !               symmetry, non-symmetry together. 
        !               
        !               V0.4 29.09 2009, include wall boundary
        !               particle interaction.
        !                 
        !               V0.3 28.07 2009, merge vgt calculation
        !               with force in this loop.
        !
        !               V0.2 28.09 2009, merge force and vgt
        !               calculation together. 
        !               In general, all particle interactions
        !               should be done here.
        !
        !               V0.1 16.03 2009, original
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
      	! Modules :
      	!----------------------------------------------------
        
        USE ppm_module_neighlist
        
	!----------------------------------------------------
      	! Arguments :
      	!----------------------------------------------------
        
        TYPE(Particles), INTENT(INOUT)  :: this
        INTEGER, INTENT(OUT)		:: stat_info
        
      	!----------------------------------------------------
      	! Local variables start here :
        !----------------------------------------------------
        
        INTEGER                         :: stat_info_sub

        !----------------------------------------------------
        ! Control parameters :
        !
        ! symmetry      : indicate if we use symmetric
        !                 inter-process communication or not.
        ! Newtonian     : if fluid is Newtonian.
        ! Browonian     : if fluid is Brownian.        
        ! p_energy      : if potential energy is needed.  
        !----------------------------------------------------
        
        INTEGER                         :: rhs_density_type
        LOGICAL                         :: symmetry
        LOGICAL                         :: Newtonian
        LOGICAL                         :: Brownian
        LOGICAL                         :: stress_tensor
        LOGICAL                         :: p_energy
        !*** Added by Adolfo ****
        LOGICAL :: bounce_back
        !************************
        
        !----------------------------------------------------
        ! Physics parameters :
        !
        ! num_species    : number of species.
        ! num_dim        : number of dimension.
        ! cut_off        : compact support domain.
        ! cut_off2       : cut_off * cut_off.
        !
        ! bcdef          : boundary condition definition.
        ! boundary       : boundary object pointer.
        ! num_wall_sym   : number of wall boundaries,
        !                  created by PPM using symmetry.
        ! num_wall_solid : number of wall boundaries,
        !                  created by MCF using solid.
        !
        ! num_wall       : number of wall boundaries,
        !                  in general, either symmetry
        !                  or solid.
        ! num_shear      : number of shear boundaries.
        !
        ! num_colloid    : number of colloidal particle.
        ! colloids       : Colloids object pointer.
        !----------------------------------------------------
        
        INTEGER                         :: num_species
        INTEGER                         :: num_dim, num_dim2
        REAL(MK)                        :: cut_off
        REAL(MK)                        :: cut_off2
        REAL(MK), DIMENSION(:),POINTER  :: dx
        REAL(MK)                        :: eta
        !----------------------------------------------------
        ! Boundary parameters :
        !
        ! num_wall_solid : number of solid wall boundaries.
        !----------------------------------------------------
       
        INTEGER, DIMENSION(:), POINTER  :: bcdef
        TYPE(Boundary), POINTER         :: tboundary
        INTEGER                         :: wall_noslip
        INTEGER                         :: num_sym
        INTEGER                         :: num_wall_sym
        INTEGER                         :: num_wall_solid
        INTEGER                         :: num_le
        INTEGER                         :: num_shear
        INTEGER                         :: num_colloid
        !***** Added by Adolfo *******
        REAL(MK)                        :: bounce_back_incr
        !*****************************
        TYPE(Colloid), POINTER          :: colloids
        INTEGER                         :: coll_noslip
        
        !----------------------------------------------------
        ! Counters / Indices
        !
        ! num_sub          : number of sumdomains on 
        !                    current process
        ! num_cell_dim_sub : number of cells in each direction 
        !                    of each subdomain.
        ! cell list        : cell list
        ! inp              : relative position to center cell 
        !                    of inteacting cell
        ! jnp              : relative position to center cell 
        !                    of inteacted cell
        ! nnp              : number of interaction between
        !                    cell and cell
        ! iinter           : index of interaction between
        !                    cell and cell
        !----------------------------------------------------
        
        INTEGER                                 :: num_sub
        INTEGER, DIMENSION(:,:), POINTER        :: num_cell_dim_sub
        TYPE(ppm_type_ptr_to_clist), DIMENSION(:), POINTER :: cell_list
        INTEGER, DIMENSION(:,:), POINTER        :: sub_bcdef
        INTEGER, DIMENSION(:,:), POINTER        :: inp
        INTEGER, DIMENSION(:,:), POINTER        :: jnp
        INTEGER                                 :: nnp
        INTEGER                                 :: iinter
        
        !----------------------------------------------------
        ! Number of cells in 1 and 2 dimension.
        !----------------------------------------------------
        
        INTEGER                         :: n1
        INTEGER                         :: n2
        
        !----------------------------------------------------
      	! Indices about subdomains and cells
        !
        ! idom  : index of subdomains
        ! i*    : cell index in first dimension
        ! j*    : cell index in secondd dimension
        ! k*    : cell index in third dimension
        ! ccell : center cell
        ! icell : interacting cell
        ! jcell : interacted cell
        !----------------------------------------------------
        
        INTEGER                         :: idom
        INTEGER                         :: icstart, icend, i
        INTEGER                         :: jcstart, jcend, j
        INTEGER                         :: kcstart, kcend, k
        INTEGER				:: ccell,icell,jcell
        
        !----------------------------------------------------
        ! Indices about particles in cells
        !
        ! i*   : indices  particles in icell.
        ! j*   : indices  particles in jcell.
        !----------------------------------------------------
        
        INTEGER				:: istart,iend, ipart
        INTEGER                         :: jstart,jend, jpart
        
        !----------------------------------------------------
        ! Indices about particles in data structure,
        ! i.e. in the sense of array, e.g. this%x(ip)
        !----------------------------------------------------
        
        INTEGER				:: ip,jp
        
        !----------------------------------------------------
      	! Inter-particle distance.
        !----------------------------------------------------
        
        REAL(MK), DIMENSION(3)          :: rij
        REAL(MK)                        :: dij
        
        !----------------------------------------------------
        ! kernel parameters.
        !----------------------------------------------------
        
        REAL(MK)                        :: w 
        REAL(MK)                        :: gradW
        
        
        !----------------------------------------------------
        ! For Non-Newtonian viscoelastic
        ! Oldroyd-B model
        !
        ! a      : index
        ! b      : index
        !
        !----------------------------------------------------
        
        INTEGER                         :: a, b
        
        !----------------------------------------------------
        ! fip, fjp : pair-wise force
        !----------------------------------------------------
        
        REAL(MK), DIMENSION(3)          :: fip
        REAL(MK), DIMENSION(3)          :: fjp
#ifdef __PARTICLES_FORCE_SEPARATE
        REAL(MK), DIMENSION(3)          :: fpip
        REAL(MK), DIMENSION(3)          :: fpjp
        REAL(MK), DIMENSION(3)          :: fvip
        REAL(MK), DIMENSION(3)          :: fvjp
        REAL(MK), DIMENSION(3)          :: frip
        REAL(MK), DIMENSION(3)          :: frjp        
#endif

        !----------------------------------------------------
        ! sip, sjp : pair-wise stress tensor
        ! sa, sb   : indices for calculating stress tensor
        !----------------------------------------------------
        REAL(MK), DIMENSION(9)          :: sip
        REAL(MK), DIMENSION(9)          :: sjp
#ifdef __PARTICLES_STRESS_SEPARATE
        REAL(MK), DIMENSION(9)          :: spip
        REAL(MK), DIMENSION(9)          :: spjp
        REAL(MK), DIMENSION(9)          :: svip
        REAL(MK), DIMENSION(9)          :: svjp
        REAL(MK), DIMENSION(9)          :: srip
        REAL(MK), DIMENSION(9)          :: srjp        
#endif
        INTEGER                         :: sa, sb


        !----------------------------------------------------
        ! First try for colloid-colloid interaction.
        !
        ! v_ip, v_jp : temparory velocity of boundary 
        !              particles, used of no-slip interpolation.
        ! rho_ip,rho_jp: desnity of boundary 
        !              particles, used for two boundary 
        !              particles interaction.      
        ! p_ip, p_jp : pressure of boundary 
        !              particles, used for two boundary 
        !              particles interaction.      
        !----------------------------------------------------
        
        REAL(MK), DIMENSION(3)          :: v_ip
        REAL(MK), DIMENSION(3)          :: v_jp
        REAL(MK)                        :: rho_ip
        REAL(MK)                        :: rho_jp        
!        REAL(MK)                        :: p_ip
!        REAL(MK)                        :: p_jp

  	!----------------------------------------------------
      	! Initialization of variables.
      	!----------------------------------------------------
        
        stat_info     = 0
        stat_info_sub = 0    
        
        NULLIFY(bcdef)
        NULLIFY(tboundary)
        wall_noslip = 1
        
        NULLIFY(colloids)
        coll_noslip = 1
        
        NULLIFY(num_cell_dim_sub)
        NULLIFY(cell_list)
        NULLIFY(sub_bcdef)        
        NULLIFY(inp)
        NULLIFY(jnp)
        
        !----------------------------------------------------
        ! Control parameters :
        !
        ! Get control variables.
        !----------------------------------------------------
        
        rhs_density_type = &
             control_get_rhs_density_type(this%ctrl,stat_info_sub)
        symmetry  = &
             control_get_symmetry(this%ctrl,stat_info_sub)
        Newtonian = &
             control_get_Newtonian(this%ctrl,stat_info_sub)
        Brownian = &
             control_get_Brownian(this%ctrl,stat_info_sub)      
        stress_tensor = &
             control_get_stress_tensor(this%ctrl,stat_info_sub)
        p_energy  = &
             control_get_p_energy(this%ctrl,stat_info_sub)
      
        !----------------------------------------------------
        ! Physics parameters :
        !
        ! from a object of Physics class.
        !
        !----------------------------------------------------
        
        num_species = &
             physics_get_num_species(this%phys,stat_info_sub)
        num_dim     = &
             physics_get_num_dim(this%phys,stat_info_sub)
        num_dim2    = num_dim**2
        cut_off     = &
             physics_get_cut_off(this%phys,stat_info_sub)
        cut_off2 = cut_off * cut_off
        NULLIFY(dx)
        CALL physics_get_dx(this%phys,dx,stat_info_sub)
        eta         = &
             physics_get_eta(this%phys,stat_info_sub)
        
        !----------------------------------------------------
        ! Boundary parameters :
        !
        ! Get boundary conditions.
        !----------------------------------------------------
        
        CALL physics_get_bcdef(this%phys,bcdef,stat_info_sub)
        CALL physics_get_boundary(this%phys,&
             tboundary,stat_info_sub)
        
        wall_noslip    = &
             boundary_get_wall_noslip_type(tboundary,stat_info_sub)
        num_sym        = &
             boundary_get_num_sym(tboundary,stat_info_sub)
        num_wall_sym   = &
             boundary_get_num_wall_sym(tboundary,stat_info_sub)
        num_wall_solid = &
             boundary_get_num_wall_solid(tboundary,stat_info_sub)
        num_le         = &
             boundary_get_num_le(tboundary,stat_info_sub)
        num_shear      = &
             boundary_get_num_shear(tboundary,stat_info_sub)
        
        !----------------------------------------------------
        ! Colloid parameters :
        !----------------------------------------------------
        
        num_colloid = &
             physics_get_num_colloid(this%phys,stat_info_sub)
        
        IF ( num_colloid > 0 ) THEN
           
           CALL physics_get_colloid(this%phys,&
                colloids,stat_info_sub)
           coll_noslip = &
                colloid_get_noslip_type(colloids,stat_info_sub)
           !****** Added by Adolfo *******
           bounce_back_incr = colloid_get_bounce_back(colloids,stat_info_sub)
           !******************************
        END IF
        
        !----------------------------------------------------
        ! Allocate memory for force, stress tensor
        !
        ! for symmetry     : allocate num_part_all
        ! for non-symmetry : allocate num_part_real
        !
        ! For symmetry pair interaction/inter-process
        ! communication, we need to allocate for 
        ! ghosts also.
        !
        ! Note that :
        ! For Wall using PPM symmetry boundaries, wall using
        ! solid boundary particles or Lees-Edwards boundaries, 
        ! we need the forces on ghosts particles,
        ! in order calculate ensemble later.
        !----------------------------------------------------
        
        IF (ASSOCIATED(this%f)) THEN
           DEALLOCATE(this%f,STAT=stat_info_sub)
        END IF
     
        IF(  symmetry .OR. &
             num_wall_sym > 0 .OR. num_le > 0 ) THEN
           
           ALLOCATE(this%f(num_dim,this%num_part_all), &
                STAT=stat_info_sub)
           
        ELSE
           
           ALLOCATE(this%f(num_dim,this%num_part_real), &
                STAT=stat_info_sub)
           
        END IF
        
        this%f(:,:) = 0.0_MK

#ifdef __PARTICLES_FORCE_SEPARATE
        IF (ASSOCIATED(this%fp)) THEN
           DEALLOCATE(this%fp,STAT=stat_info_sub)
        END IF
        IF (ASSOCIATED(this%fv)) THEN
           DEALLOCATE(this%fv,STAT=stat_info_sub)
        END IF
        IF (ASSOCIATED(this%fr)) THEN
           DEALLOCATE(this%fr,STAT=stat_info_sub)
        END IF
        
        IF(  symmetry .OR. &
             num_wall_sym > 0 .OR. num_le > 0 ) THEN
           
           ALLOCATE(this%fp(num_dim,this%num_part_all), &
                STAT=stat_info_sub)
           ALLOCATE(this%fv(num_dim,this%num_part_all), &
                STAT=stat_info_sub)
           IF ( Brownian ) THEN
              ALLOCATE(this%fr(num_dim,this%num_part_all), &
                   STAT=stat_info_sub)
           END IF
           
        ELSE
           
           ALLOCATE(this%fp(num_dim,this%num_part_real), &
                STAT=stat_info_sub)
           ALLOCATE(this%fv(num_dim,this%num_part_real), &
                STAT=stat_info_sub)
           IF ( Brownian ) THEN
              ALLOCATE(this%fr(num_dim,this%num_part_real), &
                   STAT=stat_info_sub)
           END IF
           
        END IF
        
        this%fp(:,:) = 0.0_MK
        this%fv(:,:) = 0.0_MK
        IF ( Brownian ) THEN
           this%fr(:,:) = 0.0_MK
        END IF
#endif 
        
        IF ( stress_tensor ) THEN

           IF (ASSOCIATED(this%s)) THEN
              DEALLOCATE(this%s,STAT=stat_info_sub)
           END IF
           
#ifdef __PARTICLES_STRESS_SEPARATE
           IF (ASSOCIATED(this%sp)) THEN
              DEALLOCATE(this%sp,STAT=stat_info_sub)
           END IF
           IF (ASSOCIATED(this%sv)) THEN
              DEALLOCATE(this%sv,STAT=stat_info_sub)
           END IF
           IF (ASSOCIATED(this%sr)) THEN
              DEALLOCATE(this%sr,STAT=stat_info_sub)
           END IF
#endif
           
           IF(  symmetry .OR. &
                num_wall_sym > 0 .OR. num_le > 0 ) THEN
              
              ALLOCATE(this%s(num_dim2,this%num_part_all), &
                   STAT=stat_info_sub)
              
#ifdef __PARTICLES_STRESS_SEPARATE
              ALLOCATE(this%sp(num_dim2,this%num_part_all), &
                   STAT=stat_info_sub)
              ALLOCATE(this%sv(num_dim2,this%num_part_all), &
                   STAT=stat_info_sub)
              IF ( Brownian ) THEN
                 ALLOCATE(this%sr(num_dim2,this%num_part_all), &
                      STAT=stat_info_sub)
              END IF
#endif
              
           ELSE
              
              ALLOCATE(this%s(num_dim2,this%num_part_real), &
                   STAT=stat_info_sub)
              
#ifdef __PARTICLES_STRESS_SEPARATE              
              ALLOCATE(this%sp(num_dim2,this%num_part_real), &
                   STAT=stat_info_sub)
              ALLOCATE(this%sv(num_dim2,this%num_part_real), &
                   STAT=stat_info_sub)
              IF ( Brownian ) THEN
                 ALLOCATE(this%sr(num_dim2,this%num_part_real), &
                      STAT=stat_info_sub)
              END IF
#endif
              
           END IF
           
           this%s(:,:) = 0.0_MK          
           
#ifdef __PARTICLES_STRESS_SEPARATE
           this%sp(:,:) = 0.0_MK
           this%sv(:,:) = 0.0_MK
           IF ( Brownian ) THEN
              this%sr(:,:) = 0.0_MK
           END IF
#endif
           
        END IF
        
        
        !----------------------------------------------------
        ! Allocate memory for veocity gradient 
        ! tensor in case of non-Newtonian Oldroyd-B
        ! viscoelastic model being used.
        ! 
        ! for symmetry     : allocate num_part_all
        ! for non-symmetry : allocate num_part_real
        !----------------------------------------------------
        
        IF ( .NOT. Newtonian ) THEN
           
           IF(ASSOCIATED(this%vgt)) THEN
              DEALLOCATE(this%vgt)
           END IF
           
           IF(  symmetry ) THEN
              
              ALLOCATE(this%vgt(num_dim**2,this%num_part_all),&
                   STAT=stat_info_sub)
              
           ELSE
              
              ALLOCATE(this%vgt(num_dim**2,this%num_part_real),&
                   STAT=stat_info_sub)
              
           END IF
           
           this%vgt(:,:) = 0.0_MK
           
        END IF
        
        !----------------------------------------------------
        ! Allocate memory for acceleration of
        ! potential energy in case need potential
        ! energy.
        ! 
        ! for symmetry     : allocate num_part_all
        ! for non-symmetry : allocate num_part_real
        !----------------------------------------------------
    
        IF( p_energy ) THEN
        
           IF (ASSOCIATED(this%au)) THEN
              DEALLOCATE(this%au,STAT=stat_info_sub)
           END IF
           
           IF(  symmetry ) THEN
              
              ALLOCATE(this%au(this%num_part_all), &
                   STAT=stat_info_sub)
              
           ELSE
              
              ALLOCATE(this%au(this%num_part_real), &
                   STAT=stat_info_sub)
              
           END IF
           
           this%au(:) = 0.0_MK
           
        END IF
        
        
        !----------------------------------------------------
        ! Get the cell list from a object of technique class.
        !----------------------------------------------------
        
        CALL technique_get_cell_list(this%tech,&
             num_sub,num_cell_dim_sub,&
             cell_list,inp,jnp,nnp,sub_bcdef,stat_info_sub)
        

        !----------------------------------------------------
        ! Loop over all the sub-domains on this process.
        !----------------------------------------------------
        
        DO idom = 1, num_sub
           
           !-------------------------------------------------
           ! For symmetry, cell indices starts from 0
           ! otherwise from 1 for real particles.
           ! Both symmetry and non-symmetry cell indices
           ! end at num_cell_dim_sub(*,idom) - 2
           !
           ! However, for symmetry inter-process communication,
           ! if the subdomain is at lower boundary and
           ! we have symmetry, wall using symmetry,
           ! or Lees-Edwars boundary, cell indices starting
           ! from 0 can be a problem.
           !-------------------------------------------------
           
           IF ( symmetry ) THEN
              
              icstart = 0              
              jcstart = 0
              kcstart = 0
              
           ELSE
              
              icstart = 1
              jcstart = 1
              kcstart = 1 
              
           END IF
           
           icend = num_cell_dim_sub(1,idom)-2
           jcend = num_cell_dim_sub(2,idom)-2       
           
           n1  = num_cell_dim_sub(1,idom)
           
           !-------------------------------------------------
           ! For 2D, k loops has only one iteration which
           !         means 2D; and n2 has 0 cells.
           ! For 3D, k loops similar way as i,j in 1st and
           !         2nd dimension.
           !-------------------------------------------------
           
           IF ( num_dim == 2 ) THEN
              
              kcend = kcstart
              n2    = 0
              
           ELSE              
              
              kcend = num_cell_dim_sub(3,idom)-2
              n2    = num_cell_dim_sub(1,idom) * &
                   num_cell_dim_sub(2,idom)
              
           END IF
           
           !-------------------------------------------------
           ! Loop over all real cells
           !-------------------------------------------------

           DO k = kcstart, kcend
              
              DO j = jcstart, jcend
                 
                 DO i = icstart, icend
                    
                    !----------------------------------------
                    ! Get index of the center cell.
                    !----------------------------------------
                    
                    ccell = i + 1 + n1 * j + n2 * k
                    
                    !----------------------------------------
                    ! Loop all interactions between cells
                    !----------------------------------------
                    
                    DO iinter = 1, nnp
                       
                       !-------------------------------------
                       ! Get interacting cells, i.e.,
                       ! icell and jcell indices.
                       !-------------------------------------
                       
                       icell = ccell+inp(1,iinter)+ &
                            n1 * inp(2,iinter) + &
                            n2 * inp(3,iinter)
                       
                       jcell = ccell+jnp(1,iinter)+ &
                            n1 * jnp(2,iinter) + &
                            n2 * jnp(3,iinter)
                       
                       !-------------------------------------
                       ! Get pointers of first and last
                       ! particles in icell and jcell.
                       !-------------------------------------
                       
                       istart = cell_list(idom)%lhbx(icell)
                       iend   = cell_list(idom)%lhbx(icell+1)-1
                       
                       jstart = cell_list(idom)%lhbx(jcell)
                       jend   = cell_list(idom)%lhbx(jcell+1)-1
                       
                       !-------------------------------------
                       ! Loop over particles in icell.
                       !-------------------------------------
                       
                       DO ipart = istart, iend
                          
                          !----------------------------------
                          ! Get index of particle in data
                          ! array, i.e., this%x. 
                          !----------------------------------
                          
                          ip = cell_list(idom)%lpdx(ipart)
                          
                          !----------------------------------
                          ! For symmetry case:
                          ! if icell and jcell are the
                          ! same cell, we make sure pair
                          ! particles interaction happen only
                          ! once. 
                          !----------------------------------
                          
                          IF ( jcell == icell .AND. &
                               symmetry ) THEN
                             
                             jstart = ipart + 1 
                             
                          END IF
                          
                          !----------------------------------
                          ! Loop over particles in jcell.
                          !----------------------------------
                          
                          DO jpart = jstart, jend
                             
                             !-------------------------------
                             ! Exclude 2 same particles.
                             !-------------------------------
                             
                             IF( jcell == icell .AND. &
                                  jpart == ipart ) THEN
                                
                                CYCLE
                                
                             END IF
                             
                             jp = cell_list(idom)%lpdx(jpart) 
                             
                             !-------------------------------
                             ! Since two particles index
                             ! are known,
                             ! their interactions are 
                             ! calculated in routine/file
                             ! pp_interactoin.inc.
                             !
                             ! Initialize force.
                             !-------------------------------
                             
                             fip (1:num_dim) = 0.0_MK
                             fjp (1:num_dim) = 0.0_MK
#ifdef __PARTICELS_FORCE_SEPARATE
                             fpip (1:num_dim) = 0.0_MK
                             fpjp (1:num_dim) = 0.0_MK
                             fvip (1:num_dim) = 0.0_MK
                             fvjp (1:num_dim) = 0.0_MK
                             IF ( Brownian ) THEN
                                frip (1:num_dim) = 0.0_MK
                                frjp (1:num_dim) = 0.0_MK
                             END IF
                             
#endif

                             IF ( stress_tensor ) THEN
                                
                                sip (1:num_dim2) = 0.0_MK
                                sjp (1:num_dim2) = 0.0_MK
#ifdef __PARTICELS_STRESS_SEPARATE
                                spip (1:num_dim2) = 0.0_MK
                                spjp (1:num_dim2) = 0.0_MK
                                svip (1:num_dim2) = 0.0_MK
                                svjp (1:num_dim2) = 0.0_MK
                                IF ( Brownian ) THEN
                                   srip (1:num_dim2) = 0.0_MK
                                   srjp (1:num_dim2) = 0.0_MK
                                END IF
                                
#endif
                             END IF

#include "pp_interaction.inc"
                             
                             !----------------------------------
                             ! Add up force of jp acting on ip.
                             !----------------------------------
                             
                             this%f(1:num_dim,ip) = &
                                  this%f(1:num_dim,ip) + fip(1:num_dim)
#ifdef __PARTICLES_FORCE_SEPARATE
                             this%fp(1:num_dim,ip) = &
                                  this%fp(1:num_dim,ip) + fpip(1:num_dim)
                             this%fv(1:num_dim,ip) = &
                                  this%fv(1:num_dim,ip) + fvip(1:num_dim)
                             IF ( Brownian ) THEN
                                this%fr(1:num_dim,ip) = &
                                     this%fr(1:num_dim,ip) + frip(1:num_dim)
                             END IF
#endif                            
                             
                             IF ( stress_tensor ) THEN
                                
                                this%s(1:num_dim2,ip) = &
                                     this%s(1:num_dim2,ip) + sip(1:num_dim2)
#ifdef __PARTICLES_STRESS_SEPARATE
                                this%sp(1:num_dim2,ip) = &
                                     this%sp(1:num_dim2,ip) + spip(1:num_dim2)
                                this%sv(1:num_dim2,ip) = &
                                     this%sv(1:num_dim2,ip) + svip(1:num_dim2)
                                IF ( Brownian ) THEN
                                   this%sr(1:num_dim2,ip) = &
                                        this%sr(1:num_dim2,ip) + &
                                        srip(1:num_dim2)
                                END IF
#endif
                             END IF
                             
                             !----------------------------------
                             ! 1)In symmetry inter-communiction,
                             ! add force on jp, no matter the 
                             ! particle is fluid, symmetry, 
                             ! wall using symmetry, solid wall,
                             ! Lees-Edwards or
                             ! colloid boundary particle.
                             !
                             ! 2)In non-symmetry inter-communication,
                             ! add force on jp when it is
                             ! wall_sym.
                             !
                             ! Add fj to particle jp.
                             !----------------------------------
                             
                             IF ( symmetry .OR. &
                                  ( this%id(this%sid_idx,jp) < 0  .AND. &
                                  num_wall_sym > 0 ) ) THEN
                                
                                this%f(1:num_dim,jp) = &
                                     this%f(1:num_dim,jp) + fjp(1:num_dim)
                                
#ifdef __PARTICLES_FORCE_SEPARATE
                                this%fp(1:num_dim,jp) = &
                                     this%fp(1:num_dim,jp) + fpjp(1:num_dim)
                                this%fv(1:num_dim,jp) = &
                                     this%fv(1:num_dim,jp) + fvjp(1:num_dim)
                                IF ( Brownian ) THEN
                                   this%fr(1:num_dim,jp) = &
                                        this%fr(1:num_dim,jp) + frjp(1:num_dim)
                                END IF
#endif
                                
                                IF ( stress_tensor ) THEN
                                   
                                   this%s(1:num_dim2,jp) = &
                                        this%s(1:num_dim2,jp) + sjp(1:num_dim2)
                                   
#ifdef __PARTICLES_STRESS_SEPARATE
                                   this%sp(1:num_dim2,jp) = &
                                        this%sp(1:num_dim2,jp) + &
                                        spjp(1:num_dim2)
                                   this%sv(1:num_dim2,jp) = &
                                        this%sv(1:num_dim2,jp) + &
                                        svjp(1:num_dim2)
                                   
                                   IF ( Brownian ) THEN
                                      this%sr(1:num_dim2,jp) = &
                                           this%sr(1:num_dim2,jp) + &
                                           srjp(1:num_dim2)
                                   END IF
#endif
                                END IF
                                
                             END IF
                             
                             
                          END DO ! jpart
                          
                       END DO ! ipart
                       
                    END DO ! iinter : 1, nnp
                    
                 END DO ! i : icstart, icend
                 
              END DO ! j: jcstart, jcend
              
           END DO ! k:  kcstart, kcend
           
        END DO ! idom : 1,num_sub
        
        
        !----------------------------------------------------
        ! Return.
     	!----------------------------------------------------
        
9999	CONTINUE        
        
        IF( ASSOCIATED(bcdef)) THEN
           DEALLOCATE(bcdef)
        END If
        
        IF(ASSOCIATED(num_cell_dim_sub)) THEN
           DEALLOCATE(num_cell_dim_sub)
        END IF
        
        IF(ASSOCIATED(cell_list)) THEN
           DEALLOCATE(cell_list)
        END IF
        
        IF(ASSOCIATED(sub_bcdef)) THEN
           DEALLOCATE(sub_bcdef)
        END IF
        
        IF(ASSOCIATED(inp)) THEN
           DEALLOCATE(inp)
        END IF
        
        IF(ASSOCIATED(jnp)) THEN
           DEALLOCATE(jnp)
        END IF
        
        IF(ASSOCIATED(dx)) THEN
           DEALLOCATE(dx)
        END IF

        RETURN
        
      END SUBROUTINE particles_compute_interaction
      
      !***** Added by Adolfo *****
      !--------------------------------------
!      SUBROUTINE make_bounce_back(this,d_colloids,comm, MPI_PREC, stat_info)
      SUBROUTINE make_bounce_back(this,d_colloids,comm, rank, MPI_PREC, stat_info)
        !----------------------------------------------------
        ! Subroutine  : make_bounce_back
        !----------------------------------------------------
        TYPE(Particles), INTENT(INOUT)        :: this
        TYPE(colloid), INTENT(INOUT)          :: d_colloids
        INTEGER, INTENT(IN)                  :: comm
        INTEGER, INTENT(IN)                  :: rank
        INTEGER, INTENT(IN)                  :: MPI_PREC
        INTEGER, INTENT(OUT)                  :: stat_info
        INTEGER :: i, j
        INTEGER :: num_colloid
        REAL(MK), DIMENSION(:,:), POINTER :: coll_x    
        REAL(MK), DIMENSION(:,:,:), POINTER :: coll_v
        REAL(MK), DIMENSION(:), POINTER   :: coll_m
        REAL(MK), DIMENSION(:,:), POINTER   :: coll_I
        REAL(MK), DIMENSION(:,:), POINTER :: radius
        REAL(MK), DIMENSION(:), POINTER :: pos_ij 
        REAL(MK), DIMENSION(:), POINTER :: Box
        REAL(MK), DIMENSION(:), POINTER :: min_phys
        REAL(MK), DIMENSION(:), POINTER :: max_phys
        REAL(MK), DIMENSION(:), POINTER :: n
        REAL(MK), DIMENSION(:), POINTER :: v
        REAL(MK), DIMENSION(:), POINTER :: dv
        INTEGER :: num_dim
        REAL(MK) :: rijsq, rij, v_dot_n
        REAL(MK), DIMENSION(:,:), POINTER :: total_mom
        REAL(MK), DIMENSION(:,:), POINTER :: total_mom2

        NULLIFY(coll_x)
        NULLIFY(coll_v)
        NULLIFY(pos_ij)
        NULLIFY(Box)
        NULLIFY(max_phys)
        NULLIFY(min_phys)
        NULLIFY(radius)
        NULLIFY(n)
        NULLIFY(v)
        NULLIFY(total_mom)
        NULLIFY(total_mom2)
        NULLIFY(dv)
        NULLIFY(coll_m)
        NULLIFY(coll_I)

        CALL colloid_get_max_phys(d_colloids, max_phys, stat_info)
        CALL colloid_get_min_phys(d_colloids, min_phys, stat_info)

        num_dim     = SIZE(max_phys)
        num_colloid = colloid_get_num_colloid(d_colloids, stat_info)
        
        ALLOCATE(Box(1:3))
        ALLOCATE(n(1:3))
        ALLOCATE(pos_ij(3))
        ALLOCATE(v(3))
        ALLOCATE(total_mom(3, num_colloid))
        ALLOCATE(total_mom2(3, num_colloid))
        ALLOCATE(dv(3))                        

        Box(:) = max_phys(:) - min_phys(:)

        CALL colloid_get_x(d_colloids,coll_x,stat_info)
        CALL colloid_get_v(d_colloids,coll_v,stat_info)
        CALL colloid_get_radius(d_colloids,radius,stat_info)
        CALL colloid_get_m(d_colloids,coll_m, stat_info)
        
!!$        DO j = 1, this%num_part_all
!!$!        DO j = 1, this%num_part_real
!!$           IF (this%id(1,j) == 63971) THEN
!!$              WRITE(*,'(A, I, 3E20.10)') 'xx ', rank, this%x(1:num_dim,j)
!!$              WRITE(*,'(A, I, 3E20.10)') 'aa ', rank, this%v(1:num_dim,j)
!!$              WRITE(*,'(A, I, 3E20.10)') 'bb ', rank, this%m(j) * this%v(1:num_dim,j)
!!$              WRITE(*,'(A, I, 3E20.10)') 'cc ', rank, coll_m(1) * coll_v(:,1,1)
!!$              WRITE(*,'(A, I, 3E20.10)') 'dd ', rank, coll_m(1) * coll_v(:,1,1) + this%m(j) * this%v(1:num_dim,j)
!!$           ENDIF
!!$        ENDDO


        DO i = 1, num_colloid

           total_mom(:,i) = 0.0_MK
           dv(:) = 0.0_MK !-- If dim = 2, this ensures dv(3) = 0 --
           pos_ij(:) = 0.0_MK !-- If dim = 2, this ensures pos_ij(3) = 0 --           
!           DO j = 1, this%num_part_real             
           DO j = 1, this%num_part_all
              !-------------------------------------------------
              ! Count only fluid particles here 
              !-------------------------------------------------
              IF ( this%id(2,j) == 0 ) THEN
                 pos_ij(1:num_dim) = this%x(1:num_dim, j) - coll_x(1:num_dim, i)
                 pos_ij(1:num_dim) = pos_ij(1:num_dim) - &
                      ANINT(pos_ij(1:num_dim)/Box(1:num_dim))*Box(1:num_dim)  !Periodic conditions  
                 rijsq = DOT_PRODUCT(pos_ij, pos_ij)
                 
                 IF (rijsq <= radius(1,i) * radius(1,i)) THEN

                    rij = DSQRT(rijsq)
                    n(1:num_dim) = pos_ij(1:num_dim) / rij
                    v_dot_n = DOT_PRODUCT(this%v(1:num_dim,j)-coll_v(1:num_dim,i,1), n(1:num_dim))
                    IF (v_dot_n < 0) THEN !-- If it's going deeper --
                       !-- About linear momentum --
                       dv(1:num_dim) = - 2.0_MK * v_dot_n * n(1:num_dim)
                       this%v(1:num_dim,j) = this%v(1:num_dim,j) + dv(1:num_dim)
                       !-- I am supposing that if j is a ghost particle, then j > this%num_part_real --
                       IF (j <= this%num_part_real) THEN ! If j is a real particle (not ghost)
                          total_mom(1:num_dim,i) = total_mom(1:num_dim,i) + this%m(j) * dv(1:num_dim)
                       ENDIF
                       !-- The change on angular momentum is not necessary to be calculated,
                       !   because is null. --
                    ENDIF                    
                 ENDIF
              ENDIF              
           ENDDO
        ENDDO

        !-------------------------------------------------------
        ! Sum up the changes in momentum 
        !-------------------------------------------------------
        CALL MPI_ALLREDUCE (total_mom,  &
             total_mom2,SIZE(total_mom),MPI_PREC, &
             MPI_SUM,comm,stat_info)


        !-- Now, the velocity of the colloids is changed --
        DO i = 1, num_colloid
           !**** Valid only if all the particles are equal ****
           !-- j is something to do with the number of substeps --
!           !-- I am changing all of them. I hope is fine. --
!           DO j = 1, SIZE(coll_v,3) 
           coll_v(:,i,1) = coll_v(:,i,1) - total_mom2(:,i)/coll_m(i)

!           ENDDO

        ENDDO
        !-- The obtained value is assigned to the colloids ---
        CALL colloid_set_v(d_colloids, coll_v, stat_info)

!!$        DO j = 1, this%num_part_all
!!$!        DO j = 1, this%num_part_real
!!$           IF (this%id(1,j) == 63971) THEN
!!$              WRITE(*,'(A, I, 3E20.10)') 'XX ', rank, this%x(1:num_dim,j)
!!$              WRITE(*,'(A, I, 3E20.10)') 'AA ', rank, this%v(1:num_dim,j)
!!$              WRITE(*,'(A, I, 3E20.10)') 'BB ', rank, this%m(j) * this%v(1:num_dim,j)
!!$              WRITE(*,'(A, I, 3E20.10)') 'CC ', rank, coll_m(1) * coll_v(:,1,1)
!!$              WRITE(*,'(A, I, 3E20.10)') 'DD ', rank, coll_m(1) * coll_v(:,1,1) + this%m(j) * this%v(1:num_dim,j)
!!$           ENDIF
!!$        ENDDO


        !-- Deallocate pointers ---
        IF (ASSOCIATED(coll_x)) THEN
           DEALLOCATE(coll_x)
        ENDIF
        IF (ASSOCIATED(coll_v)) THEN
           DEALLOCATE(coll_v)
        ENDIF
        IF (ASSOCIATED(pos_ij)) THEN
           DEALLOCATE(pos_ij)
        ENDIF
        IF (ASSOCIATED(Box)) THEN
           DEALLOCATE(Box)
        ENDIF
        IF (ASSOCIATED(max_phys)) THEN
           DEALLOCATE(max_phys)
        ENDIF
        IF (ASSOCIATED(min_phys)) THEN
           DEALLOCATE(min_phys)
        ENDIF
        IF (ASSOCIATED(radius)) THEN
           DEALLOCATE(radius)
        ENDIF
        IF (ASSOCIATED(n)) THEN
           DEALLOCATE(n)
        ENDIF
        IF (ASSOCIATED(v)) THEN
           DEALLOCATE(v)
        ENDIF
        IF (ASSOCIATED(total_mom)) THEN
           DEALLOCATE(total_mom)
        ENDIF
        IF (ASSOCIATED(total_mom2)) THEN
           DEALLOCATE(total_mom2)
        ENDIF
        IF (ASSOCIATED(dv)) THEN
           DEALLOCATE(dv)
        ENDIF
        IF (ASSOCIATED(coll_m)) THEN
           DEALLOCATE(coll_m)
        ENDIF
        IF (ASSOCIATED(coll_I)) THEN
           DEALLOCATE(coll_I)
        ENDIF
       
        RETURN          
        
      END SUBROUTINE make_bounce_back


!***** Added by Adolfo *****
!--------------------------------------
      SUBROUTINE make_bounce_back_wall(this, min_phys, max_phys,comm, MPI_PREC, &
           total_force_top, total_force_bottom, dt, stat_info)
        !----------------------------------------------------
        ! Subroutine  : make_bounce_back
        !----------------------------------------------------
        TYPE(Particles), INTENT(INOUT)        :: this
        REAL(MK), DIMENSION(:) :: min_phys, max_phys
        INTEGER, INTENT(IN)                  :: comm
        INTEGER, INTENT(IN)                  :: MPI_PREC
        REAL(MK), DIMENSION(:) :: total_force_top
        REAL(MK), DIMENSION(:) :: total_force_bottom
        REAL(MK), INTENT(IN)                  :: dt        
        INTEGER, INTENT(OUT)                  :: stat_info
        INTEGER :: i, j
        REAL(MK), DIMENSION(2)           :: Npen, Npen2
        REAL(MK), DIMENSION(:), POINTER :: Box
        REAL(MK), DIMENSION(:), POINTER :: total_mom_top
        REAL(MK), DIMENSION(:), POINTER :: total_mom_bottom
        REAL(MK), DIMENSION(:), POINTER :: total_mom_top2
        REAL(MK), DIMENSION(:), POINTER :: total_mom_bottom2
        INTEGER :: num_dim

        NULLIFY(Box)
        NULLIFY(total_mom_top)
        NULLIFY(total_mom_bottom)
        NULLIFY(total_mom_top2)
        NULLIFY(total_mom_bottom2)

        num_dim     = SIZE(max_phys)
        ALLOCATE(Box(1:num_dim))
        ALLOCATE(total_mom_top(num_dim))
        ALLOCATE(total_mom_bottom(num_dim))
        ALLOCATE(total_mom_top2(num_dim))
        ALLOCATE(total_mom_bottom2(num_dim))                

!!$        Npen(:) = 0
        total_mom_top(:)    = 0.0_MK
        total_mom_bottom(:) = 0.0_MK                
!        DO j = 1, this%num_part_real             
        DO j = 1, this%num_part_all
           !-------------------------------------------------
           ! Count only fluid particles here 
           !-------------------------------------------------
           IF ( this%id(2,j) == 0 ) THEN
              IF ( this%x(num_dim, j) .LE. min_phys(num_dim)) THEN
                 IF ( this%v(num_dim, j) < 0) THEN
                    !-- I am supposing that if j is a ghost particle, then j > this%num_part_real --
                    IF (j <= this%num_part_real) THEN ! If j is a real particle (not ghost)
                       !-- Change of momentum after changing velocity --
                       total_mom_bottom(num_dim) = total_mom_bottom(num_dim) - &
                            2.0_MK * this%m(j) * this%v(num_dim,j)
                    ENDIF
                    !-- The velocity is now changed --
                    this%v(num_dim,j) = -this%v(num_dim,j)
                 ENDIF
                 
!!$                 Npen(1) = Npen(1) + 1
              ENDIF

              IF ( this%x(num_dim, j) .GE. max_phys(num_dim)) THEN
                 IF ( this%v(num_dim, j) > 0) THEN
                    !-- I am supposing that if j is a ghost particle, then j > this%num_part_real --
                    IF (j <= this%num_part_real) THEN ! If j is a real particle (not ghost)
                       !-- Change of momentum after changing velocity --
                       total_mom_top(num_dim) = total_mom_top(num_dim) - &
                            2.0_MK * this%m(j) * this%v(num_dim,j)
                    ENDIF
                    !-- The velocity is now changed --
                    this%v(num_dim,j) = -this%v(num_dim,j)                    
                 ENDIF
                    
!!$                 Npen(2) = Npen(2) + 1
              ENDIF
              
           ENDIF
        ENDDO

        !-------------------------------------------------------
        ! Sum up the changes in momentum
        !-------------------------------------------------------
        CALL MPI_ALLREDUCE (total_mom_top,  &
             total_mom_top2,SIZE(total_mom_top),MPI_PREC, &
             MPI_SUM,comm,stat_info)
        CALL MPI_ALLREDUCE (total_mom_bottom,  &
             total_mom_bottom2,SIZE(total_mom_bottom),MPI_PREC, &
             MPI_SUM,comm,stat_info)

        !--- The force on the wall due to the change of momentum is calculated ---
        total_force_top(:)    = total_mom_top2(:) / dt
        total_force_bottom(:) = total_mom_bottom2(:) / dt

        !****** total_force_top and total_force_bottom are later used in
        ! boundary_collect_particles_interaction.F90 **********
        
!!$        !-------------------------------------------------------
!!$        ! Sum up the total number of particles
!!$        !-------------------------------------------------------
!!$        CALL MPI_ALLREDUCE (Npen2,  &
!!$             Npen,2,MPI_PREC, &
!!$             MPI_SUM,comm,stat_info)         

        !-- Deallocate pointers ---
        IF (ASSOCIATED(Box)) THEN
           DEALLOCATE(Box)
        ENDIF
        IF (ASSOCIATED(total_mom_top)) THEN
           DEALLOCATE(total_mom_top)
        ENDIF
        IF (ASSOCIATED(total_mom_bottom)) THEN
           DEALLOCATE(total_mom_bottom)
        ENDIF
        IF (ASSOCIATED(total_mom_top2)) THEN
           DEALLOCATE(total_mom_top2)
        ENDIF
        IF (ASSOCIATED(total_mom_bottom2)) THEN
           DEALLOCATE(total_mom_bottom2)
        ENDIF
       
        RETURN          
        
      END SUBROUTINE make_bounce_back_wall
      
