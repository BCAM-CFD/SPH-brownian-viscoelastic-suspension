      SUBROUTINE colloid_init_default(this,stat_info)
        !----------------------------------------------------
        ! Subroutine  : colloid_init_default
        !----------------------------------------------------
        !
        ! Purpose     : Default construtor of colloid Class.
        !
        ! Reference   :
        !
        ! Remark      :
        !
        ! Revisions   : V0.1 15.07.2009, original version.
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
        !----------------------------------------------------

        TYPE(Colloid),INTENT(OUT)       :: this
        INTEGER,INTENT(OUT)             :: stat_info
        
        INTEGER                         :: stat_info_sub
        INTEGER                         :: dim, num
        INTEGER                         :: integrate_num

        !----------------------------------------------------
        ! For default colloid, there is only one,
        ! i.e., 2D flow around cylinder problem.
        ! Morris et al. J. Comput. Phys. 1997. 
        !----------------------------------------------------
        
        stat_info     = 0
        stat_info_sub = 0
        
        NULLIFY(this%tech)
        
        dim = 2
        num = 1

        this%num_dim     = dim
        this%num_colloid = num
        
        this%adapt_t_coef   = 1.0_MK
        this%sub_time_step  = 1
        
        this%integrate_type = 1
        this%integrate_RK = 1
        this%integrate_AB = 1    
        this%implicit_pair_num_sweep       = 1
	!******* Added by Adolfo **********
        this%implicit_pair_num_sweep_tang  = 1
	!**********************************
        this%implicit_pair_sweep_adaptive  = .FALSE.
        this%implicit_pair_sweep_tolerance = 1.0e-3_MK
        this%implicit_pair_sweep_error     = &
             this%implicit_pair_sweep_tolerance
	!******* Added by Adolfo **********
        this%implicit_pair_sweep_error_tang = &
             this%implicit_pair_sweep_tolerance
	!**********************************
        this%implicit_pair_sweep_max       = &
             mcf_cc_lub_implicit_velocity_sweep_max
        this%explicit_sub_time_step = 1
        this%rho         = 1.e3_MK
        this%rho_type    = 0
        this%translate   = .FALSE.
        this%rotate      = .FALSE.
        this%place       = 1
        this%noslip_type = 2
        this%body_force_type = 0
        NULLIFY(this%body_force)
        ALLOCATE(this%body_force(dim))
        this%body_force(:) = 0.0_MK
        
        this%cc_lub_type    = 0
        this%cc_repul_type  = 0
        this%cc_lub_cut_off = 0.0_MK
        this%cc_lub_cut_on  = 0.0_MK
        this%cc_repul_sigma   = 0.0_MK
        this%cc_repul_cut_off = 0.0_MK
        this%cc_repul_cut_on  = 0.0_MK
        this%cc_repul_F0      = 0.0_MK
        
        this%cc_magnet_type    = 0
        this%cc_magnet_cut_off = 0.0_MK
        this%cc_magnet_cut_on  = 0.0_MK        
        this%cc_magnet_F0      = 0.0_MK
        NULLIFY(this%cc_magnet_B)
        ALLOCATE(this%cc_magnet_B(dim))
        NULLIFY(this%cc_magnet_mom)
        ALLOCATE(this%cc_magnet_mom(dim))
        this%cc_magnet_rot_vector(:)     = 0.0_MK
        this%cc_magnet_acc_rot_vector(:) = 0.0_MK
        this%cc_magnet_rot_matrix(:,:)     = 0.0_MK
        this%cc_magnet_acc_rot_matrix(:,:) = 0.0_MK
        this%cc_magnet_rot_freq          = 1.0_MK
        this%cc_magnet_f   = 0.0_MK
        this%cc_magnet_chi = 0.0_MK
        this%cc_magnet_mu  = 1.0_MK

        
        this%cw_lub_type    = 0
        this%cw_repul_type  = 0
        this%cw_lub_cut_off = 0.0_MK
        this%cw_lub_cut_on  = 0.0_MK
        this%cw_repul_cut_off = 0.0_MK
        this%cw_repul_cut_on  = 0.0_MK        
        this%cw_repul_F0      = 0.0_MK
        
        this%h                = 0.0_MK
        this%dt_f             = -1.0_MK
        
        NULLIFY(this%shape)
        ALLOCATE(this%shape(num))
        this%shape(:) = 1
        
        NULLIFY(this%radius)
        ALLOCATE(this%radius(dim,num))
        this%radius(1,:) = 0.02_MK
        
        NULLIFY(this%freq)
        ALLOCATE(this%freq(num))
        this%freq(1) = 0
        
        NULLIFY(this%m)
        ALLOCATE(this%m(num))
        this%m(:) = 0.0_MK

        NULLIFY(this%mmi)
        ALLOCATE(this%mmi(3,num))
        this%mmi(:,:) = 0.0_MK
    
        NULLIFY(this%x)
        ALLOCATE(this%x(dim,num))
        this%x(:,1) = 0.05_MK
        
        NULLIFY(this%v)
        NULLIFY(this%omega)
        NULLIFY(this%f)
        NULLIFY(this%alpha)   
        
        SELECT CASE (this%integrate_type)
           
        CASE (-2)
           
           integrate_num = 1
       
        CASE (-1)
           
           integrate_num = 1
      
        CASE (1)
           
           integrate_num = 1
          
        CASE (2)
           
           integrate_num = this%integrate_AB
           
        CASE DEFAULT
           
           PRINT *, __FILE__, __LINE__, &
                "no such integration!"
           stat_info = -1
           GOTO 9999
           
        END SELECT
        
        ALLOCATE(this%v(dim,num,integrate_num))
        this%v(:,:,:) = 0.0_MK
        ALLOCATE(this%omega(3,num,integrate_num))
        this%omega(:,:,:) = 0.0_MK
        ALLOCATE(this%f(dim,num,integrate_num))
        this%f(:,:,:) = 0.0_MK
        
        this%fa_min = 0.0_MK
        this%fa_max = 0.0_MK
        
        ALLOCATE(this%alpha(3,num,integrate_num))
        this%alpha(:,:,:) = 0.0_MK
        
#if __DRAG_PART
        NULLIFY(this%drag_lub)
        ALLOCATE(this%drag_lub(dim,num))
        this%drag_lub(:,:) = 0.0_MK
        
        NULLIFY(this%drag_repul)
        ALLOCATE(this%drag_repul(dim,num))
        this%drag_repul(:,:) = 0.0_MK     
#endif
    
        NULLIFY(this%drag)
        ALLOCATE(this%drag(dim,num))
        this%drag(:,:) = 0.0_MK
        
        NULLIFY(this%rot_vector)
        ALLOCATE(this%rot_vector(4,num))
        this%rot_vector(:,:) = 0.0_MK
    
        NULLIFY(this%acc_vector)
        ALLOCATE(this%acc_vector(4,num))
        this%acc_vector(:,:) = 0.0_MK
   
        NULLIFY(this%rot_matrix)
        ALLOCATE(this%rot_matrix(3,3,num))
        this%rot_matrix(:,:,:) = 0.0_MK
        this%rot_matrix(1,1,:) = 1.0_MK
        this%rot_matrix(2,2,:) = 1.0_MK
        this%rot_matrix(3,3,:) = 1.0_MK
        
        NULLIFY(this%acc_matrix)
        ALLOCATE(this%acc_matrix(3,3,num))
        this%acc_matrix(:,:,:) = this%rot_matrix(:,:,:)
        
        NULLIFY(this%theta)
        ALLOCATE(this%theta(3,num))
        this%theta(:,:) = 0.0_MK
    
        NULLIFY(this%torque)
        ALLOCATE(this%torque(3,num))
        this%torque(:,:) = 0.0_MK
        
        NULLIFY(this%num_physical_part)
        ALLOCATE(this%num_physical_part(num))
        this%num_physical_part(:)  = 0
        
        NULLIFY(this%num_numerical_part)
        ALLOCATE(this%num_numerical_part(num))
        this%num_numerical_part(:)  = 0
        
        NULLIFY(this%k_energy)
        ALLOCATE(this%k_energy(num))
        this%k_energy(:) = 0.0_MK

        NULLIFY(this%mom)
        ALLOCATE(this%mom(dim,num))
        this%mom(:,:) = 0.0_MK
        
        NULLIFY(this%mom_tot)
        ALLOCATE(this%mom_tot(dim))
        this%mom_tot(:) = 0.0_MK
        
        this%num_physical_part_tot = 0
        this%num_numerical_part_tot = 0
        
        NULLIFY(this%min_phys)
        ALLOCATE(this%min_phys(dim))
        this%min_phys(:) = 0.0_MK
        
        NULLIFY(this%max_phys)
        ALLOCATE(this%max_phys(dim))
        this%max_phys(:) = 0.0_MK
        
        NULLIFY(this%min_phys_t)
        ALLOCATE(this%min_phys_t(dim))
        this%min_phys_t(:) = 0.0_MK
        
        NULLIFY(this%max_phys_t)
        ALLOCATE(this%max_phys_t(dim))
        this%max_phys_t(:) = 0.0_MK
        
        NULLIFY(this%bcdef)
        ALLOCATE(this%bcdef(2*dim))
        this%bcdef(:)  = ppm_param_bcdef_periodic
        NULLIFY(this%boundary)

        this%cut_off = 0.0_MK
        this%dout    = 0.0_MK
        this%din     = 0.0_MK
        this%eta     = 0.0_MK

        !----------------------------------------------------
        ! Images of center: position and velocity.
        !----------------------------------------------------
        
        this%num_image = 0
        NULLIFY(this%x_image)
        NULLIFY(this%v_image)
        
        CALL tool_new(this%tool,stat_info_sub)
        
9999    CONTINUE
        
        RETURN          
        
      END SUBROUTINE colloid_init_default
      
      
      !************ Changed by Adolfo *********
      SUBROUTINE colloid_init(this,&
           d_dim,d_num,&
           d_integrate_type,&
           d_integrate_RK,&
           d_integrate_AB,&
           d_random, &
           stat_info)
!!$      SUBROUTINE colloid_init(this,&
!!$           d_dim,d_num,&
!!$           d_integrate_type,&
!!$           d_integrate_RK,&
!!$           d_integrate_AB,&
!!$           stat_info)
        !*****************************
        !----------------------------------------------------
        ! Subroutine  : colloid_init
        !----------------------------------------------------
        !
        ! Purpose     : Construtor of colloid Class.
        !
        ! Reference   :
        !
        ! Remark      :
        !
        ! Revisions   : V0.1 15.07.2009, original version.
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
        
        TYPE(Colloid),INTENT(OUT)       :: this
        INTEGER, INTENT(IN)             :: d_dim
        INTEGER, INTENT(IN)             :: d_num
        INTEGER, INTENT(IN)             :: d_integrate_type
        INTEGER, INTENT(IN)             :: d_integrate_RK
        INTEGER, INTENT(IN)             :: d_integrate_AB
        !******** Added by Adolfo *********
        TYPE(Random), INTENT(IN), TARGET :: d_random
        !**********************************
        INTEGER,INTENT(OUT)             :: stat_info
        
        INTEGER                         :: stat_info_sub
        INTEGER                         :: integrate_num

        !----------------------------------------------------
        ! For non default colloid(s).
        !----------------------------------------------------

        stat_info     = 0
        stat_info_sub = 0
        
        NULLIFY(this%tech)
        !**** Added by Adolfo *******
        NULLIFY(this%random)
        !****************************
        
        this%num_dim        = d_dim
        this%num_colloid    = d_num
        
        this%adapt_t_coef   = 1.0_MK
        this%sub_time_step  = 1

        this%integrate_type = d_integrate_type
        this%integrate_RK   = d_integrate_RK
        this%integrate_AB   = d_integrate_AB
        this%implicit_pair_num_sweep       = 1
	!******* Added by Adolfo **********
        this%implicit_pair_num_sweep_tang  = 1
	!**********************************
        this%implicit_pair_sweep_adaptive  = .FALSE.
        this%implicit_pair_sweep_tolerance = 1.0e-3
	!******* Added by Adolfo **********
        this%implicit_pair_sweep_error_tang = &
             this%implicit_pair_sweep_tolerance
	!**********************************
        this%implicit_pair_sweep_error     = &
             this%implicit_pair_sweep_tolerance 
        this%implicit_pair_sweep_max       = &
             mcf_cc_lub_implicit_velocity_sweep_max   
        this%explicit_sub_time_step = 1
        this%rho            = 1.e3_MK
        this%rho_type       = 0
        this%translate      = .FALSE.
        this%rotate         = .FALSE.
        this%place          = 1
        this%noslip_type    = 1
        this%body_force_type = 0
        NULLIFY(this%body_force)
        ALLOCATE(this%body_force(d_dim))
        this%body_force(:) = 0.0_MK
        
        this%cc_lub_type = 0
        this%cc_lub_cut_off = 0.0_MK
        this%cc_lub_cut_on  = 0.0_MK

        this%cc_repul_type = 0
        this%cc_repul_sigma   = 0.0_MK
        this%cc_repul_cut_off = 0.0_MK
        this%cc_repul_cut_on  = 0.0_MK
        this%cc_repul_F0      = 0.0_MK

        this%cc_magnet_type    = 0
        this%cc_magnet_cut_off = 0.0_MK
        this%cc_magnet_cut_on  = 0.0_MK        
        this%cc_magnet_F0      = 0.0_MK
        NULLIFY(this%cc_magnet_B)
        ALLOCATE(this%cc_magnet_B(d_dim))
        NULLIFY(this%cc_magnet_mom)
        ALLOCATE(this%cc_magnet_mom(d_dim))
        this%cc_magnet_rot_vector(:)     = 0.0_MK
        this%cc_magnet_acc_rot_vector(:) = 0.0_MK
        this%cc_magnet_rot_matrix(:,:)     = 0.0_MK
        this%cc_magnet_acc_rot_matrix(:,:) = 0.0_MK
        this%cc_magnet_rot_freq          = 1.0_MK        
        this%cc_magnet_f   = 0.0_MK
        this%cc_magnet_chi = 0.0_MK
        this%cc_magnet_mu  = 1.0_MK
        
        this%cw_lub_type = 0
        this%cw_lub_cut_off = 0.0_MK
        this%cw_lub_cut_on  = 0.0_MK
        
        this%cw_repul_type = 0
        this%cw_repul_cut_off = 0.0_MK
        this%cw_repul_cut_on  = 0.0_MK
        this%cw_repul_F0      = 0.0_MK
        
        this%h                = 0.0_MK
        this%dt_f             = -1.0_MK
        
        !----------------------------------------------------
        ! Default shape is sphereical.
        !----------------------------------------------------
        
        NULLIFY(this%shape)
        ALLOCATE(this%shape(d_num))
        this%shape(:) = 1
        
        NULLIFY(this%radius)
        ALLOCATE(this%radius(d_dim,d_num))
        this%radius(:,:) = 0.0_MK
        
        NULLIFY(this%freq)
        ALLOCATE(this%freq(d_num))
        this%freq(:) = 0
        
        NULLIFY(this%m)
        ALLOCATE(this%m(d_num))
        this%m(:) = 0.0_MK

        NULLIFY(this%mmi)
        ALLOCATE(this%mmi(3,d_num))
        this%mmi(:,:) = 0.0_MK
        
        NULLIFY(this%x)
        ALLOCATE(this%x(d_dim,d_num))
        this%x(:,:) = 0.0_MK
        
        
        NULLIFY(this%v)
        NULLIFY(this%omega)
        NULLIFY(this%f)
        NULLIFY(this%alpha)   
        
        SELECT CASE (this%integrate_type)
           
        CASE (-2)
           
           integrate_num = 1
         
        CASE (-1)
           
           integrate_num = 1
           
        CASE (1)
           
           integrate_num = 1
           
        CASE (2)
           
           integrate_num = this%integrate_AB
           
        CASE DEFAULT
           
           PRINT *, __FILE__, __LINE__, &
                "no such integration!"
           stat_info = -1
           GOTO 9999
           
        END SELECT
        
        ALLOCATE(this%v(d_dim,d_num,integrate_num))
        this%v(:,:,:) = 0.0_MK
        ALLOCATE(this%omega(3,d_num,integrate_num))
        this%omega(:,:,:) = 0.0_MK
        ALLOCATE(this%f(d_dim,d_num,integrate_num))
        this%f(:,:,:) = 0.0_MK
        
        this%fa_min = 0.0_MK
        this%fa_max = 0.0_MK
        
        ALLOCATE(this%alpha(3,d_num,integrate_num))
        this%alpha(:,:,:) = 0.0_MK

#if __DRAG_PART
        NULLIFY(this%drag_lub)
        ALLOCATE(this%drag_lub(d_dim,d_num))
        this%drag_lub(:,:) = 0.0_MK
        
        NULLIFY(this%drag_repul)
        ALLOCATE(this%drag_repul(d_dim,d_num))
        this%drag_repul(:,:) = 0.0_MK
#endif

        NULLIFY(this%drag)
        ALLOCATE(this%drag(d_dim,d_num))
        this%drag(:,:) = 0.0_MK
        
        NULLIFY(this%rot_vector)
        ALLOCATE(this%rot_vector(4,d_num))
        this%rot_vector(:,:) = 0.0_MK
        
        NULLIFY(this%acc_vector)
        ALLOCATE(this%acc_vector(4,d_num))
        this%acc_vector(:,:) = 0.0_MK
        
        NULLIFY(this%rot_matrix)
        ALLOCATE(this%rot_matrix(3,3,d_num))
        this%rot_matrix(:,:,:) = 0.0_MK
        
        NULLIFY(this%acc_matrix)
        ALLOCATE(this%acc_matrix(3,3,d_num))
        this%acc_matrix(:,:,:) = 0.0_MK
        
        NULLIFY(this%theta)
        ALLOCATE(this%theta(3,d_num))
        this%theta(:,:) = 0.0_MK
        
        NULLIFY(this%torque)
        ALLOCATE(this%torque(3,d_num))
        this%torque(:,:) = 0.0_MK
        
        NULLIFY(this%num_physical_part)
        ALLOCATE(this%num_physical_part(d_num))
        this%num_physical_part(:)  = 0
        
        NULLIFY(this%num_numerical_part)
        ALLOCATE(this%num_numerical_part(d_num))
        this%num_numerical_part(:)  = 0
        
        
        !----------------------------------------------------
        ! Derived quantities.                               
        !----------------------------------------------------
        
        NULLIFY(this%k_energy)
        ALLOCATE(this%k_energy(d_num))
        this%k_energy(:) = 0.0_MK

        NULLIFY(this%mom)
        ALLOCATE(this%mom(d_dim,d_num))
        this%mom(:,:) = 0.0_MK
        
        NULLIFY(this%mom_tot)
        ALLOCATE(this%mom_tot(d_dim))
        this%mom_tot(:) = 0.0_MK
        
        this%num_physical_part_tot = 0
        this%num_numerical_part_tot = 0
        
        !----------------------------------------------------
        ! Physics parameters, boundaries.
        !----------------------------------------------------
        
        NULLIFY(this%min_phys)
        ALLOCATE(this%min_phys(d_dim))
        this%min_phys(:) = 0.0_MK
        
        NULLIFY(this%max_phys)
        ALLOCATE(this%max_phys(d_dim))
        this%max_phys(:) = 0.0_MK
        
        NULLIFY(this%min_phys_t)
        ALLOCATE(this%min_phys_t(d_dim))
        this%min_phys_t(:) = 0.0_MK
        
        NULLIFY(this%max_phys_t)
        ALLOCATE(this%max_phys_t(d_dim))
        this%max_phys_t(:) = 0.0_MK
        
        NULLIFY(this%bcdef)
        ALLOCATE(this%bcdef(2*d_dim))
        this%bcdef(:)  = ppm_param_bcdef_periodic
        NULLIFY(this%boundary)

        this%cut_off = 0.0_MK
        this%dout    = 0.0_MK
        this%din     = 0.0_MK
        this%eta     = 0.0_MK

        !----------------------------------------------------
        ! Images of center: position and velocity.
        !----------------------------------------------------
        
        this%num_image = 0
        NULLIFY(this%x_image)
        NULLIFY(this%v_image)
        
        CALL tool_new(this%tool,stat_info_sub)

        !******* Added by Adolfo *****
        this%random => d_random
        !*****************************
        
9999    CONTINUE
        
        RETURN          
        
      END SUBROUTINE colloid_init
      
      
      SUBROUTINE colloid_display_parameters(this,stat_info)
        !----------------------------------------------------
        ! Subroutine  : colloid_display_parameters
        !----------------------------------------------------
        !
        ! Purpose     : To display colloid paramters.
        !
        ! Reference   :
        !
        ! Remark      :
        !
        ! Revisions   : V0.1 15.07.2009, original version.
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
        
        TYPE(Colloid),INTENT(IN)        :: this
        INTEGER,INTENT(OUT)             :: stat_info
        
        INTEGER                         :: dim, i, j, stat_info_sub
        
        
        stat_info = 0
        stat_info_sub = 0
        dim = this%num_dim
        
        
        PRINT *, '============================================================'
        PRINT *, '              Colloid  parameters'
        PRINT *, '====================Start==================================='
    
        CALL tool_print_msg(this%tool, "num_colloid", &
             this%num_colloid, stat_info_sub)
        CALL tool_print_msg(this%tool, "adapt_t_coef", &
             this%adapt_t_coef, stat_info_sub)
        CALL tool_print_msg(this%tool, "sub_time_step", &
             this%sub_time_step, stat_info_sub)
        CALL tool_print_msg(this%tool, "integrate_type", &
             this%integrate_type, stat_info_sub)
        
        SELECT CASE ( this%integrate_type )
        CASE (1)
           CALL tool_print_msg(this%tool, "integrate_RK", &
                this%integrate_RK, stat_info_sub)
        CASE (2)
           CALL tool_print_msg(this%tool, "integrate_AB", &
                this%integrate_AB, stat_info_sub)
        CASE (-2)
           CALL tool_print_msg(this%tool, "implicit pair num sweep", &
                this%implicit_pair_num_sweep, stat_info_sub)
           CALL tool_print_msg(this%tool, "implicit pair sweep adaptive", &
                this%implicit_pair_sweep_adaptive, stat_info_sub)
           CALL tool_print_msg(this%tool, "implicit pair sweep tolerance", &
                this%implicit_pair_sweep_tolerance, stat_info_sub)
           CALL tool_print_msg(this%tool, "implicit pair sweep max", &
                this%implicit_pair_sweep_max, stat_info_sub)
           CALL tool_print_msg(this%tool, "explicit_sub_time_step", &
                this%explicit_sub_time_step, stat_info_sub)
        CASE DEFAULT
           PRINT *, __FILE__, __LINE__, &
                "colloid integration type not available!"
           stat_info = -1
           GOTO 9999
        END SELECT
        
        CALL tool_print_msg(this%tool, "rho", &
             this%rho, stat_info_sub)
        CALL tool_print_msg(this%tool, "rho type", &
             this%rho_type, stat_info_sub)
        CALL tool_print_msg(this%tool, "translate", &
             this%translate, stat_info_sub)
        CALL tool_print_msg(this%tool, "rotate", &
             this%rotate, stat_info_sub)
        CALL tool_print_msg(this%tool, "particle placement", &
             this%place, stat_info_sub)
        CALL tool_print_msg(this%tool, "no slip", &
             this%noslip_type, stat_info_sub)
        CALL tool_print_msg(this%tool, "body force type", &
             this%body_force_type, stat_info_sub)
        CALL tool_print_msg(this%tool, "body force", &
             this%body_force(1:dim), stat_info_sub)
        
        CALL tool_print_msg(this%tool, "cc_lub_type", &
             this%cc_lub_type, stat_info_sub)
        
        IF ( this%cc_lub_type > mcf_cc_lub_type_no ) THEN
           CALL tool_print_msg(this%tool, "cc_lub_cut_off", &
                this%cc_lub_cut_off, stat_info_sub)
           CALL tool_print_msg(this%tool, "cc_lub_cut_on", &
                this%cc_lub_cut_on, stat_info_sub)
        END IF
        
        CALL tool_print_msg(this%tool, "cc_repul_type", &
             this%cc_repul_type, stat_info_sub)
        
        IF ( this%cc_repul_type > mcf_cc_repul_type_no ) THEN
           
           CALL tool_print_msg(this%tool, "cc_repul_sigma", &
                this%cc_repul_sigma, stat_info_sub)
           CALL tool_print_msg(this%tool, "cc_repul_cut_off", &
                this%cc_repul_cut_off, stat_info_sub)
           CALL tool_print_msg(this%tool, "cc_repul_cut_on", &
                this%cc_repul_cut_on, stat_info_sub)
           CALL tool_print_msg(this%tool, "cc_repul_F0", &
                this%cc_repul_F0, stat_info_sub)
           
        END IF
        
        CALL tool_print_msg(this%tool, "cc_magnet_type", &
             this%cc_magnet_type, stat_info_sub)

        IF ( this%cc_magnet_type > mcf_cc_magnet_type_no ) THEN
           
           CALL tool_print_msg(this%tool, "cc_magnet_cut_off", &
                this%cc_magnet_cut_off, stat_info_sub)
           CALL tool_print_msg(this%tool, "cc_magnet_cut_on", &
                this%cc_magnet_cut_on, stat_info_sub)
           CALL tool_print_msg(this%tool, "cc_magnet_F0", &
                this%cc_magnet_F0, stat_info_sub)
           !CALL tool_print_msg(this%tool, "cc_magnet_B", &
           !this%cc_magnet_B(1:dim), stat_info_sub)
           CALL tool_print_msg(this%tool, "cc_magnet_mom", &
                this%cc_magnet_mom(1:dim), stat_info_sub)
           CAlL tool_print_msg(this%tool, "cc_magnet_rot_vector", &
                this%cc_magnet_acc_rot_vector(1:3), stat_info_sub)
           CAlL tool_print_msg(this%tool, "cc_magnet_rot_angle", &
                this%cc_magnet_acc_rot_vector(4), stat_info_sub)        
           CAlL tool_print_msg(this%tool, "cc_magnet_rot_freq", &
                this%cc_magnet_rot_freq, stat_info_sub)           
           !CALL tool_print_msg(this%tool, "cc_magnet_f ", &
           !this%cc_magnet_f, stat_info_sub)
           !CALL tool_print_msg(this%tool, "cc_magnet_chi", &
           !    this%cc_magnet_chi, stat_info_sub)
           !CALL tool_print_msg(this%tool, "cc_magnet_mu", &
           !     this%cc_magnet_mu, stat_info_sub)
           
        END IF
        
        CALL tool_print_msg(this%tool, "cw_lub_type", &
             this%cw_lub_type, stat_info_sub)

        IF ( this%cw_lub_type >= mcf_cw_lub_type_no ) THEN
           
           CALL tool_print_msg(this%tool, "cw_lub_cut_off", &
                this%cw_lub_cut_off, stat_info_sub)
           CALL tool_print_msg(this%tool, "cw_lub_cut_on", &
                this%cw_lub_cut_on, stat_info_sub)
           
        END IF
        
        CALL tool_print_msg(this%tool, "cw_repul_type", &
             this%cw_repul_type, stat_info_sub)

        IF ( this%cw_repul_type > mcf_cw_repul_type_no ) THEN

           CALL tool_print_msg(this%tool, "cw_repul_cut_off", &
                this%cw_repul_cut_off, stat_info_sub)
           CALL tool_print_msg(this%tool, "cw_repul_cut_on", &
                this%cw_repul_cut_on, stat_info_sub)
           CALL tool_print_msg(this%tool, "cw_repul_F0", &
                this%cw_repul_F0, stat_info_sub)
           
        END IF
        
        CALL tool_print_msg(this%tool, "h", &
             this%h, stat_info_sub)
        CALL tool_print_msg(this%tool, "num_image", &
             this%num_image, stat_info_sub)
        
        
        DO i = 1, this%num_colloid

           PRINT *, '      -------------------------------------'
           PRINT *, '      *************************************'
           CALL tool_print_msg(this%tool, "colloid index", &
                i, stat_info_sub)
           CALL tool_print_msg(this%tool, "shape", &
             this%shape(i), stat_info_sub)
           CALL tool_print_msg(this%tool, "radius", &
                this%radius(1:dim,i), stat_info_sub)
           CALL tool_print_msg(this%tool, "freq", &
                this%freq(i), stat_info_sub)
           CALL tool_print_msg(this%tool, "m", &
                this%m(i), stat_info_sub)
           CALL tool_print_msg(this%tool, "mmi", &
                this%mmi(1:3,i), stat_info_sub)
           CALL tool_print_msg(this%tool, "x", &
                this%x(1:dim,i), stat_info_sub)
           CALL tool_print_msg(this%tool, "v", &
                this%v(1:dim,i,1), stat_info_sub)
           DO j = 1, dim
              CALL tool_print_msg(this%tool, "rotation vector", &
                   this%acc_vector((j-1)*dim+1:j*dim,i), stat_info_sub)
           END DO
           !CALL tool_print_msg(this%tool, "theta: ", &
           !     this%theta(1:3,i), stat_info_sub)
           CALL tool_print_msg(this%tool, "omega", &
                this%omega(1:3,i,1), stat_info_sub)
           CALL tool_print_msg(this%tool, "num_physical_part", &
                this%num_physical_part(i), stat_info_sub)
           CALL tool_print_msg(this%tool, "num_numerical_part", &
                this%num_numerical_part(i), stat_info_sub) 
           PRINT *, '      *************************************'          
           PRINT *, '      -------------------------------------'
           
        END DO
        
        PRINT *, '=====================END===================================='
        PRINT *, '              Colloid  parameters'
        PRINT *, '============================================================'
    
    
        

9999    CONTINUE
        
        RETURN
        
      END SUBROUTINE colloid_display_parameters
      
      
