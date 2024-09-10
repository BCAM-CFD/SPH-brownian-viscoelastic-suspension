      SUBROUTINE boundary_noslip_mirror(this,vw,sid_w,stat_info)
        !------------------------------------------
        !   Subroutine  : Implementing
        !                 no slip condition using
        !                 symmetry/mirror particles
        !                 created by PPM.
        !
        !  Revision     : V0.1 29.10.2009,
        !                 original version.
        !------------------------------------------
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
        !------------------------------------------
        
        !------------------------------------------
        ! Arguments
        !------------------------------------------
        
        TYPE(Boundary), INTENT(IN)              :: this
        REAL(MK), DIMENSION(:), INTENT(INOUT)   :: vw
        INTEGER, INTENT(IN)                     :: sid_w
        
        INTEGER, INTENT(OUT)                    :: stat_info 
        
        INTEGER                                 :: stat_info_sub
        INTEGER                                 :: dim
        INTEGER                                 :: wall_index
        
        stat_info     = 0
        stat_info_sub = 0
        
        dim = this%num_dim
        wall_index = ABS(sid_w)
        
        vw(1:dim) = -vw(1:dim) + &
             2.0_MK*this%shear_v(1:dim,wall_index)
        
        RETURN
        
      END SUBROUTINE boundary_noslip_mirror
      
