      SUBROUTINE  particles_init_partial_exter(this,stat_info)
        !----------------------------------------------------
        ! Subroutine  : particles_init_partial_exter
        !----------------------------------------------------
        !
        ! Purpose      :  Create the rest quntities besides
        !                 the ones read from external files.
        !                 Currently no need.e
        !                  
        ! Routines     :
        !
        ! Remarks      :  Create the rest quntities,
        !                 besides position, velocity,
        !                 rho,mass, IDs, for fluid particles 
        !                 in sub-domains of each process.
        !
        !                 In case of Non-Newtonian fluid,
        !                 conformation tensor C or eigenvalues
        !                 and eigenvectors have been
        !                 read already.
        !
        ! References   :
        !
        ! Revisions    :  V0.1 30.07 2009, original version.
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
        
        TYPE(Particles), INTENT(INOUT)          :: this
        INTEGER,INTENT(OUT)	                :: stat_info
    	
	!----------------------------------------------------
    	! Local variables
        !----------------------------------------------------
        
        INTEGER                                 :: stat_info_sub
        
        
        !---------------------------------------------------
    	! Initialization of variables.
    	!---------------------------------------------------
        
        stat_info     = this%num_part_real
        stat_info     = 0
        stat_info_sub = 0
        
        
9999	CONTINUE

        
	RETURN
 
    END SUBROUTINE particles_init_partial_exter
      
      
     
