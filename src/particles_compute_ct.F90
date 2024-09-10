      SUBROUTINE particles_compute_ct(this,num,stat_info)
        !----------------------------------------------------
        ! Subroutine :  particles_compute_ct
        !----------------------------------------------------
        !
        ! Purpose    :  Compute the egenvector dynamics
        !               conformation tensor from egenvalues 
        !               and egenvectors of particles, in case
        !               of egenvector dynamics of Non-Newtonian
        !               oldroyd-B model.
        !               
        !
        ! Reference  :  Vazquez-Quesada, Ellero, Espanol
        !               Phyical Review E 79. 056707, 2009.
        !
        ! Remark     :
        !
        ! Revision   :  V0.1  04.08.2009, original version.
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
        ! this           : an object of Particles Class.
        ! num            : number of particles needed updated,
        !                  i.e. first num particles in this%x 
        !                  are operated.
        ! stat_info      : return flag of status.
        !----------------------------------------------------
        
        TYPE(Particles), INTENT(INOUT)          :: this
        INTEGER, INTENT(IN)                     :: num
        INTEGER, INTENT(OUT)                    :: stat_info
        
        !----------------------------------------------------
        ! Local variables
        !----------------------------------------------------

        
        INTEGER                                 :: stat_info_sub
        INTEGER                                 :: dim
        INTEGER                                 :: i,j,k,p
        
        !----------------------------------------------------
        ! Initialization of variables.
        !----------------------------------------------------
        
        stat_info     = 0
        stat_info_sub = 0
        
        !----------------------------------------------------
        ! Calculation only for real particles.
        !----------------------------------------------------
        
        IF( num > this%num_part_real) THEN
           PRINT *, "particles_compute_ct : ", &
                "num > num_part_real, wrong !"
           stat_info = -1
           GOTO 9999      
        END IF
        
        dim = &
             physics_get_num_dim(this%phys,stat_info_sub)
        
 
        !----------------------------------------------------
        ! Allocate memory for ct.
        !----------------------------------------------------
        
        IF(ASSOCIATED(this%ct)) THEN
           DEALLOCATE(this%ct)
        ENd IF
        
        ALLOCATE(this%ct(dim**2,num))
        
        this%ct(1:dim**2,1:num) = 0.0_MK
        
        !----------------------------------------------------
        ! Compute egenvector dynamics' conformation tensor.
        !
        ! see Class_Particles for matrix notation converted 
        ! to array notation convention.
        !----------------------------------------------------
        
        DO p = 1, num ! particle index
           
           DO j =1, dim ! --- row direction
              
              Do i=1, dim ! | column direction
                 
                 DO k =1, dim
                    
                    this%ct(i+dim*(j-1),p) = & 
                         this%ct(i+dim*(j-1),p) + &
                         this%eval(k,p) * &
                         this%evec(i+(k-1)*dim,p) * &
                         this%evec(j+(k-1)*dim,p)
                    
                 END DO
                 
              END DO
              
           END DO
           
        END DO
        
        
9999    CONTINUE      
        
        RETURN
        
      END SUBROUTINE particles_compute_ct
      
      
