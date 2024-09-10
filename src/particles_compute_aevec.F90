      SUBROUTINE particles_compute_aevec(this,num,stat_info)
        !----------------------------------------------------
        ! Subroutine :  particles_compute_aevec
        !----------------------------------------------------
        !
        ! Purpose    :  Compute the accleration of egenvectors
        !               of particles, in case of egenvector
        !               dynamics for Non-Newtonian oldroyd-B
        !               model.
        !
        ! Reference  : Vazquez-Quesada, Ellero, Espanol
        !              Phyical Review E 79. 056707, 2009.
        !
        ! Remark     :
        !
        ! Revision   : V0.1  25.08.2010. The indexes of any
        !              tensors were changed.  
        !              An innecesary line that does (H = 0)
        !              was deleted. (Adolfo) 
        ! 
        !              V0.1  04.08.2009, original version.
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
        ! num            : number of particles needed to be 
        !                  updated, i.e. first num particles 
        !                  in this%x are operated.
        ! lambda         : coefficient required.
        ! stat_info      : return flag of status.
        !----------------------------------------------------
        
        TYPE(Particles), INTENT(INOUT)          :: this
        INTEGER, INTENT(IN)                     :: num
        INTEGER, INTENT(OUT)                    :: stat_info
        
        !----------------------
        ! Local variables
	!----------------------

        INTEGER                                 :: stat_info_sub
        INTEGER                                 :: dim
        INTEGER                                 :: i,j,k,p
        REAL(MK), DIMENSION(3,3)                :: H
        REAL(MK)                                :: eval_tolerance
   
        !-------------------------------
        ! Initialization of variables.
        !-------------------------------
        
        stat_info     = 0
        stat_info_sub = 0

        !--------------------------------
        ! Calculation only for real particles.
        !--------------------------------
        
        IF( num > this%num_part_real) THEN
           PRINT *, "particles_compute_aevec : ", &
                "num > num_part_real, wrong !"
           stat_info = -1
           GOTO 9999      
        END IF
        
        dim = physics_get_num_dim(this%phys,stat_info_sub)
        
        IF(ASSOCIATED(this%aevec)) THEN
           DEALLOCATE(this%aevec)
        ENd IF
        
        ALLOCATE(this%aevec(dim**2,num))
        this%aevec(:,:) = 0.0_MK
        
        eval_tolerance = &
             physics_get_eval_tolerance(this%phys,stat_info_sub)
        
        H(1:dim,1:dim) = 0.0_MK
        
        !-------------------------------------------
        ! Compute accleration of egenvectors using 
        ! egenvector dynamics velocity gradient 
        ! tensor, egenvalues and egenvectors.
        !------------------------------------------
        
        DO p = 1, num ! particle index
           
           DO j = 1, dim ! --- row direction
              DO i =1, dim ! | column direction
                 
                 IF ( i/=j ) THEN
                    
                    IF ( ABS(this%eval(i,p)-this%eval(j,p)) &
                         < eval_tolerance ) THEN
                       
!!$                       H(i,j) = this%evgt(i+dim*(j-1),p) 

                       H(i,j) = 0.0_Mk
                       
                    ELSE
                       
!!$                       H(i,j) = &
!!$                            (this%eval(i,p) * this%evgt(i+dim*(j-1),p) + &
!!$                            this%eval(j,p)  * this%evgt(j+dim*(i-1),p)) / &
!!$                            ( this%eval(i,p) - this%eval(j,p) )

                       H(i,j) = &
                            (this%eval(i,p) * this%evgt(j+dim*(i-1),p) + &
                            this%eval(j,p)  * this%evgt(i+dim*(j-1),p)) / &
                            ( this%eval(i,p) - this%eval(j,p) )

                       
                    END IF
                    
                 END IF
                 
              END DO
              
           END DO
           
           DO j= 1,dim ! --- row direction
              
              DO i = 1,dim ! |  column direction
                 
                 DO k =1,dim ! | dimension
                    
                    this%aevec(i+dim*(j-1),p) =  &
                         this%aevec(i+dim*(j-1),p) + &
                         H(j,k) * this%evec(i+dim*(k-1),p)
                    
                 END DO ! k
                 
              END DO ! i
              
           END DO ! j
           
        END DO ! p
        
        
9999    CONTINUE      
        
        RETURN
        
      END SUBROUTINE particles_compute_aevec
      
      
