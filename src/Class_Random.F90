      MODULE Class_Random
        !----------------------------------------------------
      	!  Class      :	Random
	!----------------------------------------------------
      	!
      	!  Purpose    :
	!> \brief       Variables and corresponding operations
        !>              for Random quantities.
      	!>	   	
        !>              The variable memebers are public 
        !
        !  Remarks    : Since there is a 'SAVE' after 'IMPLICIT NONE',
        !               all procedures using this Module sharing the
        !               same copy of this Module.
        !
      	!
      	!  References :
     	!
      	!  Revisions  : 0.1 03.03. 2009, original version.
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
           
        USE mcf_header
        
        IMPLICIT NONE
        SAVE
        
        TYPE Random
           !-------------------------------------------------
           ! random_type :  
           ! 1 : uniform distribution
           ! 2 : Gaussian distribution
           !-------------------------------------------------

           PRIVATE
           INTEGER                    :: random_type
           INTEGER                    :: seed
           INTEGER                    :: random_uniform_type
           INTEGER                    :: random_Gaussian_type
           INTEGER                    :: iset
           REAL(MK)                   :: gset
        END TYPE Random
        
        INTERFACE random_new
           MODULE PROCEDURE random_init
        END INTERFACE
        
      CONTAINS
        
#include "random_new.F90"
#include "random_random.F90"
#include "random_random_uniform.F90"
#include "random_random_Gaussian.F90"

        
      END MODULE Class_Random

