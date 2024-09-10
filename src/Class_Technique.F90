      MODULE Class_Technique
        !----------------------------------------------------
        ! Class	      :	Technique
        !----------------------------------------------------
        !
        !  Purpose    :
        !> \brief       Variables and corresponding operations
        !>              for technique quantities, e.g. PPM & MPI.
        !>	   	   
        !>              The variable memebers are public
        !
        !  Remarks      :
        !
        !  References   :
        !
        !  Revisions    : 0.1 03.03.2009
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
        
        USE ppm_module_data_neighlist, ONLY : ppm_type_ptr_to_clist
        USE mcf_header
        USE Class_Tool
        
        
        IMPLICIT NONE
        SAVE
        
#ifdef  __MPI
        
        INCLUDE 'mpif.h'
#endif
        
        TYPE Technique
           !-------------------------------------------------
           ! num_sub : number of sub-domains on this process.
           ! sub_bcdef: whether this sub is located on physical
           !            boundayr.
           !-------------------------------------------------
           
           INTEGER                             :: num_dim
           REAL(MK), DIMENSION(:), POINTER     :: min_phys_t
           REAL(MK), DIMENSION(:), POINTER     :: max_phys_t
           INTEGER, DIMENSION(:), POINTER      :: bcdef
           INTEGER                             :: igroup
           INTEGER                             :: ngroup
           INTEGER                             :: comm
           INTEGER                             :: rank
           INTEGER                             :: num_proc
           CHARACTER(LEN=MAX_CHAR)             :: name_proc
           INTEGER                             :: MPI_PREC
           CHARACTER(LEN=MAX_CHAR)             :: name_MPI_PREC
           INTEGER                             :: decomp
           INTEGER                             :: assig
           REAL(MK)                            :: ghost_size
           INTEGER                             :: topo_id
           REAL(MK), DIMENSION(:,:), POINTER   :: min_sub
           REAL(MK), DIMENSION(:,:), POINTER   :: max_sub
           REAL(MK), DIMENSION(:), POINTER     :: sub_cost
           INTEGER, DIMENSION(:), POINTER      :: sub2proc
           INTEGER, DIMENSION(:), POINTER      :: proc2sub
           INTEGER                             :: num_sub_tot
           INTEGER, DIMENSION(:), POINTER      :: sub_list
           INTEGER                             :: num_sub
           INTEGER, DIMENSION(:,:), POINTER    :: sub_bcdef
           !-------------------------------------------------
           ! Cell list for particles
           !-------------------------------------------------
           
           INTEGER, DIMENSION(:,:), POINTER    :: num_cell_dim_sub
           INTEGER                             :: neighbor_list
           TYPE(ppm_type_ptr_to_clist),DIMENSION(:),POINTER    :: cell_list
           INTEGER, DIMENSION(:,:), POINTER    :: inp
           INTEGER, DIMENSION(:,:), POINTER    :: jnp
           INTEGER                             :: nnp
           
           !-------------------------------------------------
           ! Cell list for colloids
           !-------------------------------------------------
         
           INTEGER, DIMENSION(:,:), POINTER    :: num_cell_dim_sub_c
           INTEGER                             :: neighbor_list_c
           TYPE(ppm_type_ptr_to_clist),DIMENSION(:),POINTER    :: cell_list_c
           INTEGER, DIMENSION(:,:), POINTER    :: inp_c
           INTEGER, DIMENSION(:,:), POINTER    :: jnp_c
           INTEGER                             :: nnp_c
      
           INTEGER                             :: ppm_debug
           INTEGER                             :: ppm_log_unit
           
           TYPE(Tool)                          :: tool

        END TYPE Technique
        
        
        INTERFACE technique_new
           MODULE PROCEDURE technique_init
        END INTERFACE
        
      CONTAINS
        
#include "technique_new.F90"
#include "technique_finalize.F90"
#include "technique_build_list.F90"
#include "technique_get.F90"
        
      END MODULE Class_Technique
      
