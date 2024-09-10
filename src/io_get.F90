!--------------------------------------------------
! Subroutine  :  io_get_*
!--------------------------------------------------
!
! Purpose     : Get routines of Class IO.
!
! Reference   :
!
! Remark      :
!
! Revisions   : V0.1 10.03.2010, original version.
!
!--------------------------------------------------
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
!-------------------------------------------------  

      INTEGER FUNCTION io_get_output_particles_relax_freq_step(this,&
           stat_info)
        
        !----------------------------------------------------
        ! Arguments.
        !----------------------------------------------------
        
        TYPE(IO), INTENT(IN)            :: this             
        INTEGER, INTENT(OUT)            :: stat_info
        
        stat_info = 0

        io_get_output_particles_relax_freq_step = &
             this%output_particles_relax_freq_step
        
        RETURN
        
      END FUNCTION io_get_output_particles_relax_freq_step
      
      
      INTEGER FUNCTION io_get_statistic_relax_freq_step(this,&
           stat_info)
        
        !----------------------------------------------------
        ! Arguments.
        !----------------------------------------------------
        
        TYPE(IO), INTENT(IN)            :: this             
        INTEGER, INTENT(OUT)            :: stat_info
        
        stat_info = 0

        io_get_statistic_relax_freq_step = &
             this%statistic_relax_freq_step
        
        RETURN
        
      END FUNCTION io_get_statistic_relax_freq_step
