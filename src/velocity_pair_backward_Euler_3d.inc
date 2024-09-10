!-- Changed by adolfo --
vij_old(:) = vi_old(:) - vj_old(:)
eij_vij_old = eij(1) * vij_old(1) + &
	    eij(2) * vij_old(2) + &
	    eij(3) * vij_old(3)
!-- Normal lubrication only --
! vij_new(:) = vij_old(:) - aux * eij_vij_old * eij(:)

!-- Brownian Normal lubrication *** aux is different than in the non-Brownian case *** --
! vij_new(:) = vij_old(:) - Aij * aux * eij_vij_old * eij(:) - &
!              aux_kt * eij(:)

!-- Brownian Normal and tangential lubrication *** aux is different than in the non-Brownian case *** --
dPij_eij =  eij(1) * dPij_vec(1) + &
	    eij(2) * dPij_vec(2) + &
	    eij(3) * dPij_vec(3)
vij_new(:) = 1.0_MK/(1.0_MK-Dij) * (vij_old(:) + aux2/num_sweep*dPij_vec(:) + &
	   (Cij - Dij)/(1.0_MK - Cij) * (eij(:) * eij_vij_old + &
           aux2/num_sweep * eij(:) * dPij_eij))

!-- Normal and tangential lubrication as Bertevas --
!!$vij_new(:) = 1.0_MK / (1.0_MK - Bij) * &
!!$	   (vij_old(:) + (Aij - Bij) / (1.0_MK - Aij) * &
!!$	   eij_vij_old * eij(:))

!-- Normal and tangential lubrication as Bertevas and repulsion force--
!vij_new(:) = 1.0_MK / (1.0_MK - Bij) * &
!	   ((vij_old(:) + F_repul_vec(:)) + &
!	   (Aij - Bij) / (1.0_MK - Aij) * &
!	   (eij_vij_old * eij(:)) + F_repul_vec(:))

!-- Normal and tangential lubrication splitted separately --
!vij_new(:) = vij_old(:) -  &
!	   Aij / (Aij - 1.0_MK) * eij_vij_old * eij(:)

!-- Vi and vj are calculated from vij --
vj_new(:) = (this%m(i) * (vi_old(:) - vij_new(:)) + &
	  this%m(j) * vj_old(:)) / (this%m(i) + this%m(j))
vi_new(:) = vij_new(:) + vj_new(:)


!!$!-- What Xin calculated previously
!!$  vi_new(1) = &
!!$       ((eij(1)*eij(3)*(vj_old(3)-vi_old(3))+&
!!$       2*vi_old(1)*eij(3)**2+eij(1)*eij(2)*(vj_old(2)-vi_old(2))+&
!!$       2*vi_old(1)*eij(2)**2+eij(1)**2*vj_old(1)+&
!!$       eij(1)**2*vi_old(1))*Aij-vi_old(1))/ &
!!$       ((2*eij(3)**2+2*eij(2)**2+2*eij(1)**2)*Aij-1)
!!$  vi_new(2) = &
!!$       ((eij(2)*eij(3)*(vj_old(3)-vi_old(3))+&
!!$       2*vi_old(2)*eij(3)**2+eij(2)**2*(vj_old(2)+vi_old(2))+&
!!$       2*eij(1)**2*vi_old(2)+(eij(1)*vj_old(1)-&
!!$       eij(1)*vi_old(1))*eij(2))*Aij-vi_old(2))/&
!!$       ((2*eij(3)**2+2*eij(2)**2+2*eij(1)**2)*Aij-1)
!!$  vi_new(3) = &
!!$       ((eij(3)**2*(vj_old(3)+vi_old(3))+&
!!$       2*eij(2)**2*vi_old(3)+2*eij(1)**2*vi_old(3)+&
!!$       (eij(2)*(vj_old(2)-vi_old(2))+eij(1)*vj_old(1)-&
!!$       eij(1)*vi_old(1))*eij(3))*Aij-vi_old(3))/&
!!$       ((2*eij(3)**2+2*eij(2)**2+2*eij(1)**2)*Aij-1)
!!$  vj_new(1) = &
!!$       ((eij(1)*eij(3)*(vi_old(3)-vj_old(3))+&
!!$       2*vj_old(1)*eij(3)**2+eij(1)*eij(2)*(vi_old(2)-vj_old(2))+&
!!$       2*vj_old(1)*eij(2)**2+eij(1)**2*vj_old(1)+&
!!$       eij(1)**2*vi_old(1))*Aij-vj_old(1))/&
!!$       ((2*eij(3)**2+2*eij(2)**2+2*eij(1)**2)*Aij-1)
!!$  vj_new(2) = &
!!$       ((eij(2)*eij(3)*(vi_old(3)-vj_old(3))+&
!!$       2*vj_old(2)*eij(3)**2+eij(2)**2*(vj_old(2)+vi_old(2))+&
!!$       2*eij(1)**2*vj_old(2)+(eij(1)*vi_old(1)-&
!!$       eij(1)*vj_old(1))*eij(2))*Aij-vj_old(2))/&
!!$       ((2*eij(3)**2+2*eij(2)**2+2*eij(1)**2)*Aij-1)
!!$  vj_new(3) = ((eij(3)**2*(vj_old(3)+vi_old(3))+&
!!$       2*eij(2)**2*vj_old(3)+2*eij(1)**2*vj_old(3)+&
!!$       (eij(2)*(vi_old(2)-vj_old(2))-eij(1)*vj_old(1)+&
!!$       eij(1)*vi_old(1))*eij(3))*Aij-vj_old(3))/&
!!$       ((2*eij(3)**2+2*eij(2)**2+2*eij(1)**2)*Aij-1)
