!-- Changed by adolfo --
vij_old(:) = vi_old(:) - vj_old(:)
eij_vij_old = eij(1) * vij_old(1) + &
	    eij(2) * vij_old(2) + &
	    eij(3) * vij_old(3)
!-- Normal lubrication only --
! vij_new(:) = vij_old(:) - aux * eij_vij_old * eij(:)

!-- Normal and tangential lubrication as Bertevas --
!vij_new(:) = 1.0_MK / (1.0_MK - Bij) * &
!	   (vij_old(:) + (Aij - Bij) / (1.0_MK - Aij) * &
!	   eij_vij_old * eij(:))

!-- Normal and tangential lubrication splitted separately --
vij_new(:) = (vij_old(:) -  &
	   Bij * eij_vij_old * eij(:)) / (1.0_MK - Bij)
 
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
