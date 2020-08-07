	subroutine Reynolds(A,P2,P3)
	use parameters, only: Nxx,Nzz,nzbs
	implicit none
	
	integer		                     :: i,k	
	real*4,        intent(in)        :: A(Nxx,Nzz)
	real*4,        intent(in)        :: P2(Nxx,Nzz)		
	real*4,        intent(inout)     :: P3(Nxx,Nzz)
  !***********************************************************************
  !**Topo*****************************************************************
  !***********************************************************************
   if (nzbs.ne.0) then
		  !$omp parallel do 
      do k=1,2
	       do i=3,Nxx-2
	          P3(i,k) = P2(i,k)+A(i,k)*(P2(i,k+1)-P2(i,k))
	       enddo
		  enddo
      !$omp end parallel do
   endif
  !***********************************************************************	
  !**Base*****************************************************************
  !***********************************************************************		
	!$omp parallel do
  do k=Nzz-1,Nzz
		 do i=3,Nxx-2
	       P3(i,k) = P2(i,k )-A(i,k)*(P2(i,k)-P2(i,k-1))
		 enddo
	enddo
  !$omp end parallel do
  !***********************************************************************
  !**Lateral Esquerda e Lateral Direita***********************************
  !***********************************************************************
	!$omp parallel do
  do k = 3, Nzz-2
		 do i = 1, 2
	   	P3(i,k) = P2(i,k)+A(i,k)*(P2(i+1,k)- P2(i,k)) !esquerda
	   enddo

	   do i = Nxx-1, Nxx
	   	P3(i,k) = P2(i,k)-A(i,k)*(P2(i,k)-P2(i-1,k)) !direita
	   enddo
	enddo
  !$omp end parallel do 
	end subroutine Reynolds
