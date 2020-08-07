	subroutine fonte_virtual(PDir,vel)!,P1,P2,P3,C)

	use parameters, only: Nxx,Nzz,Ntotal,Nrec,ixbe,ixbd,jybs,jybi,dt,sampling
	
    implicit none

	integer		                 	    :: i,k,t1	
	real,        intent(inout)      :: PDir(Nxx,Nzz,Ntotal)
  !real,        intent(inout)      :: Pnew(Nxx,Nzz)
  real,        intent(in)         :: Vel(Nxx,Nzz)
	real                            :: Pder(Nxx,Nzz,Ntotal)
 ! real,intent(inout) :: P1(Nxx,Nzz)		
 ! real,intent(inout) :: P2(Nxx,Nzz)		
 ! real,intent(inout) :: P3(Nxx,Nzz)
 ! real,intent(inout)  :: A(Nxx,Nzz),A2(Nxx,Nzz)
 ! real,intent(inout)  :: C(Nxx,Nzz),C2(Nxx,Nzz)
  
 ! A(:,:)=vel(:,:)*(dt/h)
!	C(:,:)=-A(:,:)*A(:,:)/12.
	
	!A2(:,:)=vel2(:,:)*(dt/h)
	!C2(:,:)=-A2(:,:)*A2(:,:)/12. 	
	
  !Pder=0.0
	

	!Do t1=1,Ntotal
  ! if (mod(t1,sampling).eq.0) then
	!	Do k=jybs,jybi
	!		Do i=ixbe,ixbd
	!			Pder(i,k,t1)=((PDir(i,k,t1-1)-2.0*PDir(i,k,t1)+PDir(i,k,t1+1))/(dt*dt))
	!		enddo
	!	enddo
  ! endif
	!enddo

  !Do t1=1,Ntotal

  !  if (n.le.Nf2) then
	!	 Do k=jybs,jybi
	!		 Do i=ixbe,ixbd
	!      	Pnew(pti,ptj) = Pnew(pti,ptj) + fonte(t1) 
	!		 enddo
	!	 enddo
  !  endif


  !  call wave_equation(P1,P2,P3,C)


	!enddo

  !Pdir=Pder

	end subroutine fonte_virtual
