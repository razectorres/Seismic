  subroutine wave_equation(P1,P2,P3,C)
	use parameters, only: Nxx,Nzz,dt,h

	implicit none


	integer		                    	 :: i,k	
	real,        intent(in)          :: C(Nxx,Nzz)
	real,        intent(in)          :: P1(Nxx,Nzz)
	real,        intent(in)          :: P2(Nxx,Nzz)
	real,        intent(inout)       :: P3(Nxx,Nzz)
   

  !!$OMP PARALLEL DO PRIVATE(K,i)
  !!$OMP& SHARED(P1,P3,P2,C)
  !!$OMP& SCHEDULE(STATIC,4)
  !!$OMP& REDUCTION(+:P3)
  do k = 3, Nzz-2
	 do i = 3, Nxx-2
			P3(i,k)= C(i,k)*(P2(i+2,k)+P2(i-2,k)+P2(i,k+2)+P2(i,k-2)   &
		             -16.*(P2(i+1,k)+P2(i-1,k)+P2(i,k+1)+P2(i,k-1))  &
                     + 60.*P2(i,k)) +2.*P2(i,k) -P1(i,k)
	 enddo 
  enddo 
  !!$OMP END PARALLEL DO

	end subroutine  wave_equation
