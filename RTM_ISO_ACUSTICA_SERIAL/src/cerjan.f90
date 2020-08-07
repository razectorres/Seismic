	subroutine cerjan(Gs,Gi,Ge,Gd,P2,P3)
	use parameters, only: Nxx,Nzz,nzbs,nzbi,nxbe,nxbd,ixbd,jybi 
	implicit none

	real,          intent(in)        :: Gs(nzbs+1)
	real,          intent(in)        :: Gi(nzbi+1)
	real,          intent(in)        :: Ge(nxbe+1)
	real,          intent(in)        :: Gd(nxbd+1)
	real,          intent(inout)     :: P2(Nxx,Nzz)	
	real,          intent(inout)     :: P3(Nxx,Nzz)	

	! 
	!***    parametros da rotina 
	!***    ====================
	! 
	integer 			:: ix
	integer 			:: iy
	real				:: tmp     
  !***********************************************************************
  !Aplicacao de amortecimento numerico a borda inferior*******************
  !***********************************************************************  
    do iy=jybi, nzz
			do ix=1,nxx	
				tmp = Gi(iy-jybi+1)
				P2(ix,iy) = tmp * P2(ix,iy)
				P3(ix,iy) = tmp * P3(ix,iy)	 
			enddo
		enddo
          
  !***********************************************************************
  !**Aplicacao de amortecimento numerico a borda superior*****************
  !***********************************************************************
		if (nzbs.ne.0) then     
			do iy=1,nzbs+1
				do ix=1,Nxx	  
					tmp = Gs(iy)
					P2(ix,iy) = tmp * P2(ix,iy)
					P3(ix,iy) = tmp * P3(ix,iy)     
				enddo
			enddo
		endif
  !***********************************************************************		
  !**Aplicacao de amortecimento numerico ao bordo esquerdo****************
  !*********************************************************************** 
  do iy=1, nzz
	    do ix=1,nxbe +1 
			  tmp = Ge(ix)
			  P2(ix,iy) = tmp * P2(ix,iy)
			  P3(ix,iy) = tmp * P3(ix,iy)	
	    enddo
	enddo
  !***********************************************************************
  !**Aplicacao de amortecimento numerico ao bordo direito*****************
  !***********************************************************************
  do iy=1, Nzz
		do ix=ixbd,Nxx
			  tmp = Gd(ix-ixbd+1)
			  P2(ix,iy) = tmp * P2(ix,iy)
			  P3(ix,iy) = tmp * P3(ix,iy)	
	  enddo
	enddo

	return
	end subroutine cerjan
