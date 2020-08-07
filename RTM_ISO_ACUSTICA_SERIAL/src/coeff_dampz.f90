	subroutine coeff_dampz(Gs,Gi,Ge,Gd)
	use parameters, only: Nxx,Nzz,nxbe,nxbd,nzbs,nzbi,ixbd,jybi
	implicit none
	
	real,          intent(inout)     :: Gs(nzbs+1)
	real,          intent(inout)     :: Gi(nzbi+1)
	real,          intent(inout)     :: Ge(nxbe+1)
	real,          intent(inout)     :: Gd(nxbd+1)
	!
	!***    parametros da rotina 
	!***    ====================
	!	
	real		  			:: cofat
	real, parameter   		:: fmin = 0.0025
	real, parameter   		:: fmax = 0.0025
	integer					:: k
	integer					:: ix
	real					:: tmp
!***********************************************************************
!**COEFICIENTES DE AMORTECIMENTO DA BORDA ESQUERDA**********************
!***********************************************************************	
	do ix=1, nxbe +1 
		cofat = fmin + real(nxbe+1-ix) * (fmax-fmin) / real(nxbe)
		tmp   = real((ix-nxbe-1)*(ix-nxbe-1))
		tmp   = cofat*cofat*tmp
		Ge(ix)= exp(-tmp)
	enddo
!***********************************************************************	
!**COEFICIENTES DE AMORTECIMENTO DA BORDA DIREITA***********************	
!***********************************************************************	
	k=1
	do ix=ixbd,Nxx
		cofat = fmax - real(Nxx-ix) * (fmax-fmin) / real(nxbd)
		tmp   = real((k-1)*(k-1))
		tmp   = cofat*cofat*tmp
		Gd(k) = exp(-tmp)
		k     = k+1
	enddo
!************************************************************************	
!**COEFICIENTES DE AMORTECIMENTO DA BORDA SUPERIOR***********************	
!************************************************************************		
	if (nzbs.ne.0) then
		do ix=1, nzbs + 1 
			cofat = fmin + real(nzbs+1-ix) * (fmax-fmin) / real(nzbs)
			tmp   = real((ix-nzbs-1)*(ix-nzbs-1))
			tmp   = cofat*cofat*tmp
			Gs(ix)= exp(-tmp)
		enddo
	endif	
!************************************************************************	
!**COEFICIENTES DE AMORTECIMENTO DA BORDA INFERIOR***********************	
!************************************************************************		
	k=1
	do ix=jybi, Nzz
		cofat = fmax - real(Nzz - ix) * (fmax - fmin) / real(nzbi)
		tmp   = real((k-1)*(k-1))
		tmp   = cofat*cofat*tmp
		Gi(k) = exp(-tmp)
		k	= k+1
	enddo
		
	return
	end subroutine coeff_dampz
