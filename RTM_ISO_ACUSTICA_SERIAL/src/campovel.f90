!======================================================================
!	   SUBROTINA UTILIZADA PARA ADICIONAR BORDAS AO REDOR DO MODELO
!======================================================================
!
!	LABORATORIO MULTIDISCIPLINAR DE MODELAGEM - LAB2M/COPPE/UFRJ
!======================================================================
!
	subroutine campovel(velfile,v)
	use parameters, only:Nx,Nxx,Nz,Nzz,nzbs,nzbi,nxbe,nxbd,ixbe,ixbd,jybs,jybi,Nag,bin_par
	implicit none
	character(len=100), intent(in) 	  :: velfile
	real,          intent(inout)      :: v(Nxx,Nzz)
!***********************************************************************
!**Parametros da rotina*************************************************
!***********************************************************************
	integer 		:: ix
	integer 		:: iy
!***********************************************************************
!**Inicializaçao********************************************************
!***********************************************************************
	v  = 0.0
!***********************************************************************
!**Dominio de interesse*************************************************
!***********************************************************************
	open(12,file=velfile,status='unknown',form='unformatted',access=&
	& 'direct',recl=bin_par*Nx*Nz)
	read(12,rec=1)((v(ix,iy),iy=jybs,jybi),ix=ixbe,ixbd)
	close(12)
!***********************************************************************
!***preenchimento da camada superior************************************
!***********************************************************************
	if ((nzbs.ne.0).and.(Nag.eq.0)) then
		do iy=1,nzbs
			do ix=ixbe,ixbd
				v(ix,iy) = v(ix,jybs)
			enddo
		enddo
	endif

	if (Nag.ne.0) then
		do iy=1,nzbs+Nag
			do ix=1,Nxx
				v(ix,iy) = 1500.0
			enddo
		enddo
	endif
!***********************************************************************
!***preenchimento da camada esquerda************************************
!***********************************************************************
	!$omp parallel do
  do iy=jybs,jybi
	    do ix=1,nxbe
	      v(ix,iy) = v(ixbe,iy)
	    enddo
	enddo
  !$omp parallel do
!***********************************************************************
!***preenchimento da camada direita*************************************
!***********************************************************************
	!$omp parallel do
  do iy=jybs,jybi
		do ix=ixbd+1,Nxx
			v(ix,iy) = v(ixbd,iy)
		enddo
	enddo
  !$omp end parallel do
!***********************************************************************
!***preenchimento da camada inferior************************************
!***********************************************************************
	!$omp parallel do
  do iy=jybi+1,Nzz
	    do ix=ixbe,ixbd
	      v(ix,iy) = v(ix,jybi)
	    enddo
	enddo
  !$omp end parallel do
!***********************************************************************
!***corners*************************************************************
!***********************************************************************
	if ((nzbs.ne.0).and.(Nag.eq.0)) then
	  !$omp parallel do	
    do iy=1,nzbs
			do ix=1,nxbe
				v(ix,iy)=v(ixbe,jybs)     		! corner top-left
			enddo
		enddo
    !$omp end parallel do
	endif
  !$omp parallel do
	do iy=jybi+1,Nzz
		do ix=1,nxbe
			v(ix,iy)=v(ixbe,jybi)				! corner bottom-left
		enddo
	enddo
  !$omp end parallel do

	if ((nzbs.ne.0).and.(Nag.eq.0)) then
    !$omp parallel do
		do iy=1,nzbs
			do ix=1,nxbe
				v(ix+ixbd,iy)=v(ixbd,jybs)		! corner top-right
			enddo
		enddo
    !$omp end parallel do
	endif

  !$omp parallel do
	do iy=jybi+1,Nzz
		do ix=ixbd+1,Nxx
			v(ix,iy)=v(ixbd,jybi)		        ! corner bottom-right
		enddo
	enddo
  !$omp end parallel do

	return
	end subroutine campovel

