program RTM

	use parameters
	implicit none
	
	character(len=100) :: filename, filename_grad, filename_shot, vel_string
	character(len=10)  :: itstr, freqstr, shotstr
	integer            :: csnap=0,j
	character(len=3)   :: num_snap
	integer            :: iteracao
	!***********************************************************************
	!**Variaveis da equacao da onda*****************************************
	!***********************************************************************
	real,allocatable,dimension (:,:) :: P1    	! Campo em t-dt
	real,allocatable,dimension (:,:) :: P2		    ! Campo em t
	real,allocatable,dimension (:,:) :: P3	    ! Campo em t+dt
	real,allocatable,dimension (:,:) :: vel_field
	!***********************************************************************
	!**Variavel para a correlacao********************************************
	!***********************************************************************
	real,allocatable,dimension (:,:,:) :: PDir,Pder		
	real,allocatable,dimension (:,:) :: Pcor,Prev,P_der,Fder_z 
	integer :: is,js
	
	!***********************************************************************
	!**Modelo de velocidade ************************************************
	!***********************************************************************
	real,allocatable,dimension (:,:) :: Vel,vel2 ! Modelo de velocidade
	!***********************************************************************
	!**Perfil de amortecimento da camada de cerjan**************************
	!***********************************************************************
	real,allocatable,dimension (:)   :: Gs		! fator de amortec.
	real,allocatable,dimension (:)   :: Gi		! fator de amortec.
	real,allocatable,dimension (:)   :: Ge		! fator de amortec.
	real,allocatable,dimension (:)   :: Gd		! coef. de amortec.
	!***********************************************************************
	!**Parametros da fonte: Ricker de fase zero ****************************
	!***********************************************************************
	real,allocatable,dimension (:)   :: fonte	! fonte
	!***********************************************************************
	!**Dados observado******************************************************
	!***********************************************************************
	real,allocatable,dimension (:,:) :: Sis,sis_der,sis_2,sis_new      ! sismograma modelado
	
	!***********************************************************************
	!**Variaveis auxiliares*************************************************
	!***********************************************************************
	real,allocatable,dimension (:,:) :: C,C2
	real,allocatable,dimension (:,:) :: A,A2
	
	!***********************************************************************
	!**Variaveis RTM********************************************************
	!***********************************************************************
	real,allocatable,dimension (:,:) :: Imagem,Imagem_mod,Imagem_filtrada
  real,allocatable,dimension (:,:) :: Imagem_Pcor,Imagem_Prev,Imagem_Pcor_Prev
	integer :: ntotal_sampling  
	!***********************************************************************
	!**Instante de tempo inicial********************************************
	!***********************************************************************
	call system_clock(clock_finish,ticks_per_second)
	clock_start = clock_finish
	!***********************************************************************
	!**Leitura do arquivo de parametros*************************************
	!***********************************************************************
	open(10,file="./inputs/parametros_acustico_2D.txt",status="unknown")
	read(10,*)velfile
	read(10,*)velfile2
	read(10,*)Nx,Nz
	read(10,*)nxbe,nxbd
	read(10,*)nzbs,nzbi
	read(10,*)Nag
	read(10,*)h,dt
	read(10,*)Ntotal
	read(10,*)fi,ff,df
	read(10,*)kobs,kzf
	read(10,*)ishot,dshot,Nshot
	read(10,*)irec,drec,Nrec
	close(10)
	!***********************************************************************
	!**Dimensoes do modelo modificado***************************************
	!***********************************************************************
	Nxx = nxbe + Nx + nxbd ; Nzz = nzbs + Nz + nzbi + Nag
	!***********************************************************************
	!**Identificacao do inicio e do fim do modelo de interesse**************
	!***********************************************************************
	ixbe  = nxbe + 1 ; ixbd  = Nx + nxbe
	jybs  = nzbs + Nag + 1 ; jybi  = Nz + nzbs + Nag
	!***********************************************************************
	!**Posicao da fonte*****************************************************
	!***********************************************************************
	ptj   = kzf + nzbs  ;  ishot = ishot + nxbe
	!***********************************************************************
	!**Datum da estacao receptora*******************************************
	!***********************************************************************
	ptobs = kobs + nzbs
	
	!irec= irec + nxbe +1   ; frec = irec + (drec*Nrec)
	irec= irec + nxbe    ; frec = irec + (drec*Nrec)
	!***********************************************************************
	!**Numero de intervalos de frequencias**********************************
	!***********************************************************************
	Nifreq=int((ff-fi)/df) + 1
	!***********************************************************************
	!**Calculos auxiliares**************************************************
	!***********************************************************************
	Nf = int(4.*sqrt(pi)/(fi*dt)) + 1
	!***********************************************************************
	!**Impressao de parametros**********************************************
	!***********************************************************************
	print '(2/,8x,"=====================================================================")'
	print '(8x,   "=====================================================================")'
	print '(8x,"      PROGRAMA DE MODELAGEM ACUSTICA 2D NO DOMINIO DO TEMPO          	")'
	print '(8x,   "=====================================================================")'
	print '(8x,   "=====================================================================")'
	write(6,1) Nx,Nz ; write(6,2) nxbe ; write(6,3) h ; write(6,4) Nshot
	write(6,5) h*(ishot-1) ; write(6,6) h*dshot ; write(6,7) Nx
	write(6,8) h ; write(6,9) h*(kobs-1) ; write(6,10) fi
	write(6,11) df ; write(6,12) ff
	1 format(2/,15x,"Dimensao do modelo ............... ",1x,I4,1x,"X",I4,1x,"pts",/)
	2 format(15x,"Espessura da borda de cerjan......... ",1x,I4,1x,"pts",/)
	3 format(15x,"Espacamento da malha ................ ",1x,f7.2,1x,"m",/)
	4 format(15x,"Numero de tiros ..................... ",1x,I4,/)
	5 format(15x,"Posicao do primeiro tiro ............ ",1x,f7.2,1x,"m",/)
	6 format(15x,"Intervalo entre tiros ............... ",1x,f6.2,1x,"m",/)
	7 format(15x,"Numero de receptores ................ ",1x,I4,/)
	8 format(15x,"Intervalo entre receptores........... ",1x,f6.2,1x,"m",/)
	9 format(15x,"Datum da estacao receptora .......... ",1x,f6.2,1x,"m",/)
	10 format(15x,"Frequencia inicial .................. ",1x,f6.2,1x,"Hz",/)
	11 format(15x,"Intervalo de frequencia ............. ",1x,f6.2,1x,"Hz",/)
	12 format(15x,"Frequencia final .................... ",1x,f6.2,1x,"Hz",/)
	
	!***********************************************************************
	!**Alocacao de variaveis************************************************
	!***********************************************************************
	 ntotal_sampling = Ntotal/sampling
	!alocacao do perfil de amortecimento Cerjan
	allocate(Gs(nzbs+1), Gi(nzbi+1), Ge(nxbe+1), Gd(nxbd+1))
	!alocacao dos parametros da DF
	allocate(Vel(Nxx,Nzz), Vel2(Nxx,Nzz), P1(Nxx,Nzz), P2(Nxx,Nzz), P3(Nxx,Nzz), A(Nxx,Nzz), &
	& C(Nxx,Nzz), A2(Nxx,Nzz), C2(Nxx,Nzz), fonte(Nf))
	!alocacao parametros de IO
	allocate(Sis(Nrec,Ntotal), &
           PDir(Nxx,Nzz,ntotal_sampling), Pcor(Nxx,Nzz),&
           Imagem(Nxx,Nzz), &
           Imagem_mod(Nxx,Nzz), Imagem_filtrada(Nxx,Nzz),PDer(Nxx,Nzz,ntotal_sampling), &
           Prev(Nxx,Nzz),Imagem_Pcor(Nxx,Nzz),Imagem_Prev(Nxx,Nzz),Imagem_Pcor_Prev(Nxx,Nzz), &
           P_der(Nxx,Nzz),Fder_z(Nxx,Nzz),vel_field(Nxx,Nzz))

	!***********************************************************************
	!**Monta o campo de velocidade******************************************
	!***********************************************************************
	call campovel(velfile,vel)
	call campovel(velfile2,vel2)
	!***********************************************************************
	!**Fatores Multiplicativos para Borda de Cerjan*************************s
	!***********************************************************************
	call coeff_dampz(Gs,Gi,Ge,Gd)

	!***********************************************************************
  !**Calculos auxiliares**************************************************
  !***********************************************************************
	A(:,:)=vel(:,:)*(dt/h)
	C(:,:)=-A(:,:)*A(:,:)/12.
	
	A2(:,:)=vel2(:,:)*(dt/h)
	C2(:,:)=-A2(:,:)*A2(:,:)/12.
  
	do ifreq=1,Nifreq !frequency loop start

		!***********************************************************************
		!*****Frequencia de corte***********************************************
		!***********************************************************************
		fcorte=fi+(ifreq-1)*df
		!***********************************************************************
		!*****Numero de passos de fonte*****************************************
		!***********************************************************************
		Nf2 = int(4.*sqrt(pi)/(fcorte*dt)) + 1
		!***********************************************************************
		!*****Formato de saida para inversao de cada banda de frequencia********
		!***********************************************************************
		write(6,16)
		write(6,17) 0,fcorte
		write(6,18)
		16 format(/,15x,"===============================================")
		17 format(22x,"BANDWIDTH DE FREQUENCIA :",I2,1x,"-",1x,f5.2,1x,"Hz")
		18 format(15x,"==============================================="/)

		!***********************************************************************
		!*****Fonte Sismica*****************************************************
		!***********************************************************************
		call wavelet(fonte)

    
    !==================================
		! initialization
		!==================================
		 Pdir = 0.0; Pcor = 0.0; Imagem = 0.0

		print *,' '

		do p=1,Nshot !first shot loop start
      write(*,*) ""; write(*,*) ""
		  call progress_bar(p,Nshot)
      write(*,*) ""; write(*,*) ""  
      write(*,*) "generating the seismic data"
      call acoustic_forward_problem_real(P1,P2,P3,A,C,fonte,Gs,Gi,Ge,Gd,p,sis,&
           vel)
      write(*,*) "forward modeling"
			call acoustic_forward_problem(P1,P2,P3,A2,C2,vel2,fonte,Gs,Gi,Ge,Gd,p,PDir,Pcor)
      write(*,*) "reverse modeling"
			call acoustic_backward_problem(P1,P2,P3,fonte,A2,C2,vel2,Gs,Gi,Ge,Gd,p,Sis,Pdir,Imagem)
      call Imag_mod(Vel2,pcor,Imagem,Imagem_Pcor)
		end do !shot loop end

		

    !call filtro_laplaciano(Imagem_Prev,Imagem_filtrada) 

	enddo !frequency loop end

  !close(39)

	close(22)
	!***********************************************************************
	!**Instante de tempo final**********************************************
	!***********************************************************************
	call system_clock(clock_finish,ticks_per_second)
	system_time=(clock_finish-clock_start+1)/(1.0*ticks_per_second)
	
	print '(2/,10x,"====================================================================")'
	print*, "             TEMPO DE EXECUCAO : ",system_time
	print '(10x,"====================================================================",2/)'

end program RTM
