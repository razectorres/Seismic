subroutine acoustic_backward_problem(P1,P2,P3,fonte,A,C,vel2,Gs,Gi,Ge,Gd,shot_loop,Sis,Pdir,Imagem)	
  use parameters  
  implicit none
  !***********************************************************************
  !**Variaveis da equacao da onda*****************************************
  !***********************************************************************  
  real,dimension (Nxx,Nzz),intent(inout) :: P1		! Campo em t-dt
  real,dimension (Nxx,Nzz),intent(inout) :: P2		! Campo em t
  real,dimension (Nxx,Nzz),intent(inout) :: P3 	! Campo em t+dt
  !***********************************************************************
  !**Variaveis auxiliares*************************************************
  !***********************************************************************  	     
  integer :: is,js
  real,dimension (Nf),intent(in)   :: fonte	

  !***********************************************************************
  !**Modelo de velocidade*************************************************
  !***********************************************************************
  real,dimension (Nxx,Nzz),intent(inout)  :: A
  real,dimension (Nxx,Nzz),intent(inout)  :: C
  real,dimension (Nxx,Nzz),intent(inout)  :: vel2
  !***********************************************************************
  !**Perfil de amortecimento da camada de cerjan**************************
  !***********************************************************************      
  real,dimension (nzbs+1),intent(in)   :: Gs		! fator de amortec.
  real,dimension (nzbi+1),intent(in)   :: Gi		! fator de amortec.
  real,dimension (nxbe+1),intent(in)   :: Ge		! fator de amortec.
  real,dimension (nxbd+1),intent(in)   :: Gd		! coef. de amortec.

  !***********************************************************************
  !**Dados observado******************************************************
  !***********************************************************************
  real,dimension (Nrec,Ntotal),intent(in)    ::  sis 
  real,dimension (Nxx,Nzz,Ntotal),intent(in) :: Pdir 
  real,dimension (Nxx,Nzz),intent(inout) :: Imagem 

  integer,intent(in) :: shot_loop
  character(len=3)   :: varsnap
  integer :: k_snap,dt_snap 
  k_snap=0
  dt_snap=250

  !***********************************************************************
  !********Inicializacao**************************************************
  !***********************************************************************
  P1 = 0.0 ; P2 = 0.0 ; P3 = 0.0
  !***********************************************************************
  !********Inicio do loop temporal****************************************
  !***********************************************************************
  do n = Ntotal,1,-1						
  !***********************************************************************
  !***********Termo Fonte*************************************************
  !***********************************************************************
    auxrec=1
    Do is=irec,frec,drec
        P2(is,ptobs) = P2(is,ptobs) + sis(auxrec,n)*((vel2(pti,ptj)*dt)**2)
        auxrec = auxrec + 1
    enddo
    !***********************************************************************
    !***********Equacao da Onda*********************************************
    !***********************************************************************					
    call wave_equation(P1,P2,P3,C)	
    !***********************************************************************
    !***********Bordas de Reynolds******************************************
    !***********************************************************************
	  call Reynolds(A,P2,P3)		
    !***********************************************************************
    !***********Bordas de Absorcao - Cerjan*********************************
    !***********************************************************************		
	  call cerjan(Gs,Gi,Ge,Gd,P2,P3)	
    !***********************************************************************
    !***********cerjan******************************************************
    !***********************************************************************
    !IF (mod(n,dt_snap).eq.0) THEN
    !   write(varsnap,'(i3.3)')k_snap
    !   write(*,*)"snapshot = ",varsnap
    !   open(unit=10,file="./outputs/back"//varsnap//".bin",status="replace",access="direct",form="unformatted",recl=4*Nx*Nz)
    !   write(10,rec=1)((P3(is,js),js=jybs,jybi),is=ixbe,ixbd)     
    !   close(10)
    !   k_snap=k_snap + 1    
    !END IF
    
    !***********************************************************************
    !***********Condicao de imagem: Correlacao Cruzada *********************
    !***********************************************************************
	  if (mod(n,sampling).eq.0) then
      Do js=jybs,jybi
        Do is=ixbe,ixbd
          Imagem(is,js) = Imagem(is,js) + (P3(is,js)*Pdir(is,js,(n/sampling)))
        Enddo
      Enddo
    endif
    
    !***********************************************************************
	  !***********Atualizacao do campo de onda********************************
	  !***********************************************************************
    P1 = P2
    P2 = P3
   
     if (mod(n,1000).eq.0)then
	   	write(6,22)	n
	   	22  format(36x,I5)	        						    
	   end if
   
    !***********************************************************************
    !********Fechamento do loop temporal************************************
    !*********************************************************************** 
  end do		
  !***********************************************************************
  !********Gravacao dos imagens*******************************************
  !***********************************************************************			
  open(39,file="./outputs/imagem.bin",status='unknown',&
  & form='unformatted',access='direct',recl=4*Nx*Nz)
  write(39,rec=1)((Imagem(i,k),k=jybs,jybi),i=ixbe,ixbd)
  close(39)


									
end subroutine acoustic_backward_problem
