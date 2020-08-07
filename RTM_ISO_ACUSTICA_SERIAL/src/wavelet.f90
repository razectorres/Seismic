	subroutine wavelet(fonte)
	use parameters, only: Nf,dt,fcorte,Nf2
	implicit none
	
	real,intent(inout)     		:: fonte(Nf)
	!NONE
	!***    parametros locais 
	!***    =================
	integer                   :: n       ! Contador
	real                      :: tf      ! Metade do periodo da funcao gaussiana
    real                      :: fc      ! Frequencia central
    real                      :: t       ! Tempo em segundos
    real                      :: pi
    real                      :: fon_d2g ! Funçao fonte
!***********************************************************************
!**Calculo dos parametros da funçao fonte*******************************
!***********************************************************************   
    pi=4.0*atan(1.0)        
    tf=2.*sqrt(pi)/fcorte                            
    fc=fcorte/(3.*sqrt(pi)) 
!***********************************************************************    
!**Inicializaçao********************************************************
!*********************************************************************** 
    fonte=0.0
!***********************************************************************          
!**Assinatura da fonte sismica******************************************
!***********************************************************************                  
    do n=1,Nf2                
        t=(n-1)*dt-tf           
        fon_d2g=-pi*(pi*fc*t)*(pi*fc*t)
        fon_d2g=exp(fon_d2g)
        fonte(n)=fon_d2g*(1.-2.*pi*(pi*fc*t)*(pi*fc*t))       
    enddo    

	return
	end subroutine wavelet
