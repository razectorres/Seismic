	module parameters

!***********************************************************************
!***Contadaores*********************************************************
!***********************************************************************
    integer	   					:: i,ik,k			! contador da direcao x
    integer	  					:: n,nn,p,ii		! contador de tempo
    integer	  				      	:: ifreq			! contador de frequencia
    integer,parameter       	:: bin_par=4,sampling=10
!***********************************************************************
!***variaveis da modelagem**********************************************
!***********************************************************************
    integer						  :: Ntotal			! tempo de modelagem (input-passos)
    real        				:: dt				! incremento temporal (input-segundos)
    real        				:: h				! espacamento da malha regular
!***********************************************************************
!***variaveis do modelo de velocidade***********************************
!***********************************************************************
    integer    					    :: Nx				! dimencao na direcao x (input)
    integer             		:: Nz       		! dimencao na direcao z (input)
    integer             		:: Nxx       		! dimencao na direcao x (input)
    integer             		:: Nzz       		! dimencao na direcao z (input)
    integer             		:: ixbe
    integer             		:: ixbd
    integer             		:: jybs
    integer             		:: jybi
    character(len=100)  		:: velfile, velfile2			! nome do arquivo de velocidade (input)
!***********************************************************************
!***variaveis da camada de amortecimento********************************
!***********************************************************************
    integer             		:: nxbe       		! num de pts da camada esquerda (input)
    integer             		:: nxbd       		! num de pts da camada direita  (input)
    integer             		:: nzbs       		! num de pts da camada superior (input)
    integer             		:: nzbi       		! num de pts da camada inferior (input)
    integer             		:: Nag				! num de pts da camada de agua
!***********************************************************************
!***variaveis associadas a fonte****************************************
!***********************************************************************
    real   	        		  	:: fcorte			! frequencia de corte
    real   	       				  :: fi,ff,df
    integer           			:: Nf,Nf2  			! num de passos de tempo da fonte
    integer           			:: Nifreq
!***********************************************************************
!***variaveis associadas a geometria da aquisicao***********************
!***********************************************************************
   integer    	     			:: kobs				! profundidade de observacao
   integer          			:: ptobs
   integer           			:: ixf,kzf			! indice da coordenada da fonte sismica
   integer           			:: pti,ptj
   integer 						    :: ishot,Nshot    	! tiro inicial, tiro final e passo de tiro
   integer           			:: dshot    		! intervalo de tiro na direcao x
   integer           			:: drec				! intervalo entre receptores na direcao x
   integer						    :: irec,frec
   integer 						    :: Nrec
   integer 						    :: auxrec
!***********************************************************************
!***variaveis auxiliares************************************************
!***********************************************************************
    integer         			:: status
    real,parameter  			:: pi=3.141593
	  real      						:: v
!***********************************************************************
!***definicao de variaveis de medicao de tempo de computacao************
!***********************************************************************
    integer 	    			  :: clock_start
    integer 	    		  	:: clock_finish
    integer        				:: ticks_per_second
    real   	        			:: system_time
!***********************************************************************
!***variaveis de saida**************************************************
!***********************************************************************
    character*100   	    	:: entrada1="./inputs/dado_**Hz.bin"
    character*100   	    	:: saida1="./outputs/mod_**Hz.bin"

	contains
	end module parameters
