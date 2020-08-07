subroutine filtro_laplaciano(Imagem,Imagem_filtrada)
  use parameters  
  implicit none

  !real,dimension (Nxx,Nzz),intent(in)        :: Vel   ! Modelo de velocidade
  real,dimension (Nxx,Nzz),intent(inout)     :: Imagem_filtrada  ! sismograma
  real,dimension (Nxx,Nzz),intent(in)        :: Imagem  ! sismograma

  integer :: is,js

  Imagem_filtrada = 0.0


  Do js=jybs,jybi
	  Do is=ixbe,ixbd
	    Imagem_filtrada(is,js)=( (-Imagem(is-2,js) +16*Imagem(is-1,js)-30*Imagem(is,js)+ & 
                           16*Imagem(is-1,js)-Imagem(is+2,js))/(h**2.0)+ & 
                           (-Imagem(is,js-2) +16*Imagem(is,js-1)-30*Imagem(is,js) +&
                           16*Imagem(is,js-1)-Imagem(is,js+2))/(h**2.0)  ) 
    End do
  End do

   
  



  open(39,file="./outputs/imagem_filtrada.bin",status='unknown',&
  & form='unformatted',access='direct',recl=4*Nx*Nz)
  write(39,rec=1)((Imagem_filtrada(i,k),k=jybs,jybi),i=ixbe,ixbd)
  close(39)
									
end subroutine filtro_laplaciano
