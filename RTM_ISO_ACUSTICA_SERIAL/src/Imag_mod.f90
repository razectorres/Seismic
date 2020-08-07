subroutine Imag_mod(Vel2,pcor,Imagem,Imagem_Pcor)
  use parameters  
  implicit none

  real,dimension (Nxx,Nzz),intent(in)        :: Vel2   
  real,dimension (Nxx,Nzz),intent(inout)     :: Imagem,Imagem_Pcor
  real,dimension (Nxx,Nzz),intent(in)        :: Pcor
   

  integer :: is,js

  Imagem_Pcor = 0.0

   Do js=jybs,jybi
   	 Do is=ixbe,ixbd
   	    Imagem_pcor(is,js)=Imagem(is,js)/Pcor(is,js)		
   	 End do
   End do

  open(39,file="./outputs/imagem_src.bin",status='unknown',&
  & form='unformatted',access='direct',recl=4*Nx*Nz)
  write(39,rec=1)((Imagem_Pcor(is,js),js=jybs,jybi),is=ixbe,ixbd)
  close(39)

 
									
end subroutine Imag_mod
