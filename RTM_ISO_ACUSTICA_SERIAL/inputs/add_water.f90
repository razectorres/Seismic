program modelo_2

  implicit none

  integer, parameter :: Nx=737, Nz=240, bin_par=4
  real :: eps(Nx,Nz),del(Nx,Nz),rho(Nx,Nz),eta(Nx,Nz)
  real :: a1111(Nx,Nz),a3333(Nx,Nz),c1111(Nx,Nz),c3333(Nx,Nz)
  integer :: i,j

  !input files
  open(10,file="rho737x240.bin",status='old',form='unformatted',access= &
  & 'direct',recl=bin_par*Nx*Nz)      
  read(10,rec=1)((rho(i,j),i=1,Nx),j=1,Nz)	
  close(10)

  open(10,file="etamodel.bin",status='old',form='unformatted',access= &
  & 'direct',recl=bin_par*Nx*Nz)      
  read(10,rec=1)((eta(i,j),i=1,Nx),j=1,Nz)	
  close(10)

  open(10,file="a1111model.bin",status='old',form='unformatted',access= &
  & 'direct',recl=bin_par*Nx*Nz)      
  read(10,rec=1)((a1111(i,j),i=1,Nx),j=1,Nz)	
  close(10)

  open(10,file="a3333model.bin",status='old',form='unformatted',access= &
  & 'direct',recl=bin_par*Nx*Nz)      
  read(10,rec=1)((a3333(i,j),i=1,Nx),j=1,Nz)	
  close(10)

  !open(10,file="marmousi_eps737x240.bin",status='old',form='unformatted',access= &
  !& 'direct',recl=bin_par*Nx*Nz)      
  !read(10,rec=1)((eps(i,j),i=1,Nx),j=1,Nz)	
  !close(10) 
 
  c1111=0.0; c3333=0.0; eps=0.0; del=0.0
   

  !operations
  Do j=1,Nz
    Do i=1,Nx
       c1111(i,j)=(a1111(i,j)**2.0)*rho(i,j) 
    end do
  end do

  Do j=1,Nz
    Do i=1,Nx
       c3333(i,j)=(a3333(i,j)**2.0)*rho(i,j) 
    end do
  end do

  Do j=1,Nz
    Do i=1,Nx
       eps(i,j)=(c1111(i,j)-c3333(i,j))/(2*c3333(i,j))
    end do
  end do

  Do j=1,Nz
    Do i=1,Nx
       del(i,j)= (sqrt((1+(2*eps(i,j)))/(1+(2*eta(i,j)))) - 1.0)*(1./2.)

!(eps(i,j)-eta(i,j))/(1+(2*eta(i,j)))
    end do
  end do
  

  !outputfiles
  open(11,file="marmousi_eps737x240.bin",status='replace',form='unformatted',access= &
  & 'direct',recl=bin_par*Nx*Nz)      
  write(11,rec=1)((eps(i,j),i=1,Nx),j=1,Nz)	
  close(11)

  open(22,file="marmousi_del737x240.bin",status='replace',form='unformatted',access= &
  & 'direct',recl=bin_par*Nx*Nz)      
  write(22,rec=1)((del(i,j),i=1,Nx),j=1,Nz)	
  close(22)
    
end program modelo_2
