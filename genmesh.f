subroutine genmesh

  implicit none
  
  integer  :: i,j
  
  !------------- Mesh Generation ---------------
  
   Nx = N
   Ny = N
   niter = N*N*log10(REAL(N))+1
   threshold = 1e-5

   call allocarray

   Xmin = -1
   Xmax = 1
   Ymin = -1
   Ymax = 1
   Lx = Xmax - Xmin
   Ly = Ymax - Ymin

   dx = Lx / (Nx-1)
   dy = Ly / (Ny-1)
   sq_delta = dx*dy 

   do i = 1,Nx
      xnode(i) = Xmin + dx*(i-1)
   enddo
   do i = 1,Ny
      ynode(i) = Ymin + dy*(i-1)
   enddo


end subroutine
