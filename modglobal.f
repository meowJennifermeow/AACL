module global
   
   integer    :: Nx, Ny ,N,niter
   real(8)    :: Xmin, Xmax, Ymin, Ymax, Lx, Ly,dx,dy,sq_delta
   real(8)    :: threshold
   real(8),dimension(:),allocatable      :: xnode,ynode,uvec
   real(8),dimension(:,:),allocatable    :: f,u,A

   contains
 
   include 'allocarray.f'
   include 'genmesh.f'
   include 'coA.f'
   include 'bound.f'
   include 'rhs.f'
   include 'norm2.f'
   include 'Jacobi.f'
   include 'Gauss_Seidel.f'
   include 'output.f'
   


end module global
