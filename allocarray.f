subroutine allocarray

implicit none

allocate( xnode(Nx), ynode(Ny) )
allocate(f(Nx,Ny),u(Nx,Ny))
!allocate( A(Nx*Ny,Nx*Ny), uvec(Nx*Ny))

end subroutine allocarray
