subroutine coA

implicit none

integer    :: i,j,itemp,jtemp

!----------- Construct coefficient matrix -------
!        (i,j+1)
!(i-1,j) (i,j)   (i+1,j) 
!        (i,j-1)
!(Ny-1)*Nx+Nx,(Ny-1)*Nx+Nx-1!

A = 0
! Non-boundary points contribution
do j = 2,Nx-1
  do i = 2,Ny-1
     itemp = (j-1)*Nx + i ! Only relevant to the center point

     jtemp = (j-1)*Nx + i-1
     A(itemp,jtemp) = 1  !u(i-1,j) = 1 

     jtemp = (j-1)*Nx + i+1
     A(itemp,jtemp) = 1  !u(i+1,j) = 1

     jtemp = (j-1-1)*Nx + i
     A(itemp,jtemp) = 1  !u(i,j-1) = 1

     jtemp = (j+1-1)*Nx + i
     A(itemp,jtemp) = 1  !u(i,j+1) = 1

     jtemp = (j-1)*Nx + i
     A(itemp,jtemp) = -4  !u(i,j)   = -4
  enddo
enddo

! Boundary points contribution
! Upper boundary J = Nx
j = Nx
do i = 2,Ny-1
    itemp = (j-1)*Nx + i 

    jtemp = (j-1)*Nx + i-1
    A(itemp,jtemp) = 1  !u(i-1,j) = 1 

    jtemp = (j-1)*Nx + i+1
    A(itemp,jtemp) = 1  !u(i+1,j) = 1

    jtemp = (j-1-1)*Nx + i
    A(itemp,jtemp) = 1  !u(i,j-1) = 1

    jtemp = (j-1)*Nx + i
    A(itemp,jtemp) = -4 !u(i,j)   = -4
enddo


! Lower boundary J = 1
j = 1
do i = 2,Ny-1
    itemp = (j-1)*Nx + i 

    jtemp = (j-1)*Nx + i-1
    A(itemp,jtemp) = 1  !u(i-1,j) = 1 

    jtemp = (j-1)*Nx + i+1
    A(itemp,jtemp) = 1  !u(i+1,j) = 1
    
    jtemp = (j+1-1)*Nx + i
    A(itemp,jtemp) = 1  !u(i,j+1) = 1

    jtemp = (j-1)*Nx + i
    A(itemp,jtemp) = -4 !u(i,j)   = -4
enddo


! Left boundary I = 1
i = 1
do j = 2,Ny-1
     itemp = (j-1)*Nx + i ! Only relevant to the center point

     jtemp = (j-1)*Nx + i+1
     A(itemp,jtemp) = 1  !u(i+1,j) = 1

     jtemp = (j-1-1)*Nx + i
     A(itemp,jtemp) = 1  !u(i,j-1) = 1

     jtemp = (j+1-1)*Nx + i
     A(itemp,jtemp) = 1  !u(i,j+1) = 1

     jtemp = (j-1)*Nx + i
     A(itemp,jtemp) = -4  !u(i,j)   = -4
enddo

! Right boundary I = Ny
i = Ny
do j = 2,Ny-1
     itemp = (j-1)*Nx + i ! Only relevant to the center point

     jtemp = (j-1)*Nx + i-1
     A(itemp,jtemp) = 1  !u(i-1,j) = 1 

     jtemp = (j-1-1)*Nx + i
     A(itemp,jtemp) = 1  !u(i,j-1) = 1

     jtemp = (j+1-1)*Nx + i
     A(itemp,jtemp) = 1  !u(i,j+1) = 1

     jtemp = (j-1)*Nx + i
     A(itemp,jtemp) = -4  !u(i,j)   = -4
enddo


! Four points in the cotner
! (1,Ny)-------(Nx,Ny)
!   |             |
!   |             |
! (1,1)--------(Nx,1)

A(1,1) = -4     !u(1,1) = -4
A(1,Ny+1) = 1   !u(1,2) = 1
A(1,2) = 1      !u(2,1) = 1

A((Ny-1)*Nx+1,(Ny-1)*Nx+1) = -4    !u(1,Ny) = -4
A((Ny-1)*Nx+1,Nx*(Ny-1-1)+1) = 1   !u(1,Ny-1) = 1
A((Ny-1)*Nx+1,Nx*(Ny-1)+2) = 1     !u(2,Ny) = 1

A(Nx,Nx) = -4      !u(Nx,1) = -4
A(Nx,Nx-1) = 1     !u(Nx-1,1) = 1 
A(Nx,Nx+Nx) = 1    !u(Nx,2) = 1

A((Ny-1)*Nx+Nx,(Ny-1)*Nx+Nx) = 1    !u(Nx,Ny) = -4
A((Ny-1)*Nx+Nx,(Ny-1-1)*Nx+Nx) = 1    !u(Nx,Ny-1) = 1
A((Ny-1)*Nx+Nx,(Ny-1)*Nx+Nx-1) = 1    !u(Nx-1,Ny) = 1
 

end subroutine coA
