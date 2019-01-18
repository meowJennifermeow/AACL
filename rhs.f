subroutine rhs

implicit none

integer   :: i,j,k
real(8)   :: it,jt
real(8)   :: ileft, iright, jupper, jlower 


f = 0
K = 0

ileft = 0
iright = 1.0/2.0
jupper = -1.0/3.0
jlower = -2.0/3.0

!$omp parallel do
do i = 1,Nx
  do j = 1,Ny
     it = xnode(i)
     jt = ynode(j)    
     if(it>=ileft .AND. it<=iright .AND. jt>=jlower .AND. jt<=jupper  ) then
        f(i,j) = 200
        k = k+1
        !print*, i,j,it ,jt,k
     endif
  enddo
enddo
!$omp end parallel do


end subroutine rhs
