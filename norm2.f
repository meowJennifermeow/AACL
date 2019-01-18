
subroutine matnorm2(x,normx)
        implicit none
        
        REAL(8),dimension(:,:) :: x
        REAL(8),dimension(:),allocatable   :: tempvec
        REAL(8) :: normx
        integer  :: m,n,i,j,k
        
        m = size(x,1)
        n = size(x,2)

        allocate(tempvec(m*n))
        
        !$OMP parallel do
        do i = 1,m
          do j = 1,n
             k = (j-1)*m + i
             tempvec(k) = x(i,j)
          enddo
        enddo
        !$OMP end parallel do

        normx = sqrt(dot_product(tempvec,tempvec))
        deallocate(tempvec)

end subroutine
