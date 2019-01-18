subroutine bound(umat)

implicit none

real(8),dimension(:,:),intent(inout)  :: umat
real(8),dimension(:,:),allocatable    :: utemp

allocate(utemp(size(umat,1),size(umat,2)))

utemp = umat

utemp(:,Ny) = 20   ! Upper boundary
utemp(:,1) = 0     ! Lower boundary
utemp(1,:) = 20    ! Left boundary
utemp(Nx,:) = 20   ! Right boundary

umat = utemp

deallocate(utemp)


end subroutine bound
