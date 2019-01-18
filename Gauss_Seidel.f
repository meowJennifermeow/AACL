
Subroutine Gauss_Seidel

  implicit none
  integer     :: i,j,k,iter_num
  integer     :: startt,endt,costt
  real(8)     :: time0,time1,delta_time
  real(8)     :: d_norm,rate
  real(8),allocatable     :: u0(:,:),delta_u(:,:)
  allocate(u0(Nx,Ny),delta_u(Nx,Ny))


  ! Initial calculation
  u0 = 20 
  call bound(u0)
  do i = 2, Ny-1
      do j = 2,Nx-1
        u0(i,j) = 1.0/4.0*(u0(i-1,j) + u0(i+1,j) + u0(i,j-1) + u0(i,j+1) + sq_delta*f(i,j))
      enddo
  enddo
 
  call system_clock(count=startt)
  call cpu_time(time0)

  iter_num = 0
  do k = 1,niter
    
    call bound(u)  
    do i = 2, Ny-1
      do j = 2,Nx-1
        u(i,j) = 1.0/4.0*(u(i-1,j) + u0(i+1,j) + u(i,j-1) + u0(i,j+1) + sq_delta*f(i,j))
      enddo
    enddo

    delta_u(1:Nx,1:Ny) = u(1:Nx,1:Ny) - u0(1:Nx,1:Ny)

    call matnorm2(delta_u,d_norm)

    if (d_norm .lt. threshold) exit
    u0 = u
    iter_num = iter_num + 1

  enddo

  call system_clock(count=endt)
  costt = endt - startt
  call cpu_time(time1)
  delta_time = time1 - time0
  print*, 'Num of iterations:', iter_num
  print*, 'Size of grid: ',Nx,'*',Ny ,'=', Nx*Ny
  print*, 'Time cost: ',delta_time ,'[s]'
  !!print*, 'processortime',costt
  print*,'Rate:',iter_num / delta_time ,'Iter/[s]'

  deallocate(u0)  

end subroutine Gauss_Seidel














