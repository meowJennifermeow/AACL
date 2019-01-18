subroutine output

implicit none

   INTEGER  :: i,j
   character(20)  :: filestring,filebin
   INTEGER :: nstep 

   nstep = N*N*log10(REAL(N))+1
   
   write(filestring,'(A,A)') 'fieldU.','dat'
   write(filebin,'(A,A)') 'fieldU.','bin'
   !write(filestring,'(A,I6.6,A)') 'fieldU.',nstep,'.dat'
   !write(filebin,'(A,I6.6,A)') 'fieldU.',nstep,'.bin'
   
   open(10,file=filestring)
   do j= 1,Ny
     do i = 1,Nx
        write(10,'(3E12.4)')real(i-1)*dx,real(j-1)*dy,u(i,j)
     enddo
     write(10,'(A)') !Will produce a new empty line - and tell gnuplot to lift the pen
   enddo
   close(10)
   
   open(20,file=filebin,form='unformatted')
   write(20) u
   close(20)

end subroutine output
