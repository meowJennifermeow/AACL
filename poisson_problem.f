
program poisson_problem

   use global
   implicit none

   integer    :: i,j

   
   !------------- Mesh Generation ---------------
   N = 32*2*2
   
   call genmesh

   !-------- Construct coefficient matrix -------
   !call coA

   !----------- Boundary condition ---------------
   u = 0   
   call bound(u)

   !--------------- Right hand side ---------------
   call rhs

   !-------------------- Solver -------------------
    call Jacobi
   !call Gauss_Seidel

   !------------------- Output --------------------
   call output


   deallocate(f, u, xnode, ynode )

end program poisson_problem
