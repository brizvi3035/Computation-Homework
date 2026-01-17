!Name: bilal_rizvi
!Purpose: Solving the Poisson equation for a 2-D steady state
!          electromagnetics problem with two charges
!Date: 11/6/2025

! The electrical potential found at the cell (25,50) after a total of 2592
! was -2.98454370E-02 statVolts. It also obtains a denormal flag, I believe
! since there are a large amount of cells, the furthest have such low potentials
! they are not properly contained by single precision. Hence the denormal flag.

program two_charge_poisson
  implicit none

  !Variable Dictionary

  integer, parameter :: NX = 100 ! This is the no. of cells on the x-axis
  integer, parameter :: NY = 100 ! This is the no. of cells on the y-axis

  real, parameter :: H = 0.1 ! This is just the cell size

  real, dimension(0:NY+1,0:NX+1) :: u ! An old estimate of potential 
  real, dimension(0:NY+1,0:NX+1) :: u_new ! An new estimate of potential
  ! Both potentials are in (statVolts)

  real, dimension(0:NY+1,0:NX+1) :: q ! The charge density (in statC/cm^2)

  real :: max_change = 1.0 ! The max change to evaluate a solid iteration amount
                           ! initialized to 1 so it can clear the condition once

  integer :: iterations = 0 ! Iteration number

  integer :: i,j ! Indices for loops

  real, parameter :: PI = 3.141592654 ! Just pi

  !For additional gnuplot section
  
  real :: x,y ! Coordinates for cells

  integer :: lun1 ! Logical unit for the I/O clause

  ! The code, initializing values

  u = 0.0 ! Initialize to zero everywhere
  u_new = u ! Initialize this to u

  q = 0.0 ! Initialize charge to zero as well
  
  q(25,25) = -4.0 ! Set specific charges at pre-determined locations
  q(75,75) = 4.0

  ! Loop for evaluating potential

  iterative_loop: do while(max_change > 1.0e-5) ! Condition to continue

     do j = 1,NX
        do i = 1,NY ! Leftmost index inside for optimized speed

           u_new(i,j) = (u( i + 1 ,j) + u( i - 1 ,j) + u(i, j + 1 ) + &
           u(i, j - 1 ) + 4.0*PI*(H**2)*q(i,j))/4.0
           ! Actual evaluation of potential in cells

        enddo
     enddo

     max_change = maxval(abs(u-u_new)) ! Tells the max change at found iteration

     write(*,*) "The maximum change was =", max_change
     
     u = u_new
     
     iterations = iterations + 1 ! Counting iterations

  enddo iterative_loop

  write(*,*) "It converged at", iterations,"iterations"

  ! Potential at desired location
  write(*,*) "Potential at cell (25,50) =", u_new(25,50), "statVolts"

  ! File to send Gnuplot
  ! My comments are actually swapped compared to the lesson
  ! Im not sure if I'm mistaken about which variable is considered the x 
  ! or y coordinates of the cell
  
  open(newunit=lun1,file="poissonhw.dat",action="WRITE",status="REPLACE")

  x = 0.5*H ! Initialize the x coordinate of cell
  
  do j=0,NX+1 !Loop columns
     
     y = 0.5*H ! Initialize y coordinate of cell

     do i = 0,NY+1 ! Loop rows

        write(lun1,*) x,y,u_new(i,j)

        y = y + H ! Increment y coordinate

     enddo

     write(lun1,*) " " ! Write blank lines

     x = x+h ! Increment the x coordinate

  enddo

  close(unit=lun1)

  stop 0

end program two_charge_poisson
     
        
