!Name: bilal_rizvi
!Purpose: Integrate a complex function using Simpson's rule by calling
!         subroutines and functions
!Date: 11/10/2025
!
!
! Double Precision Convergence Test
!
!  N          Result
!  100        1023.9408866276436
!  150        993.87867815771926
!  200        993.62351775169157
!  300        993.63147806334530
!  900        993.63147786715615
!  1000       993.63147786635045
!  1500       993.63147786511547
!  2000       993.63147786490879
!  3000       993.63147786482966
!  5000       993.63147786481522
!  10000      993.63147786480920
!  10100      993.63147786481045
!  10200      993.63147786481159
!  10300      993.63147786481386
!  10400      993.63147786481284
!  10500      993.63147786481113
!  10600      993.63147786481409
!  10700      993.63147786480931
!  10800      993.63147786481227
!  10900      993.63147786481272
!  11000      993.63147786481045
!
!  The value of the integral with the double precision reals is about 
!  993.63147786481000 with oscillations of about 0.00000000000500
!  The first encounter of this stabilized value is at 10100, as such
!  I take 10100 subintervals as the most accurate. 
!
!  The convergence test has values varying in the 100's which have large
!  variation, as such I increased n by a few magnitudes until the value
!  began to converge. This happened at around 10000 and is the logic
!  for the table and conclusion made directly above for n.
!
!  Even with double precision, the floating point underflow and denormal
!  flag still appears. This is due to the e to the negative exponent resulting
!  in a very small number past what double precision accounts for, giving
!  a denormal flag in the same way as single precision. This is expected from
!  the term and as such doesn't affect which n I deemed most accurate.
!


module simpsons_rule_mod  ! Module containing the function and limits
  implicit none
  
  integer, parameter :: port_dou = kind(1.0d0) ! Portable double precision

contains

  subroutine limits_of_integration(x_low, x_hi) ! Where we get the limits
    implicit none

    ! Lower and upper bounds

    real(kind=port_dou), intent(out) :: x_low, x_hi

    ! Initialize/set both

    x_low = 1.0d0
    
    x_hi = 20.0d0

    return

  end subroutine limits_of_integration

  function integrand(x) result(f_val) ! Function for integrand
    implicit none

    ! Integrand constants
    real(kind=port_dou), parameter :: A = 4000.0d0, B = 15.15d0, C = 0.01d0

    ! Function Dictionary
    
    real(kind=port_dou), intent(in) :: x

    real(kind=port_dou) :: f_val

    ! Integrand evaluation
    
    f_val = (x + cos(x)) * exp(cos(x)) + A * exp(-((x - B)**2) / C)

    return

  end function integrand

end module simpsons_rule_mod ! Ending the module


program simpsons_rule_main ! The main program and the use association

  use simpsons_rule_mod, only: port_dou, limits_of_integration, integrand

  implicit none

  ! DISCLAIMER, the assignment says for the main program to not have any
  ! information relevant to the integral but asks that I call the function
  ! three times, meaning the do loop should be in the main program.
  ! For that to be the case, the sub-interval width and numbers are declared
  ! in the main program. I believe this shouldn't conflict with the
  ! instruction for the assignment in that case.

  ! Variable Dictionary

  integer :: n ! Number of sub-intervals

  integer :: i ! Loop index

  real(kind=port_dou) :: x_low, x_hi ! Integration bounds

  real(kind=port_dou) :: dx ! Sub-interval width

  real(kind=port_dou) :: sum ! The sum

  real(kind=port_dou) :: x_i ! Current x position

  real(kind=port_dou) :: x_mid ! Midpoint x position

  real(kind=port_dou) :: f_i, f_mid, f_ip1 ! All three function evaluations

  ! Retrieve the limits
  call limits_of_integration(x_low, x_hi)

  write(*,*) "Enter number of sub-intervals desired"
  read(*,*) n

  dx = (x_hi - x_low) / real(n, kind=port_dou) ! The sub-interval width

  sum = 0.0d0 ! Initialize the sum

  ! Simpson's Rule

  ! I changed the loop to start at i=0 since the abscissa expression
  ! I used starts with x_low + i*dx and I didn't initialize it as x_low
  ! initially like I did for assignment 6
  
  do i = 0, n-1

     ! Calculate abscissas

     x_i = x_low + real(i, kind=port_dou) * dx

     x_mid = x_i + 0.5d0 * dx

     ! Call the function

     f_i = integrand(x_i)

     f_mid = integrand(x_mid)

     f_ip1 = integrand(x_i + dx)

     ! Sum the area

     sum = sum + (f_i + 4.0d0 * f_mid + f_ip1) * dx / 6.0d0

  enddo

  write(*,*) "When n is equal to", n, "the integral results as:", sum

  stop 0

end program simpsons_rule_main


     
  


    
