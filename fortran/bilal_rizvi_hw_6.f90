!Name: bilal_rizvi
!Purpose: Integrate a complex function using Simpson's Rule
!Date: 10/7/2025
! 
!  Convergence Test
!  N          Result
!  100        1023.94409
!  150        993.877625
!  200        993.621765
!  300        993.630249
!  400        993.635498
!  500        993.630188
!  600        993.630615
!  700        993.638489
!  800        993.623657
!  900        993.620300
!  1000       993.630554
!                                                                              
!  The value of the integral is about 993.630 with oscillations of about 0.01
!  and stablilize after n = 300, the value 993.630 occurs multiple times with 
!  its first encounter at n = 300.
!  As such, I take 300 subintervals as most accurate.
!
! DISCLAIMER : The second term with the A coefficient will result in very small
! values causing a floating point underflow and denormal flags to appear.
! This is expected from the term's behavior and single precision arithmetic and
! does not influence the conclusion on which n I deem most accurate. 


program simpson_rule_integral

  implicit none

  !Variable Dictionary

  integer :: n ! Amount of sub-intervals

  integer :: i ! Loop index

  real :: x_low = 1.0 ! Lower bound (x-values here and below are in radians)

  real :: x_hi = 20.0 ! Upper bound

  real :: dx ! Sub-interval width

  real :: sum ! The sum

  real :: x_i ! This will hold the ith point 

  real :: x_mid ! This will hold the midpoint

  real :: f_i, f_mid, fip1 ! Will hold all three outputs for Simpson's Rule 

  !Constant Dictionary
  
  real, parameter :: A = 4000.0, B = 15.15, C = 0.01 

  
  write(*,*) "Enter number of subintervals desired:"

  read(*,*) n

  dx = (x_hi - x_low)/ real(n) ! Sub-interval width

  sum = 0.0 ! Initialize the sum for later

  x_i = x_low ! Set location for the first ith point

  ! The integral itself

  do i = 1, n   ! Increments by 1 by default

     x_mid = x_i + 0.5*dx ! Sets location of midpoint from x_i

     !Evaluation on the left
     f_i = (x_i + cos(x_i)) * exp(cos(x_i)) + &
     A * exp(-((x_i - B)**2) / C)

     !Evaluation on the midpoint
     f_mid = (x_mid + cos(x_mid)) * exp(cos(x_mid)) + &
     A * exp(-((x_mid - B)**2) / C)
     
     !Evaluation on the right
     fip1 = (x_i + dx + cos(x_i + dx)) * exp(cos(x_i + dx)) + &
     A * exp(-(((x_i + dx) - B)**2) / C)

     !Adds the area to the sum with Simpson's Rule
     sum = sum + (f_i + 4.0 * f_mid + fip1) * dx / 6.0 

     x_i = x_i + dx ! Moves location of x_i to the next point    

  enddo

  write(*,*) "When n is equal to",n,"the integral results as:", sum  

  stop 0

end program simpson_rule_integral



  

  

  

  
