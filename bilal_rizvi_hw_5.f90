  !Name: bilal_rizvi
  !Purpose: Calculating a summation using an iterative do loop
  !Date: 10/01/2025

program summation_itdo_loop

  implicit none

  !Variable dictionary
  real :: x, whole_term, summation
  integer :: n, i, c_i, factorial_term 


  write(*,*) "Enter a real value for x > 0:"

  read(*,*) x

  write(*,*) "Enter an integer value for N >= 0:"

  read(*,*) n

  !I can initialize for when i=0 so I can omit an if statement

  summation = 1.0 !at i = 0 the sum is 1.0

  factorial_term = 1 !must define to hold previous factorial values  

  c_i = -1 ! This is going to allow me omit an if statement for odds and evens  

  do i = 1, n

     factorial_term = factorial_term * (2*i - 1) * (2*i)

     whole_term = c_i * ((x**2)**i) / real(factorial_term)

     summation = summation + whole_term

     c_i = -c_i !This is the line allowing the omission of the if statement

  enddo

  write(*,*) "The sum is:", summation

  stop 0
  
end program summation_itdo_loop

     

     
