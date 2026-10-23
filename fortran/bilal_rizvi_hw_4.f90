  !Name: bilal_rizvi
  !Purpose: Calculating the apparent magnitude of a binary star system
  !Date: 9/22/2025

program apparent_magnitude_binary_star

  implicit none

  !Variable Dictionary

  real :: input_time, phase_time !The input and phase time computed (days)      
  
  real :: apparent_magnitude !Apparent magnitude for brightness

  integer :: period_amount !Essential for the integer arithmetic

  !Constants
  
  real, parameter :: PERIOD=6.4 ! Time for one period (days)

  real, parameter :: PI=3.1415927

  write(*,*) "Enter a time (days)(real) to calculate the apparent magnitude:"
  read(*,*) input_time

  period_amount = int(input_time / PERIOD) !Turns input to phase time under 6.4

  phase_time = input_time - real(period_amount) * PERIOD !Converts back to real

  !The conditions were already restrictive from the sheet so no re-order
  
  if (phase_time >= 0.0 .and. phase_time < 0.9) then

     apparent_magnitude = 2.5

  elseif (phase_time >= 0.9 .and. phase_time < 2.3) then

     apparent_magnitude = 3.335 - log(1.352 + cos(PI*(phase_time - 0.9)/0.7))

  elseif (phase_time >= 2.3 .and. phase_time < 4.4) then   

     apparent_magnitude = 2.5
     
  elseif (phase_time >= 4.4 .and. phase_time < 5.2) then

     apparent_magnitude = 3.598 - log(1.998 + cos(PI*(phase_time - 4.4)/0.4))

  elseif (phase_time >= 5.2 .and. phase_time < 6.4) then
     
     apparent_magnitude = 2.5

  endif
  
  write(*,*) "For time:",input_time,&
       "is apparent magnitude:",apparent_magnitude

  stop 0

end program apparent_magnitude_binary_star

