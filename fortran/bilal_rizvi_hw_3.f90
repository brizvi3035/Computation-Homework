  !Name: bilal_rizvi
  !Purpose : To calculate non-relativistic and relativistic kinetic energies of a particle, given mass and velocity in MKS units
  !Date : 9/12/2025

  !This program fails to accurately give the relativistic kinetic energy when given a non-relativistic speed.
  !This is due to the limits of single precision arithmetic in the calculation for gamma.
  !At non-relativistic speed, the lorentz factor is so close to 1 that it rounds to 1
  !Therefore the term (gamma-1.0) in the expression for t_r results in 0 and causes the entire value to be multiplied by 0.

program nonrel_and_rel_kinetic_energy_calculator

  implicit none

  !Constant Dictionary 

  real, parameter :: C=2.99792458e8 ! The speed of light                   (in m/s)

  !Variable Dictionary

  real :: mass                      ! Mass                                 (in kg)
  real :: velocity                  ! Velocity                             (in m/s) 

  real :: gamma                     ! The Lorentz Factor    

  real :: t_nr                      ! Non-relativistic kinetic energy      (in J)
  real :: t_r                       ! Relativistic kinetic energy          (in J)            

  
  write(*,*) ""                                                               ! All subsequent uses of write(*,*) "" are for clarity and spacing
  write(*,*) "Relativistic and non-relativistic kinetic energy calculator."
  write(*,*) ""
  write(*,*) "Enter relevant quantities below :"

  write(*,*) ""
  write(*,*) "First, state the mass, in kilograms, of your particle (real) :"
  write(*,*) ""
  read(*,*) mass

  write(*,*) ""
  write(*,*) "Next, state the velocity, in meters per second, of your particle (real) :"
  write(*,*) ""
  read(*,*) velocity

  ! Non-relativistic kinetic energy calculation
  t_nr = 0.5 * mass * velocity**2

  write(*,*) ""
  write(*,*) "The non-relativistic kinetic energy, in joules, is equal to :", t_nr

  ! Lorentz factor calculation
  gamma = 1.0 / (1.0 - (velocity/C)**2)**(1.0/2.0)                     ! I used an exponent of 1/2 since we haven't covered a square root operator in class

  ! Relativistic kinetic energy calculation
  t_r = mass * C**2 * (gamma - 1.0) 

  write(*,*) ""
  write(*,*) "The relativistic kinetic energy, in joules, is equal to :", t_r

  write(*,*) ""
  stop 0

end program nonrel_and_rel_kinetic_energy_calculator











  

  
