//Name: bilal_rizvi
//Purpose: Finding roots of 4 different cases of a quadratic polynomial
//Date: 11/18/2025

#include <iostream>
#include <cmath> // We need this in order to us the sqrt() and pow() functions

// DISCLAIMER, I am using brace initialization just about everywhere I can
// as according to the slides, the example codes don't but the wording
// in the initialization section seems to ask for braces as often as possible

int main()
{

  //Variable Dictionary
  
  double a; // Coefficient A
  double b; // Coefficient B
  double c; // Coefficient C
  double discriminant; // B^2 - 4AC, which we'll use to know which case it is
  double root1; // First root
  double root2; // Second root
  double realPart; // Real part of the complex roots
  double imagPart; // Imaginary part for complex roots

  // Read in the coefficients from the user
  std::cout << "Enter coefficient A: " << std::endl;
  std::cin >> a;
  
  std::cout << "Enter coefficient B: " << std::endl;
  std::cin >> b;

  std::cout << "Enter coefficient C: " << std::endl;
  std::cin >> c;

  // The first case (A=0) a linear equation
  if (a == 0.0) // Check if linear
  {
    if (b == 0.0) // Check if b is zero
    {
       std::cout << "Coefficients don't suffice to give roots" << std::endl;
    }
    else
    {
       root1 = -c / b; // the single root of a linear equation
       std::cout << "Linear equation of single root = " << root1 << std::endl;
    }
  }
  // The second case, quadratic equation  
  else
  {
    discriminant = pow(b,2) - 4.0 * a * c; // Evaluate and set discriminant

    // for two distinct real roots
    if (discriminant > 0.0) // when two distinct real roots occur
    {
       root1 = (-b + sqrt(discriminant)) / (2.0 * a); // Root 1
       root2 = (-b - sqrt(discriminant)) / (2.0 * a); // Root 2
       std::cout << "Two distinct real roots:" << std::endl;
       std::cout << "First root = " << root1 << std::endl;
       std::cout << "Second root = " << root2 << std::endl;
    }
    
    // for a single real root 
    else if (discriminant == 0.0) // when a single real root occurs
    {
       root1 = -b / (2.0 * a);
       std::cout << "The single real root is: " << root1 << std::endl;
    }
    
    // for two complex roots 
    else
    {
       realPart = -b / (2.0 * a);
       imagPart = sqrt(-discriminant) / (2.0 * a);
       std::cout << "Two complex roots:" << std::endl;
       // We'll use "i" for the image part
       std::cout << "The first root = " << realPart << " + " << imagPart <<
       "i" << std::endl;
       std::cout << "The second root = " << realPart << " - " << imagPart <<
       "i" << std::endl;
    }
  }
      

  return 0;

}
