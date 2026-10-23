//Name: bilal_rizvi
//Purpose: Finding the real roots of x+3sin(2x) using the Newton-Raphson
//         iteration method
//Date: 11/27/2025

//Results : I did a range of initial guesses from -3.0 to 3.0 in increments of
// 0.5. Guesses -3.0, -2.5 resulted in the root -2.61298, guesses -2.0, -1.5
// resulted in the root -1.9175. Guesses from -0.5 to 0.5 all resulted in a 
// root at 0. Guesses 1.5, 2.0 resulted in 1.9175, and guesses 2.5, 3.0
// gave root 2.61298. It is easy to see that the guesses reflect themselves 
// with positive and negative roots respectively depending on the sign of
// the guess. However, specifically guesses -1.0 and 1.0 resulted in the root
// -2.61298 and 2.61298, even though they are closer to +- 1.9175. This is due
// to the non-foolproof nature of the Newton-Raphson method and its tendency to
// possibly skip closer roots depending on the shape of the function it is
// estimating from, this is because the guess depends on the derivative/slope
// of the graph.
//
// To summarize however, there were a total of 5 roots, -2.61298, -1.9175, 0,
// 1.9175, and 2.61298. All were found within 6 iterations with a guess of 0
// needing only 1 iteration as it is an exact guess. You can conclude that a
// MAXNUMITER of 300 was overkill in this case.

#include <iostream>
#include <cmath>
#include <limits> // For the overflow check

int main()
{
  // Variable Dictionary

  double xOld; // Prior estimate of the root
  double xNew; // Next estimate of the root
  double fOld; // Prior value of f(xOld)
  double fPrimeOld; // Value of df/dx(xOld)
  double epsilon{1.0e-6}; // Convergence criterion
  bool converged; // This boolean will be used to indicate convergence
  int numIter{1}; // Number of iterations initialized to 1
  const int MAXNUMITER{300}; // Max number of iterations set so it can loop

  // The prompt for the user
  std::cout << "Enter an initial guess for the root: " << std::endl;
  std::cin >> xOld; // Setting answer as our prior estimate

  // The actual loop
  while (numIter < MAXNUMITER) // while less than the max number of iterations
  {

    fOld = xOld + 3.0 * sin(2.0 * xOld); // The function
    fPrimeOld = 1.0 + 6.0 * cos(2.0 * xOld); // Its derivative

    // Overflow check using limits library
    if (std::abs(fPrimeOld) < 1.0) // to cover small values of df/dx
    {
      if (std::abs(fOld) > std::abs(fPrimeOld) *
	    std::numeric_limits<double>::max())
      {
	std::cout << "The derivative is small and this method will overflow"
		  << "f(x) =" << fOld << ", f'(x) =" << fPrimeOld << std::endl;
	return 2;
      }
    }

    // Calculating the new estimate using the actual formula
    xNew = xOld - fOld / fPrimeOld;

    // Apply the convergence criterion by giving a condition to our boolean
    if (xOld != 0.0) // This part covers when our root is 0
    {
      converged = std::abs(xNew - xOld) < epsilon * std::abs(xOld);
    }
    else
    {
      converged = std::abs(xNew - xOld) < epsilon;
    }

    // Outputting our converged result
    if (converged)
    {
      std::cout << " A root was found at x = " << xNew << std::endl;
      std::cout << "within " << numIter << " iterations" << std::endl;
      return 0;
    }

    // Update the values of numIter and xOld for next iteration
    xOld = xNew;
    numIter = numIter + 1;
  }

  std::cout << "Too many iterations, method failed!" << std::endl;
  return 1;

}



    
	  



	
	  
      
