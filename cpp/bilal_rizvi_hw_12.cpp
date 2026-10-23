//Name: bilal_rizvi
//Purpose: Outputting data fitted with a line after sorting
//Date: 12/3/2025

#include <iostream>
#include <fstream> // For defining file streams, reading data 
#include <string> // For files once more
#include <cmath>
// I decided to write a check for variance, the homework 
// technically didn't say whether the arbitrary
// data should be assumed to have a meaningful fit 

int main()
{
  //Variable Dictionary
  std::string inputFileName; // String for the input file name
  std::string outputFileName; // String for the output file name

  std::ifstream inputFile; // The input file stream
  std::ofstream outputFile; // The output file stream

  double xTemp; // Temporary x for counting
  double yTemp; // Temporary y for counting
  int npts{0}; // Number of points

  double xSum{0.0}; // Sum of x
  double ySum{0.0}; // Sum of y
  double x2Sum{0.0}; // Sum of x^2
  double xySum{0.0}; // Sum of xy

  double slope{0.0}; // m
  double intercept{0.0}; // b
  double chiSquared{0.0}; // literally chi^2
  double denominator{0.0}; // the denominator term of the slope

  const double SMALL{1.0e-8}; // For checking if variance is pretty much trivial
  
  double temp{0.0}; // for the bubble sort

  double yFit{0.0}; // Fitted y values at each x
  double epsilon{0.0}; // The residual (y_actual - yFit)

  double *xArray{nullptr}; // Pointer to array for the x values
  double *yArray{nullptr}; // Same for y values
    
  // The prompts
  std::cout << "Enter the file with the data: " << std::endl;
  std::cin >> inputFileName;

  std::cout << "Enter where you want to send the fitted data: " << std::endl;
  std::cin >> outputFileName;

  // Opening the file
  inputFile.open(inputFileName); // To read the data
  if(inputFile.fail())
    {
      std::cout << "Couldn't open the file " << inputFileName << std::endl;
      return 1;
    }
  
  // Count the data points
  while(inputFile.good())
    {
      inputFile >> xTemp >> yTemp; // Read from the lines
      if(inputFile.fail()) { break; } // If it fails then exit
      npts++ ; // Increment
    }

  // Clear errors and basically rewind
  inputFile.clear();
  inputFile.seekg(0, std::ios::beg);

  // Time to allocate the arrays
  if(xArray == nullptr) { xArray = new double[npts]; }
  if(yArray == nullptr) { yArray = new double[npts]; }

  // Read data into there
  for (int i{0}; i < npts; i++)
    {
      inputFile >> xArray[i] >> yArray[i];

      // Time to start doing stuff for the least squares, accumulate the sums
      xSum = xSum + xArray[i];
      ySum = ySum + yArray[i];
      x2Sum = x2Sum + xArray[i] * xArray[i];
      xySum = xySum + xArray[i] * yArray[i];
    }

  // Close the file now
  inputFile.close();

  // Now to calculate the slope and intercept using the formulas
  denominator = npts * x2Sum - xSum * xSum;

  // Now we can check if the variance is trivial
  if (std::abs(denominator) < SMALL)
    {
      std::cout << "The data has nearly no variance, the best fit is "
		<< "not meaningful" << std::endl;
      
      delete[] xArray; // delete memory for x 
      xArray = nullptr; // nullify pointer for x
      delete[] yArray;  // delete memory for y
      yArray = nullptr; // nullify pointer for y

      return 2;
    }

  // The equations are finally used here 
  slope = (npts * xySum - xSum * ySum) / denominator;
  intercept = (ySum - slope * xSum) / npts;

  // Now for chi^2, etc
  for (int i{0}; i < npts; i = i + 1)
    {
      yFit = slope * xArray[i] + intercept; // The actual fit
      epsilon = yArray[i] - yFit; // Residuals
      chiSquared = chiSquared + epsilon * epsilon; // The chi^2
    }

  // Time to sort, I used the bubble sort
  // I don't imagine I need to write much, same algorithm from previous hws

  for (int i{0}; i < npts - 1; i = i + 1)
    {
      for (int j{0}; j < npts - 1; j = j + 1)
        {
          if(xArray[j] > xArray[j + 1])
            {
               
                temp = xArray[j];
                xArray[j] = xArray[j + 1];
                xArray[j + 1] = temp;
                
                temp = yArray[j];
                yArray[j] = yArray[j + 1];
                yArray[j + 1] = temp;
            }
        }
    }

  // Time for the output
  outputFile.open(outputFileName);
  if(outputFile.fail()) // Same drill here for the input file above
    {
      std::cout << "Couldn't open output file " << outputFileName << std::endl;

      delete[] xArray;
      xArray = nullptr;
      delete[] yArray;
      yArray = nullptr;

      return 3;
    }

  // With the sort and fitted values we can match the positions and output 
  for (int i{0}; i < npts; i = i + 1)
    {
      yFit = slope * xArray[i] + intercept; // Recalculate yFit at sorted x
      outputFile << xArray[i] << " " << yArray[i] << " " << yFit << std::endl;
    }

  // Close output file
  outputFile.close();

  // The results calculated
  
  std::cout << "Linear fit results:" << std::endl;
  std::cout << "Slope (m) = " << slope << std::endl;
  std::cout << "Intercept (b) = " << intercept << std::endl;
  std::cout << "chi^2 = " << chiSquared << std::endl;

  std::cout << "Sorted data written to: " << outputFileName << std::endl;

  // This is doing the same thing that it was doing twice before above
  delete[] xArray;
  xArray = nullptr;
  delete[] yArray;
  yArray = nullptr;    

  return 0;

}

  
