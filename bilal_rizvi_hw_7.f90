!Name: bilal_rizvi
!Purpose: Program that reads real data and computes mean, median,
!         standard deviation while outputting a file with sorted data
!Date: 10/23/2025

program datastats_finder
  implicit none

  !Variable Dictionary
  character(len=30) :: inputfile, outputfile ! To hold the user inputted names
  real, allocatable :: thedata(:) ! Declares our array as allocatable
  integer :: npts=0, ierror ! Number of datapoints, error variable for iostat
  integer :: i, k ! Loop indexes
  integer :: luninput, lunoutput ! Logical units for both of the files
  real :: mean, standarddev, median ! Stats for our data
  real :: sumformean, sumforstd ! The accumulation for our stat loops
  real :: temp ! To hold a temporary value for the sort swaps and array
  integer :: min_loc ! Lesser value position for the test to swap
                     ! has to be integer to be array index

  ! Name the input data file

  write(*,*) "Enter the name of the file to input (max 30 characters)" 
  read(*,*) inputfile

  ! Open said file and associated error message

  open(newunit=luninput,file=inputfile,status="OLD",iostat=ierror)
  if(ierror /= 0) then
    write(*,*) "Couldn't open the file"
    stop 1
  endif

  ! Find npts or how many points are in inputfile

  do while(ierror == 0)
     read(luninput,*,iostat=ierror) temp ! I used temp to store for now
     if(ierror == 0) then
        npts = npts + 1 ! Increment npts to count
     else
        exit
     endif
  enddo

  write(*,*) " The number of data points is: ",npts

  allocate(thedata(npts)) ! Allocating the space in the array
  rewind(luninput) ! Rewinding so we can read again

  ! Time to read the actual data in

  do i=1,npts,1
     read(luninput,*) thedata(i) ! Read this time with npts
  enddo

  close(unit=luninput) ! Closes the file

  ! We can begin computing the actual stats
  ! Im gonna space out the loops to read easier for the stats section

  sumformean = 0.0 ! Initialize to calculate mean

  do i=1,npts,1

     sumformean = sumformean + thedata(i) ! Add up our points

  enddo

  mean = sumformean/npts ! Mean formula, on hw5 you told me real() wasnt needed

  ! Now standard deviation

  sumforstd = 0.0 ! Initialize to calculate standard deviation

  do i=1,npts,1

     sumforstd = sumforstd + (thedata(i) - mean)**2 ! 1st part of stddev formula

  enddo

  standarddev = sqrt(sumforstd/npts) ! Completes the formula

  ! The actual selection sort needed for the median
  ! Name loops so the nested loops wont be confusing

  outside: do i=1,npts-1,1 ! npts-1 will already be max

        min_loc = i ! Set to location where swap will occur

        inside: do k=i+1,npts,1
        
             if(thedata(k) < thedata(min_loc)) then !Finds lesser value to swap

               min_loc = k ! Update position to the lesser value

             endif

        enddo inside

        if(min_loc /= i) then ! Basically checks whether inside swapped min_loc

           temp = thedata(i) ! store the data for i

           thedata(i) = thedata(min_loc) ! swap the data at i for the lower

           thedata(min_loc) = temp ! and swap the lower location with data at i

        endif

  enddo outside

  ! Back to our final stat that required the sort

  if(mod(npts,2) == 1) then ! Uses the remainder from div by 2 to decide even
                            ! or odd, mod() was listed in brightspace under
                            ! gfortran intrinsic functions in fortran resources
     median = thedata(npts/2 + 1) ! For odd npts

  else
     
     median = (thedata(npts/2) + thedata(npts/2 + 1)) / 2.0 ! For even npts

  endif

  ! Lets output to a file and terminal, we wrote out npts earlier already

  write(*,*) "The mean is:", mean
  write(*,*) "The standard deviation is:", standarddev
  write(*,*) "The median is:", median

  write(*,*) "Enter the name of the file to output (max 30 characters):"
  read(*,*) outputfile

  open(newunit=lunoutput,file=outputfile,status="REPLACE",iostat=ierror)
  if(ierror /= 0) then ! I put an error condition but I doubt it would occur for
                       ! file creation?
     write(*,*) "Couldn't create a file"
     stop 1
  endif

  ! Write all results and sorted data into the output file
  
  do i=1,npts,1 
     write(lunoutput,*) thedata(i)
  enddo

  write(lunoutput,*) "The mean is:", mean
  write(lunoutput,*) "The standard deviation is:", standarddev
  write(lunoutput,*) "The median is:", median

  close(unit=lunoutput) ! Close it up

  write(*,*) "The sorted data and the stats were also sent to:",outputfile

  deallocate(thedata) !Deallocate the space

  stop 0

end program datastats_finder


      



     

  
  
  
  

  

  

  

  
  
  
