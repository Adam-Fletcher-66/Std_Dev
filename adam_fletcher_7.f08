  ! Name: Adam_Fletcher
  ! Date: 3/21/2022
  ! Purpose: To calculate the standard dev of a set of data in an external file .

program std_dev
  implicit none

                        ! Variable Dictionary
 
  character(len=256) :: filename     ! Name of file data inputed from
  integer :: lun                     ! Var of new logical unit number
  integer :: N=0                     ! Initialization of data lines in file
  integer :: lunstd                  ! Var of new file written to
  integer :: i                       ! Index var for do loops
  integer :: ierror                  ! Var for I/O status 
  real :: mu                         ! Var for mu within summation eq
  real :: x                          ! Var for each line of data values
  real :: stddev                     ! Var for standard deviation
  real :: sum = 0.0                  ! Initial pt of sum (for mu calc)
  real :: sum2 = 0.0                 ! Initial pt of sum2 (for calc of stddev)

  write(*,*) "Enter a file name:"    ! Prompts user for data file to analyze
  read(*,*) filename

  open(newunit=lun,file=trim(filename),status='OLD',iostat=ierror)
                                     ! Opens old data file
  if(ierror/=0) then
     write(*,*) "Error, please enter a new file name."
     stop 1                          ! Err message if no such file exists
  endif

  do while(ierror==0)                ! Loop calc the sum of the data and how &
                                     ! many data points there are
     read(lun,*,iostat=ierror) x
   
        sum = sum + x
        N = N + 1

        if(ierror/=0) then
           
           exit
        endif
        
        mu = sum/N                   ! Calculates the average mean (mu)
     
  enddo

     rewind(lun)                     ! Returns to the beginning of the data file

     do i=1,N-1,1                    ! Loops through data again to calc        &
                                     ! summation portion of the std dev
        read(lun,*,iostat=ierror) x
  
   sum2 = sum2 + (x - mu)**2

  enddo

  close(unit=lun)                    ! Closes file

  stddev = sqrt(sum2/(N-1))          ! Calculates standard Deviation


  write(*,*) "The standard deviation is:", stddev


  open(newunit=lunstd,file='stddev.dat',status='NEW',iostat=ierror)
     stddev = stddev                 ! Writes value to a new file 'stddev.dat'
  close(unit=lunstd)

  stop 0                             ! Terminates program execution
end program std_dev
