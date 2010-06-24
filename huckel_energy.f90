! A short F95 code to calculate the energy eigenvalues for a hamiltonian
! for a simple molecule using Huckel theory
! 
! We assume that all the bond lengths are the same so that the energies
! can be given in units of the off-diagonal hamiltonian elements and the
! 0-energy taken to be the average energy ( the diagonal hamiltonian elements)
!
! The hamiltonian is stored in a file called "hamiltonian.ham" in the same directory 
! as the executable
!
! INPUT FILE FORMAT: 1st line is the name of the output file which will store the eigenvalues
!                    2nd line is the precision "S" or "D" and whether we want only eigenvalues ("N") or eigenvectors output aswel ("V") 
!                    3rd line is the size of the hamiltonian 
!                    subsequent lines are a space separated list of matrix elements
!
!
! IGNORE THIS FORMATTING
! INPUT FILE FORMAT: lines beginning with "#" are comments. the line starting with a "@" must contain
!                    only a single integer which gives us the dimension of the hamiltonian contained in the
!                    remainder of the file. the line starting with "!" is the name of the file which the eigenvalues will be written to
!                    The remainder of the file is a space separated matrix of 1s and 0s, however the matrix must start and end with a "* 
!                    on a new line
!                    the  1s are the non-0 hamiltonian matrix elements between the Huckel basis functions, and
!                    the 0s are 0 elements or the diagonal elements
!
!TODO: add support for hamiltonian calculation based on molecule structure? 
!TODO: write a printout function 
!TODO: add in choice to choose single or double precision
program huckelEnergy 

implicit none
character(len=*), parameter :: inFile = 'hamiltonian.ham' !  file containing our hamiltonian
character(len=50) :: outFile       ! file to write to
real, allocatable, dimension(:,:) :: hamiltonian ! we don't know how big a system we are dealing with yet
real, allocatable, dimension(:) :: eigenvalues
integer :: systemSize,exitStatus ! the size of system (i.e. hamiltonian dimensions), and the exitStatus of our subroutines, changed each time it is passed
character(len=1) :: calcMode,precisionMode      ! "N" to compute just eigenvalues; "V" to compute eigenvectors aswel: "S" for single precision, "D" for double
logical :: Symettric                            ! take a guess...
!DEBUG
integer :: i,j
!END DEBUG
call getParams(inFile,systemSize,precisionMode,calcMode,outFile) ! read system data from our input file

!DEBUG
 write(*,*) 'System size in main loop:', systemSize
 write(*,*) 'main loop precisionMode:' , precisionMode
 write(*,*) 'main loop calcMode:', calcMode
!END DEBUG
allocate(hamiltonian(systemSize,systemSize))   ! now all the hamiltonian attributes (shape) are set
allocate(eigenvalues(systemSize))

call parseArray(inFile,hamiltonian,systemSize)            ! read in the hamiltonian from file
! DEBUG: call printMatrix(hamiltonian,systemSize)
    !DEBUG 
    do i=1,systemSize
        write(*,'(3(F2.0,X))') (hamiltonian(i,j), j=1,systemSize)
    end do   
    !END DEBUG
call isSymmetric(hamiltonian,systemSize,exitStatus)
if(exitStatus .ne. 0) then ! no way to recover from asymmetric hamiltonian
    deallocate(hamiltonian)
    deallocate(eigenvalues)
    stop
end if

write(*,*) 'diagonalizing the hamiltonian:' ! output to give user a visual
call printmatrix(hamiltonian,systemSize)
call diagonalize(hamiltonian,eigenvalues,systemSize,calcMode,precisionMode,exitStatus) ! eigenvalues is unallocated vector
if(exitStatus .ne. 0) then ! if there were problems
    deallocate(hamiltonian)
    deallocate(eigenvalues)
    stop
end if
!call printMatrix(hamiltonian)
call printVector(eigenvalues,systemSize)
! Print our output to the specified file
call printOutput(eigenvalues,hamiltonian,systemSize)
open(unit=10,file=outFile,action="WRITE",status="REPLACE")
if(calcMode .eq. 'N') then
    call printOutput(eigenvalues,systemSize,10) ! if only eigenvalues calculated, only print eigenvalues..
else 
    call printOutput(eigenvalues,hamiltonian,systemSize,10) ! else print the eigenvectors too
end if
close(10)

deallocate(hamiltonian)
deallocate(eigenvalues)
! >>> END
end program huckelEnergy

subroutine getParams(inFile,systemSize,precisionMode,calcMode,outFile) !TODO: test, more error handling
! function to return the size of the hamiltonian stored in the file
    implicit none
    integer, intent(out) :: systemSize
    character(len=1), intent(out) :: precisionMode, calcMode
    character(len=*), intent(in) :: inFile
    character(len=50), intent(out) :: outFile

    open(10,file=inFile,action="read")
    read(10,fmt='(A50)') outFile         ! outfile name up to 50 characters long
    read(10,'(A1)') precisionMode         ! this line contains the precision (single or double)
    read(10,'(A1)') calcMode              ! just eigenvalues or eigenvalues and eigenvectors?
    read(10,'(I2)') systemSize           ! hamiltonian dimensions
    close(10)
    
    !DEBUG
     write(*,*) outFile
     write(*,*) precisionMode
     write(*,*) calcMode
     write(*,*) systemSize
    ! END DEBUG
 
    if(precisionMode .ne. 'D' .or. precisionMode .ne. 'S') then    
        write(*,*) 'WARNING: no precision mode specified (or multiple specified) in the input file: choosing double precision'
        precisionMode = 'D'
    end if
    
    if(calcMode .ne. 'N' .or. calcMode .ne. 'V') then 
        write(*,*) 'WARNING: invalid calculation mode in input file: choosing "N"...'
        calcMode = 'N' 
    end if

    
    return
  
end subroutine getParams

subroutine parseArray(inFile,array,systemSize) !TODO:test, add in error handling
! Parse the array from the file to the array variable 
    implicit none
    character(len=*), intent(in) :: inFile     
    integer, intent(in) :: systemSize 
    real, dimension(systemSize,systemSize), intent(out) :: array ! the array to parse the stuff to
    character(len=20) :: frmBase = '((F1.0,X))'   ! the form of 1 "entry" of input
    character(len=20) :: frm                      ! output format string
    character(len=20) :: rowLength                ! how long is the output?
    integer :: i,j                                ! loop variables 
   !DEBUG
    character(len=50) :: tmp 
    integer :: temp
   !END DEBUG

    write(rowLength,*) systemSize                 ! workaround to get my format statement working: doesn't work with direct concetenation
    frm = frmBase(1:1)//trim(adjustl(rowLength))//frmBase(2:) ! set up the format string
    
    open(10,file=inFile,action="read")
    read(10,*)          ! advance past the outfile, calculation parameters and array size
    read(10,*) 
    read(10,*)
    read(10,*) 

    rows: do i=1,systemSize
        read(unit=10,fmt=frm) (array(i,j), j=1,systemSize)    ! best to have whole matrix stored just to be safe, even though Lapack only needs 1 triangle
    end do rows                                             ! the extra computing power will be trivial for my hamiltonians
    close(10)
    
    return

end subroutine parseArray

subroutine printMatrix(matrix,matSize,outUnit) ! TODO: test, add in error handling
    ! write the array to stdout
    implicit none
    integer, intent(in) :: matSize
    real,dimension(matSize,matSize), intent(in) :: matrix
    integer, intent(in),optional :: outUnit       ! the stream to print to 
    logical,save :: isOpen = .false.              ! set to true if stream is open, false if not 
    character(len=20) :: frmBase = '((F6.3,X))'   ! the form of 1 "entry" of output
    character(len=20) :: frm                      ! output format string
    character(len=20) :: rowLength                ! how long is the output?
    integer :: i,j                                ! loop variables 
    write(rowLength,*) matSize                 ! workaround to get my format statement working: doesn't work with direct concetenation
    frm = frmBase(1:1)//trim(adjustl(rowLength))//frmBase(2:) ! set up the format string
    
    if(present(outUnit)) then ! if the optional unit number is given, write there
        inquire(unit=outUnit,opened=isOpen)
        if(isOpen) then
            do i=1,matSize
                write(outUnit,fmt=frm) (matrix(i,j), j=1, matsize)
            end do
            return
        end if
    else ! go to standard output
        do i=1,matSize
            write(*,fmt=frm) (matrix(i,j), j=1, matSize)
        end do
        return
    end if

end subroutine printMatrix

subroutine printVector(vector,vecSize,outUnit)

    implicit none
    integer, intent(in) :: vecSize
    real,dimension(vecsize), intent(in) :: vector
    integer, intent(in),optional :: outUnit   ! the stream to print to 
    logical,save :: isOpen = .false.              ! set to true if stream is open, false if not 
    character(len=20) :: frmBase = '((F7.4,X))'   ! the form of 1 "entry" of output
    character(len=20) :: frm                    ! output format string
    character(len=20) :: rowLength              ! how long is the output?
    integer :: i
    write(rowLength,*) vecSize ! workaround to get my format statement working: doesn't work with direct concetenation
    frm = frmBase(1:1)//trim(adjustl(rowLength))//frmBase(2:) ! set up the format string
    
    if(present(outUnit)) then ! if the optional unit number is given, write there
        inquire(unit=outUnit,opened=isOpen)
        if(isOpen) then
                write(outUnit,fmt=frm) (vector(i), i=1,vecSize)
                return
        end if
    else ! go to standard output
        write(*,fmt=frm) (vector(i), i=1,vecSize)
        return
    end if
    
    return

end subroutine printVector

subroutine printOutput(vector,matrix,systemSize,outUnit) ! TODO: finish subroutine
    implicit none
    integer, intent(in) :: systemSize
    real, dimension(systemSize,systemSize),optional, intent(in) :: matrix ! matrix of eigenvalues
    real, dimension(systemSize), intent(in) :: vector
    integer,optional, intent(in) :: outUnit ! stream to print to 
    logical :: isOpen

    isOpen = .false.

    if(present(outUnit)) then ! print to that unit
        inquire(unit=outUnit, opened=isOpen)
        if(isOpen) then
            write(outUnit,'(A,/)') "Eigenvalues of this hamiltonian are:"
            call printVector(vector,systemSize,outUnit)
            if(present(matrix)) then
                write(outUnit,'(/,A,/)') "Eigenvectors of this hamiltonian are:"
                call printMatrix(matrix,systemSize,outUnit)
            end if
        else
            write(*,*) "WARNING: the specified unit is not open for writing; no output written"
            return
        end if
    else !print to stdout
         write(outUnit,'(A,/)') "Eigenvalues of this hamiltonian are:"
         call printVector(vector,systemSize)
         if(present(matrix)) then
             write(outUnit,'(/,A,/)') "Eigenvectors of this hamiltonian are:"
             call printMatrix(matrix,systemSize)
         end if
    end if
    return

end subroutine printOutput

subroutine diagonalize(toDiagonalize,eigenvalues,systemSize,calcMode,precisionMode,exitStatus) ! TODO: test, add in more error handling?
! keep the calls to LAPACK out of the way, maybe come up with a better interface later
    
    implicit none
    integer, intent(in) :: systemSize
    real, dimension(systemSize,systemSize), intent(inout) :: toDiagonalize   ! array that needs to be altered by lapack routine (hamiltonian)
    real, dimension(systemSize), intent(inout) :: eigenvalues       ! array holding the eigenvalues
    real, allocatable, dimension(:) :: work                            ! necessary arguments to do with efficiency I think...
    integer :: lwork                                             
    integer, intent(inout) :: exitStatus                              ! contains exit status
    character(len=1), intent(inout) :: calcMode            ! which modes to use with the LAPACK routine
    character(len=1), intent(inout) :: precisionMode       ! single or double precision
    character(len=1), parameter :: whichTriangle='U'       ! use the upper or lower triangle of the hamiltonian matrix
    
    exitStatus = 0
    lwork = 3*systemSize - 1
  
   ! TODO: find how to check array lengths and resize from within subroutine 
   ! if(allocated(eigenvalues)) then 
   !     deallocate(eigenvalues)
   !     allocate(eigenvalues(sysmtemSize)
   ! end if
    
    if(calcMode .ne. 'N' .or. calcMode .ne. 'V') then
        write(*,*) 'WARNING: bad calculation mode:', calcMode, ' passed to DIAGONALIZE; changing to "N"...'
        calcmode = 'N'
    end if
    if(precisionMode .ne. 'S' .or. precisionMode .ne. 'D') then
        write(*,*) 'WARNING: bad precision mode:', precisionMode, ' passed to DIAGONALIZE; changing to "S"...'
        precisionMode = 'S'
    end if 
    ! actually call the LAPACK routine (after all that hassle)
    allocate(work(lwork))
    
    !DEBUG
     write(*,*) 'system size in subr: ',systemSize
     write(*,*) 
    !END DEBUG    
    if(precisionMode .eq. 'S') then
        call SSYEV(calcMode,whichTriangle,systemSize,toDiagonalize,systemSize,eigenvalues,work,lwork,exitStatus)
    else 
        call DSYEV(calcMode,whichTriangle,systemSize,toDiagonalize,systemSize,eigenvalues,work,lwork,exitStatus)
    end if

    deallocate(work)
    
    ! react to failed subroutine calls
    select case(exitStatus) !should really move this to main loop or separate subroutine called in the main loop, it's a TODO
    case(-9:-1)
        write(*,*) 'FATAL: bad argument number:', exitStatus, ' passed to LAPACK subroutine...' ! drama queen...
        return
    case(0) ! all clear
        return
    case default
        write(*,*) 'FATAL: LAPACK subroutine failed to converge...'
        return
    end select

end subroutine diagonalize

subroutine isSymmetric(array,systemSize,exitStatus) ! TODO:test, add in more error handling? 
! Is this necessary? Lapack only works with upper or lower triangle anyway...
! For large matrices would just specify the upper triangle in the inFile and then have the other elements as 0...
! should I do error handling outside the subroutine?
! test whether the parsed hamiltonian is symmetric (as it must be)
! return: 0 on success, 1 if not symmetric, 2 if not the correct shape
    implicit none
    integer, intent(in) :: systemSize
    real, dimension(systemSize,systemSize), intent(in) :: array      ! array to test for symmetry (hamlitonian must be symmetric)           
    integer, intent(out) :: exitStatus             ! Nuff said
    integer :: i,j                                 ! loop variables 
    
    ! now we just assume its square
    !if(size(array,1) .ne. size(array,2)) then       ! if the array is not square 
    !    write(*,*)
    !    write(*,*) 'FATAL: hamiltonian is not square' ! I love being dramatic
    !    exitStatus = 2
    !    return
    !end if  
   
    !DEBUG 
    do i=1,systemSize
        write(*,'(3(F2.0,X))') (array(i,j), j=1,systemSize)
    end do   
    !END DEBUG
    ! go through rows and columns check if array(i)(j)== array(j)(i), if not then set isSymmetric to 1
    rows: do i=1,systemSize
        columns: do j=i,systemSize     ! only need to check upper triangle 
            if(array(i,j) .ne. array(j,i)) then
                write(*,*)
                write(*,*) 'FATAL: hamiltonian is not symmetric'
                exitStatus = 1
                return
            end if
        end do columns
    end do rows
    exitStatus = 0  ! all clear- hamiltonian is symmetric
    return
end subroutine isSymmetric 


