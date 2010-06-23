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
!                    2nd line is the size of the hamiltonian 
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
program huckelEnergy 

implicit none
character :: inFile = 'hamiltonian.ham' !  file containing our hamiltonian
integer, dimension(*,*), allocatable :: hamiltonian ! we don't know how big a system we are dealing with yet
integer :: systemSize ! the size of system (i.e. hamiltonian dimensions)
!TODO: include more variables to store eigenvalues (dont yet know format of LAPACK output
character(len=1), parameter :: calcMode='N' ! "N" to compute just eigenvalues; "V" to compute eigenvectors aswel

systemSize = getSize(inFile) ! get the system size from our file
allocate(hamiltonian(systemSize,systemSize))   ! now all the hamiltonian attributes (shape) are set
call parseArray(inFile,hamiltonian)            ! read in the hamiltonian from file
write(*,*) 'diagonalizing the hamiltonian:' ! output to give user a visual
call printArray(hamiltonian,systemSize)

if(isSymmetric(hamiltonian) .ne. 0) then ! no way to recover from asymmetric hamiltonian
    deallocate(hamiltonian)
    stop
endif

diagonalize(hamiltonian,eigenvectors,calcMode)

! write(toScreen,toFile)
!>>>END



contains function getSize(inFile) !TODO: test, more error handling
! function to return the size of the hamiltonian stored in the file
    implicit none
    integer, intent(out) :: getSize
    character(len=*),intent(in) :: inFile

    open(10,file=inFile)
    
    read(10,*) ! advance past the output file name
    read(10,*) getsize !hamiltonian size
    close(10)
    
    return
  
end function getSize

contains subroutine parseArray(inFile,array) !TODO:test, add in error handling
! Parse the array from the file to the array variable 
    implicit none
    character(len=*), intent(in) :: inFile
    integer, dimension(:,:) intent(out) :: array ! does the array carry over its attributes like shape? better play it safe...

    ! only need the upper triangle, as this is all the LAPACK routine, SSYEV needs 
    open(10,file=inFile)
    rows: do i=lbound(array,1),ubound(array,1)
        read(10,*) (array(i,j), j=lbound(array,2),ubound(array,2)) ! best to have whole matrix stored just to be safe
    end do rows                                                   ! the extra computing power will be trivial for my hamiltonians
    close(10)
    
    return

end function parseArray

contains subroutine printArray(array,outUnit) ! TODO: test, add in error handling
    ! write the array to stdout
    implicit none
    integer,dimension(:,:), intent(in) :: array
    integer, intent(in),optional :: outUnit   ! the stream to print to 
    logical,save :: isOpen=.false.              ! set to true if stream is open, false if not 
    character(len=20) :: frmBase = '((I2,X))'   ! the form of 1 "unit2 of output
    character(len=20) :: frm                    ! output format string
    character(len=20) :: rowLength              ! how long is the output?
    
    write(rowLength,*) size(array,2) ! workaround to get my format statement working: doesn't work with direct concetenation
    frm = frmBase(1:1)//trim(rowLength)//frmBase(2:) ! set up the format string
    
    if(present(outUnit) then ! if the optional unit number is given, write there
        inquire(unit=outUnit,opened=isOpen)
        if(isOpen) then
            do i=lbound(array,1),ubound(array,1)
                write(outUnit,fmt=frm) (array(i,j), j=lbound(array,2), ubound(a rray,2))
            end do
            return
        endif
    else ! go to standard output
        do i=lbound(array,1),ubound(array,1)
        write(*,fmt=frm) (array(i,j), j=lbound(array,2), ubound(a rray,2))
        end do
        return
    endif

end subroutine printArray

contains subroutine printOutput(outUnit) ! TODO: finish subroutine
    implicit none
    integer, intent(in), optional :: outUnit ! stream to print to 
    


end subroutine printOutput

contains subroutine diagonalize(toDiagonalize,eigenvalues,calcMode) ! TODO: test, add in more error handling?
! keep the calls to LAPACK out of the way, maybe come up with a better interface later
    
    implicit none
    real, dimension(:,:), intent(inout) :: toDiagonalize   ! array that needs to be altered by lapack routine (hamiltonian)
    real, dimension(:), intent(inout) :: eigenvalues       ! array holding the eigenvalues
    real, dimension(:), intent(inout) ::  work              ! necessary arguments to do with efficiency I think...
    integer, intent(inout) :: systemSize,lwork,exitStatus  ! no need to make repeated SIZE() calls, also exit status information from subr    
    character(len=1), intent(inout) :: calcMode                ! which modes to use with the LAPACK routine
    character(len=1), parameter :: whichTriangle       ! use the upper or lower triangle of the hamiltonian matrix
    
    exitStatus = 0
    systemSize = size(toDiagonalize,1)
    lwork = 3*systemSize - 1
    whichTriangle = 'U'
    
    ! just check we've been passed good arguments
    if(size(eigenvalues,1) .ne. systemSize) then 
        write(*,*) 'WARNING: bad eigenvalue array passed to DIAGONALIZE; reallocating...'
        deallocate(eigenvalues)
        allocate(eigenvalues(systemSize))
    endif
    if(calcMode .ne. 'N' .or. calcMode .ne. 'V') then
        write(*,*) 'WARNING: bad calculation mode:', calcMode, 'passed to DIAGONALIZE; changing to "N"...'
        calcmode
    endif
    
    ! actually call the LAPACK routine (after all that hassle)
    allocate(work(lwork))
    SSEYV(calcMode,whichTriangle,systemSize,toDiagonalize,systemSize,eigenvalues,work,lwork,exitStatus)
    deallocate(work)
    
    ! react to failed subroutine calls
    select case(exitStatus)
    case(-9:-1)
        write(*,*) 'FATAL: bad argument number:', exitStatus, ' passed to LAPACK subroutine...' ! drama queen...
        deallocate(toDiagonalize)
        stop
    case(0) ! all clear
        return
    case default
        write(*,*) 'FATAL: LAPACK subroutine failed to converge...'
        deallocate(toDiagonalize)
        stop

end subroutine diagonalize

contains function isSymmetric(array) ! TODO:test, add in more error handling?
! test whether the parsed hamiltonian is symmetric (as it must be)

    implicit none
    integer, dimension(:,:), intent(in) :: array      ! array to test for symmetry (hamlitonian must be symmetric)
    integer, intent(out) :: isSymmetric               ! return: 0 on success, 1 if not symmetric, 2 if not the correct shape
    
    if(shape(array,1) .ne. shape(array,2)) then       ! if the array is not square 
        write(*,*)
        write(*,*) 'FATAL: hamiltonian is not square' ! I love being dramatic
        isSymmetric = 2
        return
    endif  
    
    ! go through rows and columns check if array(i)(j)== array(j)(i), if not then set isSymmetric to 1
    rows: do i=lbound(array,1),ubound(array,1)
        columns: do j=i+1,ubound(array,2)     ! only need to check upper triangle 
            if(array(i,j) .ne. array(j,i)) then
                write(*,*)
                write(*,*) 'FATAL: hamiltonian is not symmetric'
                isSymmetric = 1
                return
            end if
        end do columns
    end do rows
    isSymmetric = 0  ! all clear- hamiltonian is symmetric
    return
end function isSymmetric 

end program huckelEnergy
