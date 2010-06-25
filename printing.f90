!
! Module for the PRINTING subroutines to be used with huckel_energy
!
!

module printing

implicit none

contains

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


end module printing



