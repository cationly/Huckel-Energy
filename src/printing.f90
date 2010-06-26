!
! Module for the PRINTING subroutines to be used with huckel_energy
!
!

module printing

    use doublePrecision

    implicit none

    contains

    subroutine printMatrix(matrix,matSize,outUnit,exitStatus) 
    ! Print out a matrix of DP variables in a pretty format
    !
    ! RETURN: 0: success, 1: file WRITE error, 2: file OPEN error, 3: stdout error
    !
        
        implicit none
        integer, intent(in) :: matSize
        integer, intent(out) :: exitStatus
        real(kind=dp),dimension(matSize,matSize), intent(in) :: matrix
        integer, intent(in),optional :: outUnit       ! the stream to print to 
        logical,save :: isOpen = .false.              ! set to true if stream is open, false if not 
        character(len=20) :: frmBase = '((F6.3,2X))'   ! the form of 1 "entry" of output
        character(len=20) :: frm                      ! output format string
        character(len=20) :: rowLength                ! how long is the output?
        integer :: i,j                                ! loop variables 
        

        ! create the format string
        write(rowLength,*) matSize                 ! workaround to get my format statement working: doesn't work with direct concetenation
        frm = frmBase(1:1)//trim(adjustl(rowLength))//frmBase(2:) ! set up the format string
        
        !Write the output based on arguments
        if(present(outUnit)) then ! if the optional unit number is given, write there
            inquire(unit=outUnit,opened=isOpen)
            if(isOpen) then
                do i=1,matSize
                    write(outUnit,fmt=frm,iostat=exitStatus) (matrix(i,j), j=1, matsize)
                end do
                return
            else                
                exitStatus = 2  ! unit OPEN error
                return
            end if
        else ! go to standard output
            do i=1,matSize
                write(*,fmt=frm,iostat=exitStatus) (matrix(i,j), j=1, matSize)
            end do
            if(exitStatus .ne. 0) exitStatus=3 !stdout error
            return
        end if
        
    end subroutine printMatrix


    subroutine printVector(vector,vecSize,outUnit,exitStatus)
    ! Print vector of DP variables in a pretty format
    !
    ! RETURN: 0: success, 1:file WRITE error, 2: file OPEN error, 3: stdout error
    !

        implicit none
        integer, intent(in) :: vecSize
        integer, intent(out) :: exitStatus
        real(kind=dp),dimension(vecsize), intent(in) :: vector
        integer, intent(in),optional :: outUnit   ! the stream to print to 
        logical,save :: isOpen = .false.              ! set to true if stream is open, false if not 
        character(len=20) :: frmBase = '((F7.4,2X))'   ! the form of 1 "entry" of output
        character(len=20) :: frm                    ! output format string
        character(len=20) :: rowLength              ! how long is the output?
        integer :: i
        write(rowLength,*) vecSize ! workaround to get my format statement working: doesn't work with direct concetenation
        frm = frmBase(1:1)//trim(adjustl(rowLength))//frmBase(2:) ! set up the format string
        
        if(present(outUnit)) then ! if the optional unit number is given, write there
            inquire(unit=outUnit,opened=isOpen)
            if(isOpen) then
                    write(outUnit,fmt=frm,iostat=exitStatus) (vector(i), i=1,vecSize)
                    if(exitStatus .ne. 0) exitStatus=1 ! don't want to pollute my exitStatus space 
                    return
            else ! the unit is not open
                exitStatus = 2
                return
            end if
        else ! go to standard output
            write(*,fmt=frm,iostat=exitStatus) (vector(i), i=1,vecSize)
            if(exitStatus .ne. 0) exitStatus=3
            return
        end if
        
        return

    end subroutine printVector


    subroutine printOutput(vector,matrix,systemSize,calcMode,outUnit,exitStatus) ! TODO: finish subroutine
    !   Print the calculation output 
    !
    ! RETURN: 0: success, 1: file OPEN error, 2: file WRITE error, 3: stdout error
    !
        implicit none
        integer, intent(in) :: systemSize
        integer, intent(out) :: exitStatus
        real(kind=dp), dimension(systemSize,systemSize),optional, intent(in) :: matrix ! matrix of eigenvalues
        real(kind=dp), dimension(systemSize), intent(in) :: vector
        character(len=1),intent(in) :: calcMode
        integer,optional, intent(in) :: outUnit ! stream to print to 
        logical :: isOpen

        isOpen = .false.

        if(present(outUnit)) then ! print to that unit
            inquire(unit=outUnit, opened=isOpen)
            if(isOpen) then 
                call printVector(vector,systemSize,outUnit,exitStatus)
                if(exitStatus .ne. 0) return ! pass exitStatus up
                if(present(matrix)) then
                    write(outUnit,*) ! blank line
                    call printMatrix(matrix,systemSize,outUnit,exitStatus)
                    if(exitStatus .ne. 0) return ! pass exitStatus up
                end if
            else
                exitStatus=2
                return
            end if
        else !print to stdout
             write(*,'(A,/)') "Eigenvalues of this hamiltonian are:"
             call printVector(vector,systemSize,exitStatus=exitStatus)
             if(exitStatus .ne. 0) return
             if(present(matrix) .and. calcMode .eq. "V") then
                 write(*,'(/,A,/)') "Eigenvectors of this hamiltonian are:"
                 call printMatrix(matrix,systemSize,exitStatus=exitStatus)
                 if(exitStatus .ne. 0) return
             end if
        end if
        return

    end subroutine printOutput


end module printing



