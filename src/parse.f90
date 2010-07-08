!
! Module for the PARSING subroutines to be used in huckel_energy
!

module parse

    use doublePrecision

    implicit none 

    contains 

    subroutine getParams(inFile,systemSize,calcMode,outFile,exitStatus) !TODO: test, more error handling
    ! function to return the size of the hamiltonian stored in the file
    ! RETURN: 0: success, 1: missing args, 2: invalid CalcMode, 3: OPEN error on input file, 4: generic IO error

        implicit none
        integer, intent(out) :: exitStatus
        integer, intent(out) :: systemSize    
        character(len=1), intent(out) :: calcMode
        character(len=50), intent(out) :: inFile,outFile
        

        if(iargc() .ne. 3) then ! there is an argument missing
            exitStatus = 1
            return
        end if
        
        !get the parameters from the command line args passed and from the hamiltonian file
        call getarg(1,inFile)
        call getarg(2,outfile)
        call getarg(3,calcMode)
    
        ! we store the hamiltonian size in the file, not on the command line: more flexible that way
        open(10,file=inFile,action="read",iostat=exitStatus) 
        if(exitStatus .ne. 0) then
            exitStatus = 3 ! generic OPEN error
            return
        end if
        read(10,'(I2)',iostat=exitStatus) systemSize 
        if(exitStatus .ne. 0) then
            exitStatus = 4 ! generic IO error
            return
        end if
        close(10)
        
        ! check validity of args
        if(.not.(calcMode .eq. 'N' .or. calcMode .eq. 'V')) then ! invalud calcMode
            exitStatus=2
            return 
        end if
        
        exitStatus = 0
        return
      
    end subroutine getParams


    subroutine parseArray(inFile,array,systemSize,exitStatus) !TODO:test, add in error handling
    ! Parse the array from the file to the array variable 
        implicit none
        character(len=*), intent(in) :: inFile     
        integer, intent(in) :: systemSize 
        integer, intent(out) :: exitStatus
        real(kind=dp), dimension(systemSize,systemSize), intent(out) :: array ! the array to parse the stuff to
        character(len=20) :: frmBase = '((F1.0,X))'   ! how to format 1 entry of the input hamiltonian 
        character(len=20) :: frm                      ! the input format string
        character(len=20) :: rowLength                ! how long is the output?
        integer :: i,j                                ! loop variables 

        write(rowLength,*) systemSize                 ! workaround to get my format string working: doesn't work with direct concetenation
        frm = frmBase(1:1)//trim(adjustl(rowLength))//frmBase(2:) ! set up the format string
        
        ! open and advance past the hamiltonian size parameter
        open(10,file=inFile,action="read",iostat=exitStatus)
        if(exitStatus .ne. 0) then
            exitStatus = 3 ! OPEN error
            return
        end if
        read(10,*,iostat=exitStatus)          ! advance past the array size parameter

        rows: do i=1,systemSize  !changed the fmt statement as was inflexible
            read(unit=10,fmt=*) (array(i,j), j=1,systemSize)  ! LAPACK only needs 1 triangle, but we do whole array to be safe
        end do rows                                             ! the extra computing power will be trivial for my hamiltonians
        if(exitStatus .ne. 0) then
            exitStatus =4
            return
        end if

        close(10)
        
        exitStatus = 0
        return

    end subroutine parseArray


end module parse

