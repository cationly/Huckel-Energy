!
! Module for the PARSING subroutines to be used in huckel_energy
!
!

module parse

use doublePrecision

implicit none 

contains 

subroutine getParams(inFile,systemSize,calcMode,outFile,exitStatus) !TODO: test, more error handling
! function to return the size of the hamiltonian stored in the file
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
    open(10,file=inFile,action="read")   ! the size is stored with the hamiltonian, thus the same args can 
    read(10,'(I2)') systemSize           ! be passed with different input hamiltonians with no limit on size
    close(10)
    
    if(.not.(calcMode .eq. 'N' .or. calcMode .eq. 'V')) then 
        write(*,*) 'WARNING: invalid calculation mode in input file: choosing "N"...'
        calcMode = 'N'
        exitStatus=2
        return 
    end if
    
    exitStatus = 0
    return
  
end subroutine getParams


subroutine parseArray(inFile,array,systemSize) !TODO:test, add in error handling
! Parse the array from the file to the array variable 
    implicit none
    character(len=*), intent(in) :: inFile     
    integer, intent(in) :: systemSize 
    real(kind=dp), dimension(systemSize,systemSize), intent(out) :: array ! the array to parse the stuff to
    character(len=20) :: frmBase = '((F1.0,X))'   ! the form of 1 "entry" of input
    character(len=20) :: frm                      ! output format string
    character(len=20) :: rowLength                ! how long is the output?
    integer :: i,j                                ! loop variables 

    write(rowLength,*) systemSize                 ! workaround to get my format statement working: doesn't work with direct concetenation
    frm = frmBase(1:1)//trim(adjustl(rowLength))//frmBase(2:) ! set up the format string
    
    open(10,file=inFile,action="read")
    read(10,*)          ! advance past the array size parameter

    rows: do i=1,systemSize
        read(unit=10,fmt=frm) (array(i,j), j=1,systemSize)    ! best to have whole matrix stored just to be safe, even though Lapack only needs 1 triangle
    end do rows                                             ! the extra computing power will be trivial for my hamiltonians
    close(10)
    
    return

end subroutine parseArray


end module parse

