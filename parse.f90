!
! Module for the PARSING subroutines to be used in huckel_energy
!
!

module parse

use doublePrecision

implicit none 

contains 

subroutine getParams(inFile,systemSize,calcMode,outFile) !TODO: test, more error handling
! function to return the size of the hamiltonian stored in the file
    implicit none
    integer, intent(out) :: systemSize    
    character(len=1), intent(out) :: calcMode
    character(len=*), intent(in) :: inFile
    character(len=50), intent(out) :: outFile

    open(10,file=inFile,action="read")
    read(10,fmt='(A50)') outFile         ! outfile name up to 50 characters long
    read(10,'(A1)') calcMode              ! just eigenvalues or eigenvalues and eigenvectors?
    read(10,'(I2)') systemSize           ! hamiltonian dimensions
    close(10)
    
    if(.not.(calcMode .eq. 'N' .or. calcMode .eq. 'V')) then 
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
    real(kind=dp), dimension(systemSize,systemSize), intent(out) :: array ! the array to parse the stuff to
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

    rows: do i=1,systemSize
        read(unit=10,fmt=frm) (array(i,j), j=1,systemSize)    ! best to have whole matrix stored just to be safe, even though Lapack only needs 1 triangle
    end do rows                                             ! the extra computing power will be trivial for my hamiltonians
    close(10)
    
    return

end subroutine parseArray


end module parse

