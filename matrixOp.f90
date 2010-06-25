!
! Module for the MATRIX OPERATIONS sunroutines to be used with huckel_energy
!
!

module matrixOp

use doublePrecision

implicit none

contains 

subroutine diagonalize(toDiagonalize,eigenvalues,systemSize,calcMode,exitStatus) ! TODO: test, add in more error handling?
! keep the calls to LAPACK out of the way, maybe come up with a better interface later
    
    implicit none
    integer, intent(in) :: systemSize
    real(kind=dp), dimension(systemSize,systemSize), intent(inout) :: toDiagonalize   ! array that needs to be altered by lapack routine (hamiltonian)
    real(kind=dp), dimension(systemSize), intent(inout) :: eigenvalues       ! array holding the eigenvalues
    real(kind=dp), allocatable, dimension(:) :: work                            ! necessary arguments to do with efficiency I think...
    integer :: lwork                                             
    integer, intent(inout) :: exitStatus                              ! contains exit status
    character(len=1), intent(inout) :: calcMode            ! which modes to use with the LAPACK routine
    character(len=1), parameter :: whichTriangle='U'       ! use the upper or lower triangle of the hamiltonian matrix
    
    exitStatus = 0
    lwork = 3*systemSize - 1
  
   ! TODO: find how to check array lengths and resize from within subroutine 
   ! if(allocated(eigenvalues)) then 
   !     deallocate(eigenvalues)
   !     allocate(eigenvalues(sysmtemSize)
   ! end if
    
    if(.not.(calcMode .eq. 'N' .or. calcMode .eq. 'V')) then
        write(*,*) 'WARNING: bad calculation mode: ', calcMode, ' passed to DIAGONALIZE; changing to "N"...'
        calcmode = 'N'
    end if
    
    ! actually call the LAPACK routine (after all that hassle)
    allocate(work(lwork))
    call DSYEV(calcMode,whichTriangle,systemSize,toDiagonalize,systemSize,eigenvalues,work,lwork,exitStatus)
    deallocate(work)
    
    ! react to failed subroutine calls
    select case(exitStatus) !should really move this to main loop or separate subroutine called in the main loop, it's a TODO
    case(-9:-1)
        write(*,*) 'FATAL: bad argument number: ', exitStatus, ' passed to LAPACK subroutine...' ! drama queen...
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
    real(kind=dp), dimension(systemSize,systemSize), intent(in) :: array      ! array to test for symmetry (hamlitonian must be symmetric)           
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


end module matrixOp
