!
! Module for the MATRIX OPERATIONS subroutines to be used with huckel_energy
!

module matrixOp

    use doublePrecision

    implicit none

    contains 

    subroutine diagonalize(toDiagonalize,eigenvalues,systemSize,calcMode,exitStatus) ! TODO: test, add in more error handling?
    ! keep the calls to LAPACK out of the way, maybe come up with a better interface later
    !
    !RETURN: 0: success, -9 -> -1: bad argument number passed to LAPACK, >0: LAPACK failed to diagonalize
    !

        implicit none
        integer, intent(in) :: systemSize
        real(kind=dp), dimension(systemSize,systemSize), intent(inout) :: toDiagonalize ! array that needs to be altered by lapack routine (hamiltonian)
        real(kind=dp), dimension(systemSize), intent(inout) :: eigenvalues              ! array holding the eigenvalues
        real(kind=dp), allocatable, dimension(:) :: work                                ! workspace to carry out diagonalisation
        integer :: lwork                                                                ! Size of workspace
        integer, intent(inout) :: exitStatus                                            ! contains exit status
        character(len=1), intent(inout) :: calcMode                                     ! which modes to use with the LAPACK routine
        character(len=1), parameter :: whichTriangle='U'                                ! use the upper or lower triangle of the hamiltonian matrix

       ! gfortran 4.1.2 will not allow us to pass assumed size ALLOCATABLE arrays and re/de/allocate them 
       ! makes my error checking job a whole lot harder
       !  
       ! if(allocated(eigenvalues)) then 
       !     deallocate(eigenvalues)
       !     allocate(eigenvalues(sysmtemSize)
       ! end if
        
        if(.not.(calcMode .eq. 'N' .or. calcMode .eq. 'V')) then
            write(stderr,'(A,A,A)') 'WARNING: bad calculation mode: ', calcMode, ' passed to DIAGONALIZE; changing to "N"...'
            calcmode = 'N'
        end if
        
        ! set the workspace to optimal size to perform the diagonalisation
        lwork=-1 !if lwork is set to -1 DSEYV sets work(1) as the optimal lwork
        allocate(work(1))
        call DSYEV(calcMode,whichTriangle,systemSize,toDiagonalize,systemSize,eigenvalues,work,lwork,exitStatus)
        lwork = work(1)
        deallocate(work)
        allocate(work(lwork)) ! allocate the optimal workspace

        ! actually call the LAPACK routine (after all that hassle)
        call DSYEV(calcMode,whichTriangle,systemSize,toDiagonalize,systemSize,eigenvalues,work,lwork,exitStatus)
        deallocate(work) 

    end subroutine diagonalize


    subroutine isSymmetric(array,systemSize,exitStatus)
    ! Check for symmetry of the parsed hamiltonian
    !
    ! RETURN: 0: symmetric, 1: not symmetric
    !
        implicit none
        integer, intent(in) :: systemSize
        real(kind=dp), dimension(systemSize,systemSize), intent(in) :: array   ! array to test for symmetry (hamlitonian must be symmetric)           
        integer, intent(out) :: exitStatus             ! Nuff said
        integer :: i,j                                 ! loop variables 
        
        ! iterate through upper triangle  and check symmetries
        rows: do i=1,systemSize
            columns: do j=i,systemSize     
                if(array(i,j) .ne. array(j,i)) then
                    exitStatus = 1 ! if 1 element is not correct, return with exit status 1
                    return
                end if
            end do columns
        end do rows
        exitStatus = 0  ! all clear- hamiltonian is symmetric
        return
    end subroutine isSymmetric 


    subroutine checkOrthogonal(matrix,systemSize,exitStatus)
    !
    ! RETURN: 0: matrix is orthogonal, 1: not orthogonal
    !

        implicit none
        real(kind=dp), dimension(systemSize,systemSize), intent(in) :: matrix
        real(kind=dp), dimension(systemSize,systemSize) :: prod ! matrix*matrix^T <-- very inefficient but easy to implement
        integer, intent(in) ::systemSize
        integer, intent(out) :: exitStatus
        integer :: i,j
        
        
        ! This is sooo inefficient; should really use the matrix as a scratchspace or in 
        ! some other fashion optimize this code, but this isn't really an important program so...
        prod = matmul(transpose(matrix),matrix)

        ! Check that the product mat*mat^T is the identity. As we are dealing with floating point
        ! arithmetic, we define a "zeroPoint" (in the doublePrecision module), which we 
        ! take as small enough so that any numbers less than this can be considered 0
        do i=1,systemSize
            do j=1,systemSize
                if(i .ne. j ) then
                    if(.not.(abs(prod(i,j)) .lt. zeroPoint)) then  ! if off diagonals are not close to 0 
                        exitStatus = 1
                        return
                    end if
                else if(.not.(abs(prod(i,j)-1) .lt. zeroPoint)) then    ! if diagonals are not close to 1
                    exitStatus = 1
                    return
                end if
            end do
        end do

        exitStatus = 0
        return

    end subroutine checkOrthogonal


end module matrixOp
