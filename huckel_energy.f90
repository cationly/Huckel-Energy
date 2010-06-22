! A short F95 code to calculate the energy eigenvalues for a hamiltonian
! for a simple molecule using Huckel theory
! 
! We assume that all the bond lengths are the same so that the energies
! can be given in units of the off-diagonal hamiltonian elements and the
! 0-energy taken to be the average energy ( the diagonal hamiltonian elements)
!
! The hamiltonian is stored in a file calles "hamiltonian.ham" in the same directory 
! as the executable
!
!TODO: add support for hamiltonian calculation based on molecule structure? 

program huckelEnergy 

implicit none
character :: inFile = 'hamiltonian.ham'
integer, dimension(*,*) :: hamiltonian

hamiltonian = parseArray(inFile)            ! read in the hamiltonian from file
write(*,*) 'diagonalizing the hamiltonian:' ! output to give user a visual
printArray(hamiltonian)

if(isSymmetric .ne. 0) stop ! no way to recover from asymetric hamiltonian
! call lapack diagonalise routine on hamiltonian


contains function parseArray(inFile)

    implicit none
    character(len=*), intent(in) :: inFile
    integer, dimension(:,:) intent(out) :: array
    
    !TODO: read in space separated list of single precision numbers to array "hamiltonian"
    


    return
end function parseHamiltonian

contains function isSymmetric(array) ! TODO:test

    implicit none
    integer, dimension(:,:), intent(in) :: array ! array to test for symmetry (hamlitonian must be symmetric)
    integer, intent(out) :: isSymmetric          ! return: 0 on success, 1 if not symmetric, 2 if not the correct shape
    integer, intent(inout) :: i,j                ! counters and the number of rows and columns (dimension)
    
    if(shape(array)(1) .ne. shape(array)(2)) then ! if the array is not square 
        write(*,*)
        write(*,*) 'FATAL: hamiltonian is not square' ! I love being dramatic
        isSymmetric = 1
        return
    endif  
    
    ! go through rows and columns check if array(i)(j)== array(j)(i), if not then set isSymmetric to 1
rows    do i=lbound(array,1),ubound(array,1)
columns     do j=i+1,ubound(array,2)     ! only need to check upper triangle 
                if(array(i,j) .ne. array(j,i)) then
                    write(*,*)
                    write(*,*) 'FATAL: hamiltonian is not symmetric'
                    isSymmetric = 2
                    return
                end if
            end do columns
        end do rows
    isSymmetric = 0  ! all clear, is symmetric and 
    return
end function isSymmetric 

contains subroutine printArray(array) ! TODO:test
    ! write the array to stdout
    implicit none
    integer,dimension(:,:), intent(in) :: array
    integer
    do i=lbound(array,1), ubound(array(1))
        write(*,*) (array(i,j), j=lbound(array,2), ubound(array,2))
    end do
    return
end subroutine printArray

end program huckelEnergy
