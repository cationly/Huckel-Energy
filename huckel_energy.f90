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

use module parse   
use module printing
use module matrixOp

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
call diagonalize(hamiltonian,eigenvalues,systemSize,calcMode,precisionMode,exitStatus) 
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

end program huckelEnergy
