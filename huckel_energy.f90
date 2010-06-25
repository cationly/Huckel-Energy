! A short F95 code to calculate the energy eigenvalues for a hamiltonian
! for a simple molecule using Huckel theory
! 
! We assume that all the bond lengths are the same so that the energies
! can be given in units of the off-diagonal hamiltonian elements and the
! 0-energy taken to be the average energy ( the diagonal hamiltonian elements)
!
! COMPILING HUCKEL_ENERGY: compile all source files (huckel_energy.f90, doublePrecision.f90
!                          parse.f90, printing.f90, matrixOp.f90) separately and then link
!                          the object files together and with the LAPACK and LBLAS libraries
!
!
! CALLING HUCKEL_ENERGY: /path/to/huckel:~$ ./huckel_energy $INFILE $OUTFILE $CALCMODE
!                        
!                         INFILE: the file to read the hamiltonian from. It MUST be formatted
!                                 so that the 1st line contains only the size of the hamiltonian
!                                 and subsequent lines contain a matrix of space-separated numbers
!                                 representing the hamiltonian in the Huckel Basis, with beta set
!                                 to 1 and alpha set to 0 (and all other elements 0)
!
!                        OUTFILE: the name of the file to have the results written to. Output is
!                                 in the formatted such that the first line contains a space separated
!                                 list of the hamiltonian's eigenvalues and (optionally) subsequent lines
!                                 contain an orthogonal matrix whose columns correspond to the eigenvectors
!                                 of the hamiltonian. 
!
!                       CALCMODE: A single character, either an "N" or a "V". "N" means that the output will
!                                 only contain the eigenvalues, "V" means that output will also contain the 
!                                 matrix of eigenvectors 
!    
!

program huckelEnergy 

use doublePrecision
use parse   
use printing
use matrixOp

implicit none
character(len=50) :: inFile,outFile          !  file containing our hamiltonian and the file to write to
real(kind=dp), allocatable, dimension(:,:) :: hamiltonian ! we don't know how big a system we are dealing with yet
real(kind=dp), allocatable, dimension(:) :: eigenvalues   ! thus we don't know how many eigenvalues we need to store
integer :: systemSize,exitStatus                   ! the size of system (i.e. hamiltonian dimensions), and the exitStatus of our subroutines, changed each time it is passed
character(len=1) :: calcMode                       ! "N" to compute just eigenvalues; "V" to compute eigenvectors as well

call getParams(inFile,systemSize,calcMode,outFile,exitStatus) ! read system data from our input file
select case(exitStatus)
    case(1) ! bad num. of command line options: huckel_energy takes 3
        write(*,*) "FATAL: bad number of command line arguments: ", iargc()
        write(*,*) "command line: huckel_energy $INFILE $OUTFILE $CALCMODE"
        stop
    case(2) ! bad character specifying 
        write(*,*) "WARNING: invalid CalcMode argument: ", calcMode, " passed: choosing 'N'..."
        calcMode ="N"
    case(0)
    ! returned success    
    case default
        write(*,*) "WARNING: Unknown exitStatus returned from getParams"
end select

allocate(hamiltonian(systemSize,systemSize))   ! now all the hamiltonian attributes (shape) are set
allocate(eigenvalues(systemSize))

call parseArray(inFile,hamiltonian,systemSize)            ! read in the hamiltonian from file
call isSymmetric(hamiltonian,systemSize,exitStatus)
if(exitStatus .ne. 0) then ! no way to recover from asymmetric hamiltonian
    deallocate(hamiltonian)
    deallocate(eigenvalues)
    stop
end if

write(*,*) 'diagonalizing the hamiltonian:' ! output to give user a visual
call printmatrix(hamiltonian,systemSize)
call diagonalize(hamiltonian,eigenvalues,systemSize,calcMode,exitStatus) 
if(exitStatus .ne. 0) then ! if there were problems
    deallocate(hamiltonian)
    deallocate(eigenvalues)
    stop
end if

call printOutput(eigenvalues,hamiltonian,systemSize)
open(unit=10,file=outFile,action="WRITE",status="REPLACE")
if(calcMode .eq. 'N') then                 
    call printOutput(vector=eigenvalues,systemSize=systemSize,outUnit=10) ! if only eigenvalues calculated, only print eigenvalues..
else                ! Why will this not interpret a type mismatch as the "optional" argument not being present?
    call printOutput(eigenvalues,hamiltonian,systemSize,10) ! else print the eigenvectors too
end if
close(10)

deallocate(hamiltonian)
deallocate(eigenvalues)

end program huckelEnergy
