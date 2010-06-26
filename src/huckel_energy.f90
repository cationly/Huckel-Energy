! A short F95 code to calculate the energy eigenvalues for a hamiltonian
! for a simple molecule using Huckel theory
! 
! We assume that all the bond lengths are the same so that the energies
! can be given in units of the off-diagonal hamiltonian elements and the
! 0-energy taken to be the average energy ( the diagonal hamiltonian elements)

program huckelEnergy 

    use doublePrecision ! defines DP vars. and also stderr unit & 0-point number (see module for details)
    use parse           ! lets us parse info from command line and inFiles
    use printing        ! lets us print in a pretty format
    use matrixOp        ! lets us fiddle with the hamiltonian

    implicit none
    character(len=50) :: inFile,outFile                       !  file containing our hamiltonian and the file to write to
    real(kind=dp), allocatable, dimension(:,:) :: hamiltonian ! we don't know how big a system we are dealing with yet
    real(kind=dp), allocatable, dimension(:) :: eigenvalues   ! thus we don't know how many eigenvalues we need to store
    integer :: systemSize,exitStatus                          ! the hamiltonian dimension and the exitStatus of our subroutines
    character(len=1) :: calcMode                              ! "N" to compute just eigenvalues; "V" to compute eigenvectors as well

    ! read system data from the command line and input file
    call getParams(inFile,systemSize,calcMode,outFile,exitStatus) 
    select case(exitStatus)
        case(1) ! bad num. of command line options: huckel_energy takes 3
            write(stderr,'(A,I1)') "FATAL: bad number of command line arguments: ", iargc()
            write(stderr,*) "command line: huckel_energy $INFILE $OUTFILE $CALCMODE"
            stop
        case(2) ! bad character specification 
            write(stderr,'(A,A1,A)') "WARNING: invalid CalcMode argument: ", calcMode, " passed: choosing 'N'..."
            calcMode ="N"
        case(3) ! OPEN error
            write(stderr,*) "FATAL: opening file '", trim(adjustl(inFile)), "' failed"
            stop
        case(4) ! READ error
            write(stderr,*)"FATAL: reading file '", trim(adjustl(inFile)), "' failed"
            stop
        case(0)
        ! returned success    
        case default
            write(stderr,*) "FATAL: Unknown exitStatus returned from getParams"
            stop
    end select

    allocate(hamiltonian(systemSize,systemSize))   ! now all the hamiltonian attributes (shape) are set
    allocate(eigenvalues(systemSize))

    call parseArray(inFile,hamiltonian,systemSize,exitStatus)            ! read in the hamiltonian from file
    select case(exitStatus)
        case(3) ! OPEN error
            write(stderr,*) "FATAL: opening file '", trim(adjustl(inFile)), "' failed"
            stop
        case(4) ! READ error
            write(stderr,*)"FATAL: reading file '", trim(adjustl(inFile)), "' failed"
        case(0)
        ! returned success    
        case default
            write(stderr,*) "FATAL: Unknown exitStatus returned from getParams"
            stop
    end select

    ! check that hamiltonian is symmetric
    call isSymmetric(hamiltonian,systemSize,exitStatus) 
    if(exitStatus .ne. 0) then ! no way to recover from asymmetric hamiltonian
        write(stderr,*) "FATAL: hamiltonian parsed from '", trim(adjustl(inFile)), "' is not symmetric"
        deallocate(hamiltonian)
        deallocate(eigenvalues)
        stop
    end if

    ! output the hamiltonian, to give user visual
    write(*,*) 'diagonalizing the hamiltonian:' 
    call printMatrix(hamiltonian,systemSize,exitStatus=exitStatus)
    if(exitStatus .ne. 0) write(stderr,*) "WARNING: write error to stdout"

    ! Diagonalize our hamiltonian
    call diagonalize(hamiltonian,eigenvalues,systemSize,calcMode,exitStatus) 
    select case(exitStatus) !should really move this to main loop or separate subroutine called in the main loop, it's a TODO
    case(-9:-1) ! bad argument passed to lapack routine: LAPACK will complain anyway but still...
            write(stderr,'(A,I2,A)') 'FATAL: bad argument number: ', exitStatus, ' passed to LAPACK subroutine' ! drama queen...
            deallocate(hamiltonian)
            deallocate(eigenvalues)
            stop
        case(0) ! all clear
        case default ! a +ve exit status means that LAPACK failed to diagonalize
            write(stderr,*) 'FATAL: LAPACK subroutine failed to converge'
            deallocate(hamiltonian)
            deallocate(eigenvalues)
            stop
        end select

    ! Check that the output eigenvectors are orthogonal
    if(calcMode .eq. 'V') then ! if the hamiltonian was replaced my eigenvector matrix
        call checkOrthogonal(hamiltonian,systemSize,exitStatus)
        if(exitStatus .ne. 0) write(*,*) "INFO: calculated eigenvectors are not orthogonal" 
    end if

    ! Print output to the screen  and to the outfile specified
    call printOutput(eigenvalues,hamiltonian,systemSize,calcMode,exitStatus=exitStatus) ! print to stdout
    select case(exitStatus)
        case(0)
            ! success
        case(3) !stdout print error
            write(stderr,*) "WARNING: IO error when printing to stdout"
        case default
            write(stderr,*) "WARNING: unknown exitStatus returned from printOutput()"
    end select

    open(unit=10,file=outFile,action="WRITE",status="REPLACE")
    if(calcMode .eq. 'N') then !only print the eigenvalues if that was all that was calculated                
        call printOutput(vector=eigenvalues,systemSize=systemSize,calcMode=calcMode,outUnit=10,exitStatus=exitStatus) 
    else               
        call printOutput(eigenvalues,hamiltonian,systemSize,calcMode,10,exitStatus) ! else print the eigenvectors too
    end if
    close(10)
    select case(exitStatus)
    case(0)
        !success
    case(1) !file print err
        write(stderr,*) "WARNING: IO error when printing to '",trim(adjustl(outFile)), "'"
    case(2) ! unit not open
        write(stderr,*) "WARNING: unit 10 not open for writing, no output written"
    case(3) !stdout print error
        write(stderr,*) "WARNING: IO err when printing to stdout"
    end select

    ! Cleanup
    deallocate(hamiltonian)
    deallocate(eigenvalues)

end program huckelEnergy
