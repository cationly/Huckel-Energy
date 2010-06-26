!
! Module containing the double precision definitions and also parameters which Huckel will use
!

module doublePrecision

    integer, parameter :: dp = selected_real_kind(15,307) ! define 15 sig fig of decimal precision, max exponent of 307
    real(kind=dp), parameter :: zeroPoint= 1.0E-12        ! define "0" as anything with magnitude smaller than 10^-12
    integer, parameter :: stderr = 0                      ! this will be used to acertain whether eigenvectors are orthogonal or
    ! stderr stream unit number                           ! not

end module doublePrecision
