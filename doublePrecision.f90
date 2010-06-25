!
! Module containing the double precision definitions and also parameters which Huckel will use
!

module doublePrecision

    integer, parameter :: dp = selected_real_kind(15,307) ! define 15 sig fig of decimal precision, max exponent of 307

end module doublePrecision
