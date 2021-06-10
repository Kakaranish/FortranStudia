PROGRAM p2
    IMPLICIT NONE

    REAL, PARAMETER :: annual_interest_rate = 0.02
    REAL :: deposit_val = 1000.0
    INTEGER :: deposit_yrs = 30, i

    DO i=1, deposit_yrs
        deposit_val =  deposit_val + deposit_val * annual_interest_rate
        deposit_val = nint(deposit_val * 100.0) * 1E-2
    END DO

    PRINT *, "After ", deposit_yrs, " years in deposit will be ", deposit_val, " zl"
STOP
END PROGRAM p2
