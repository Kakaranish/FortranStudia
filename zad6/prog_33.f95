PROGRAM p33
    IMPLICIT NONE
    INTEGER, PARAMETER :: rdp = selected_real_kind(15)
    INTEGER, PARAMETER :: idp = selected_int_kind(15)
    REAL(kind = rdp) :: xx, yy, rr, pi_moje, delta
    REAL(kind = rdp ) :: pi = 3.14159265358979326433824338327950288_rdp
    REAL :: x
    INTEGER(kind = idp) :: i, wewnatrz
    INTEGER, PARAMETER :: max_do = 1000000

    CALL init_random_seed()
    wewnatrz = 0
    DO i = 1, max_do
        CALL RANDOM_NUMBER(XX)
        CALL RANDOM_NUMBER(YY)
        rr = xx ** 2 + yy**2
        IF (rr <= 1.0_rdp) wewnatrz = wewnatrz + 1
    END DO

    pi_moje = REAL(wewnatrz) / REAL(max_do) * 4.0_rdp
    delta = ABS((pi - pi_moje)/pi)

    PRINT *
    PRINT *, "pi_moje = ", pi_moje, "blad_wzgledny = ", delta
STOP
CONTAINS
    SUBROUTINE init_random_seed()
        INTEGER :: i, n, clock
        INTEGER, DIMENSION(:), ALLOCATABLE :: seed

        CALL RANDOM_SEED(SIZE = n)
        ALLOCATE(seed(n))

        CALL SYSTEM_CLOCK(COUNT = clock)

        seed = clock + 37 * (/(i - 1, i = 1, n)/)
        CALL RANDOM_SEED(PUT = seed)
        DEALLOCATE(seed)
    END SUBROUTINE
END PROGRAM p33
