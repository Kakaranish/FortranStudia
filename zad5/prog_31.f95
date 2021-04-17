PROGRAM p31
    IMPLICIT NONE

    INTEGER, PARAMETER :: rdp = SELECTED_REAL_KIND(15)
    REAL(KIND=rdp) :: x, xx
    INTEGER :: i

    DO i=1,5
        CALL random_number(x)
        CALL random_number(xx)
        PRINT *, x, xx
    END DO

    CALL init_random_seed()

    PRINT *
    DO
        CALL random_number(x)
        CALL random_number(xx)
        PRINT *, x, xx
    END DO
    
STOP

CONTAINS
    SUBROUTINE init_random_seed()
        INTEGER :: i, n, clock
        INTEGER, DIMENSION(:), ALLOCATABLE :: seed
        CALL random_seed(SIZE = n)
        PRINT *, "SIZE = ", n
        ALLOCATE(seed(n))
        CALL random_seed(GET = seed)
        PRINT *, "SEED = ", seed
        CALL system_clock(COUNT = clock)
        PRINT *, "CLOCK =", clock
        seed = clock + 37 * (/(i -1, i = 1, n)/)
        CALL random_seed(PUT = seed)
        print *, "SEED = ", seed

        DEALLOCATE(seed)
    END SUBROUTINE
END PROGRAM p31