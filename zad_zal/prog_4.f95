PROGRAM p4
    IMPLICIT NONE
    INTEGER, PARAMETER :: rdp = selected_real_kind(15)
    INTEGER, PARAMETER :: idp = selected_int_kind(15)
    INTEGER(kind = idp), PARAMETER :: max_do = 10000000
    REAL(kind = rdp ), PARAMETER :: pi = 3.14159265358979326433824338327950288_rdp
    REAL(kind = rdp) :: start = 0.0, end = pi
    REAL(kind = rdp) :: result

    CALL init_random_seed()

    result = integration(sin_rdp, start, end, max_do)

    PRINT *, result
STOP
CONTAINS

    REAL(kind = rdp) function sin_rdp(x)
        implicit none
        REAL(kind = rdp), intent(in) :: x
        sin_rdp = sin(x)
    end function sin_rdp

    FUNCTION integration(func, start, end, samples_num) RESULT (monte_carlo_integration)
        REAL(kind = rdp) :: func
        REAL(kind = rdp) :: monte_carlo_integration
        REAL(kind = rdp), INTENT(IN) :: start, end
        INTEGER(kind = idp), INTENT(IN) :: samples_num
    
        INTEGER(kind = idp) :: i
        REAL(kind = rdp) :: sum, rand
        
        sum = 0
        do i=1, samples_num
          call random_number(rand)
          sum = sum + func(start + (end-start)*rand)
        end do  
        monte_carlo_integration = (end-start) * sum / samples_num
    END FUNCTION integration

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
END PROGRAM p4
