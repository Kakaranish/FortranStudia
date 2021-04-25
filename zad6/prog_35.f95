PROGRAM p35
    IMPLICIT NONE
    REAL :: a, b, c, d, e, f
    INTERFACE 
        SUBROUTINE root(rank, x, y)
            IMPLICIT NONE
            REAL, INTENT(IN) :: rank, x
            REAL, INTENT(OUT) :: y
        END SUBROUTINE root
    END INTERFACE 

    a = 37 ; CALL root(3., a, b) ; WRITE(*,*) "cube root of ", a, " equal ", b
    c = 16 ; CALL root(4., c, d) ; WRITE(*,*) "4-th root of ", c, " equal ", d
    e = 1024 ; CALL root(10., e, f) ; WRITE(*,*) "10-th root of ", e, " equal ", f
STOP
END PROGRAM p35

SUBROUTINE root(rank, x, y)
    IMPLICIT NONE
    REAL, INTENT(IN) :: rank, x
    REAL, INTENT(OUT) :: y
    y = EXP(LOG(X) / rank)
    RETURN
END SUBROUTINE