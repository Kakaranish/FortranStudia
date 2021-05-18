PROGRAM p42
    IMPLICIT NONE
    REAL :: a, b, c
    WRITE (UNIT=*, FMT=*) " please enter three numbers to be sorted."
    READ (UNIT=*, FMT=*) a, b, c
    IF(a > b) CALL SWAP (a,b)
    IF(a > c) CALL SWAP (a,c)
    IF(b > c) CALL SWAP (b, c)
    WRITE (UNIT=*, FMT=*) " the numbers in increasing order are.", a, b, c    
STOP

    CONTAINS
        SUBROUTINE swap(x,y)
            REAL, INTENT(IN OUT) :: x, y
            REAL :: aux
            aux = x
            x = y
            y = aux

            RETURN
        END SUBROUTINE swap
END PROGRAM p42
