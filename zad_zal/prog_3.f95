PROGRAM p3
    IMPLICIT NONE
    INTEGER :: n1, n2, sum = 0

    PRINT *, "This program sums all natural numbers between n1 and n2"

    PRINT *, "Provide n1: "
    READ *, n1

    PRINT *, "Provide n2: "
    READ *, n2

    IF (n2 < n1) THEN
        CALL swap(n1, n2)
    END IF

    sum = SumInterval(n1, n2)

    PRINT *, sum

    
    CONTAINS
    SUBROUTINE swap(a,b)
        IMPLICIT NONE
        INTEGER, INTENT(OUT) :: a,b
        INTEGER :: temp

        temp = a
        a = b
        b = temp
    END SUBROUTINE swap

    RECURSIVE FUNCTION SumInterval(start, end) RESULT(sum)
        INTEGER, INTENT (IN) :: start, end
        INTEGER :: sum

        IF(start.NE.end) THEN
            sum = start + SumInterval(start + 1, end)
        ELSE 
            sum = start
        END IF

        RETURN
    END FUNCTION SumInterval
END PROGRAM p3
