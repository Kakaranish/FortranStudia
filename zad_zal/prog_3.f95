PROGRAM p3
    IMPLICIT NONE
    INTEGER, PARAMETER :: idp = selected_int_kind(15)

    INTEGER(kind = idp) :: n1, n2, sum = 0

    PRINT *, "This program sums all natural numbers between n1 and n2"

    PRINT *, "Provide n1: "
    READ *, n1

    PRINT *, "Provide n2: "
    READ *, n2

    IF (n2 < n1) THEN
        CALL swap(n1, n2)
    END IF

    sum = SumInterval(n1, n2)
    WRITE (UNIT=*, FMT="(I20)") sum

    CONTAINS
    SUBROUTINE swap(a,b)
        IMPLICIT NONE
        INTEGER(kind = idp), INTENT(OUT) :: a,b
        INTEGER(kind = idp) :: temp

        temp = a
        a = b
        b = temp
    END SUBROUTINE swap

    RECURSIVE FUNCTION SumInterval(start, end) RESULT(sum)
        INTEGER(kind = idp), INTENT (IN) :: start, end
        INTEGER(kind = idp) :: sum

        IF(start.NE.end) THEN
            sum = start + SumInterval(start + 1, end)
        ELSE 
            sum = start
        END IF

        RETURN
    END FUNCTION SumInterval
END PROGRAM p3
