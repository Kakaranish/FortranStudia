PROGRAM p50
    IMPLICIT NONE
    INTEGER, DIMENSION(1:10) :: inputdata
    INTEGER :: j,k

    OPEN(11, file="wejsciowe.txt", status="old", action="read", position="rewind")
    DO j=1,10,1
        READ(11, *) k
        inputdata(j)=k
    END DO

    CALL sort(inputdata, 10)
    OPEN(12, file="wyjsciowe.txt", status="new", action="write", position="append")
    DO j=1,10,1
        WRITE(12, *) inputdata(j)
    END DO

    CONTAINS
    INTEGER FUNCTION minfind(x, start, end)
        IMPLICIT NONE
        INTEGER, DIMENSION(1:), INTENT(IN) :: x
        INTEGER, INTENT(IN) :: start, end
        INTEGER :: minimum
        INTEGER :: location
        INTEGER :: i

        minimum = x(start)
        location = start
        DO i = start + 1, end
            IF (x(i) < minimum) THEN
                minimum = x(i)
                location = i
            END IF 
            minfind = location
        END DO
    END FUNCTION minfind

    SUBROUTINE swap(a,b)
        IMPLICIT NONE
        INTEGER, INTENT(OUT) :: a,b
        INTEGER :: temp

        temp = a
        a = b
        b = temp
    END SUBROUTINE swap

    SUBROUTINE sort(x, size)
        IMPLICIT NONE
        INTEGER, DIMENSION(1:), INTENT(inout) :: x
        INTEGER, INTENT(IN) :: size
        INTEGER :: i
        INTEGER :: location

        DO i=1, size-1
            location = minfind(x, i, size)
            CALL swap(x(i), x(location))
        END DO
    END SUBROUTINE sort
END PROGRAM p50
