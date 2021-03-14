PROGRAM p5
    IMPLICIT NONE
    INTEGER :: i,k
    DO i= 1,10 !!! step = 1
        DO k =1,12,5 !!! step = 5
            WRITE (UNIT=*, FMT=*) i,k
        END DO
    END DO
STOP
END PROGRAM p5