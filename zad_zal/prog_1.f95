PROGRAM p1
    IMPLICIT NONE

    INTEGER :: i, eof, current_val
    INTEGER, PARAMETER :: nums_count = 15

    INTEGER :: even_sum = 0, odd_sum = 0

    OPEN (UNIT = 1, FILE = "prog1_input.txt", STATUS = "OLD", ACTION = "READ", POSITION="REWIND")
        
    DO i=1, nums_count
        READ (UNIT=1, FMT=*, IOSTAT=eof) current_val

        IF (MOD(current_val,  2) == 0) THEN
            even_sum = even_sum + current_val
        ELSE
            odd_sum = odd_sum + current_val
        END IF

        IF (eof < 0) THEN
            EXIT
        END IF
    END DO

    PRINT *, "even_sum = ", even_sum
    PRINT *, "odd_sum = ", odd_sum
STOP
END PROGRAM p1
