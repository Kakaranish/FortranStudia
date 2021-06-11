PROGRAM p6
    IMPLICIT NONE
    REAL :: start = -3.0, end = 4.0, step
    INTEGER :: division_num = 7000
    REAL :: i, left_val, right_val, estimated_zero

    step = (end - start) / division_num
    
    i = start
    DO WHILE (i < end)
        left_val = fx(i)
        right_val = fx(i + step)

        IF(is_zero(left_val, right_val)) THEN
            estimated_zero = (2 * i + step) / 2
            PRINT *, "root = ", estimated_zero
        END IF 
        
        i = i + step
    END DO

    PRINT *, step

    CONTAINS
    FUNCTION fx(x) RESULT (f)
        IMPLICIT NONE
        REAL :: f
        REAL, INTENT (IN) :: x
    
        f = (x**3) - (3 * x**2) - (4 * x) + 12
    END FUNCTION fx

    FUNCTION is_zero(left_val, right_val) RESULT(result)
        REAL, INTENT(IN) :: left_val, right_val
        LOGICAL :: result

        IF ((left_val.LT.0 .and. right_val.GT.0) .or. (left_val.GT.0 .and. right_val.LT.0) & 
            .or. (left_val.EQ.0 .and. right_val.EQ.0)) THEN
            result = .TRUE.
        ELSE
            result = .FALSE.
        END IF

        RETURN
    END FUNCTION is_zero

END PROGRAM p6
