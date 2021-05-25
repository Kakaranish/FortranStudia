MODULE paramtery_opcjonalne
    IMPLICIT NONE
    PUBLIC :: Options
    CONTAINS 
    SUBROUTINE Options(str1, str2)
        CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: str1, str2
        INTEGER :: l1, l2
        WRITE (UNIT=*, FMT=*) "========&
        =================================="

        IF(PRESENT(str1)) THEN
            l1 = LEN(str1)
            WRITE(UNIT=*,FMT=*) " 1: ", str1
        ELSE
            l1 = -1
        END IF

        IF(PRESENT(str2)) THEN
            l2 = LEN(str2)
            WRITE(UNIT=*, FMT=*) str2
        ELSE 
            l2 = -1
        END IF

        WRITE(UNIT=*, FMT=*) "Lenghts: ", l1, l2
        RETURN
    END SUBROUTINE Options
END MODULE paramtery_opcjonalne

PROGRAM p48
    USE paramtery_opcjonalne
    IMPLICIT NONE

    CALL Options()
    CALL Options("pierwszy_parameter", "drugi_parameter")
    CALL Options("pierwszy_parameter")
    CALL Options(str1="333")
    CALL Options(str2="4444")
    CALL Options(str1="5555", str2="666666")
    CALL Options(str1="")
    CALL Options(str2="")
    CALL Options(str2="drugi", str1="pierwszy")
STOP
END PROGRAM p48
