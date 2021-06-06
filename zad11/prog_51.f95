MODULE wektor
    IMPLICIT NONE
    TYPE vector
        DOUBLE PRECISION :: x,y,z
    END TYPE vector

    TYPE(vector), PARAMETER :: i_ = vector(1.0, 0.0, 0.0)
    TYPE(vector), PARAMETER :: j_ = vector(0.0, 1.0, 0.0)
    TYPE(vector), PARAMETER :: k_ = vector(0.0, 0.0, 1.0)

    INTERFACE OPERATOR (+)
        MODULE PROCEDURE suma
    END INTERFACE

    INTERFACE OPERATOR (.dodaj.)
        MODULE PROCEDURE suma
    END INTERFACE

    CONTAINS 
        TYPE(vector) FUNCTION suma(v1, v2)
            IMPLICIT NONE
            TYPE(vector), INTENT(IN) :: v1, v2
            
            suma%x = v1%x + v2%x 
            suma%y = v1%y + v2%y
            suma%z = v1%z + v2%z
            return
        END FUNCTION suma

        SUBROUTINE results(head, v)
            IMPLICIT NONE
            CHARACTER*(*), INTENT(IN) :: head
            TYPE(vector), INTENT(IN) :: v
            WRITE(6, '(1x,a10,3f8.3)') head, v%x, v%y, v%z
        END SUBROUTINE results
    END MODULE wektor

PROGRAM p51
    USE wektor
    IMPLICIT NONE
    
    TYPE(vector) :: v1 = vector(1.0, 2.0, 3.0)
    TYPE(vector) :: v2 = vector(1.0, 4.0, 9.0)
    TYPE(vector) :: v3 = vector(-1.0, -1.0, -1.0)

    CALL results('v1 = ', v1)
    CALL results('v2 = ', v2)
    CALL results('v3 = ', v3)
    v3 = suma(v1, v2)
    CALL results('nowy v3 = ', v3)
    v3 = v1.dodaj.i_
    CALL results('nowy v3 = ', v3)
    v3 = v1.dodaj.j_
    CALL results('nowy v3 = ', v3)
STOP
END PROGRAM p51
