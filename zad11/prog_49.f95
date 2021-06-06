PROGRAM p49
    IMPLICIT NONE
    INTEGER, ALLOCATABLE, DIMENSION(:) :: matrix
    INTEGER :: rozmiar, ptr, j

    PRINT *, "Podaj gorna granice przeszukiwanego zakresu"
    READ *, rozmiar
    PRINT *, "Poszukiwanie liczb pierwszych w zakresie 0: ", rozmiar
    ALLOCATE(matrix(rozmiar))
    matrix = 1
    ptr = 2

    DO 
        IF(ptr > rozmiar) EXIT
        IF(matrix(ptr) == 1) THEN
            j = ptr

            DO
                j = j + ptr
                IF(j > rozmiar) EXIT
                matrix(j) = 0
            END DO
        END IF

        ptr = ptr + 1
    END DO

    PRINT *, "Liczby pierwsze: "
    DO j=2, rozmiar
        IF(matrix(j) == 1) PRINT *,j
    END DO
STOP
END PROGRAM p49
