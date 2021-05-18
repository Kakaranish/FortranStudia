MODULE zawiera_funkcje_search
    IMPLICIT NONE
    PUBLIC :: search
    CONTAINS
        FUNCTION search(list, key) RESULT(search_r)
            CHARACTER (LEN = *), DIMENSION(:), INTENT(IN) :: list
            CHARACTER (LEN = *), INTENT(IN) :: key
            INTEGER, DIMENSION (2) :: search_r
            INTEGER :: middle, last

            last = SIZE(list)
            search_r(1) = 0
            search_r(2) = last + 1

            DO WHILE (search_r(2) - search_r(1) > 1)
                middle = (search_r(1) + search_r(2)) / 2
                IF(list(middle) == key) THEN
                    search_r = middle
                    EXIT
                ELSE IF (list(middle) > key) THEN
                    search_r(2) = middle
                ELSE
                    search_r(1) = middle
                END IF
            END DO

            RETURN 
        END FUNCTION search
END MODULE zawiera_funkcje_search

PROGRAM p43
    USE zawiera_funkcje_search
    IMPLICIT NONE
    INTEGER, PARAMETER :: array_size = 20, name_length = 20
    CHARACTER (LEN = name_length), DIMENSION(0: array_size + 1) :: data_array
    CHARACTER (LEN = NAME_LENGTH) :: x
    INTEGER :: loop, eof
    INTEGER, DIMENSION(2) :: ans

    OPEN (UNIT=1, FILE="words.txt", STATUS="OLD", ACTION="READ", POSITION="REWIND")

    DO loop=1, array_size
        READ (UNIT=1, FMT=*, IOSTAT=eof) data_array(loop)
        IF (eof < 0) THEN
            EXIT
        END IF
    END DO

    data_array(0) = " "
    data_array(loop) = "ZZZZZZZZZZZZZZZZZZZ"
    WRITE(*,*) "wprowadz tekst do znalezienia"
    READ(UNIT=*,FMT=*) x
    ans = search(data_array(: loop -1), x)
    WRITE(UNIT=*, FMT=*) "found after element = ", data_array(ans(1) - 1)
    WRITE(UNIT=*, FMT=*) "found in element = ", data_array(ans(2) - 1)
STOP
END PROGRAM p43
