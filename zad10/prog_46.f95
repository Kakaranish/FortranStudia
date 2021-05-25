MODULE zawiera_funkcje_rekursywna
    IMPLICIT NONE
    PUBLIC :: Euclid
    CONTAINS
        RECURSIVE FUNCTION Euclid(i1, i2) RESULT(gcd)
            INTEGER, INTENT (IN) :: i1, i2
            INTEGER :: gcd
            INTEGER :: remainder
            remainder = MOD(i1, i2)

            IF(remainder == 0) THEN
                gcd = i2
                RETURN
            ELSE
                gcd = Euclid(i2, remainder)
            END IF

            WRITE(*, FMT=897) i1, i2, remainder, gcd
            897 FORMAT ('reszta z dzielenia liczby = ', I6, 2X, 'przez liczbe = ', I4,2X, 'daje reszte = ', I4,2X, 'RESULT = ', I4)
            RETURN
        END FUNCTION Euclid
    END MODULE zawiera_funkcje_rekursywna

PROGRAM p46
    USE zawiera_funkcje_rekursywna
    IMPLICIT NONE
    INTEGER :: p,q
    PRINT *, 'wprowadz liczbe naturalna do dzielenia "p"'
    READ (UNIT=*, FMT=*) p
    PRINT *, 'wprowadz dzielnik "q"'
    READ (UNIT=*, FMT=*) q
    PRINT *, Euclid(p,q), 'to najwiekszy wspolny podzielnik zwracany przez $ Euclid(p,q)'
STOP
END PROGRAM p46
