PROGRAM p6
IMPLICIT NONE
CHARACTER(LEN=10) :: date ! string with length = 10
CHARACTER (LEN=2) :: month

WRITE (UNIT=*, FMT=*) "napisz date w formacie yyyy-mm-dd" ! stdin
READ (UNIT=*, FMT=*) date
month=date(6:7)
WRITE (UNIT=*, FMT="(A10)") month
SELECT CASE (month)
CASE ("03":"05") ! if month = 3 or 4 or 5, then write "to wiosna"
WRITE (UNIT=*, FMT="(A12)") "to wiosna"
CASE ("06", "O7", "08")
WRITE (UNIT=*, FMT="(A12)") "to lato"
CASE DEFAULT
WRITE (UNIT=*, FMT="(A25)") "to nie wiosna ani lato"
END SELECT

STOP
END PROGRAM p6