PROGRAM p4
IMPLICIT NONE ! all variables must be declared before use
REAL :: a,b
INTEGER :: c
OPEN (UNIT=66, FILE="data_in", STATUS="OLD", ACTION="READ", POSITION="REWIND")
READ (UNIT=66, FMT=*) a,b,c
WRITE (UNIT=*, FMT="(A8, F6.3, T40, AS, ES12.4, A6, I8)") &
"a rowne", a, "b rowne", b, "c=", c
! A8 - 8x alphanumeric
! F6.3 - REAL num format
! T40 - 40 char TAB
! A5 - 5x char
! BS12.4 - liczba REAL zajmujaca 12 znakow, np. -.12345E-154, co oznacza:
! I8 - 8 digit INTEGER
STOP
END PROGRAM p4