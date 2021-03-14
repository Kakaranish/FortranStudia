PROGRAM p3
OPEN (UNIT=66, FILE="data_1", STATUS="OLD",ACTION="WRITE", POSITION="APPEND")
! When file has status OLD, it must be created before program execution
WRITE (UNIT=66, FMT=*) "Hello"
STOP
END PROGRAM p3