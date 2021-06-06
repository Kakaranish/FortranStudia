MODULE Universal_Constants
    IMPLICIT NONE
    REAL, PARAMETER :: pi = 3.14159265536
END MODULE Universal_Constants

REAL FUNCTION trig_fun_degrees(trig_fun, degrees, minutes, seconds)
    USE Universal_Constants
    IMPLICIT NONE

    REAL, EXTERNAL :: trig_fun
    INTEGER, INTENT(IN) :: degrees, minutes, seconds
    
    REAL :: angle

    angle = (degrees + minutes / 60.0 + seconds/3600.0) * pi/180.0

    trig_fun_degrees = trig_fun(angle)

END FUNCTION trig_fun_degrees

PROGRAM p52
    REAL, INTRINSIC :: SIN, COS, TAN
    REAL, EXTERNAL :: trig_fun_degrees
    INTEGER :: degs, mins, secs
    CHARACTER :: answer

    DO
        PRINT *, "Plase give an angle in degrees, minutes and seconds"
        PRINT *, "without any fractional parts"
        PRINT *, "Degrees"
        READ *, degs
        PRINT *, "Minutes (0-59): "
        READ *, mins
        PRINT *, "Seconds (0-59): "
        READ *, secs

        PRINT *, "Its sine is ", trig_fun_degrees(SIN, degs, mins, secs)
        PRINT *, "Its consine is ", trig_fun_degrees(COS, degs, mins, secs)
        PRINT *, "Its tangent is ", trig_fun_degrees(TAN, degs, mins, secs)

        PRINT *, "Another one? (Y/N)"
        READ *, answer

        IF(answer /= "Y" .AND. answer /= "y") EXIT
    END DO
    
STOP
END PROGRAM p52
