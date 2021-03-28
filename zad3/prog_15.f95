PROGRAM p15
    IMPLICIT NONE
    REAL, DIMENSION(2:10, -30:30, 0:10) :: t, y, tplusy
    t = 2.0
    y = 3.0
    tplusy = t + y
    PRINT *, tplusy
STOP
END PROGRAM p15
