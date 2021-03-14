PROGRAM p7
IMPLICIT NONE
CHARACTER (LEN=16):: a,b,c,d
a="q kindly gigant"
b="a small man"
c=b(:8)//"step" ! take first 8 chars from var and join with "step"
d="for a"//b(8:) ! join "for a" with 8+ chars from var b
b=" "//d(:4) //b(9:11)//a(3:6)
a=a(:2)//a(10:15)//"leap"

PRINT *, "your first name"
READ *,a
PRINT *, "your second name"
READ *,b
PRINT *,a,b, "student"
PRINT *, TRIM(a) , " ", TRIM(b)," student"
END PROGRAM p7