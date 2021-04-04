PROGRAM p25
IMPLICIT NONE
INTEGER :: i, j
INTEGER, DIMENSION(2,3) :: matrix_A
INTEGER, DIMENSION(3,2) :: matrix_B
INTEGER, DIMENSION(2,2) :: matrix_AB
INTEGER, DIMENSION(2) :: vector_C=(/1,2/)
INTEGER, DIMENSION(3) :: vector_BC
matrix_A(1,1)=1
matrix_A(1,2)=2
matrix_A(1,3)=3
matrix_A(2,1)=4
matrix_A(2,2)=5
matrix_A(2,3)=6

matrix_B=TRANSPOSE(matrix_A)
DO i=1,3
    PRINT *, (matrix_B(i,j), j=1,2)
END DO

matrix_AB=MATMUL(matrix_A, matrix_B)
PRINT *
DO i=1,2
    PRINT *, (matrix_AB(i,j),j=1,2)
END DO

vector_BC=MATMUL(matrix_B, vector_C)
PRINT *
PRINT *, vector_BC

STOP
END PROGRAM p25