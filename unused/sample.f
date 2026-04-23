      SUBROUTINE DGEMV(M,N,ALPHA,A,LDA,X,BETA,Y)
*  -- Reference BLAS level2 routine --
*     y := alpha*A*x + beta*y
      DOUBLE PRECISION ALPHA,BETA
      INTEGER LDA,M,N
      DOUBLE PRECISION A(LDA,*),X(*),Y(*)
      DOUBLE PRECISION TEMP
      INTEGER I, J
      DO 20 I = 1,M
            Y(I) = BETA*Y(I)
  20  CONTINUE
      DO 60 J = 1,N
            TEMP = ALPHA*X(J)
            DO 50 I = 1,M
                  Y(I) = Y(I) + TEMP*A(I,J)
  50        CONTINUE
  60  CONTINUE
      RETURN
      END

