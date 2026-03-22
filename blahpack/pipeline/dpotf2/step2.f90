! DPOTF2: Cholesky factorization of a symmetric positive definite matrix
! (unblocked algorithm, Level 2 BLAS)
      SUBROUTINE DPOTF2(UPLO, N, A, LDA, INFO)
      CHARACTER :: UPLO
      INTEGER :: INFO, LDA, N
      DOUBLE PRECISION :: A(LDA, *)

      DOUBLE PRECISION :: ONE, ZERO
      PARAMETER(ONE = 1.0D+0, ZERO = 0.0D+0)
      LOGICAL :: UPPER
      INTEGER :: J
      DOUBLE PRECISION :: AJJ
      LOGICAL :: LSAME, DISNAN
      DOUBLE PRECISION :: DDOT
      EXTERNAL :: LSAME, DDOT, DISNAN
      EXTERNAL :: DGEMV, DSCAL, XERBLA
      INTRINSIC :: MAX, SQRT

      INFO = 0
      UPPER = LSAME(UPLO, 'U')
      IF (.NOT. UPPER .AND. .NOT. LSAME(UPLO, 'L')) THEN
        INFO = -1
      ELSE IF (N .LT. 0) THEN
        INFO = -2
      ELSE IF (LDA .LT. MAX(1, N)) THEN
        INFO = -4
      END IF
      IF (INFO .NE. 0) THEN
        CALL XERBLA('DPOTF2', -INFO)
        RETURN
      END IF

      IF (N .EQ. 0) RETURN

      IF (UPPER) THEN
        ! Compute the Cholesky factorization A = U**T * U.
        DO J = 1, N
          AJJ = A(J, J) - DDOT(J - 1, A(1, J), 1, A(1, J), 1)
          IF (AJJ .LE. ZERO .OR. DISNAN(AJJ)) THEN
            A(J, J) = AJJ
            INFO = J
            RETURN
          END IF
          AJJ = SQRT(AJJ)
          A(J, J) = AJJ
          IF (J .LT. N) THEN
            CALL DGEMV('Transpose', J - 1, N - J, -ONE,              &
     &                 A(1, J + 1), LDA, A(1, J), 1, ONE,            &
     &                 A(J, J + 1), LDA)
            CALL DSCAL(N - J, ONE / AJJ, A(J, J + 1), LDA)
          END IF
        END DO
      ELSE
        ! Compute the Cholesky factorization A = L * L**T.
        DO J = 1, N
          AJJ = A(J, J) - DDOT(J - 1, A(J, 1), LDA, A(J, 1), LDA)
          IF (AJJ .LE. ZERO .OR. DISNAN(AJJ)) THEN
            A(J, J) = AJJ
            INFO = J
            RETURN
          END IF
          AJJ = SQRT(AJJ)
          A(J, J) = AJJ
          IF (J .LT. N) THEN
            CALL DGEMV('No transpose', N - J, J - 1, -ONE,           &
     &                 A(J + 1, 1), LDA, A(J, 1), LDA, ONE,         &
     &                 A(J + 1, J), 1)
            CALL DSCAL(N - J, ONE / AJJ, A(J + 1, J), 1)
          END IF
        END DO
      END IF
      RETURN
      END
