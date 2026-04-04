program test_dgbsvx
  use test_utils
  implicit none

  ! Max dimensions
  integer, parameter :: NMAX = 5, NRHSMAX = 3
  integer, parameter :: KLMAX = 2, KUMAX = 2
  integer, parameter :: LDAB = KLMAX + KUMAX + 1
  integer, parameter :: LDAFB = 2*KLMAX + KUMAX + 1

  double precision :: AB(LDAB, NMAX), AFB(LDAFB, NMAX)
  double precision :: B(NMAX, NRHSMAX), X(NMAX, NRHSMAX)
  double precision :: R(NMAX), C(NMAX)
  double precision :: FERR(NRHSMAX), BERR(NRHSMAX)
  double precision :: WORK(3*NMAX)
  integer :: IWORK(NMAX), IPIV(NMAX)
  double precision :: RCOND
  integer :: INFO, N, KL, KU, NRHS
  character :: FACT, TRANS, EQUED

  ! ============================================================
  ! Test 1: FACT='N', TRANS='N', 4x4 tridiagonal (KL=1, KU=1)
  ! A = [4 -1 0 0; -1 4 -1 0; 0 -1 4 -1; 0 0 -1 4], b = A*[1,1,1,1]
  N = 4; KL = 1; KU = 1; NRHS = 1
  AB = 0.0d0
  ! Row 1 = superdiag (KU), Row 2 = diag, Row 3 = subdiag (KL)
  ! Using LDAB = KL+KU+1 = 3 rows
  AB(1,1) = 0.0d0;  AB(2,1) = 4.0d0;  AB(3,1) = -1.0d0
  AB(1,2) = -1.0d0; AB(2,2) = 4.0d0;  AB(3,2) = -1.0d0
  AB(1,3) = -1.0d0; AB(2,3) = 4.0d0;  AB(3,3) = -1.0d0
  AB(1,4) = -1.0d0; AB(2,4) = 4.0d0;  AB(3,4) = 0.0d0
  B = 0.0d0
  ! b = A * [1,1,1,1] = [3, 2, 2, 3]
  B(1,1) = 3.0d0; B(2,1) = 2.0d0; B(3,1) = 2.0d0; B(4,1) = 3.0d0
  FACT = 'N'; TRANS = 'N'; EQUED = 'N'
  AFB = 0.0d0; IPIV = 0; R = 0.0d0; C = 0.0d0
  X = 0.0d0; FERR = 0.0d0; BERR = 0.0d0; WORK = 0.0d0; IWORK = 0
  call DGBSVX(FACT, TRANS, N, KL, KU, NRHS, AB, LDAB, AFB, LDAFB, &
              IPIV, EQUED, R, C, B, NMAX, X, NMAX, RCOND, FERR, BERR, &
              WORK, IWORK, INFO)
  call begin_test('nofact_notrans_tridiag')
  call print_array('x', X(1,1), N)
  call print_int('info', INFO)
  call print_scalar('rcond', RCOND)
  call print_scalar('rpvgrw', WORK(1))
  call print_array('ferr', FERR, NRHS)
  call print_array('berr', BERR, NRHS)
  call print_char('equed', EQUED)
  call end_test()

  ! ============================================================
  ! Test 2: FACT='N', TRANS='T', same tridiagonal, single RHS
  AB = 0.0d0
  AB(1,1) = 0.0d0;  AB(2,1) = 4.0d0;  AB(3,1) = -1.0d0
  AB(1,2) = -1.0d0; AB(2,2) = 4.0d0;  AB(3,2) = -1.0d0
  AB(1,3) = -1.0d0; AB(2,3) = 4.0d0;  AB(3,3) = -1.0d0
  AB(1,4) = -1.0d0; AB(2,4) = 4.0d0;  AB(3,4) = 0.0d0
  B = 0.0d0
  ! b = A^T * [1,1,1,1] = same as A * [1,1,1,1] for symmetric
  B(1,1) = 3.0d0; B(2,1) = 2.0d0; B(3,1) = 2.0d0; B(4,1) = 3.0d0
  FACT = 'N'; TRANS = 'T'; EQUED = 'N'
  AFB = 0.0d0; IPIV = 0; R = 0.0d0; C = 0.0d0
  X = 0.0d0; FERR = 0.0d0; BERR = 0.0d0; WORK = 0.0d0; IWORK = 0
  call DGBSVX(FACT, TRANS, N, KL, KU, NRHS, AB, LDAB, AFB, LDAFB, &
              IPIV, EQUED, R, C, B, NMAX, X, NMAX, RCOND, FERR, BERR, &
              WORK, IWORK, INFO)
  call begin_test('nofact_trans_tridiag')
  call print_array('x', X(1,1), N)
  call print_int('info', INFO)
  call print_scalar('rcond', RCOND)
  call end_test()

  ! ============================================================
  ! Test 3: FACT='E' (equilibrate), TRANS='N', 4x4 tridiag, single RHS
  AB = 0.0d0
  AB(1,1) = 0.0d0;  AB(2,1) = 4.0d0;  AB(3,1) = -1.0d0
  AB(1,2) = -1.0d0; AB(2,2) = 4.0d0;  AB(3,2) = -1.0d0
  AB(1,3) = -1.0d0; AB(2,3) = 4.0d0;  AB(3,3) = -1.0d0
  AB(1,4) = -1.0d0; AB(2,4) = 4.0d0;  AB(3,4) = 0.0d0
  B = 0.0d0
  B(1,1) = 3.0d0; B(2,1) = 2.0d0; B(3,1) = 2.0d0; B(4,1) = 3.0d0
  FACT = 'E'; TRANS = 'N'; EQUED = 'N'
  AFB = 0.0d0; IPIV = 0; R = 0.0d0; C = 0.0d0
  X = 0.0d0; FERR = 0.0d0; BERR = 0.0d0; WORK = 0.0d0; IWORK = 0
  call DGBSVX(FACT, TRANS, N, KL, KU, NRHS, AB, LDAB, AFB, LDAFB, &
              IPIV, EQUED, R, C, B, NMAX, X, NMAX, RCOND, FERR, BERR, &
              WORK, IWORK, INFO)
  call begin_test('equil_notrans_tridiag')
  call print_array('x', X(1,1), N)
  call print_int('info', INFO)
  call print_scalar('rcond', RCOND)
  call print_char('equed', EQUED)
  call print_array('r', R, N)
  call print_array('c', C, N)
  call print_array('ferr', FERR, NRHS)
  call print_array('berr', BERR, NRHS)
  call end_test()

  ! ============================================================
  ! Test 4: FACT='F' (factored), reuse factorization from test 1
  ! First, re-factor:
  AB = 0.0d0
  AB(1,1) = 0.0d0;  AB(2,1) = 4.0d0;  AB(3,1) = -1.0d0
  AB(1,2) = -1.0d0; AB(2,2) = 4.0d0;  AB(3,2) = -1.0d0
  AB(1,3) = -1.0d0; AB(2,3) = 4.0d0;  AB(3,3) = -1.0d0
  AB(1,4) = -1.0d0; AB(2,4) = 4.0d0;  AB(3,4) = 0.0d0
  B = 0.0d0
  B(1,1) = 3.0d0; B(2,1) = 2.0d0; B(3,1) = 2.0d0; B(4,1) = 3.0d0
  FACT = 'N'; TRANS = 'N'; EQUED = 'N'
  AFB = 0.0d0; IPIV = 0; R = 0.0d0; C = 0.0d0
  X = 0.0d0; FERR = 0.0d0; BERR = 0.0d0; WORK = 0.0d0; IWORK = 0
  call DGBSVX(FACT, TRANS, N, KL, KU, NRHS, AB, LDAB, AFB, LDAFB, &
              IPIV, EQUED, R, C, B, NMAX, X, NMAX, RCOND, FERR, BERR, &
              WORK, IWORK, INFO)
  ! Now use FACT='F' with the factored AFB and IPIV
  B = 0.0d0
  ! New RHS: b = A * [2,3,4,5] = [5, 8, 8, 16]
  B(1,1) = 5.0d0; B(2,1) = 8.0d0; B(3,1) = 8.0d0; B(4,1) = 16.0d0
  FACT = 'F'; TRANS = 'N'; EQUED = 'N'
  X = 0.0d0; FERR = 0.0d0; BERR = 0.0d0; WORK = 0.0d0; IWORK = 0
  call DGBSVX(FACT, TRANS, N, KL, KU, NRHS, AB, LDAB, AFB, LDAFB, &
              IPIV, EQUED, R, C, B, NMAX, X, NMAX, RCOND, FERR, BERR, &
              WORK, IWORK, INFO)
  call begin_test('factored_notrans_tridiag')
  call print_array('x', X(1,1), N)
  call print_int('info', INFO)
  call print_scalar('rcond', RCOND)
  call end_test()

  ! ============================================================
  ! Test 5: Multiple RHS (NRHS=2), FACT='N', TRANS='N'
  N = 4; KL = 1; KU = 1; NRHS = 2
  AB = 0.0d0
  AB(1,1) = 0.0d0;  AB(2,1) = 4.0d0;  AB(3,1) = -1.0d0
  AB(1,2) = -1.0d0; AB(2,2) = 4.0d0;  AB(3,2) = -1.0d0
  AB(1,3) = -1.0d0; AB(2,3) = 4.0d0;  AB(3,3) = -1.0d0
  AB(1,4) = -1.0d0; AB(2,4) = 4.0d0;  AB(3,4) = 0.0d0
  B = 0.0d0
  ! b1 = A * [1,1,1,1] = [3, 2, 2, 3]
  B(1,1) = 3.0d0; B(2,1) = 2.0d0; B(3,1) = 2.0d0; B(4,1) = 3.0d0
  ! b2 = A * [1,2,3,4] = [2, 5, 8, 13]
  B(1,2) = 2.0d0; B(2,2) = 5.0d0; B(3,2) = 8.0d0; B(4,2) = 13.0d0
  FACT = 'N'; TRANS = 'N'; EQUED = 'N'
  AFB = 0.0d0; IPIV = 0; R = 0.0d0; C = 0.0d0
  X = 0.0d0; FERR = 0.0d0; BERR = 0.0d0; WORK = 0.0d0; IWORK = 0
  call DGBSVX(FACT, TRANS, N, KL, KU, NRHS, AB, LDAB, AFB, LDAFB, &
              IPIV, EQUED, R, C, B, NMAX, X, NMAX, RCOND, FERR, BERR, &
              WORK, IWORK, INFO)
  call begin_test('nofact_notrans_2rhs')
  call print_matrix('x', X, NMAX, N, NRHS)
  call print_int('info', INFO)
  call print_scalar('rcond', RCOND)
  call print_array('ferr', FERR, NRHS)
  call print_array('berr', BERR, NRHS)
  call end_test()

  ! ============================================================
  ! Test 6: 5x5 pentadiagonal (KL=2, KU=2), FACT='N', TRANS='N'
  N = 5; KL = 2; KU = 2; NRHS = 1
  AB = 0.0d0
  ! LDAB = KL+KU+1 = 5
  ! Row 1,2 = superdiags, Row 3 = diag, Row 4,5 = subdiags
  AB(3,1) = 6.0d0;  AB(4,1) = -2.0d0; AB(5,1) = 1.0d0
  AB(2,2) = -2.0d0; AB(3,2) = 6.0d0;  AB(4,2) = -2.0d0; AB(5,2) = 1.0d0
  AB(1,3) = 1.0d0;  AB(2,3) = -2.0d0; AB(3,3) = 6.0d0;  AB(4,3) = -2.0d0; AB(5,3) = 1.0d0
  AB(1,4) = 1.0d0;  AB(2,4) = -2.0d0; AB(3,4) = 6.0d0;  AB(4,4) = -2.0d0
  AB(1,5) = 1.0d0;  AB(2,5) = -2.0d0; AB(3,5) = 6.0d0
  B = 0.0d0
  ! b = A * [1,1,1,1,1] = [5, 3, 4, 3, 5]
  B(1,1) = 5.0d0; B(2,1) = 3.0d0; B(3,1) = 4.0d0; B(4,1) = 3.0d0; B(5,1) = 5.0d0
  FACT = 'N'; TRANS = 'N'; EQUED = 'N'
  AFB = 0.0d0; IPIV = 0; R = 0.0d0; C = 0.0d0
  X = 0.0d0; FERR = 0.0d0; BERR = 0.0d0; WORK = 0.0d0; IWORK = 0
  call DGBSVX(FACT, TRANS, N, KL, KU, NRHS, AB, LDAB, AFB, LDAFB, &
              IPIV, EQUED, R, C, B, NMAX, X, NMAX, RCOND, FERR, BERR, &
              WORK, IWORK, INFO)
  call begin_test('nofact_notrans_pentadiag')
  call print_array('x', X(1,1), N)
  call print_int('info', INFO)
  call print_scalar('rcond', RCOND)
  call print_scalar('rpvgrw', WORK(1))
  call end_test()

  ! ============================================================
  ! Test 7: FACT='E', TRANS='T', 4x4 asymmetric band matrix
  ! A = [2  3  0  0]    b = A^T * [1,2,3,4]
  !     [1  5  2  0]
  !     [0  1  4  3]
  !     [0  0  2  6]
  N = 4; KL = 1; KU = 1; NRHS = 1
  AB = 0.0d0
  AB(1,1) = 0.0d0;  AB(2,1) = 2.0d0;  AB(3,1) = 1.0d0
  AB(1,2) = 3.0d0;  AB(2,2) = 5.0d0;  AB(3,2) = 1.0d0
  AB(1,3) = 2.0d0;  AB(2,3) = 4.0d0;  AB(3,3) = 2.0d0
  AB(1,4) = 3.0d0;  AB(2,4) = 6.0d0;  AB(3,4) = 0.0d0
  B = 0.0d0
  ! b = A^T * [1,2,3,4] = [2+1, 3+5*2+1*3, 2*2+4*3+2*4, 3*3+6*4]
  !                      = [4, 16, 24, 33]
  B(1,1) = 4.0d0; B(2,1) = 16.0d0; B(3,1) = 24.0d0; B(4,1) = 33.0d0
  FACT = 'E'; TRANS = 'T'; EQUED = 'N'
  AFB = 0.0d0; IPIV = 0; R = 0.0d0; C = 0.0d0
  X = 0.0d0; FERR = 0.0d0; BERR = 0.0d0; WORK = 0.0d0; IWORK = 0
  call DGBSVX(FACT, TRANS, N, KL, KU, NRHS, AB, LDAB, AFB, LDAFB, &
              IPIV, EQUED, R, C, B, NMAX, X, NMAX, RCOND, FERR, BERR, &
              WORK, IWORK, INFO)
  call begin_test('equil_trans_asymmetric')
  call print_array('x', X(1,1), N)
  call print_int('info', INFO)
  call print_scalar('rcond', RCOND)
  call print_char('equed', EQUED)
  call print_array('r', R, N)
  call print_array('c', C, N)
  call print_array('ferr', FERR, NRHS)
  call print_array('berr', BERR, NRHS)
  call end_test()

  ! ============================================================
  ! Test 8: Singular matrix test (diagonal has a zero)
  N = 3; KL = 1; KU = 1; NRHS = 1
  AB = 0.0d0
  AB(1,1) = 0.0d0; AB(2,1) = 2.0d0;  AB(3,1) = 0.0d0
  AB(1,2) = 0.0d0; AB(2,2) = 0.0d0;  AB(3,2) = 0.0d0
  AB(1,3) = 0.0d0; AB(2,3) = 3.0d0;  AB(3,3) = 0.0d0
  B = 0.0d0
  B(1,1) = 1.0d0; B(2,1) = 2.0d0; B(3,1) = 3.0d0
  FACT = 'N'; TRANS = 'N'; EQUED = 'N'
  AFB = 0.0d0; IPIV = 0; R = 0.0d0; C = 0.0d0
  X = 0.0d0; FERR = 0.0d0; BERR = 0.0d0; WORK = 0.0d0; IWORK = 0
  call DGBSVX(FACT, TRANS, N, KL, KU, NRHS, AB, LDAB, AFB, LDAFB, &
              IPIV, EQUED, R, C, B, NMAX, X, NMAX, RCOND, FERR, BERR, &
              WORK, IWORK, INFO)
  call begin_test('singular_matrix')
  call print_int('info', INFO)
  call print_scalar('rcond', RCOND)
  call print_scalar('rpvgrw', WORK(1))
  call end_test()

  ! ============================================================
  ! Test 9: Pivoting test with asymmetric matrix
  ! A = [1 3 0; 4 5 2; 0 1 6], KL=1, KU=1
  ! b = A * [1,2,3] = [7, 20, 20]
  N = 3; KL = 1; KU = 1; NRHS = 1
  AB = 0.0d0
  AB(1,1) = 0.0d0;  AB(2,1) = 1.0d0;  AB(3,1) = 4.0d0
  AB(1,2) = 3.0d0;  AB(2,2) = 5.0d0;  AB(3,2) = 1.0d0
  AB(1,3) = 2.0d0;  AB(2,3) = 6.0d0;  AB(3,3) = 0.0d0
  B = 0.0d0
  B(1,1) = 7.0d0; B(2,1) = 20.0d0; B(3,1) = 20.0d0
  FACT = 'N'; TRANS = 'N'; EQUED = 'N'
  AFB = 0.0d0; IPIV = 0; R = 0.0d0; C = 0.0d0
  X = 0.0d0; FERR = 0.0d0; BERR = 0.0d0; WORK = 0.0d0; IWORK = 0
  call DGBSVX(FACT, TRANS, N, KL, KU, NRHS, AB, LDAB, AFB, LDAFB, &
              IPIV, EQUED, R, C, B, NMAX, X, NMAX, RCOND, FERR, BERR, &
              WORK, IWORK, INFO)
  call begin_test('pivot_asymmetric')
  call print_array('x', X(1,1), N)
  call print_int('info', INFO)
  call print_scalar('rcond', RCOND)
  call print_int_array('ipiv', IPIV, N)
  call end_test()

end program
