program test_dorgtr
  use test_utils
  implicit none

  integer, parameter :: NMAX = 4
  double precision :: A(NMAX, NMAX), Aorig(NMAX, NMAX)
  double precision :: Q(NMAX, NMAX), T(NMAX, NMAX), QTQ(NMAX, NMAX)
  double precision :: QTAQ(NMAX, NMAX), TMP(NMAX, NMAX)
  double precision :: TAU(NMAX), D(NMAX), E(NMAX), WORK(256)
  double precision :: Aflat(NMAX*NMAX), Qflat(NMAX*NMAX)
  integer :: INFO, N, I, J, LWORK

  LWORK = 256

  ! ============================================================
  ! Test 1: UPLO='U', 4x4 symmetric matrix
  ! ============================================================
  N = 4
  ! Symmetric matrix (upper triangle stored, but fill full for clarity)
  A(1,1) =  4.0d0; A(1,2) =  1.0d0; A(1,3) = -2.0d0; A(1,4) =  2.0d0
  A(2,1) =  1.0d0; A(2,2) =  2.0d0; A(2,3) =  0.0d0; A(2,4) =  1.0d0
  A(3,1) = -2.0d0; A(3,2) =  0.0d0; A(3,3) =  3.0d0; A(3,4) = -2.0d0
  A(4,1) =  2.0d0; A(4,2) =  1.0d0; A(4,3) = -2.0d0; A(4,4) = -1.0d0

  Aorig = A

  ! Reduce to tridiagonal form
  call DSYTRD('U', N, A, NMAX, D, E, TAU, WORK, LWORK, INFO)

  ! Save A (contains reflectors) for dorgtr
  Q = A

  ! Generate Q
  call DORGTR('U', N, Q, NMAX, TAU, WORK, LWORK, INFO)

  ! Output Q and verify
  call begin_test('uplo_U_4x4')
  call print_int('info', INFO)
  call print_int('N', N)
  ! Flatten Q column-major
  do J = 1, N
    do I = 1, N
      Qflat((J-1)*N + I) = Q(I,J)
    end do
  end do
  call print_array('Q', Qflat, N*N)
  call print_array('D', D, N)
  call print_array('E', E, N-1)

  ! Verify Q^T * Q = I
  call DGEMM('T', 'N', N, N, N, 1.0d0, Q, NMAX, Q, NMAX, 0.0d0, QTQ, NMAX)
  do J = 1, N
    do I = 1, N
      Qflat((J-1)*N + I) = QTQ(I,J)
    end do
  end do
  call print_array('QTQ', Qflat, N*N)

  ! Verify Q^T * A * Q = T (tridiagonal)
  ! TMP = A * Q
  call DGEMM('N', 'N', N, N, N, 1.0d0, Aorig, NMAX, Q, NMAX, 0.0d0, TMP, NMAX)
  ! QTAQ = Q^T * TMP
  call DGEMM('T', 'N', N, N, N, 1.0d0, Q, NMAX, TMP, NMAX, 0.0d0, QTAQ, NMAX)
  do J = 1, N
    do I = 1, N
      Qflat((J-1)*N + I) = QTAQ(I,J)
    end do
  end do
  call print_array('QTAQ', Qflat, N*N)
  call end_test()

  ! ============================================================
  ! Test 2: UPLO='L', 4x4 symmetric matrix
  ! ============================================================
  N = 4
  A(1,1) =  4.0d0; A(1,2) =  1.0d0; A(1,3) = -2.0d0; A(1,4) =  2.0d0
  A(2,1) =  1.0d0; A(2,2) =  2.0d0; A(2,3) =  0.0d0; A(2,4) =  1.0d0
  A(3,1) = -2.0d0; A(3,2) =  0.0d0; A(3,3) =  3.0d0; A(3,4) = -2.0d0
  A(4,1) =  2.0d0; A(4,2) =  1.0d0; A(4,3) = -2.0d0; A(4,4) = -1.0d0

  Aorig = A

  ! Reduce to tridiagonal form
  call DSYTRD('L', N, A, NMAX, D, E, TAU, WORK, LWORK, INFO)

  Q = A

  ! Generate Q
  call DORGTR('L', N, Q, NMAX, TAU, WORK, LWORK, INFO)

  call begin_test('uplo_L_4x4')
  call print_int('info', INFO)
  call print_int('N', N)
  do J = 1, N
    do I = 1, N
      Qflat((J-1)*N + I) = Q(I,J)
    end do
  end do
  call print_array('Q', Qflat, N*N)
  call print_array('D', D, N)
  call print_array('E', E, N-1)

  ! Verify Q^T * Q = I
  call DGEMM('T', 'N', N, N, N, 1.0d0, Q, NMAX, Q, NMAX, 0.0d0, QTQ, NMAX)
  do J = 1, N
    do I = 1, N
      Qflat((J-1)*N + I) = QTQ(I,J)
    end do
  end do
  call print_array('QTQ', Qflat, N*N)

  ! Verify Q^T * A * Q = T
  call DGEMM('N', 'N', N, N, N, 1.0d0, Aorig, NMAX, Q, NMAX, 0.0d0, TMP, NMAX)
  call DGEMM('T', 'N', N, N, N, 1.0d0, Q, NMAX, TMP, NMAX, 0.0d0, QTAQ, NMAX)
  do J = 1, N
    do I = 1, N
      Qflat((J-1)*N + I) = QTAQ(I,J)
    end do
  end do
  call print_array('QTAQ', Qflat, N*N)
  call end_test()

  ! ============================================================
  ! Test 3: N=1 edge case (UPLO='U')
  ! ============================================================
  N = 1
  A(1,1) = 5.0d0

  call DSYTRD('U', N, A, NMAX, D, E, TAU, WORK, LWORK, INFO)
  Q = A
  call DORGTR('U', N, Q, NMAX, TAU, WORK, LWORK, INFO)

  call begin_test('N1_uplo_U')
  call print_int('info', INFO)
  call print_int('N', N)
  call print_array('Q', Q(1,1), 1)
  call end_test()

  ! ============================================================
  ! Test 4: N=1 edge case (UPLO='L')
  ! ============================================================
  N = 1
  A(1,1) = 5.0d0

  call DSYTRD('L', N, A, NMAX, D, E, TAU, WORK, LWORK, INFO)
  Q = A
  call DORGTR('L', N, Q, NMAX, TAU, WORK, LWORK, INFO)

  call begin_test('N1_uplo_L')
  call print_int('info', INFO)
  call print_int('N', N)
  call print_array('Q', Q(1,1), 1)
  call end_test()

  ! ============================================================
  ! Test 5: N=0 quick return (UPLO='U')
  ! ============================================================
  call DORGTR('U', 0, A, NMAX, TAU, WORK, LWORK, INFO)

  call begin_test('N0_uplo_U')
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 6: N=0 quick return (UPLO='L')
  ! ============================================================
  call DORGTR('L', 0, A, NMAX, TAU, WORK, LWORK, INFO)

  call begin_test('N0_uplo_L')
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 7: UPLO='U', 3x3 symmetric matrix (different size)
  ! ============================================================
  N = 3
  A(1,1) =  2.0d0; A(1,2) =  1.0d0; A(1,3) =  3.0d0
  A(2,1) =  1.0d0; A(2,2) =  5.0d0; A(2,3) = -1.0d0
  A(3,1) =  3.0d0; A(3,2) = -1.0d0; A(3,3) =  4.0d0

  Aorig = 0.0d0
  Aorig(1:3,1:3) = A(1:3,1:3)

  call DSYTRD('U', N, A, NMAX, D, E, TAU, WORK, LWORK, INFO)
  Q = 0.0d0
  Q(1:N,1:N) = A(1:N,1:N)
  call DORGTR('U', N, Q, NMAX, TAU, WORK, LWORK, INFO)

  call begin_test('uplo_U_3x3')
  call print_int('info', INFO)
  call print_int('N', N)
  do J = 1, N
    do I = 1, N
      Qflat((J-1)*N + I) = Q(I,J)
    end do
  end do
  call print_array('Q', Qflat, N*N)

  ! Verify Q^T * Q = I
  QTQ = 0.0d0
  call DGEMM('T', 'N', N, N, N, 1.0d0, Q, NMAX, Q, NMAX, 0.0d0, QTQ, NMAX)
  do J = 1, N
    do I = 1, N
      Qflat((J-1)*N + I) = QTQ(I,J)
    end do
  end do
  call print_array('QTQ', Qflat, N*N)

  ! Verify Q^T * A * Q = T
  TMP = 0.0d0
  QTAQ = 0.0d0
  call DGEMM('N', 'N', N, N, N, 1.0d0, Aorig, NMAX, Q, NMAX, 0.0d0, TMP, NMAX)
  call DGEMM('T', 'N', N, N, N, 1.0d0, Q, NMAX, TMP, NMAX, 0.0d0, QTAQ, NMAX)
  do J = 1, N
    do I = 1, N
      Qflat((J-1)*N + I) = QTAQ(I,J)
    end do
  end do
  call print_array('QTAQ', Qflat, N*N)
  call end_test()

  ! ============================================================
  ! Test 8: UPLO='L', 3x3 symmetric matrix (different size)
  ! ============================================================
  N = 3
  A(1,1) =  2.0d0; A(1,2) =  1.0d0; A(1,3) =  3.0d0
  A(2,1) =  1.0d0; A(2,2) =  5.0d0; A(2,3) = -1.0d0
  A(3,1) =  3.0d0; A(3,2) = -1.0d0; A(3,3) =  4.0d0

  Aorig = 0.0d0
  Aorig(1:3,1:3) = A(1:3,1:3)

  call DSYTRD('L', N, A, NMAX, D, E, TAU, WORK, LWORK, INFO)
  Q = 0.0d0
  Q(1:N,1:N) = A(1:N,1:N)
  call DORGTR('L', N, Q, NMAX, TAU, WORK, LWORK, INFO)

  call begin_test('uplo_L_3x3')
  call print_int('info', INFO)
  call print_int('N', N)
  do J = 1, N
    do I = 1, N
      Qflat((J-1)*N + I) = Q(I,J)
    end do
  end do
  call print_array('Q', Qflat, N*N)

  ! Verify Q^T * Q = I
  QTQ = 0.0d0
  call DGEMM('T', 'N', N, N, N, 1.0d0, Q, NMAX, Q, NMAX, 0.0d0, QTQ, NMAX)
  do J = 1, N
    do I = 1, N
      Qflat((J-1)*N + I) = QTQ(I,J)
    end do
  end do
  call print_array('QTQ', Qflat, N*N)

  ! Verify Q^T * A * Q = T
  TMP = 0.0d0
  QTAQ = 0.0d0
  call DGEMM('N', 'N', N, N, N, 1.0d0, Aorig, NMAX, Q, NMAX, 0.0d0, TMP, NMAX)
  call DGEMM('T', 'N', N, N, N, 1.0d0, Q, NMAX, TMP, NMAX, 0.0d0, QTAQ, NMAX)
  do J = 1, N
    do I = 1, N
      Qflat((J-1)*N + I) = QTAQ(I,J)
    end do
  end do
  call print_array('QTAQ', Qflat, N*N)
  call end_test()

end program
