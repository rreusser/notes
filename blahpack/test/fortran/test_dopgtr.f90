program test_dopgtr
  use test_utils
  implicit none

  integer, parameter :: NMAX = 4
  double precision :: AP(30), TAU(10), WORK(256)
  double precision :: Q(NMAX, NMAX), Qflat(NMAX*NMAX)
  double precision :: QTQ(NMAX, NMAX)
  double precision :: D(NMAX), E(NMAX)
  double precision :: Aorig(NMAX, NMAX), TMP(NMAX, NMAX), QTAQ(NMAX, NMAX)
  integer :: INFO, N, I, J

  ! ============================================================
  ! Test 1: UPLO='U', 4x4 symmetric matrix (packed storage)
  ! ============================================================
  N = 4
  !  4  1 -2  2
  !  1  2  0  1
  ! -2  0  3 -2
  !  2  1 -2 -1
  ! Upper packed (column-major upper triangle):
  ! col 1: 4
  ! col 2: 1, 2
  ! col 3: -2, 0, 3
  ! col 4: 2, 1, -2, -1
  AP = 0.0d0; TAU = 0.0d0; D = 0.0d0; E = 0.0d0

  ! Save original matrix for verification
  Aorig = 0.0d0
  Aorig(1,1) =  4.0d0; Aorig(1,2) =  1.0d0; Aorig(1,3) = -2.0d0; Aorig(1,4) =  2.0d0
  Aorig(2,1) =  1.0d0; Aorig(2,2) =  2.0d0; Aorig(2,3) =  0.0d0; Aorig(2,4) =  1.0d0
  Aorig(3,1) = -2.0d0; Aorig(3,2) =  0.0d0; Aorig(3,3) =  3.0d0; Aorig(3,4) = -2.0d0
  Aorig(4,1) =  2.0d0; Aorig(4,2) =  1.0d0; Aorig(4,3) = -2.0d0; Aorig(4,4) = -1.0d0

  AP(1) = 4.0d0
  AP(2) = 1.0d0; AP(3) = 2.0d0
  AP(4) = -2.0d0; AP(5) = 0.0d0; AP(6) = 3.0d0
  AP(7) = 2.0d0; AP(8) = 1.0d0; AP(9) = -2.0d0; AP(10) = -1.0d0

  ! Reduce to tridiagonal form using packed storage
  call DSPTRD('U', N, AP, D, E, TAU, INFO)

  ! Print AP and TAU after dsptrd for use as JS inputs
  call begin_test('uplo_U_4x4')
  call print_int('N', N)
  call print_array('AP', AP, N*(N+1)/2)
  call print_array('TAU', TAU, N-1)

  ! Generate Q from packed reflectors
  Q = 0.0d0
  call DOPGTR('U', N, AP, TAU, Q, NMAX, WORK, INFO)

  call print_int('info', INFO)
  do J = 1, N
    do I = 1, N
      Qflat((J-1)*N + I) = Q(I,J)
    end do
  end do
  call print_array('Q', Qflat, N*N)

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
  ! Test 2: UPLO='L', 4x4 same symmetric matrix (packed storage)
  ! ============================================================
  N = 4
  AP = 0.0d0; TAU = 0.0d0; D = 0.0d0; E = 0.0d0

  ! Lower packed (column-major lower triangle):
  ! col 1: 4, 1, -2, 2
  ! col 2: 2, 0, 1
  ! col 3: 3, -2
  ! col 4: -1
  AP(1) = 4.0d0; AP(2) = 1.0d0; AP(3) = -2.0d0; AP(4) = 2.0d0
  AP(5) = 2.0d0; AP(6) = 0.0d0; AP(7) = 1.0d0
  AP(8) = 3.0d0; AP(9) = -2.0d0
  AP(10) = -1.0d0

  call DSPTRD('L', N, AP, D, E, TAU, INFO)

  call begin_test('uplo_L_4x4')
  call print_int('N', N)
  call print_array('AP', AP, N*(N+1)/2)
  call print_array('TAU', TAU, N-1)

  Q = 0.0d0
  call DOPGTR('L', N, AP, TAU, Q, NMAX, WORK, INFO)

  call print_int('info', INFO)
  do J = 1, N
    do I = 1, N
      Qflat((J-1)*N + I) = Q(I,J)
    end do
  end do
  call print_array('Q', Qflat, N*N)

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
  ! Test 3: UPLO='U', 3x3 symmetric matrix
  ! ============================================================
  N = 3
  AP = 0.0d0; TAU = 0.0d0; D = 0.0d0; E = 0.0d0

  Aorig = 0.0d0
  Aorig(1,1) =  2.0d0; Aorig(1,2) =  1.0d0; Aorig(1,3) =  3.0d0
  Aorig(2,1) =  1.0d0; Aorig(2,2) =  5.0d0; Aorig(2,3) = -1.0d0
  Aorig(3,1) =  3.0d0; Aorig(3,2) = -1.0d0; Aorig(3,3) =  4.0d0

  ! Upper packed: 2, 1, 5, 3, -1, 4
  AP(1) = 2.0d0
  AP(2) = 1.0d0; AP(3) = 5.0d0
  AP(4) = 3.0d0; AP(5) = -1.0d0; AP(6) = 4.0d0

  call DSPTRD('U', N, AP, D, E, TAU, INFO)

  call begin_test('uplo_U_3x3')
  call print_int('N', N)
  call print_array('AP', AP, N*(N+1)/2)
  call print_array('TAU', TAU, N-1)

  Q = 0.0d0
  call DOPGTR('U', N, AP, TAU, Q, NMAX, WORK, INFO)

  call print_int('info', INFO)
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
  call end_test()

  ! ============================================================
  ! Test 4: UPLO='L', 3x3 symmetric matrix
  ! ============================================================
  N = 3
  AP = 0.0d0; TAU = 0.0d0; D = 0.0d0; E = 0.0d0

  ! Lower packed: 2, 1, 3, 5, -1, 4
  AP(1) = 2.0d0; AP(2) = 1.0d0; AP(3) = 3.0d0
  AP(4) = 5.0d0; AP(5) = -1.0d0
  AP(6) = 4.0d0

  call DSPTRD('L', N, AP, D, E, TAU, INFO)

  call begin_test('uplo_L_3x3')
  call print_int('N', N)
  call print_array('AP', AP, N*(N+1)/2)
  call print_array('TAU', TAU, N-1)

  Q = 0.0d0
  call DOPGTR('L', N, AP, TAU, Q, NMAX, WORK, INFO)

  call print_int('info', INFO)
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
  call end_test()

  ! ============================================================
  ! Test 5: N=1 (UPLO='U')
  ! ============================================================
  N = 1
  AP = 0.0d0; TAU = 0.0d0
  AP(1) = 5.0d0

  call DSPTRD('U', N, AP, D, E, TAU, INFO)

  call begin_test('N1_uplo_U')
  call print_int('N', N)
  call print_array('AP', AP, 1)
  Q = 0.0d0
  call DOPGTR('U', N, AP, TAU, Q, NMAX, WORK, INFO)
  call print_int('info', INFO)
  call print_array('Q', Q(1,1), 1)
  call end_test()

  ! ============================================================
  ! Test 6: N=1 (UPLO='L')
  ! ============================================================
  N = 1
  AP = 0.0d0; TAU = 0.0d0
  AP(1) = 5.0d0

  call DSPTRD('L', N, AP, D, E, TAU, INFO)

  call begin_test('N1_uplo_L')
  call print_int('N', N)
  call print_array('AP', AP, 1)
  Q = 0.0d0
  call DOPGTR('L', N, AP, TAU, Q, NMAX, WORK, INFO)
  call print_int('info', INFO)
  call print_array('Q', Q(1,1), 1)
  call end_test()

  ! ============================================================
  ! Test 7: N=0 quick return (UPLO='U')
  ! ============================================================
  call DOPGTR('U', 0, AP, TAU, Q, NMAX, WORK, INFO)
  call begin_test('N0_uplo_U')
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 8: N=0 quick return (UPLO='L')
  ! ============================================================
  call DOPGTR('L', 0, AP, TAU, Q, NMAX, WORK, INFO)
  call begin_test('N0_uplo_L')
  call print_int('info', INFO)
  call end_test()

end program
