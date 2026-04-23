program test_zungql
  use test_utils
  implicit none

  integer, parameter :: NMAX = 4
  integer :: M, N, K, LDA, LWORK, INFO
  complex*16 :: A(NMAX, NMAX), TAU(NMAX), WORK(200)
  double precision :: A_r(2*NMAX*NMAX)
  equivalence (A, A_r)

  ! =====================================================
  ! Test 1: 3x3 full Q (M=N=K=3)
  ! =====================================================
  M = 3
  N = 3
  K = 3
  LDA = NMAX
  LWORK = 200

  ! Same reflectors as zung2l test
  A(1,1) = (0.5d0, 0.5d0)
  A(2,1) = (1.0d0, 0.0d0)
  A(3,1) = (0.0d0, 0.0d0)
  A(1,2) = (0.0d0, 1.0d0)
  A(2,2) = (0.5d0, -0.5d0)
  A(3,2) = (0.0d0, 0.0d0)
  A(1,3) = (1.0d0, 0.0d0)
  A(2,3) = (0.0d0, 0.5d0)
  A(3,3) = (0.3d0, 0.0d0)

  TAU(1) = (1.2d0, 0.1d0)
  TAU(2) = (0.8d0, -0.2d0)
  TAU(3) = (1.5d0, 0.3d0)

  call ZUNGQL(M, N, K, A, LDA, TAU, WORK, LWORK, INFO)
  call begin_test('zungql_3x3')
  call print_int('info', INFO)
  call print_array('Q', A_r, 2*LDA*N)
  call end_test()

  ! =====================================================
  ! Test 2: 4x3 rectangular (M=4, N=3, K=2)
  ! =====================================================
  M = 4
  N = 3
  K = 2
  LDA = NMAX
  LWORK = 200

  A(1,1) = (0.0d0, 0.0d0)
  A(2,1) = (0.0d0, 0.0d0)
  A(3,1) = (0.0d0, 0.0d0)
  A(4,1) = (0.0d0, 0.0d0)
  A(1,2) = (0.3d0, 0.4d0)
  A(2,2) = (0.5d0, -0.1d0)
  A(3,2) = (1.0d0, 0.0d0)
  A(4,2) = (0.0d0, 0.0d0)
  A(1,3) = (0.2d0, 0.1d0)
  A(2,3) = (0.4d0, -0.3d0)
  A(3,3) = (0.0d0, 0.6d0)
  A(4,3) = (0.7d0, 0.0d0)

  TAU(1) = (1.1d0, 0.2d0)
  TAU(2) = (0.9d0, -0.1d0)

  call ZUNGQL(M, N, K, A, LDA, TAU, WORK, LWORK, INFO)
  call begin_test('zungql_4x3')
  call print_int('info', INFO)
  call print_array('Q', A_r, 2*LDA*N)
  call end_test()

  ! =====================================================
  ! Test 3: N=0
  ! =====================================================
  M = 3
  N = 0
  K = 0
  LDA = NMAX
  LWORK = 200

  call ZUNGQL(M, N, K, A, LDA, TAU, WORK, LWORK, INFO)
  call begin_test('zungql_N0')
  call print_int('info', INFO)
  call end_test()

end program test_zungql
