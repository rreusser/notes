program test_zung2l
  use test_utils
  implicit none

  integer, parameter :: NMAX = 4
  integer :: M, N, K, LDA, INFO
  complex*16 :: A(NMAX, NMAX), TAU(NMAX), WORK(200)
  double precision :: A_r(2*NMAX*NMAX), TAU_r(2*NMAX)
  equivalence (A, A_r)
  equivalence (TAU, TAU_r)

  ! =====================================================
  ! Test 1: 3x3 full Q (M=N=K=3), simple reflectors
  ! =====================================================
  M = 3
  N = 3
  K = 3
  LDA = NMAX

  ! Set up reflectors in the columns as for QL factorization
  ! Column 0: v1
  A(1,1) = (0.5d0, 0.5d0)
  A(2,1) = (1.0d0, 0.0d0)
  A(3,1) = (0.0d0, 0.0d0)
  ! Column 1: v2
  A(1,2) = (0.0d0, 1.0d0)
  A(2,2) = (0.5d0, -0.5d0)
  A(3,2) = (0.0d0, 0.0d0)
  ! Column 2: v3
  A(1,3) = (1.0d0, 0.0d0)
  A(2,3) = (0.0d0, 0.5d0)
  A(3,3) = (0.3d0, 0.0d0)

  TAU(1) = (1.2d0, 0.1d0)
  TAU(2) = (0.8d0, -0.2d0)
  TAU(3) = (1.5d0, 0.3d0)

  call ZUNG2L(M, N, K, A, LDA, TAU, WORK, INFO)
  call begin_test('zung2l_3x3')
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

  ! Initialize columns
  A(1,1) = (0.0d0, 0.0d0)
  A(2,1) = (0.0d0, 0.0d0)
  A(3,1) = (0.0d0, 0.0d0)
  A(4,1) = (0.0d0, 0.0d0)
  ! Column 1 (reflector for H(1))
  A(1,2) = (0.3d0, 0.4d0)
  A(2,2) = (0.5d0, -0.1d0)
  A(3,2) = (1.0d0, 0.0d0)
  A(4,2) = (0.0d0, 0.0d0)
  ! Column 2 (reflector for H(2))
  A(1,3) = (0.2d0, 0.1d0)
  A(2,3) = (0.4d0, -0.3d0)
  A(3,3) = (0.0d0, 0.6d0)
  A(4,3) = (0.7d0, 0.0d0)

  TAU(1) = (1.1d0, 0.2d0)
  TAU(2) = (0.9d0, -0.1d0)

  call ZUNG2L(M, N, K, A, LDA, TAU, WORK, INFO)
  call begin_test('zung2l_4x3')
  call print_int('info', INFO)
  call print_array('Q', A_r, 2*LDA*N)
  call end_test()

  ! =====================================================
  ! Test 3: K=0 (identity matrix)
  ! =====================================================
  M = 3
  N = 3
  K = 0
  LDA = NMAX

  A(1,1) = (9.0d0, 9.0d0)
  A(2,2) = (9.0d0, 9.0d0)
  A(3,3) = (9.0d0, 9.0d0)

  call ZUNG2L(M, N, K, A, LDA, TAU, WORK, INFO)
  call begin_test('zung2l_K0')
  call print_int('info', INFO)
  call print_array('Q', A_r, 2*LDA*N)
  call end_test()

  ! =====================================================
  ! Test 4: N=0
  ! =====================================================
  M = 3
  N = 0
  K = 0
  LDA = NMAX

  call ZUNG2L(M, N, K, A, LDA, TAU, WORK, INFO)
  call begin_test('zung2l_N0')
  call print_int('info', INFO)
  call end_test()

end program test_zung2l
