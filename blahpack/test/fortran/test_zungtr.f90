program test_zungtr
  use test_utils
  implicit none

  integer, parameter :: NMAX = 4
  integer :: N, LDA, LWORK, INFO, IINFO
  complex*16 :: A(NMAX, NMAX), TAU(NMAX), WORK(200)
  double precision :: A_r(2*NMAX*NMAX), TAU_r(2*NMAX)
  double precision :: D(NMAX), E(NMAX)
  equivalence (A, A_r)
  equivalence (TAU, TAU_r)

  ! =====================================================
  ! Test 1: UPLO='L', 4x4 Hermitian matrix
  ! First call ZHETRD to produce reflectors, then ZUNGTR
  ! =====================================================
  N = 4
  LDA = NMAX
  LWORK = 200

  ! Build a Hermitian matrix (lower triangle)
  A(1,1) = (2.0d0, 0.0d0)
  A(2,1) = (1.0d0, 1.0d0)
  A(3,1) = (0.5d0, -0.5d0)
  A(4,1) = (0.0d0, 0.0d0)
  A(2,2) = (3.0d0, 0.0d0)
  A(3,2) = (0.0d0, 2.0d0)
  A(4,2) = (1.0d0, -1.0d0)
  A(3,3) = (4.0d0, 0.0d0)
  A(4,3) = (0.5d0, 0.5d0)
  A(4,4) = (5.0d0, 0.0d0)

  call ZHETRD('L', N, A, LDA, D, E, TAU, WORK, LWORK, IINFO)
  call ZUNGTR('L', N, A, LDA, TAU, WORK, LWORK, INFO)

  call begin_test('zungtr_4x4_L')
  call print_int('N', N)
  call print_int('info', INFO)
  call print_array('Q', A_r, 2*N*N)
  call end_test()

  ! =====================================================
  ! Test 2: UPLO='U', 4x4 Hermitian matrix
  ! =====================================================
  N = 4
  LDA = NMAX
  LWORK = 200

  ! Build the same matrix (upper triangle)
  A(1,1) = (2.0d0, 0.0d0)
  A(1,2) = (1.0d0, -1.0d0)
  A(1,3) = (0.5d0, 0.5d0)
  A(1,4) = (0.0d0, 0.0d0)
  A(2,2) = (3.0d0, 0.0d0)
  A(2,3) = (0.0d0, -2.0d0)
  A(2,4) = (1.0d0, 1.0d0)
  A(3,3) = (4.0d0, 0.0d0)
  A(3,4) = (0.5d0, -0.5d0)
  A(4,4) = (5.0d0, 0.0d0)

  call ZHETRD('U', N, A, LDA, D, E, TAU, WORK, LWORK, IINFO)
  call ZUNGTR('U', N, A, LDA, TAU, WORK, LWORK, INFO)

  call begin_test('zungtr_4x4_U')
  call print_int('N', N)
  call print_int('info', INFO)
  call print_array('Q', A_r, 2*N*N)
  call end_test()

  ! =====================================================
  ! Test 3: UPLO='L', 3x3
  ! =====================================================
  N = 3
  LDA = NMAX
  LWORK = 200

  A(1,1) = (4.0d0, 0.0d0)
  A(2,1) = (1.0d0, -2.0d0)
  A(3,1) = (0.0d0, 1.0d0)
  A(2,2) = (5.0d0, 0.0d0)
  A(3,2) = (2.0d0, 0.0d0)
  A(3,3) = (6.0d0, 0.0d0)

  call ZHETRD('L', N, A, LDA, D, E, TAU, WORK, LWORK, IINFO)
  call ZUNGTR('L', N, A, LDA, TAU, WORK, LWORK, INFO)

  call begin_test('zungtr_3x3_L')
  call print_int('N', N)
  call print_int('info', INFO)
  call print_array('Q', A_r, 2*LDA*N)
  call end_test()

  ! =====================================================
  ! Test 4: N=1
  ! =====================================================
  N = 1
  LDA = NMAX
  LWORK = 200

  A(1,1) = (5.0d0, 0.0d0)
  call ZHETRD('L', N, A, LDA, D, E, TAU, WORK, LWORK, IINFO)
  call ZUNGTR('L', N, A, LDA, TAU, WORK, LWORK, INFO)

  call begin_test('zungtr_1x1')
  call print_int('N', N)
  call print_int('info', INFO)
  call print_array('Q', A_r, 2)
  call end_test()

  ! =====================================================
  ! Test 5: N=0
  ! =====================================================
  N = 0
  LDA = NMAX
  LWORK = 200

  call ZUNGTR('L', N, A, LDA, TAU, WORK, LWORK, INFO)

  call begin_test('zungtr_0x0')
  call print_int('N', N)
  call print_int('info', INFO)
  call end_test()

end program test_zungtr
