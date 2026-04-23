program test_zheevx
  use test_utils
  implicit none

  integer, parameter :: NMAX = 4
  integer :: N, LDA, LDZ, LWORK, IL, IU, M, INFO, i, j
  complex*16 :: A(NMAX, NMAX), Z(NMAX, NMAX), WORK(200)
  double precision :: W(NMAX), RWORK(7*NMAX)
  integer :: IWORK(5*NMAX), IFAIL(NMAX)
  double precision :: VL, VU, ABSTOL
  double precision :: A_r(2*NMAX*NMAX), Z_r(2*NMAX*NMAX)
  equivalence (A, A_r)
  equivalence (Z, Z_r)

  ! =====================================================
  ! Test 1: JOBZ='V', RANGE='A', UPLO='L', 4x4
  ! =====================================================
  N = 4
  LDA = NMAX
  LDZ = NMAX
  LWORK = 200
  ABSTOL = 0.0d0

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

  call ZHEEVX('V', 'A', 'L', N, A, LDA, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, LWORK, RWORK, IWORK, IFAIL, INFO)
  call begin_test('zheevx_4x4_V_A_L')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('w', W, M)
  call print_array('Z', Z_r, 2*N*M)
  call end_test()

  ! =====================================================
  ! Test 2: JOBZ='V', RANGE='A', UPLO='U', 4x4
  ! =====================================================
  N = 4
  LDA = NMAX
  LDZ = NMAX
  LWORK = 200

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

  call ZHEEVX('V', 'A', 'U', N, A, LDA, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, LWORK, RWORK, IWORK, IFAIL, INFO)
  call begin_test('zheevx_4x4_V_A_U')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('w', W, M)
  call print_array('Z', Z_r, 2*N*M)
  call end_test()

  ! =====================================================
  ! Test 3: JOBZ='N', RANGE='A', UPLO='L', 4x4
  ! =====================================================
  N = 4
  LDA = NMAX
  LDZ = NMAX
  LWORK = 200

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

  call ZHEEVX('N', 'A', 'L', N, A, LDA, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, LWORK, RWORK, IWORK, IFAIL, INFO)
  call begin_test('zheevx_4x4_N_A_L')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('w', W, M)
  call end_test()

  ! =====================================================
  ! Test 4: JOBZ='V', RANGE='V' (value range), UPLO='L'
  ! Select eigenvalues in [1.5, 4.5]
  ! =====================================================
  N = 4
  LDA = NMAX
  LDZ = NMAX
  LWORK = 200
  VL = 1.5d0
  VU = 4.5d0

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

  call ZHEEVX('V', 'V', 'L', N, A, LDA, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, LWORK, RWORK, IWORK, IFAIL, INFO)
  call begin_test('zheevx_4x4_V_V_L')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('w', W, M)
  call print_array('Z', Z_r, 2*N*M)
  call end_test()

  ! =====================================================
  ! Test 5: JOBZ='V', RANGE='I' (index range), UPLO='L'
  ! Select eigenvalues 2 through 3
  ! =====================================================
  N = 4
  LDA = NMAX
  LDZ = NMAX
  LWORK = 200
  IL = 2
  IU = 3

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

  call ZHEEVX('V', 'I', 'L', N, A, LDA, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, LWORK, RWORK, IWORK, IFAIL, INFO)
  call begin_test('zheevx_4x4_V_I_L')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('w', W, M)
  call print_array('Z', Z_r, 2*N*M)
  call end_test()

  ! =====================================================
  ! Test 6: JOBZ='N', RANGE='V', UPLO='U'
  ! =====================================================
  N = 4
  LDA = NMAX
  LDZ = NMAX
  LWORK = 200
  VL = 0.0d0
  VU = 3.0d0

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

  call ZHEEVX('N', 'V', 'U', N, A, LDA, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, LWORK, RWORK, IWORK, IFAIL, INFO)
  call begin_test('zheevx_4x4_N_V_U')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('w', W, M)
  call end_test()

  ! =====================================================
  ! Test 7: N=1, JOBZ='V'
  ! =====================================================
  N = 1
  LDA = NMAX
  LDZ = NMAX
  LWORK = 200
  ABSTOL = 0.0d0

  A(1,1) = (7.5d0, 0.0d0)

  call ZHEEVX('V', 'A', 'L', N, A, LDA, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, LWORK, RWORK, IWORK, IFAIL, INFO)
  call begin_test('zheevx_1x1_V')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('w', W, M)
  call print_array('Z', Z_r, 2)
  call end_test()

  ! =====================================================
  ! Test 8: N=0
  ! =====================================================
  N = 0
  LDA = NMAX
  LDZ = NMAX
  LWORK = 200

  call ZHEEVX('V', 'A', 'L', N, A, LDA, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, LWORK, RWORK, IWORK, IFAIL, INFO)
  call begin_test('zheevx_0x0')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! =====================================================
  ! Test 9: 3x3 diagonal, RANGE='I', IL=1, IU=1 (smallest only)
  ! =====================================================
  N = 3
  LDA = NMAX
  LDZ = NMAX
  LWORK = 200
  IL = 1
  IU = 1
  ABSTOL = 0.0d0

  A(1,1) = (5.0d0, 0.0d0)
  A(2,1) = (0.0d0, 0.0d0)
  A(3,1) = (0.0d0, 0.0d0)
  A(2,2) = (2.0d0, 0.0d0)
  A(3,2) = (0.0d0, 0.0d0)
  A(3,3) = (8.0d0, 0.0d0)

  call ZHEEVX('V', 'I', 'L', N, A, LDA, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, LWORK, RWORK, IWORK, IFAIL, INFO)
  call begin_test('zheevx_3x3_diag_I')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('w', W, M)
  call print_array('Z', Z_r, 2*N*M)
  call end_test()

  ! =====================================================
  ! Test 10: 3x3, JOBZ='V', RANGE='V', UPLO='U'
  ! =====================================================
  N = 3
  LDA = NMAX
  LDZ = NMAX
  LWORK = 200
  VL = 2.0d0
  VU = 7.0d0
  ABSTOL = 0.0d0

  A(1,1) = (4.0d0, 0.0d0)
  A(1,2) = (1.0d0, 2.0d0)
  A(1,3) = (0.0d0, -1.0d0)
  A(2,2) = (5.0d0, 0.0d0)
  A(2,3) = (2.0d0, 0.0d0)
  A(3,3) = (6.0d0, 0.0d0)

  call ZHEEVX('V', 'V', 'U', N, A, LDA, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, LWORK, RWORK, IWORK, IFAIL, INFO)
  call begin_test('zheevx_3x3_V_V_U')
  call print_int('N', N)
  call print_int('M', M)
  call print_int('info', INFO)
  call print_array('w', W, M)
  call print_array('Z', Z_r, 2*N*M)
  call end_test()

end program test_zheevx
