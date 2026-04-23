program test_dlaein
  use test_utils
  implicit none

  integer, parameter :: MAXN = 5
  integer, parameter :: LDH = MAXN
  integer, parameter :: LDB = MAXN + 1
  double precision :: H(LDH, MAXN), B(LDB, MAXN)
  double precision :: VR(MAXN), VI(MAXN), WORK(MAXN)
  double precision :: WR, WI, EPS3, SMLNUM, BIGNUM, ULP, UNFL, HNORM
  integer :: INFO, i, j, N
  logical :: RIGHTV, NOINIT
  double precision :: DLAMCH, DLANHS
  external :: DLAMCH, DLANHS, DLAEIN

  ! --- common machine constants ---
  UNFL = DLAMCH('Safe minimum')
  ULP = DLAMCH('Precision')

  ! ===============================================================
  ! Test 1: right eigenvector, real eigenvalue, NOINIT=true, 4x4
  !   [ 4  3  2  1 ]
  !   [ 1  4  3  2 ]
  !   [ 0  1  4  3 ]
  !   [ 0  0  1  4 ]
  ! Pick WR = 4.0 (close to an eigenvalue)
  ! ===============================================================
  N = 4
  H = 0.0d0
  H(1,1) = 4.0d0; H(1,2) = 3.0d0; H(1,3) = 2.0d0; H(1,4) = 1.0d0
  H(2,1) = 1.0d0; H(2,2) = 4.0d0; H(2,3) = 3.0d0; H(2,4) = 2.0d0
  H(3,2) = 1.0d0; H(3,3) = 4.0d0; H(3,4) = 3.0d0
  H(4,3) = 1.0d0; H(4,4) = 4.0d0
  HNORM = DLANHS('I', N, H, LDH, WORK)
  EPS3 = HNORM * ULP
  if (EPS3.lt.UNFL*(N/ULP)) EPS3 = UNFL*(N/ULP)
  SMLNUM = UNFL*(N/ULP)
  BIGNUM = (1.0d0 - ULP) / SMLNUM
  WR = 4.0d0
  WI = 0.0d0
  VR = 0.0d0
  VI = 0.0d0
  B = 0.0d0
  WORK = 0.0d0
  RIGHTV = .TRUE.
  NOINIT = .TRUE.
  call DLAEIN(RIGHTV, NOINIT, N, H, LDH, WR, WI, VR, VI, B, LDB, &
              WORK, EPS3, SMLNUM, BIGNUM, INFO)
  call begin_test('right_real_noinit_4x4')
  call print_int('info', INFO)
  call print_array('vr', VR, N)
  call end_test()

  ! ===============================================================
  ! Test 2: left eigenvector, real eigenvalue, NOINIT=true, 4x4
  ! ===============================================================
  N = 4
  H = 0.0d0
  H(1,1) = 4.0d0; H(1,2) = 3.0d0; H(1,3) = 2.0d0; H(1,4) = 1.0d0
  H(2,1) = 1.0d0; H(2,2) = 4.0d0; H(2,3) = 3.0d0; H(2,4) = 2.0d0
  H(3,2) = 1.0d0; H(3,3) = 4.0d0; H(3,4) = 3.0d0
  H(4,3) = 1.0d0; H(4,4) = 4.0d0
  HNORM = DLANHS('I', N, H, LDH, WORK)
  EPS3 = HNORM * ULP
  if (EPS3.lt.UNFL*(N/ULP)) EPS3 = UNFL*(N/ULP)
  SMLNUM = UNFL*(N/ULP)
  BIGNUM = (1.0d0 - ULP) / SMLNUM
  WR = 4.0d0
  WI = 0.0d0
  VR = 0.0d0
  VI = 0.0d0
  B = 0.0d0
  WORK = 0.0d0
  call DLAEIN(.FALSE., .TRUE., N, H, LDH, WR, WI, VR, VI, B, LDB, &
              WORK, EPS3, SMLNUM, BIGNUM, INFO)
  call begin_test('left_real_noinit_4x4')
  call print_int('info', INFO)
  call print_array('vr', VR, N)
  call end_test()

  ! ===============================================================
  ! Test 3: right eigenvector, complex eigenvalue pair, 4x4
  !   [ 0  -1   2   1 ]
  !   [ 1   0   1   2 ]
  !   [ 0   1   0  -1 ]
  !   [ 0   0   1   0 ]
  ! ===============================================================
  N = 4
  H = 0.0d0
  H(1,1) = 0.0d0; H(1,2) = -1.0d0; H(1,3) = 2.0d0; H(1,4) = 1.0d0
  H(2,1) = 1.0d0; H(2,2) =  0.0d0; H(2,3) = 1.0d0; H(2,4) = 2.0d0
  H(3,2) = 1.0d0; H(3,3) = 0.0d0; H(3,4) = -1.0d0
  H(4,3) = 1.0d0; H(4,4) = 0.0d0
  HNORM = DLANHS('I', N, H, LDH, WORK)
  EPS3 = HNORM * ULP
  if (EPS3.lt.UNFL*(N/ULP)) EPS3 = UNFL*(N/ULP)
  SMLNUM = UNFL*(N/ULP)
  BIGNUM = (1.0d0 - ULP) / SMLNUM
  WR = 0.0d0
  WI = 1.5d0
  VR = 0.0d0
  VI = 0.0d0
  B = 0.0d0
  WORK = 0.0d0
  call DLAEIN(.TRUE., .TRUE., N, H, LDH, WR, WI, VR, VI, B, LDB, &
              WORK, EPS3, SMLNUM, BIGNUM, INFO)
  call begin_test('right_complex_noinit_4x4')
  call print_int('info', INFO)
  call print_array('vr', VR, N)
  call print_array('vi', VI, N)
  call end_test()

  ! ===============================================================
  ! Test 4: left eigenvector, complex pair, 4x4
  ! ===============================================================
  N = 4
  H = 0.0d0
  H(1,1) = 0.0d0; H(1,2) = -1.0d0; H(1,3) = 2.0d0; H(1,4) = 1.0d0
  H(2,1) = 1.0d0; H(2,2) =  0.0d0; H(2,3) = 1.0d0; H(2,4) = 2.0d0
  H(3,2) = 1.0d0; H(3,3) = 0.0d0; H(3,4) = -1.0d0
  H(4,3) = 1.0d0; H(4,4) = 0.0d0
  HNORM = DLANHS('I', N, H, LDH, WORK)
  EPS3 = HNORM * ULP
  if (EPS3.lt.UNFL*(N/ULP)) EPS3 = UNFL*(N/ULP)
  SMLNUM = UNFL*(N/ULP)
  BIGNUM = (1.0d0 - ULP) / SMLNUM
  WR = 0.0d0
  WI = 1.5d0
  VR = 0.0d0
  VI = 0.0d0
  B = 0.0d0
  WORK = 0.0d0
  call DLAEIN(.FALSE., .TRUE., N, H, LDH, WR, WI, VR, VI, B, LDB, &
              WORK, EPS3, SMLNUM, BIGNUM, INFO)
  call begin_test('left_complex_noinit_4x4')
  call print_int('info', INFO)
  call print_array('vr', VR, N)
  call print_array('vi', VI, N)
  call end_test()

  ! ===============================================================
  ! Test 5: right eigenvector, real, NOINIT=false (use provided VR)
  ! ===============================================================
  N = 4
  H = 0.0d0
  H(1,1) = 4.0d0; H(1,2) = 3.0d0; H(1,3) = 2.0d0; H(1,4) = 1.0d0
  H(2,1) = 1.0d0; H(2,2) = 4.0d0; H(2,3) = 3.0d0; H(2,4) = 2.0d0
  H(3,2) = 1.0d0; H(3,3) = 4.0d0; H(3,4) = 3.0d0
  H(4,3) = 1.0d0; H(4,4) = 4.0d0
  HNORM = DLANHS('I', N, H, LDH, WORK)
  EPS3 = HNORM * ULP
  if (EPS3.lt.UNFL*(N/ULP)) EPS3 = UNFL*(N/ULP)
  SMLNUM = UNFL*(N/ULP)
  BIGNUM = (1.0d0 - ULP) / SMLNUM
  WR = 4.0d0
  WI = 0.0d0
  VR(1) = 1.0d0; VR(2) = 0.5d0; VR(3) = -0.5d0; VR(4) = -1.0d0
  VI = 0.0d0
  B = 0.0d0
  WORK = 0.0d0
  call DLAEIN(.TRUE., .FALSE., N, H, LDH, WR, WI, VR, VI, B, LDB, &
              WORK, EPS3, SMLNUM, BIGNUM, INFO)
  call begin_test('right_real_withinit_4x4')
  call print_int('info', INFO)
  call print_array('vr', VR, N)
  call end_test()

  ! ===============================================================
  ! Test 6: 3x3 triangular, real eigenvalue, right vector
  ! ===============================================================
  N = 3
  H = 0.0d0
  H(1,1) = 1.0d0; H(1,2) = 2.0d0; H(1,3) = 3.0d0
  H(2,2) = 4.0d0; H(2,3) = 5.0d0
  H(3,3) = 6.0d0
  HNORM = DLANHS('I', N, H, LDH, WORK)
  EPS3 = HNORM * ULP
  if (EPS3.lt.UNFL*(N/ULP)) EPS3 = UNFL*(N/ULP)
  SMLNUM = UNFL*(N/ULP)
  BIGNUM = (1.0d0 - ULP) / SMLNUM
  WR = 4.0d0
  WI = 0.0d0
  VR = 0.0d0
  VI = 0.0d0
  B = 0.0d0
  WORK = 0.0d0
  call DLAEIN(.TRUE., .TRUE., N, H, LDH, WR, WI, VR, VI, B, LDB, &
              WORK, EPS3, SMLNUM, BIGNUM, INFO)
  call begin_test('right_real_triangular_3x3')
  call print_int('info', INFO)
  call print_array('vr', VR, N)
  call end_test()

  ! ===============================================================
  ! Test 7: 2x2 complex, right vector
  ! ===============================================================
  N = 2
  H = 0.0d0
  H(1,1) = 0.0d0; H(1,2) = -2.0d0
  H(2,1) = 1.0d0; H(2,2) = 0.0d0
  HNORM = DLANHS('I', N, H, LDH, WORK)
  EPS3 = HNORM * ULP
  if (EPS3.lt.UNFL*(N/ULP)) EPS3 = UNFL*(N/ULP)
  SMLNUM = UNFL*(N/ULP)
  BIGNUM = (1.0d0 - ULP) / SMLNUM
  WR = 0.0d0
  WI = 1.41421356d0
  VR = 0.0d0
  VI = 0.0d0
  B = 0.0d0
  WORK = 0.0d0
  call DLAEIN(.TRUE., .TRUE., N, H, LDH, WR, WI, VR, VI, B, LDB, &
              WORK, EPS3, SMLNUM, BIGNUM, INFO)
  call begin_test('right_complex_2x2')
  call print_int('info', INFO)
  call print_array('vr', VR, N)
  call print_array('vi', VI, N)
  call end_test()

  ! ===============================================================
  ! Test 8: N=1 edge case
  ! ===============================================================
  N = 1
  H = 0.0d0
  H(1,1) = 3.5d0
  HNORM = DLANHS('I', N, H, LDH, WORK)
  EPS3 = HNORM * ULP
  if (EPS3.lt.UNFL*(N/ULP)) EPS3 = UNFL*(N/ULP)
  SMLNUM = UNFL*(N/ULP)
  BIGNUM = (1.0d0 - ULP) / SMLNUM
  WR = 3.5d0
  WI = 0.0d0
  VR = 0.0d0
  VI = 0.0d0
  B = 0.0d0
  WORK = 0.0d0
  call DLAEIN(.TRUE., .TRUE., N, H, LDH, WR, WI, VR, VI, B, LDB, &
              WORK, EPS3, SMLNUM, BIGNUM, INFO)
  call begin_test('n1')
  call print_int('info', INFO)
  call print_array('vr', VR, N)
  call end_test()

  ! ===============================================================
  ! Test 9: left eigenvector, real, NOINIT=false
  ! ===============================================================
  N = 4
  H = 0.0d0
  H(1,1) = 4.0d0; H(1,2) = 3.0d0; H(1,3) = 2.0d0; H(1,4) = 1.0d0
  H(2,1) = 1.0d0; H(2,2) = 4.0d0; H(2,3) = 3.0d0; H(2,4) = 2.0d0
  H(3,2) = 1.0d0; H(3,3) = 4.0d0; H(3,4) = 3.0d0
  H(4,3) = 1.0d0; H(4,4) = 4.0d0
  HNORM = DLANHS('I', N, H, LDH, WORK)
  EPS3 = HNORM * ULP
  if (EPS3.lt.UNFL*(N/ULP)) EPS3 = UNFL*(N/ULP)
  SMLNUM = UNFL*(N/ULP)
  BIGNUM = (1.0d0 - ULP) / SMLNUM
  WR = 4.0d0
  WI = 0.0d0
  VR(1) = 0.5d0; VR(2) = 1.0d0; VR(3) = 0.5d0; VR(4) = 0.25d0
  VI = 0.0d0
  B = 0.0d0
  WORK = 0.0d0
  call DLAEIN(.FALSE., .FALSE., N, H, LDH, WR, WI, VR, VI, B, LDB, &
              WORK, EPS3, SMLNUM, BIGNUM, INFO)
  call begin_test('left_real_withinit_4x4')
  call print_int('info', INFO)
  call print_array('vr', VR, N)
  call end_test()

  ! ===============================================================
  ! Test 10: 5x5 mixed real matrix, right vector, complex eig
  ! ===============================================================
  N = 5
  H = 0.0d0
  H(1,1) = 5.0d0; H(1,2) = 4.0d0; H(1,3) = 1.0d0; H(1,4) = 0.5d0; H(1,5) = 0.1d0
  H(2,1) = 1.0d0; H(2,2) = 3.0d0; H(2,3) = 2.0d0; H(2,4) = 1.0d0; H(2,5) = 0.5d0
  H(3,2) = 2.0d0; H(3,3) = 1.0d0; H(3,4) = 3.0d0; H(3,5) = 1.0d0
  H(4,3) = 1.5d0; H(4,4) = 2.0d0; H(4,5) = 2.0d0
  H(5,4) = 0.5d0; H(5,5) = 4.0d0
  HNORM = DLANHS('I', N, H, LDH, WORK)
  EPS3 = HNORM * ULP
  if (EPS3.lt.UNFL*(N/ULP)) EPS3 = UNFL*(N/ULP)
  SMLNUM = UNFL*(N/ULP)
  BIGNUM = (1.0d0 - ULP) / SMLNUM
  WR = 2.5d0
  WI = 1.0d0
  VR = 0.0d0
  VI = 0.0d0
  B = 0.0d0
  WORK = 0.0d0
  call DLAEIN(.TRUE., .TRUE., N, H, LDH, WR, WI, VR, VI, B, LDB, &
              WORK, EPS3, SMLNUM, BIGNUM, INFO)
  call begin_test('right_complex_5x5')
  call print_int('info', INFO)
  call print_array('vr', VR, N)
  call print_array('vi', VI, N)
  call end_test()

end program
