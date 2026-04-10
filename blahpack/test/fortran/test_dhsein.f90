program test_dhsein
  use test_utils
  implicit none

  integer, parameter :: MAXN = 6
  integer, parameter :: LDH = MAXN
  integer, parameter :: LDVL = MAXN
  integer, parameter :: LDVR = MAXN
  integer, parameter :: MM = MAXN
  integer, parameter :: LWORK = (MAXN + 2) * MAXN
  double precision :: H(LDH, MAXN)
  double precision :: VL(LDVL, MM), VR(LDVR, MM)
  double precision :: WR(MAXN), WI(MAXN), WORK(LWORK)
  integer :: IFAILL(MM), IFAILR(MM)
  logical :: SELECT(MAXN)
  integer :: INFO, M, N, i, j
  external :: DHSEIN

  ! ===============================================================
  ! Test 1: right eigenvectors only, 4x4 real upper Hessenberg
  !   [ 4 3 2 1 ]
  !   [ 1 4 3 2 ]
  !   [ 0 1 4 3 ]
  !   [ 0 0 1 4 ]
  ! Select all eigenvalues.
  ! Use eigenvalues computed ahead of time by DHSEQR — but as this
  ! routine just needs WR/WI close, use approximate diag values.
  ! ===============================================================
  N = 4
  H = 0.0d0
  H(1,1) = 4.0d0; H(1,2) = 3.0d0; H(1,3) = 2.0d0; H(1,4) = 1.0d0
  H(2,1) = 1.0d0; H(2,2) = 4.0d0; H(2,3) = 3.0d0; H(2,4) = 2.0d0
  H(3,2) = 1.0d0; H(3,3) = 4.0d0; H(3,4) = 3.0d0
  H(4,3) = 1.0d0; H(4,4) = 4.0d0
  ! Approximate eigenvalues (all real for this symmetric-ish setup)
  WR(1) = 8.290547d0; WI(1) = 0.0d0
  WR(2) = 4.735207d0; WI(2) = 0.0d0
  WR(3) = 2.285640d0; WI(3) = 0.0d0
  WR(4) = 0.688606d0; WI(4) = 0.0d0
  SELECT = .TRUE.
  VL = 0.0d0
  VR = 0.0d0
  WORK = 0.0d0
  IFAILL = 0
  IFAILR = 0
  call DHSEIN('R', 'N', 'N', SELECT, N, H, LDH, WR, WI, &
              VL, LDVL, VR, LDVR, MM, M, WORK, IFAILL, IFAILR, INFO)
  call begin_test('right_all_4x4')
  call print_int('info', INFO)
  call print_int('m', M)
  call print_array('wr', WR, N)
  call print_array('vr1', VR(:,1), N)
  call print_array('vr2', VR(:,2), N)
  call print_array('vr3', VR(:,3), N)
  call print_array('vr4', VR(:,4), N)
  call print_int_array('ifailr', IFAILR, N)
  call end_test()

  ! ===============================================================
  ! Test 2: left eigenvectors only, same 4x4 matrix
  ! ===============================================================
  N = 4
  H = 0.0d0
  H(1,1) = 4.0d0; H(1,2) = 3.0d0; H(1,3) = 2.0d0; H(1,4) = 1.0d0
  H(2,1) = 1.0d0; H(2,2) = 4.0d0; H(2,3) = 3.0d0; H(2,4) = 2.0d0
  H(3,2) = 1.0d0; H(3,3) = 4.0d0; H(3,4) = 3.0d0
  H(4,3) = 1.0d0; H(4,4) = 4.0d0
  WR(1) = 8.290547d0; WI(1) = 0.0d0
  WR(2) = 4.735207d0; WI(2) = 0.0d0
  WR(3) = 2.285640d0; WI(3) = 0.0d0
  WR(4) = 0.688606d0; WI(4) = 0.0d0
  SELECT = .TRUE.
  VL = 0.0d0
  VR = 0.0d0
  WORK = 0.0d0
  IFAILL = 0
  IFAILR = 0
  call DHSEIN('L', 'N', 'N', SELECT, N, H, LDH, WR, WI, &
              VL, LDVL, VR, LDVR, MM, M, WORK, IFAILL, IFAILR, INFO)
  call begin_test('left_all_4x4')
  call print_int('info', INFO)
  call print_int('m', M)
  call print_array('vl1', VL(:,1), N)
  call print_array('vl2', VL(:,2), N)
  call print_array('vl3', VL(:,3), N)
  call print_array('vl4', VL(:,4), N)
  call print_int_array('ifaill', IFAILL, N)
  call end_test()

  ! ===============================================================
  ! Test 3: both sides, 4x4 matrix with complex eigenvalue pair
  !   [ 0 -1  2  1 ]
  !   [ 1  0  1  2 ]
  !   [ 0  1  0 -1 ]
  !   [ 0  0  1  0 ]
  ! ===============================================================
  N = 4
  H = 0.0d0
  H(1,1) = 0.0d0; H(1,2) = -1.0d0; H(1,3) = 2.0d0; H(1,4) = 1.0d0
  H(2,1) = 1.0d0; H(2,2) =  0.0d0; H(2,3) = 1.0d0; H(2,4) = 2.0d0
  H(3,2) = 1.0d0; H(3,3) = 0.0d0; H(3,4) = -1.0d0
  H(4,3) = 1.0d0; H(4,4) = 0.0d0
  ! Approximate eigenvalues: two complex conjugate pairs
  WR(1) = 0.0d0; WI(1) =  1.732051d0
  WR(2) = 0.0d0; WI(2) = -1.732051d0
  WR(3) = 0.0d0; WI(3) =  0.816497d0
  WR(4) = 0.0d0; WI(4) = -0.816497d0
  SELECT = .TRUE.
  VL = 0.0d0
  VR = 0.0d0
  WORK = 0.0d0
  IFAILL = 0
  IFAILR = 0
  call DHSEIN('B', 'N', 'N', SELECT, N, H, LDH, WR, WI, &
              VL, LDVL, VR, LDVR, MM, M, WORK, IFAILL, IFAILR, INFO)
  call begin_test('both_complex_4x4')
  call print_int('info', INFO)
  call print_int('m', M)
  call print_array('vr1', VR(:,1), N)
  call print_array('vr2', VR(:,2), N)
  call print_array('vr3', VR(:,3), N)
  call print_array('vr4', VR(:,4), N)
  call print_array('vl1', VL(:,1), N)
  call print_array('vl2', VL(:,2), N)
  call print_array('vl3', VL(:,3), N)
  call print_array('vl4', VL(:,4), N)
  call print_int_array('ifailr', IFAILR, N)
  call print_int_array('ifaill', IFAILL, N)
  call end_test()

  ! ===============================================================
  ! Test 4: selective, right only, 5x5, only eigenvalues 1,3,5
  !   Upper triangular (diagonal eigenvalues)
  ! ===============================================================
  N = 5
  H = 0.0d0
  H(1,1) = 1.0d0; H(1,2) = 2.0d0; H(1,3) = 1.0d0; H(1,4) = 3.0d0; H(1,5) = 0.5d0
  H(2,2) = 2.0d0; H(2,3) = 1.5d0; H(2,4) = 1.0d0; H(2,5) = 0.5d0
  H(3,3) = 3.0d0; H(3,4) = 2.0d0; H(3,5) = 1.0d0
  H(4,4) = 4.0d0; H(4,5) = 1.0d0
  H(5,5) = 5.0d0
  WR(1) = 1.0d0; WI(1) = 0.0d0
  WR(2) = 2.0d0; WI(2) = 0.0d0
  WR(3) = 3.0d0; WI(3) = 0.0d0
  WR(4) = 4.0d0; WI(4) = 0.0d0
  WR(5) = 5.0d0; WI(5) = 0.0d0
  SELECT(1) = .TRUE.
  SELECT(2) = .FALSE.
  SELECT(3) = .TRUE.
  SELECT(4) = .FALSE.
  SELECT(5) = .TRUE.
  VL = 0.0d0
  VR = 0.0d0
  WORK = 0.0d0
  IFAILL = 0
  IFAILR = 0
  call DHSEIN('R', 'N', 'N', SELECT, N, H, LDH, WR, WI, &
              VL, LDVL, VR, LDVR, MM, M, WORK, IFAILL, IFAILR, INFO)
  call begin_test('right_selective_5x5')
  call print_int('info', INFO)
  call print_int('m', M)
  call print_array('vr1', VR(:,1), N)
  call print_array('vr2', VR(:,2), N)
  call print_array('vr3', VR(:,3), N)
  call print_int_array('ifailr', IFAILR, 3)
  call end_test()

  ! ===============================================================
  ! Test 5: both sides, fromqr, 5x5 with a zero subdiagonal
  !   block structure: 3x3 block + 2x2 block
  ! ===============================================================
  N = 5
  H = 0.0d0
  H(1,1) = 2.0d0; H(1,2) = 1.0d0; H(1,3) = 0.5d0; H(1,4) = 0.2d0; H(1,5) = 0.1d0
  H(2,1) = 1.0d0; H(2,2) = 3.0d0; H(2,3) = 1.0d0; H(2,4) = 0.3d0; H(2,5) = 0.2d0
  H(3,2) = 1.0d0; H(3,3) = 4.0d0; H(3,4) = 0.4d0; H(3,5) = 0.3d0
  ! H(4,3) = 0, creating a block split
  H(4,4) = 5.0d0; H(4,5) = 1.0d0
  H(5,4) = 1.0d0; H(5,5) = 6.0d0
  WR(1) = 1.267949d0; WI(1) = 0.0d0
  WR(2) = 3.0d0;      WI(2) = 0.0d0
  WR(3) = 4.732051d0; WI(3) = 0.0d0
  WR(4) = 4.5d0;      WI(4) = 0.866025d0
  WR(5) = 4.5d0;      WI(5) = -0.866025d0
  ! NOTE: choose eigenvalues far from actual ones to force inv iter
  ! Actually use diag-based approximations
  WR(1) = 1.381966d0; WI(1) = 0.0d0
  WR(2) = 3.0d0;      WI(2) = 0.0d0
  WR(3) = 4.618034d0; WI(3) = 0.0d0
  WR(4) = 5.5d0;      WI(4) = 1.0d0
  WR(5) = 5.5d0;      WI(5) = -1.0d0
  SELECT = .TRUE.
  VL = 0.0d0
  VR = 0.0d0
  WORK = 0.0d0
  IFAILL = 0
  IFAILR = 0
  call DHSEIN('B', 'Q', 'N', SELECT, N, H, LDH, WR, WI, &
              VL, LDVL, VR, LDVR, MM, M, WORK, IFAILL, IFAILR, INFO)
  call begin_test('both_fromqr_block_5x5')
  call print_int('info', INFO)
  call print_int('m', M)
  call print_array('vr1', VR(:,1), N)
  call print_array('vr2', VR(:,2), N)
  call print_array('vr3', VR(:,3), N)
  call print_array('vr4', VR(:,4), N)
  call print_array('vr5', VR(:,5), N)
  call print_array('vl1', VL(:,1), N)
  call print_array('vl2', VL(:,2), N)
  call print_array('vl3', VL(:,3), N)
  call print_array('vl4', VL(:,4), N)
  call print_array('vl5', VL(:,5), N)
  call end_test()

  ! ===============================================================
  ! Test 6: N=1
  ! ===============================================================
  N = 1
  H = 0.0d0
  H(1,1) = 3.5d0
  WR(1) = 3.5d0; WI(1) = 0.0d0
  SELECT(1) = .TRUE.
  VL = 0.0d0
  VR = 0.0d0
  WORK = 0.0d0
  IFAILL = 0
  IFAILR = 0
  call DHSEIN('B', 'N', 'N', SELECT, N, H, LDH, WR, WI, &
              VL, LDVL, VR, LDVR, MM, M, WORK, IFAILL, IFAILR, INFO)
  call begin_test('n1_both')
  call print_int('info', INFO)
  call print_int('m', M)
  call print_array('vr1', VR(:,1), N)
  call print_array('vl1', VL(:,1), N)
  call end_test()

  ! ===============================================================
  ! Test 7: right only, 3x3 triangular
  ! ===============================================================
  N = 3
  H = 0.0d0
  H(1,1) = 1.0d0; H(1,2) = 2.0d0; H(1,3) = 3.0d0
  H(2,2) = 4.0d0; H(2,3) = 5.0d0
  H(3,3) = 6.0d0
  WR(1) = 1.0d0; WI(1) = 0.0d0
  WR(2) = 4.0d0; WI(2) = 0.0d0
  WR(3) = 6.0d0; WI(3) = 0.0d0
  SELECT(1) = .TRUE.
  SELECT(2) = .TRUE.
  SELECT(3) = .TRUE.
  VL = 0.0d0
  VR = 0.0d0
  WORK = 0.0d0
  IFAILL = 0
  IFAILR = 0
  call DHSEIN('R', 'N', 'N', SELECT, N, H, LDH, WR, WI, &
              VL, LDVL, VR, LDVR, MM, M, WORK, IFAILL, IFAILR, INFO)
  call begin_test('right_triangular_3x3')
  call print_int('info', INFO)
  call print_int('m', M)
  call print_array('vr1', VR(:,1), N)
  call print_array('vr2', VR(:,2), N)
  call print_array('vr3', VR(:,3), N)
  call end_test()

end program
