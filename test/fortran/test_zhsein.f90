program test_zhsein
  use test_utils
  implicit none

  integer, parameter :: MAXN = 6
  integer, parameter :: LDH = MAXN
  integer, parameter :: LDVL = MAXN
  integer, parameter :: LDVR = MAXN
  integer, parameter :: MM = MAXN
  integer, parameter :: LWORK = MAXN * MAXN
  complex*16 :: H(LDH, MAXN)
  complex*16 :: VL(LDVL, MM), VR(LDVR, MM)
  complex*16 :: W(MAXN), WORK(LWORK)
  double precision :: RWORK(MAXN)
  double precision :: H_r(2*LDH*MAXN), VL_r(2*LDVL*MM), VR_r(2*LDVR*MM)
  double precision :: W_r(2*MAXN)
  integer :: IFAILL(MM), IFAILR(MM)
  logical :: SELECT(MAXN)
  integer :: INFO, M, N
  equivalence (H, H_r)
  equivalence (VL, VL_r)
  equivalence (VR, VR_r)
  equivalence (W, W_r)
  external :: ZHSEIN

  ! ===============================================================
  ! Test 1: right eigenvectors, 3x3 complex upper Hessenberg
  ! ===============================================================
  N = 3
  H = (0.0d0, 0.0d0)
  H(1,1) = (2.0d0, 1.0d0); H(1,2) = (1.0d0, 0.5d0); H(1,3) = (0.5d0, 0.0d0)
  H(2,1) = (0.1d0, 0.0d0); H(2,2) = (3.0d0, 0.0d0); H(2,3) = (1.0d0, -1.0d0)
  H(3,2) = (0.05d0, 0.0d0); H(3,3) = (4.0d0, -1.0d0)
  W(1) = (2.0d0, 1.0d0)
  W(2) = (3.0d0, 0.0d0)
  W(3) = (4.0d0, -1.0d0)
  SELECT(1) = .TRUE.; SELECT(2) = .TRUE.; SELECT(3) = .TRUE.
  VL = (0.0d0, 0.0d0); VR = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0); RWORK = 0.0d0
  IFAILL = 0; IFAILR = 0
  call ZHSEIN('R', 'N', 'N', SELECT, N, H, LDH, W, &
              VL, LDVL, VR, LDVR, MM, M, WORK, RWORK, IFAILL, IFAILR, INFO)
  call begin_test('right_all_3x3')
  call print_int('info', INFO)
  call print_int('m', M)
  call print_array('vr1', VR_r(1:2*N), 2*N)
  call print_array('vr2', VR_r(2*LDVR+1:2*LDVR+2*N), 2*N)
  call print_array('vr3', VR_r(4*LDVR+1:4*LDVR+2*N), 2*N)
  call print_int_array('ifailr', IFAILR, N)
  call end_test()

  ! ===============================================================
  ! Test 2: left eigenvectors, same matrix
  ! ===============================================================
  N = 3
  H = (0.0d0, 0.0d0)
  H(1,1) = (2.0d0, 1.0d0); H(1,2) = (1.0d0, 0.5d0); H(1,3) = (0.5d0, 0.0d0)
  H(2,1) = (0.1d0, 0.0d0); H(2,2) = (3.0d0, 0.0d0); H(2,3) = (1.0d0, -1.0d0)
  H(3,2) = (0.05d0, 0.0d0); H(3,3) = (4.0d0, -1.0d0)
  W(1) = (2.0d0, 1.0d0)
  W(2) = (3.0d0, 0.0d0)
  W(3) = (4.0d0, -1.0d0)
  SELECT(1) = .TRUE.; SELECT(2) = .TRUE.; SELECT(3) = .TRUE.
  VL = (0.0d0, 0.0d0); VR = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0); RWORK = 0.0d0
  IFAILL = 0; IFAILR = 0
  call ZHSEIN('L', 'N', 'N', SELECT, N, H, LDH, W, &
              VL, LDVL, VR, LDVR, MM, M, WORK, RWORK, IFAILL, IFAILR, INFO)
  call begin_test('left_all_3x3')
  call print_int('info', INFO)
  call print_int('m', M)
  call print_array('vl1', VL_r(1:2*N), 2*N)
  call print_array('vl2', VL_r(2*LDVL+1:2*LDVL+2*N), 2*N)
  call print_array('vl3', VL_r(4*LDVL+1:4*LDVL+2*N), 2*N)
  call print_int_array('ifaill', IFAILL, N)
  call end_test()

  ! ===============================================================
  ! Test 3: both sides, 4x4 complex matrix
  ! ===============================================================
  N = 4
  H = (0.0d0, 0.0d0)
  H(1,1) = (1.0d0, 0.5d0); H(1,2) = (0.5d0, 0.0d0); H(1,3) = (0.2d0, 0.1d0); H(1,4) = (0.1d0, 0.0d0)
  H(2,1) = (0.3d0, 0.0d0); H(2,2) = (2.0d0, -0.5d0); H(2,3) = (0.8d0, 0.2d0); H(2,4) = (0.3d0, 0.1d0)
  H(3,2) = (0.2d0, 0.0d0); H(3,3) = (3.0d0, 1.0d0); H(3,4) = (0.7d0, -0.3d0)
  H(4,3) = (0.15d0, 0.0d0); H(4,4) = (4.0d0, 0.5d0)
  W(1) = (1.0d0, 0.5d0)
  W(2) = (2.0d0, -0.5d0)
  W(3) = (3.0d0, 1.0d0)
  W(4) = (4.0d0, 0.5d0)
  SELECT(1) = .TRUE.; SELECT(2) = .TRUE.; SELECT(3) = .TRUE.; SELECT(4) = .TRUE.
  VL = (0.0d0, 0.0d0); VR = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0); RWORK = 0.0d0
  IFAILL = 0; IFAILR = 0
  call ZHSEIN('B', 'N', 'N', SELECT, N, H, LDH, W, &
              VL, LDVL, VR, LDVR, MM, M, WORK, RWORK, IFAILL, IFAILR, INFO)
  call begin_test('both_4x4')
  call print_int('info', INFO)
  call print_int('m', M)
  call print_array('vr1', VR_r(1:2*N), 2*N)
  call print_array('vr2', VR_r(2*LDVR+1:2*LDVR+2*N), 2*N)
  call print_array('vr3', VR_r(4*LDVR+1:4*LDVR+2*N), 2*N)
  call print_array('vr4', VR_r(6*LDVR+1:6*LDVR+2*N), 2*N)
  call print_array('vl1', VL_r(1:2*N), 2*N)
  call print_array('vl2', VL_r(2*LDVL+1:2*LDVL+2*N), 2*N)
  call print_array('vl3', VL_r(4*LDVL+1:4*LDVL+2*N), 2*N)
  call print_array('vl4', VL_r(6*LDVL+1:6*LDVL+2*N), 2*N)
  call print_int_array('ifailr', IFAILR, N)
  call print_int_array('ifaill', IFAILL, N)
  call end_test()

  ! ===============================================================
  ! Test 4: selective (only eigenvalues 1, 3) on 5x5 upper triangular
  ! ===============================================================
  N = 5
  H = (0.0d0, 0.0d0)
  H(1,1) = (1.0d0, 0.0d0); H(1,2) = (2.0d0, 0.0d0); H(1,3) = (1.0d0, 0.0d0); H(1,4) = (3.0d0, 0.0d0); H(1,5) = (0.5d0, 0.0d0)
  H(2,2) = (2.0d0, 1.0d0); H(2,3) = (1.5d0, 0.0d0); H(2,4) = (1.0d0, 0.0d0); H(2,5) = (0.5d0, 0.0d0)
  H(3,3) = (3.0d0, -1.0d0); H(3,4) = (2.0d0, 0.0d0); H(3,5) = (1.0d0, 0.0d0)
  H(4,4) = (4.0d0, 0.5d0); H(4,5) = (1.0d0, 0.0d0)
  H(5,5) = (5.0d0, -0.5d0)
  W(1) = (1.0d0, 0.0d0)
  W(2) = (2.0d0, 1.0d0)
  W(3) = (3.0d0, -1.0d0)
  W(4) = (4.0d0, 0.5d0)
  W(5) = (5.0d0, -0.5d0)
  SELECT(1) = .TRUE.
  SELECT(2) = .FALSE.
  SELECT(3) = .TRUE.
  SELECT(4) = .FALSE.
  SELECT(5) = .FALSE.
  VL = (0.0d0, 0.0d0); VR = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0); RWORK = 0.0d0
  IFAILL = 0; IFAILR = 0
  call ZHSEIN('R', 'N', 'N', SELECT, N, H, LDH, W, &
              VL, LDVL, VR, LDVR, MM, M, WORK, RWORK, IFAILL, IFAILR, INFO)
  call begin_test('right_selective_5x5')
  call print_int('info', INFO)
  call print_int('m', M)
  call print_array('vr1', VR_r(1:2*N), 2*N)
  call print_array('vr2', VR_r(2*LDVR+1:2*LDVR+2*N), 2*N)
  call print_int_array('ifailr', IFAILR, 2)
  call end_test()

  ! ===============================================================
  ! Test 5: N=1
  ! ===============================================================
  N = 1
  H = (0.0d0, 0.0d0)
  H(1,1) = (3.5d0, -1.2d0)
  W(1) = (3.5d0, -1.2d0)
  SELECT(1) = .TRUE.
  VL = (0.0d0, 0.0d0); VR = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0); RWORK = 0.0d0
  IFAILL = 0; IFAILR = 0
  call ZHSEIN('B', 'N', 'N', SELECT, N, H, LDH, W, &
              VL, LDVL, VR, LDVR, MM, M, WORK, RWORK, IFAILL, IFAILR, INFO)
  call begin_test('n1_both')
  call print_int('info', INFO)
  call print_int('m', M)
  call print_array('vr1', VR_r(1:2*N), 2*N)
  call print_array('vl1', VL_r(1:2*N), 2*N)
  call end_test()

  ! ===============================================================
  ! Test 6: fromqr with block structure (5x5, zero subdiag at H(4,3))
  ! ===============================================================
  N = 5
  H = (0.0d0, 0.0d0)
  H(1,1) = (2.0d0, 0.5d0); H(1,2) = (1.0d0, 0.0d0); H(1,3) = (0.5d0, 0.1d0); H(1,4) = (0.2d0, 0.0d0); H(1,5) = (0.1d0, 0.0d0)
  H(2,1) = (0.3d0, 0.0d0); H(2,2) = (3.0d0, -0.5d0); H(2,3) = (1.0d0, 0.2d0); H(2,4) = (0.3d0, 0.0d0); H(2,5) = (0.2d0, 0.0d0)
  H(3,2) = (0.2d0, 0.0d0); H(3,3) = (4.0d0, 1.0d0); H(3,4) = (0.4d0, 0.0d0); H(3,5) = (0.3d0, 0.0d0)
  ! H(4,3) = 0 -> block split
  H(4,4) = (5.0d0, -1.0d0); H(4,5) = (1.0d0, 0.5d0)
  H(5,4) = (0.3d0, 0.0d0); H(5,5) = (6.0d0, 0.5d0)
  W(1) = (2.0d0, 0.5d0)
  W(2) = (3.0d0, -0.5d0)
  W(3) = (4.0d0, 1.0d0)
  W(4) = (5.0d0, -1.0d0)
  W(5) = (6.0d0, 0.5d0)
  SELECT(1) = .TRUE.; SELECT(2) = .TRUE.; SELECT(3) = .TRUE.; SELECT(4) = .TRUE.; SELECT(5) = .TRUE.
  VL = (0.0d0, 0.0d0); VR = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0); RWORK = 0.0d0
  IFAILL = 0; IFAILR = 0
  call ZHSEIN('B', 'Q', 'N', SELECT, N, H, LDH, W, &
              VL, LDVL, VR, LDVR, MM, M, WORK, RWORK, IFAILL, IFAILR, INFO)
  call begin_test('both_fromqr_block_5x5')
  call print_int('info', INFO)
  call print_int('m', M)
  call print_array('vr1', VR_r(1:2*N), 2*N)
  call print_array('vr2', VR_r(2*LDVR+1:2*LDVR+2*N), 2*N)
  call print_array('vr3', VR_r(4*LDVR+1:4*LDVR+2*N), 2*N)
  call print_array('vr4', VR_r(6*LDVR+1:6*LDVR+2*N), 2*N)
  call print_array('vr5', VR_r(8*LDVR+1:8*LDVR+2*N), 2*N)
  call print_array('vl1', VL_r(1:2*N), 2*N)
  call print_array('vl2', VL_r(2*LDVL+1:2*LDVL+2*N), 2*N)
  call print_array('vl3', VL_r(4*LDVL+1:4*LDVL+2*N), 2*N)
  call print_array('vl4', VL_r(6*LDVL+1:6*LDVL+2*N), 2*N)
  call print_array('vl5', VL_r(8*LDVL+1:8*LDVL+2*N), 2*N)
  call end_test()

end program
