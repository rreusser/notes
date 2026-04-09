program test_dtgevc
  use test_utils
  implicit none

  integer, parameter :: MAXN = 6
  double precision :: S(MAXN, MAXN), P(MAXN, MAXN)
  double precision :: VL(MAXN, MAXN), VR(MAXN, MAXN)
  double precision :: WORK(6*MAXN)
  logical :: SELEC(MAXN)
  integer :: INFO, M, N, I

  ! ============================================================
  ! Test 1: SIDE='R', HOWMNY='A', 4x4 with 2 real + 1 complex pair
  ! S is quasi-triangular, P is upper triangular
  ! ============================================================
  N = 4
  S = 0.0d0; P = 0.0d0; VR = 0.0d0; VL = 0.0d0; WORK = 0.0d0
  ! S: quasi-triangular with 1x1 blocks at (1,1),(2,2) and 2x2 block at (3:4,3:4)
  S(1,1) = 1.0d0; S(1,2) = 0.5d0; S(1,3) = 0.2d0; S(1,4) = 0.1d0
  S(2,2) = 2.0d0; S(2,3) = 0.3d0; S(2,4) = 0.15d0
  S(3,3) = 4.0d0; S(3,4) = -2.0d0
  S(4,3) = 1.0d0; S(4,4) = 4.0d0
  ! P: upper triangular (diagonal for 2x2 block)
  P(1,1) = 1.0d0; P(1,2) = 0.1d0; P(1,3) = 0.05d0; P(1,4) = 0.02d0
  P(2,2) = 2.0d0; P(2,3) = 0.15d0; P(2,4) = 0.08d0
  P(3,3) = 1.0d0; P(3,4) = 0.0d0
  P(4,4) = 1.0d0

  call DTGEVC('R', 'A', SELEC, N, S, MAXN, P, MAXN, VL, MAXN, VR, MAXN, N, M, WORK, INFO)
  call begin_test('right all 4x4')
  call print_matrix('S', S, MAXN, N, N)
  call print_matrix('P', P, MAXN, N, N)
  call print_matrix('VR', VR, MAXN, N, N)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 2: SIDE='L', HOWMNY='A', same matrices
  ! ============================================================
  VR = 0.0d0; VL = 0.0d0; WORK = 0.0d0
  call DTGEVC('L', 'A', SELEC, N, S, MAXN, P, MAXN, VL, MAXN, VR, MAXN, N, M, WORK, INFO)
  call begin_test('left all 4x4')
  call print_matrix('VL', VL, MAXN, N, N)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 3: SIDE='B', HOWMNY='A', same matrices
  ! ============================================================
  VR = 0.0d0; VL = 0.0d0; WORK = 0.0d0
  call DTGEVC('B', 'A', SELEC, N, S, MAXN, P, MAXN, VL, MAXN, VR, MAXN, N, M, WORK, INFO)
  call begin_test('both all 4x4')
  call print_matrix('VR', VR, MAXN, N, N)
  call print_matrix('VL', VL, MAXN, N, N)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 4: SIDE='R', HOWMNY='S', select eigenvalues 1 and 3 (complex pair)
  ! ============================================================
  VR = 0.0d0; VL = 0.0d0; WORK = 0.0d0
  SELEC = .FALSE.
  SELEC(1) = .TRUE.
  SELEC(3) = .TRUE.
  call DTGEVC('R', 'S', SELEC, N, S, MAXN, P, MAXN, VL, MAXN, VR, MAXN, N, M, WORK, INFO)
  call begin_test('right selected 4x4')
  call print_matrix('VR', VR, MAXN, N, M)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 5: 3x3 all real eigenvalues
  ! ============================================================
  N = 3
  S = 0.0d0; P = 0.0d0; VR = 0.0d0; VL = 0.0d0; WORK = 0.0d0
  S(1,1) = 1.0d0; S(1,2) = 0.3d0; S(1,3) = 0.2d0
  S(2,2) = 2.0d0; S(2,3) = 0.4d0
  S(3,3) = 3.0d0
  P(1,1) = 1.0d0; P(1,2) = 0.1d0; P(1,3) = 0.05d0
  P(2,2) = 1.0d0; P(2,3) = 0.1d0
  P(3,3) = 1.0d0
  call DTGEVC('B', 'A', SELEC, N, S, MAXN, P, MAXN, VL, MAXN, VR, MAXN, N, M, WORK, INFO)
  call begin_test('both all 3x3 real')
  call print_matrix('S', S, MAXN, N, N)
  call print_matrix('P', P, MAXN, N, N)
  call print_matrix('VR', VR, MAXN, N, N)
  call print_matrix('VL', VL, MAXN, N, N)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 6: 2x2 complex pair only
  ! ============================================================
  N = 2
  S = 0.0d0; P = 0.0d0; VR = 0.0d0; VL = 0.0d0; WORK = 0.0d0
  S(1,1) = 1.0d0; S(1,2) = -2.0d0
  S(2,1) = 3.0d0; S(2,2) = 1.0d0
  P(1,1) = 1.0d0; P(1,2) = 0.0d0
  P(2,2) = 1.0d0
  call DTGEVC('B', 'A', SELEC, N, S, MAXN, P, MAXN, VL, MAXN, VR, MAXN, N, M, WORK, INFO)
  call begin_test('both all 2x2 complex')
  call print_matrix('S', S, MAXN, N, N)
  call print_matrix('P', P, MAXN, N, N)
  call print_matrix('VR', VR, MAXN, N, N)
  call print_matrix('VL', VL, MAXN, N, N)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 7: SIDE='L', HOWMNY='S', select only eigenvalue 2
  ! ============================================================
  N = 4
  S = 0.0d0; P = 0.0d0; VR = 0.0d0; VL = 0.0d0; WORK = 0.0d0
  S(1,1) = 1.0d0; S(1,2) = 0.5d0; S(1,3) = 0.2d0; S(1,4) = 0.1d0
  S(2,2) = 2.0d0; S(2,3) = 0.3d0; S(2,4) = 0.15d0
  S(3,3) = 4.0d0; S(3,4) = -2.0d0
  S(4,3) = 1.0d0; S(4,4) = 4.0d0
  P(1,1) = 1.0d0; P(1,2) = 0.1d0; P(1,3) = 0.05d0; P(1,4) = 0.02d0
  P(2,2) = 2.0d0; P(2,3) = 0.15d0; P(2,4) = 0.08d0
  P(3,3) = 1.0d0; P(3,4) = 0.0d0
  P(4,4) = 1.0d0
  SELEC = .FALSE.
  SELEC(2) = .TRUE.
  call DTGEVC('L', 'S', SELEC, N, S, MAXN, P, MAXN, VL, MAXN, VR, MAXN, N, M, WORK, INFO)
  call begin_test('left selected eig2 4x4')
  call print_matrix('VL', VL, MAXN, N, M)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 8: N=1 trivial case
  ! ============================================================
  N = 1
  S = 0.0d0; P = 0.0d0; VR = 0.0d0; VL = 0.0d0; WORK = 0.0d0
  S(1,1) = 5.0d0
  P(1,1) = 2.0d0
  call DTGEVC('B', 'A', SELEC, N, S, MAXN, P, MAXN, VL, MAXN, VR, MAXN, N, M, WORK, INFO)
  call begin_test('both all 1x1')
  call print_matrix('VR', VR, MAXN, N, N)
  call print_matrix('VL', VL, MAXN, N, N)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 9: HOWMNY='B' (backtransform) with right eigenvectors
  ! Start with identity VR, then backtransform
  ! ============================================================
  N = 4
  S = 0.0d0; P = 0.0d0; VR = 0.0d0; VL = 0.0d0; WORK = 0.0d0
  S(1,1) = 1.0d0; S(1,2) = 0.5d0; S(1,3) = 0.2d0; S(1,4) = 0.1d0
  S(2,2) = 2.0d0; S(2,3) = 0.3d0; S(2,4) = 0.15d0
  S(3,3) = 4.0d0; S(3,4) = -2.0d0
  S(4,3) = 1.0d0; S(4,4) = 4.0d0
  P(1,1) = 1.0d0; P(1,2) = 0.1d0; P(1,3) = 0.05d0; P(1,4) = 0.02d0
  P(2,2) = 2.0d0; P(2,3) = 0.15d0; P(2,4) = 0.08d0
  P(3,3) = 1.0d0; P(3,4) = 0.0d0
  P(4,4) = 1.0d0
  ! Set VR to identity for backtransform
  do I = 1, N
    VR(I,I) = 1.0d0
  end do
  call DTGEVC('R', 'B', SELEC, N, S, MAXN, P, MAXN, VL, MAXN, VR, MAXN, N, M, WORK, INFO)
  call begin_test('right backtransform 4x4')
  call print_matrix('VR', VR, MAXN, N, N)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 10: HOWMNY='B' (backtransform) with left eigenvectors
  ! ============================================================
  VR = 0.0d0; VL = 0.0d0; WORK = 0.0d0
  do I = 1, N
    VL(I,I) = 1.0d0
  end do
  call DTGEVC('L', 'B', SELEC, N, S, MAXN, P, MAXN, VL, MAXN, VR, MAXN, N, M, WORK, INFO)
  call begin_test('left backtransform 4x4')
  call print_matrix('VL', VL, MAXN, N, N)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 11: 4x4 with complex pair at top (rows 1-2)
  ! Tests complex forward substitution in left eigenvectors
  ! ============================================================
  N = 4
  S = 0.0d0; P = 0.0d0; VR = 0.0d0; VL = 0.0d0; WORK = 0.0d0
  ! 2x2 complex block at top
  S(1,1) = 2.0d0; S(1,2) = -1.0d0
  S(2,1) = 3.0d0; S(2,2) = 2.0d0
  S(1,3) = 0.3d0; S(1,4) = 0.1d0
  S(2,3) = 0.2d0; S(2,4) = 0.15d0
  S(3,3) = 5.0d0; S(3,4) = 0.4d0
  S(4,4) = 7.0d0
  P(1,1) = 1.0d0; P(1,2) = 0.0d0; P(1,3) = 0.1d0; P(1,4) = 0.05d0
  P(2,2) = 1.0d0; P(2,3) = 0.08d0; P(2,4) = 0.04d0
  P(3,3) = 1.0d0; P(3,4) = 0.1d0
  P(4,4) = 1.0d0
  call DTGEVC('B', 'A', SELEC, N, S, MAXN, P, MAXN, VL, MAXN, VR, MAXN, N, M, WORK, INFO)
  call begin_test('both all 4x4 cpx top')
  call print_matrix('S', S, MAXN, N, N)
  call print_matrix('P', P, MAXN, N, N)
  call print_matrix('VR', VR, MAXN, N, N)
  call print_matrix('VL', VL, MAXN, N, N)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 12: 5x5 with complex pair in middle
  ! ============================================================
  N = 5
  S = 0.0d0; P = 0.0d0; VR = 0.0d0; VL = 0.0d0; WORK = 0.0d0
  S(1,1) = 1.0d0; S(1,2) = 0.5d0; S(1,3) = 0.2d0; S(1,4) = 0.1d0; S(1,5) = 0.05d0
  S(2,2) = 3.0d0; S(2,3) = -1.5d0
  S(3,2) = 2.0d0; S(3,3) = 3.0d0
  S(2,4) = 0.3d0; S(2,5) = 0.15d0
  S(3,4) = 0.2d0; S(3,5) = 0.1d0
  S(4,4) = 6.0d0; S(4,5) = 0.4d0
  S(5,5) = 8.0d0
  P(1,1) = 1.0d0; P(1,2) = 0.1d0; P(1,3) = 0.05d0; P(1,4) = 0.02d0; P(1,5) = 0.01d0
  P(2,2) = 1.0d0; P(2,3) = 0.0d0; P(2,4) = 0.1d0; P(2,5) = 0.05d0
  P(3,3) = 1.0d0; P(3,4) = 0.08d0; P(3,5) = 0.04d0
  P(4,4) = 1.0d0; P(4,5) = 0.1d0
  P(5,5) = 1.0d0
  call DTGEVC('B', 'A', SELEC, N, S, MAXN, P, MAXN, VL, MAXN, VR, MAXN, N, M, WORK, INFO)
  call begin_test('both all 5x5 cpx mid')
  call print_matrix('VR', VR, MAXN, N, N)
  call print_matrix('VL', VL, MAXN, N, N)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 13: Degenerate eigenvector case (near-zero S and P diagonal)
  ! ============================================================
  N = 3
  S = 0.0d0; P = 0.0d0; VR = 0.0d0; VL = 0.0d0; WORK = 0.0d0
  S(1,1) = 1.0d-320; S(1,2) = 0.5d0; S(1,3) = 0.2d0
  S(2,2) = 2.0d0; S(2,3) = 0.3d0
  S(3,3) = 3.0d0
  P(1,1) = 1.0d-320; P(1,2) = 0.1d0; P(1,3) = 0.05d0
  P(2,2) = 1.0d0; P(2,3) = 0.1d0
  P(3,3) = 1.0d0
  call DTGEVC('B', 'A', SELEC, N, S, MAXN, P, MAXN, VL, MAXN, VR, MAXN, N, M, WORK, INFO)
  call begin_test('both all degenerate')
  call print_matrix('VR', VR, MAXN, N, N)
  call print_matrix('VL', VL, MAXN, N, N)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 14: SIDE='L', HOWMNY='S', select complex pair at top
  ! ============================================================
  N = 4
  S = 0.0d0; P = 0.0d0; VR = 0.0d0; VL = 0.0d0; WORK = 0.0d0
  S(1,1) = 2.0d0; S(1,2) = -1.0d0
  S(2,1) = 3.0d0; S(2,2) = 2.0d0
  S(1,3) = 0.3d0; S(1,4) = 0.1d0
  S(2,3) = 0.2d0; S(2,4) = 0.15d0
  S(3,3) = 5.0d0; S(3,4) = 0.4d0
  S(4,4) = 7.0d0
  P(1,1) = 1.0d0; P(1,2) = 0.0d0; P(1,3) = 0.1d0; P(1,4) = 0.05d0
  P(2,2) = 1.0d0; P(2,3) = 0.08d0; P(2,4) = 0.04d0
  P(3,3) = 1.0d0; P(3,4) = 0.1d0
  P(4,4) = 1.0d0
  SELEC = .FALSE.
  SELEC(1) = .TRUE.
  call DTGEVC('L', 'S', SELEC, N, S, MAXN, P, MAXN, VL, MAXN, VR, MAXN, N, M, WORK, INFO)
  call begin_test('left sel cpx top 4x4')
  call print_matrix('VL', VL, MAXN, N, M)
  call print_int('M', M)
  call print_int('info', INFO)
  call end_test()

end program test_dtgevc
