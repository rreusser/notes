program test_dgeevx
  use test_utils
  implicit none

  integer, parameter :: NMAX = 8
  double precision :: A(NMAX,NMAX), ACOPY(NMAX,NMAX)
  double precision :: WR(NMAX), WI(NMAX)
  double precision :: VL(NMAX,NMAX), VR(NMAX,NMAX)
  double precision :: SCALE(NMAX), RCONDE(NMAX), RCONDV(NMAX)
  double precision :: WORK(8*NMAX*NMAX + 32*NMAX)
  double precision :: ABNRM
  integer :: IWORK(4*NMAX)
  integer :: N, INFO, LWORK, ILO, IHI

  LWORK = 8*NMAX*NMAX + 32*NMAX

  ! ================================================
  ! Test 1: 4x4 diagonal matrix with BALANC='B'
  ! ================================================
  N = 4
  A = 0.0D0
  A(1,1) = 1.0D0
  A(2,2) = 2.0D0
  A(3,3) = 3.0D0
  A(4,4) = 4.0D0
  ACOPY(1:N,1:N) = A(1:N,1:N)

  call DGEEVX('B', 'V', 'V', 'N', N, A, NMAX, WR, WI, VL, NMAX, VR, NMAX, &
              ILO, IHI, SCALE, ABNRM, RCONDE, RCONDV, WORK, LWORK, IWORK, INFO)
  call begin_test('diagonal_4x4_both_balance')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_int('ilo', ILO)
  call print_int('ihi', IHI)
  call print_scalar('abnrm', ABNRM)
  call print_array('wr', WR, N)
  call print_array('wi', WI, N)
  call print_array('scale', SCALE, N)
  call print_matrix('vl', VL, N, N, NMAX)
  call print_matrix('vr', VR, N, N, NMAX)
  call print_matrix('a_input', ACOPY, N, N, NMAX)
  call end_test()

  ! ================================================
  ! Test 2: 4x4 with complex conjugate eigenvalue pairs, BALANC='N'
  ! ================================================
  N = 4
  A = 0.0D0
  A(1,1) = 0.0D0;  A(1,2) = 1.0D0;  A(1,3) = 0.0D0;  A(1,4) = 0.0D0
  A(2,1) = -1.0D0; A(2,2) = 0.0D0;  A(2,3) = 0.0D0;  A(2,4) = 0.0D0
  A(3,1) = 0.0D0;  A(3,2) = 0.0D0;  A(3,3) = 0.0D0;  A(3,4) = 2.0D0
  A(4,1) = 0.0D0;  A(4,2) = 0.0D0;  A(4,3) = -2.0D0; A(4,4) = 0.0D0
  ACOPY(1:N,1:N) = A(1:N,1:N)

  call DGEEVX('N', 'V', 'V', 'N', N, A, NMAX, WR, WI, VL, NMAX, VR, NMAX, &
              ILO, IHI, SCALE, ABNRM, RCONDE, RCONDV, WORK, LWORK, IWORK, INFO)
  call begin_test('complex_pairs_4x4_none')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_int('ilo', ILO)
  call print_int('ihi', IHI)
  call print_scalar('abnrm', ABNRM)
  call print_array('wr', WR, N)
  call print_array('wi', WI, N)
  call print_array('scale', SCALE, N)
  call print_matrix('vl', VL, N, N, NMAX)
  call print_matrix('vr', VR, N, N, NMAX)
  call print_matrix('a_input', ACOPY, N, N, NMAX)
  call end_test()

  ! ================================================
  ! Test 3: Eigenvalues only (JOBVL='N', JOBVR='N'), BALANC='P'
  ! ================================================
  N = 4
  A = 0.0D0
  A(1,1) = 4.0D0; A(1,2) = -5.0D0
  A(2,1) = 2.0D0; A(2,2) = -3.0D0
  A(3,3) = 1.0D0; A(3,4) = 1.0D0
  A(4,3) = -1.0D0; A(4,4) = 1.0D0
  ACOPY(1:N,1:N) = A(1:N,1:N)

  call DGEEVX('P', 'N', 'N', 'N', N, A, NMAX, WR, WI, VL, NMAX, VR, NMAX, &
              ILO, IHI, SCALE, ABNRM, RCONDE, RCONDV, WORK, LWORK, IWORK, INFO)
  call begin_test('eigenvalues_only_4x4_permute')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_int('ilo', ILO)
  call print_int('ihi', IHI)
  call print_scalar('abnrm', ABNRM)
  call print_array('wr', WR, N)
  call print_array('wi', WI, N)
  call print_array('scale', SCALE, N)
  call print_matrix('a_input', ACOPY, N, N, NMAX)
  call end_test()

  ! ================================================
  ! Test 4: Right eigenvectors only (JOBVR='V'), BALANC='S'
  ! ================================================
  N = 3
  A = 0.0D0
  A(1,1) = 1.0D0; A(1,2) = 2.0D0; A(1,3) = 3.0D0
  A(2,1) = 0.0D0; A(2,2) = 4.0D0; A(2,3) = 5.0D0
  A(3,1) = 0.0D0; A(3,2) = 0.0D0; A(3,3) = 6.0D0
  ACOPY(1:N,1:N) = A(1:N,1:N)

  call DGEEVX('S', 'N', 'V', 'N', N, A, NMAX, WR, WI, VL, NMAX, VR, NMAX, &
              ILO, IHI, SCALE, ABNRM, RCONDE, RCONDV, WORK, LWORK, IWORK, INFO)
  call begin_test('right_only_upper_tri_3x3_scale')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_int('ilo', ILO)
  call print_int('ihi', IHI)
  call print_scalar('abnrm', ABNRM)
  call print_array('wr', WR, N)
  call print_array('wi', WI, N)
  call print_array('scale', SCALE, N)
  call print_matrix('vr', VR, N, N, NMAX)
  call print_matrix('a_input', ACOPY, N, N, NMAX)
  call end_test()

  ! ================================================
  ! Test 5: Left eigenvectors only (JOBVL='V'), BALANC='B'
  ! ================================================
  N = 3
  A = 0.0D0
  A(1,1) = 1.0D0; A(1,2) = 2.0D0; A(1,3) = 3.0D0
  A(2,1) = 0.0D0; A(2,2) = 4.0D0; A(2,3) = 5.0D0
  A(3,1) = 0.0D0; A(3,2) = 0.0D0; A(3,3) = 6.0D0
  ACOPY(1:N,1:N) = A(1:N,1:N)

  call DGEEVX('B', 'V', 'N', 'N', N, A, NMAX, WR, WI, VL, NMAX, VR, NMAX, &
              ILO, IHI, SCALE, ABNRM, RCONDE, RCONDV, WORK, LWORK, IWORK, INFO)
  call begin_test('left_only_upper_tri_3x3_both')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_int('ilo', ILO)
  call print_int('ihi', IHI)
  call print_scalar('abnrm', ABNRM)
  call print_array('wr', WR, N)
  call print_array('wi', WI, N)
  call print_array('scale', SCALE, N)
  call print_matrix('vl', VL, N, N, NMAX)
  call print_matrix('a_input', ACOPY, N, N, NMAX)
  call end_test()

  ! ================================================
  ! Test 6: N=1 edge case
  ! ================================================
  N = 1
  A(1,1) = 7.5D0

  call DGEEVX('B', 'V', 'V', 'N', N, A, NMAX, WR, WI, VL, NMAX, VR, NMAX, &
              ILO, IHI, SCALE, ABNRM, RCONDE, RCONDV, WORK, LWORK, IWORK, INFO)
  call begin_test('n1_edge')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_int('ilo', ILO)
  call print_int('ihi', IHI)
  call print_scalar('abnrm', ABNRM)
  call print_array('wr', WR, N)
  call print_array('wi', WI, N)
  call print_array('scale', SCALE, N)
  call print_matrix('vl', VL, N, N, NMAX)
  call print_matrix('vr', VR, N, N, NMAX)
  call end_test()

  ! ================================================
  ! Test 7: Mixed real and complex eigenvalues (general 4x4), BALANC='B'
  ! ================================================
  N = 4
  A = 0.0D0
  A(1,1) = 1.0D0; A(1,2) = 0.0D0; A(1,3) = 0.0D0; A(1,4) = 0.0D0
  A(2,1) = 0.0D0; A(2,2) = 2.0D0; A(2,3) = 0.0D0; A(2,4) = 0.0D0
  A(3,1) = 0.0D0; A(3,2) = 0.0D0; A(3,3) = 0.0D0; A(3,4) = -1.0D0
  A(4,1) = 0.0D0; A(4,2) = 0.0D0; A(4,3) = 1.0D0; A(4,4) = 0.0D0
  ACOPY(1:N,1:N) = A(1:N,1:N)

  call DGEEVX('B', 'V', 'V', 'N', N, A, NMAX, WR, WI, VL, NMAX, VR, NMAX, &
              ILO, IHI, SCALE, ABNRM, RCONDE, RCONDV, WORK, LWORK, IWORK, INFO)
  call begin_test('mixed_real_complex_4x4_both')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_int('ilo', ILO)
  call print_int('ihi', IHI)
  call print_scalar('abnrm', ABNRM)
  call print_array('wr', WR, N)
  call print_array('wi', WI, N)
  call print_array('scale', SCALE, N)
  call print_matrix('vl', VL, N, N, NMAX)
  call print_matrix('vr', VR, N, N, NMAX)
  call print_matrix('a_input', ACOPY, N, N, NMAX)
  call end_test()

  ! ================================================
  ! Test 8: General 4x4 with larger values to trigger balancing
  ! ================================================
  N = 4
  A = 0.0D0
  A(1,1) = 1.0D0; A(1,2) = 1.0D4;  A(1,3) = 1.0D8;  A(1,4) = 1.0D12
  A(2,1) = 1.0D-4; A(2,2) = 1.0D0; A(2,3) = 1.0D4;  A(2,4) = 1.0D8
  A(3,1) = 1.0D-8; A(3,2) = 1.0D-4; A(3,3) = 1.0D0; A(3,4) = 1.0D4
  A(4,1) = 1.0D-12; A(4,2) = 1.0D-8; A(4,3) = 1.0D-4; A(4,4) = 1.0D0
  ACOPY(1:N,1:N) = A(1:N,1:N)

  call DGEEVX('B', 'V', 'V', 'N', N, A, NMAX, WR, WI, VL, NMAX, VR, NMAX, &
              ILO, IHI, SCALE, ABNRM, RCONDE, RCONDV, WORK, LWORK, IWORK, INFO)
  call begin_test('graded_4x4_both')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_int('ilo', ILO)
  call print_int('ihi', IHI)
  call print_scalar('abnrm', ABNRM)
  call print_array('wr', WR, N)
  call print_array('wi', WI, N)
  call print_array('scale', SCALE, N)
  call print_matrix('vl', VL, N, N, NMAX)
  call print_matrix('vr', VR, N, N, NMAX)
  call print_matrix('a_input', ACOPY, N, N, NMAX)
  call end_test()

  ! ================================================
  ! Test 9: SENSE='E' (eigenvalues only) on a diagonal 3x3 matrix, BALANC='N'
  ! ================================================
  N = 3
  A = 0.0D0
  A(1,1) = 1.0D0
  A(2,2) = 2.0D0
  A(3,3) = 4.0D0
  ACOPY(1:N,1:N) = A(1:N,1:N)

  call DGEEVX('N', 'V', 'V', 'E', N, A, NMAX, WR, WI, VL, NMAX, VR, NMAX, &
              ILO, IHI, SCALE, ABNRM, RCONDE, RCONDV, WORK, LWORK, IWORK, INFO)
  call begin_test('diag3_sense_E')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_scalar('abnrm', ABNRM)
  call print_array('wr', WR, N)
  call print_array('wi', WI, N)
  call print_array('rconde', RCONDE, N)
  call print_matrix('a_input', ACOPY, N, N, NMAX)
  call end_test()

  ! ================================================
  ! Test 10: SENSE='V' (right eigenvector condition) on the same matrix
  ! ================================================
  N = 3
  A = 0.0D0
  A(1,1) = 1.0D0
  A(2,2) = 2.0D0
  A(3,3) = 4.0D0
  ACOPY(1:N,1:N) = A(1:N,1:N)

  call DGEEVX('N', 'V', 'V', 'V', N, A, NMAX, WR, WI, VL, NMAX, VR, NMAX, &
              ILO, IHI, SCALE, ABNRM, RCONDE, RCONDV, WORK, LWORK, IWORK, INFO)
  call begin_test('diag3_sense_V')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_scalar('abnrm', ABNRM)
  call print_array('wr', WR, N)
  call print_array('wi', WI, N)
  call print_array('rcondv', RCONDV, N)
  call print_matrix('a_input', ACOPY, N, N, NMAX)
  call end_test()

  ! ================================================
  ! Test 11: SENSE='B' (both) on a non-diagonal upper-triangular 3x3 matrix
  ! ================================================
  N = 3
  A = 0.0D0
  A(1,1) = 1.0D0; A(1,2) = 0.5D0; A(1,3) = 0.25D0
  A(2,2) = 2.0D0; A(2,3) = 0.5D0
  A(3,3) = 4.0D0
  ACOPY(1:N,1:N) = A(1:N,1:N)

  call DGEEVX('N', 'V', 'V', 'B', N, A, NMAX, WR, WI, VL, NMAX, VR, NMAX, &
              ILO, IHI, SCALE, ABNRM, RCONDE, RCONDV, WORK, LWORK, IWORK, INFO)
  call begin_test('upper3_sense_B')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_scalar('abnrm', ABNRM)
  call print_array('wr', WR, N)
  call print_array('wi', WI, N)
  call print_array('rconde', RCONDE, N)
  call print_array('rcondv', RCONDV, N)
  call print_matrix('a_input', ACOPY, N, N, NMAX)
  call end_test()

end program
