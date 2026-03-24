program test_dgeev
  use test_utils
  implicit none

  integer, parameter :: NMAX = 8
  double precision :: A(NMAX,NMAX), ACOPY(NMAX,NMAX)
  double precision :: WR(NMAX), WI(NMAX)
  double precision :: VL(NMAX,NMAX), VR(NMAX,NMAX)
  double precision :: WORK(8*NMAX)
  integer :: N, INFO, LWORK, i, j

  LWORK = 8*NMAX

  ! ================================================
  ! Test 1: 4x4 diagonal matrix (all real eigenvalues, trivial)
  ! ================================================
  N = 4
  A = 0.0D0
  A(1,1) = 1.0D0
  A(2,2) = 2.0D0
  A(3,3) = 3.0D0
  A(4,4) = 4.0D0
  ACOPY(1:N,1:N) = A(1:N,1:N)

  call DGEEV('V', 'V', N, A, NMAX, WR, WI, VL, NMAX, VR, NMAX, &
             WORK, LWORK, INFO)
  call begin_test('diagonal_4x4_both')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('wr', WR, N)
  call print_array('wi', WI, N)
  call print_matrix('vl', VL, N, N, NMAX)
  call print_matrix('vr', VR, N, N, NMAX)
  call print_matrix('a_input', ACOPY, N, N, NMAX)
  call end_test()

  ! ================================================
  ! Test 2: 4x4 with complex conjugate eigenvalue pairs
  ! ================================================
  N = 4
  A = 0.0D0
  A(1,1) = 0.0D0; A(1,2) = 1.0D0; A(1,3) = 0.0D0; A(1,4) = 0.0D0
  A(2,1) = -1.0D0; A(2,2) = 0.0D0; A(2,3) = 0.0D0; A(2,4) = 0.0D0
  A(3,1) = 0.0D0; A(3,2) = 0.0D0; A(3,3) = 0.0D0; A(3,4) = 2.0D0
  A(4,1) = 0.0D0; A(4,2) = 0.0D0; A(4,3) = -2.0D0; A(4,4) = 0.0D0
  ACOPY(1:N,1:N) = A(1:N,1:N)

  call DGEEV('V', 'V', N, A, NMAX, WR, WI, VL, NMAX, VR, NMAX, &
             WORK, LWORK, INFO)
  call begin_test('complex_pairs_4x4_both')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('wr', WR, N)
  call print_array('wi', WI, N)
  call print_matrix('vl', VL, N, N, NMAX)
  call print_matrix('vr', VR, N, N, NMAX)
  call print_matrix('a_input', ACOPY, N, N, NMAX)
  call end_test()

  ! ================================================
  ! Test 3: Eigenvalues only (JOBVL='N', JOBVR='N')
  ! ================================================
  N = 4
  A = 0.0D0
  A(1,1) = 4.0D0; A(1,2) = -5.0D0
  A(2,1) = 2.0D0; A(2,2) = -3.0D0
  A(3,3) = 1.0D0; A(3,4) = 1.0D0
  A(4,3) = -1.0D0; A(4,4) = 1.0D0
  ACOPY(1:N,1:N) = A(1:N,1:N)

  call DGEEV('N', 'N', N, A, NMAX, WR, WI, VL, NMAX, VR, NMAX, &
             WORK, LWORK, INFO)
  call begin_test('eigenvalues_only_4x4')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('wr', WR, N)
  call print_array('wi', WI, N)
  call print_matrix('a_input', ACOPY, N, N, NMAX)
  call end_test()

  ! ================================================
  ! Test 4: Right eigenvectors only (JOBVR='V')
  ! ================================================
  N = 3
  A = 0.0D0
  A(1,1) = 1.0D0; A(1,2) = 2.0D0; A(1,3) = 3.0D0
  A(2,1) = 0.0D0; A(2,2) = 4.0D0; A(2,3) = 5.0D0
  A(3,1) = 0.0D0; A(3,2) = 0.0D0; A(3,3) = 6.0D0
  ACOPY(1:N,1:N) = A(1:N,1:N)

  call DGEEV('N', 'V', N, A, NMAX, WR, WI, VL, NMAX, VR, NMAX, &
             WORK, LWORK, INFO)
  call begin_test('right_only_upper_tri_3x3')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('wr', WR, N)
  call print_array('wi', WI, N)
  call print_matrix('vr', VR, N, N, NMAX)
  call print_matrix('a_input', ACOPY, N, N, NMAX)
  call end_test()

  ! ================================================
  ! Test 5: Left eigenvectors only (JOBVL='V')
  ! ================================================
  N = 3
  A = 0.0D0
  A(1,1) = 1.0D0; A(1,2) = 2.0D0; A(1,3) = 3.0D0
  A(2,1) = 0.0D0; A(2,2) = 4.0D0; A(2,3) = 5.0D0
  A(3,1) = 0.0D0; A(3,2) = 0.0D0; A(3,3) = 6.0D0
  ACOPY(1:N,1:N) = A(1:N,1:N)

  call DGEEV('V', 'N', N, A, NMAX, WR, WI, VL, NMAX, VR, NMAX, &
             WORK, LWORK, INFO)
  call begin_test('left_only_upper_tri_3x3')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('wr', WR, N)
  call print_array('wi', WI, N)
  call print_matrix('vl', VL, N, N, NMAX)
  call print_matrix('a_input', ACOPY, N, N, NMAX)
  call end_test()

  ! ================================================
  ! Test 6: N=1 edge case
  ! ================================================
  N = 1
  A(1,1) = 7.5D0

  call DGEEV('V', 'V', N, A, NMAX, WR, WI, VL, NMAX, VR, NMAX, &
             WORK, LWORK, INFO)
  call begin_test('n1_edge')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('wr', WR, N)
  call print_array('wi', WI, N)
  call print_matrix('vl', VL, N, N, NMAX)
  call print_matrix('vr', VR, N, N, NMAX)
  call end_test()

  ! ================================================
  ! Test 7: N=0 edge case
  ! ================================================
  N = 0
  call DGEEV('V', 'V', N, A, NMAX, WR, WI, VL, NMAX, VR, NMAX, &
             WORK, LWORK, INFO)
  call begin_test('n0_edge')
  call print_int('info', INFO)
  call print_int('n', N)
  call end_test()

  ! ================================================
  ! Test 8: Mixed real and complex eigenvalues (general 4x4)
  ! ================================================
  N = 4
  A = 0.0D0
  A(1,1) = 1.0D0; A(1,2) = 0.0D0; A(1,3) = 0.0D0; A(1,4) = 0.0D0
  A(2,1) = 0.0D0; A(2,2) = 2.0D0; A(2,3) = 0.0D0; A(2,4) = 0.0D0
  A(3,1) = 0.0D0; A(3,2) = 0.0D0; A(3,3) = 0.0D0; A(3,4) = -1.0D0
  A(4,1) = 0.0D0; A(4,2) = 0.0D0; A(4,3) = 1.0D0; A(4,4) = 0.0D0
  ACOPY(1:N,1:N) = A(1:N,1:N)

  call DGEEV('V', 'V', N, A, NMAX, WR, WI, VL, NMAX, VR, NMAX, &
             WORK, LWORK, INFO)
  call begin_test('mixed_real_complex_4x4')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('wr', WR, N)
  call print_array('wi', WI, N)
  call print_matrix('vl', VL, N, N, NMAX)
  call print_matrix('vr', VR, N, N, NMAX)
  call print_matrix('a_input', ACOPY, N, N, NMAX)
  call end_test()

  ! ================================================
  ! Test 9: General non-symmetric matrix with scaling needed
  ! ================================================
  N = 3
  A = 0.0D0
  A(1,1) = 1.0D+15; A(1,2) = 2.0D+15; A(1,3) = 3.0D+15
  A(2,1) = 0.0D0;   A(2,2) = 4.0D+15; A(2,3) = 5.0D+15
  A(3,1) = 0.0D0;   A(3,2) = 0.0D0;   A(3,3) = 6.0D+15
  ACOPY(1:N,1:N) = A(1:N,1:N)

  call DGEEV('V', 'V', N, A, NMAX, WR, WI, VL, NMAX, VR, NMAX, &
             WORK, LWORK, INFO)
  call begin_test('scaled_upper_tri_3x3')
  call print_int('info', INFO)
  call print_int('n', N)
  call print_array('wr', WR, N)
  call print_array('wi', WI, N)
  call print_matrix('vr', VR, N, N, NMAX)
  call print_matrix('a_input', ACOPY, N, N, NMAX)
  call end_test()

end program
