program test_dgebal
  use test_utils
  implicit none

  double precision :: A(6,6), SCALE(6)
  integer :: ILO, IHI, INFO, N, LDA, I, J

  ! ============================================================
  ! Test 1: N=0 quick return
  ! ============================================================
  N = 0
  LDA = 1
  call DGEBAL('B', N, A, LDA, ILO, IHI, SCALE, INFO)
  call begin_test('n0')
  call print_int('info', INFO)
  call print_int('ilo', ILO)
  call print_int('ihi', IHI)
  call end_test()

  ! ============================================================
  ! Test 2: N=1
  ! ============================================================
  N = 1
  LDA = 1
  A(1,1) = 5.0d0
  call DGEBAL('B', N, A, LDA, ILO, IHI, SCALE, INFO)
  call begin_test('n1')
  call print_int('info', INFO)
  call print_int('ilo', ILO)
  call print_int('ihi', IHI)
  call print_array('scale', SCALE, N)
  call print_array('A', A(1:1,1), N)
  call end_test()

  ! ============================================================
  ! Test 3: JOB='N' (no balancing)
  ! ============================================================
  N = 3
  LDA = 6
  A(1,1) = 1.0d0; A(1,2) = 2.0d0; A(1,3) = 3.0d0
  A(2,1) = 4.0d0; A(2,2) = 5.0d0; A(2,3) = 6.0d0
  A(3,1) = 7.0d0; A(3,2) = 8.0d0; A(3,3) = 9.0d0
  call DGEBAL('N', N, A, LDA, ILO, IHI, SCALE, INFO)
  call begin_test('job_n')
  call print_int('info', INFO)
  call print_int('ilo', ILO)
  call print_int('ihi', IHI)
  call print_array('scale', SCALE, N)
  call end_test()

  ! ============================================================
  ! Test 4: JOB='P' (permute only) - matrix with isolated eigenvalues
  ! Row 3 has nonzero only in column 3 (diagonal) -> isolated
  ! ============================================================
  N = 4
  LDA = 6
  A = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0; A(1,3) = 3.0d0; A(1,4) = 0.0d0
  A(2,1) = 0.0d0; A(2,2) = 5.0d0; A(2,3) = 6.0d0; A(2,4) = 0.0d0
  A(3,1) = 0.0d0; A(3,2) = 8.0d0; A(3,3) = 9.0d0; A(3,4) = 0.0d0
  A(4,1) = 0.0d0; A(4,2) = 0.0d0; A(4,3) = 0.0d0; A(4,4) = 4.0d0
  call DGEBAL('P', N, A, LDA, ILO, IHI, SCALE, INFO)
  call begin_test('job_p')
  call print_int('info', INFO)
  call print_int('ilo', ILO)
  call print_int('ihi', IHI)
  call print_array('scale', SCALE, N)
  ! Print the permuted A in column-major
  call print_array('A', A(1:6,1), N)
  call print_array('A2', A(1:6,2), N)
  call print_array('A3', A(1:6,3), N)
  call print_array('A4', A(1:6,4), N)
  call end_test()

  ! ============================================================
  ! Test 5: JOB='S' (scale only) - matrix needing scaling
  ! ============================================================
  N = 3
  LDA = 6
  A = 0.0d0
  A(1,1) = 1.0d0;    A(1,2) = 0.0d0;    A(1,3) = 0.0d0
  A(2,1) = 1000.0d0; A(2,2) = 1.0d0;    A(2,3) = 0.0d0
  A(3,1) = 0.0d0;    A(3,2) = 1000.0d0; A(3,3) = 1.0d0
  call DGEBAL('S', N, A, LDA, ILO, IHI, SCALE, INFO)
  call begin_test('job_s')
  call print_int('info', INFO)
  call print_int('ilo', ILO)
  call print_int('ihi', IHI)
  call print_array('scale', SCALE, N)
  call print_array('A1', A(1:6,1), N)
  call print_array('A2', A(1:6,2), N)
  call print_array('A3', A(1:6,3), N)
  call end_test()

  ! ============================================================
  ! Test 6: JOB='B' (both permute and scale) - 4x4 general matrix
  ! ============================================================
  N = 4
  LDA = 6
  A = 0.0d0
  A(1,1) = 1.0d0;    A(1,2) = 0.0d0;    A(1,3) = 0.0d0;    A(1,4) = 0.0d0
  A(2,1) = 100.0d0;  A(2,2) = 2.0d0;    A(2,3) = 300.0d0;  A(2,4) = 0.0d0
  A(3,1) = 0.0d0;    A(3,2) = 0.01d0;   A(3,3) = 3.0d0;    A(3,4) = 0.0d0
  A(4,1) = 0.0d0;    A(4,2) = 0.0d0;    A(4,3) = 0.0d0;    A(4,4) = 4.0d0
  call DGEBAL('B', N, A, LDA, ILO, IHI, SCALE, INFO)
  call begin_test('job_b')
  call print_int('info', INFO)
  call print_int('ilo', ILO)
  call print_int('ihi', IHI)
  call print_array('scale', SCALE, N)
  call print_array('A1', A(1:6,1), N)
  call print_array('A2', A(1:6,2), N)
  call print_array('A3', A(1:6,3), N)
  call print_array('A4', A(1:6,4), N)
  call end_test()

  ! ============================================================
  ! Test 7: Already balanced (diagonal) matrix with JOB='B'
  ! ============================================================
  N = 3
  LDA = 6
  A = 0.0d0
  A(1,1) = 2.0d0
  A(2,2) = 3.0d0
  A(3,3) = 5.0d0
  call DGEBAL('B', N, A, LDA, ILO, IHI, SCALE, INFO)
  call begin_test('diagonal')
  call print_int('info', INFO)
  call print_int('ilo', ILO)
  call print_int('ihi', IHI)
  call print_array('scale', SCALE, N)
  call end_test()

  ! ============================================================
  ! Test 8: 5x5 matrix with permutations on both ends
  ! Row 1 has nonzero only in col 1 (isolated at top)
  ! Row 5 has nonzero only in col 5 (isolated at bottom)
  ! Middle needs scaling
  ! ============================================================
  N = 5
  LDA = 6
  A = 0.0d0
  ! Row 1: only diagonal
  A(1,1) = 1.0d0
  ! Row 2: dense in middle
  A(2,2) = 2.0d0;    A(2,3) = 1000.0d0; A(2,4) = 0.0d0
  ! Row 3:
  A(3,2) = 0.001d0;  A(3,3) = 3.0d0;    A(3,4) = 500.0d0
  ! Row 4:
  A(4,2) = 0.0d0;    A(4,3) = 0.002d0;  A(4,4) = 4.0d0
  ! Row 5: only diagonal
  A(5,5) = 5.0d0
  call DGEBAL('B', N, A, LDA, ILO, IHI, SCALE, INFO)
  call begin_test('perm_and_scale')
  call print_int('info', INFO)
  call print_int('ilo', ILO)
  call print_int('ihi', IHI)
  call print_array('scale', SCALE, N)
  call print_array('A1', A(1:6,1), N)
  call print_array('A2', A(1:6,2), N)
  call print_array('A3', A(1:6,3), N)
  call print_array('A4', A(1:6,4), N)
  call print_array('A5', A(1:6,5), N)
  call end_test()

  ! ============================================================
  ! Test 9: JOB='P' with a matrix that has column isolation
  ! Column 1 has nonzero only in row 1 -> isolated at left
  ! ============================================================
  N = 4
  LDA = 6
  A = 0.0d0
  A(1,1) = 7.0d0; A(1,2) = 1.0d0; A(1,3) = 2.0d0; A(1,4) = 3.0d0
  A(2,1) = 0.0d0; A(2,2) = 4.0d0; A(2,3) = 5.0d0; A(2,4) = 0.0d0
  A(3,1) = 0.0d0; A(3,2) = 6.0d0; A(3,3) = 8.0d0; A(3,4) = 0.0d0
  A(4,1) = 0.0d0; A(4,2) = 0.0d0; A(4,3) = 0.0d0; A(4,4) = 9.0d0
  call DGEBAL('P', N, A, LDA, ILO, IHI, SCALE, INFO)
  call begin_test('col_isolation')
  call print_int('info', INFO)
  call print_int('ilo', ILO)
  call print_int('ihi', IHI)
  call print_array('scale', SCALE, N)
  call print_array('A1', A(1:6,1), N)
  call print_array('A2', A(1:6,2), N)
  call print_array('A3', A(1:6,3), N)
  call print_array('A4', A(1:6,4), N)
  call end_test()

end program test_dgebal
