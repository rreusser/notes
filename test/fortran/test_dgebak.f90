program test_dgebak
  use test_utils
  implicit none

  double precision :: A(5,5), V(5,5), SCALE(5)
  integer :: ILO, IHI, INFO, N, M, LDV, i, j
  double precision :: V_flat(25)

  ! ===========================================================
  ! Test 1: JOB='B', SIDE='R' — full round-trip with dgebal
  ! Balance a 4x4 matrix, then back-transform identity-like eigenvectors
  ! ===========================================================
  N = 4
  M = 4
  LDV = 5

  ! A matrix (column-major) — designed so dgebal does both permutation and scaling
  A = 0.0d0
  A(1,1) = 1.0d0;  A(1,2) = 0.0d0;  A(1,3) = 0.0d0;  A(1,4) = 1000.0d0
  A(2,1) = 0.0d0;  A(2,2) = 2.0d0;  A(2,3) = 0.001d0; A(2,4) = 0.0d0
  A(3,1) = 0.0d0;  A(3,2) = 1000.0d0; A(3,3) = 3.0d0; A(3,4) = 0.0d0
  A(4,1) = 0.001d0; A(4,2) = 0.0d0;  A(4,3) = 0.0d0;  A(4,4) = 4.0d0

  call DGEBAL('B', N, A, 5, ILO, IHI, SCALE, INFO)

  ! Set V to identity (representing eigenvectors of balanced matrix)
  V = 0.0d0
  do i = 1, N
    V(i,i) = 1.0d0
  end do

  call DGEBAK('B', 'R', N, ILO, IHI, SCALE, M, V, LDV, INFO)

  call begin_test('job_B_side_R')
  call print_int('info', INFO)
  call print_int('ilo', ILO)
  call print_int('ihi', IHI)
  call print_array('scale', SCALE, N)
  call print_matrix('V', V, LDV, N, M)
  call end_test()

  ! ===========================================================
  ! Test 2: JOB='B', SIDE='L' — left eigenvectors
  ! ===========================================================
  V = 0.0d0
  do i = 1, N
    V(i,i) = 1.0d0
  end do

  call DGEBAK('B', 'L', N, ILO, IHI, SCALE, M, V, LDV, INFO)

  call begin_test('job_B_side_L')
  call print_int('info', INFO)
  call print_matrix('V', V, LDV, N, M)
  call end_test()

  ! ===========================================================
  ! Test 3: JOB='S' (scaling only), SIDE='R'
  ! ===========================================================
  V = 0.0d0
  do i = 1, N
    V(i,i) = 1.0d0
  end do

  call DGEBAK('S', 'R', N, ILO, IHI, SCALE, M, V, LDV, INFO)

  call begin_test('job_S_side_R')
  call print_int('info', INFO)
  call print_matrix('V', V, LDV, N, M)
  call end_test()

  ! ===========================================================
  ! Test 4: JOB='P' (permutation only), SIDE='R'
  ! ===========================================================
  V = 0.0d0
  do i = 1, N
    V(i,i) = 1.0d0
  end do

  call DGEBAK('P', 'R', N, ILO, IHI, SCALE, M, V, LDV, INFO)

  call begin_test('job_P_side_R')
  call print_int('info', INFO)
  call print_matrix('V', V, LDV, N, M)
  call end_test()

  ! ===========================================================
  ! Test 5: JOB='N' (no-op)
  ! ===========================================================
  V = 0.0d0
  do i = 1, N
    V(i,i) = 1.0d0
  end do

  call DGEBAK('N', 'R', N, ILO, IHI, SCALE, M, V, LDV, INFO)

  call begin_test('job_N')
  call print_int('info', INFO)
  call print_matrix('V', V, LDV, N, M)
  call end_test()

  ! ===========================================================
  ! Test 6: N=0 (quick return)
  ! ===========================================================
  call DGEBAK('B', 'R', 0, 1, 0, SCALE, 0, V, LDV, INFO)

  call begin_test('n_zero')
  call print_int('info', INFO)
  call end_test()

  ! ===========================================================
  ! Test 7: ILO=IHI (only permutation, no scaling loop)
  ! Use a 3x3 matrix with scale values that encode permutations
  ! ===========================================================
  N = 3
  M = 2

  ! Set up SCALE for permutation: rows permuted, ILO=IHI=2
  SCALE(1) = 3.0d0  ! row 1 swaps with row 3
  SCALE(2) = 1.0d0  ! scaling factor (not used when ILO=IHI)
  SCALE(3) = 1.0d0  ! row 3 identity (already swapped)

  V = 0.0d0
  V(1,1) = 1.0d0; V(1,2) = 2.0d0
  V(2,1) = 3.0d0; V(2,2) = 4.0d0
  V(3,1) = 5.0d0; V(3,2) = 6.0d0

  call DGEBAK('B', 'R', N, 2, 2, SCALE, M, V, LDV, INFO)

  call begin_test('ilo_eq_ihi')
  call print_int('info', INFO)
  call print_matrix('V', V, LDV, N, M)
  call end_test()

  ! ===========================================================
  ! Test 8: JOB='S', SIDE='L' (scaling only, left eigenvectors)
  ! ===========================================================
  N = 4
  M = 4

  ! Re-run dgebal for fresh SCALE values
  A(1,1) = 1.0d0;  A(1,2) = 0.0d0;  A(1,3) = 0.0d0;  A(1,4) = 1000.0d0
  A(2,1) = 0.0d0;  A(2,2) = 2.0d0;  A(2,3) = 0.001d0; A(2,4) = 0.0d0
  A(3,1) = 0.0d0;  A(3,2) = 1000.0d0; A(3,3) = 3.0d0; A(3,4) = 0.0d0
  A(4,1) = 0.001d0; A(4,2) = 0.0d0;  A(4,3) = 0.0d0;  A(4,4) = 4.0d0

  call DGEBAL('B', N, A, 5, ILO, IHI, SCALE, INFO)

  V = 0.0d0
  V(1,1) = 2.0d0; V(1,2) = 0.5d0; V(1,3) = 1.0d0; V(1,4) = 0.0d0
  V(2,1) = 0.0d0; V(2,2) = 3.0d0; V(2,3) = 0.0d0; V(2,4) = 1.0d0
  V(3,1) = 1.0d0; V(3,2) = 0.0d0; V(3,3) = 2.0d0; V(3,4) = 0.5d0
  V(4,1) = 0.5d0; V(4,2) = 1.0d0; V(4,3) = 0.5d0; V(4,4) = 2.0d0

  call DGEBAK('S', 'L', N, ILO, IHI, SCALE, M, V, LDV, INFO)

  call begin_test('job_S_side_L')
  call print_int('info', INFO)
  call print_array('scale', SCALE, N)
  call print_matrix('V', V, LDV, N, M)
  call end_test()

  ! ===========================================================
  ! Test 9: JOB='P', SIDE='L' (permutation only, left)
  ! ===========================================================

  ! Re-balance to get SCALE
  A(1,1) = 1.0d0;  A(1,2) = 0.0d0;  A(1,3) = 0.0d0;  A(1,4) = 1000.0d0
  A(2,1) = 0.0d0;  A(2,2) = 2.0d0;  A(2,3) = 0.001d0; A(2,4) = 0.0d0
  A(3,1) = 0.0d0;  A(3,2) = 1000.0d0; A(3,3) = 3.0d0; A(3,4) = 0.0d0
  A(4,1) = 0.001d0; A(4,2) = 0.0d0;  A(4,3) = 0.0d0;  A(4,4) = 4.0d0

  call DGEBAL('B', N, A, 5, ILO, IHI, SCALE, INFO)

  V = 0.0d0
  do i = 1, N
    V(i,i) = 1.0d0
  end do

  call DGEBAK('P', 'L', N, ILO, IHI, SCALE, M, V, LDV, INFO)

  call begin_test('job_P_side_L')
  call print_int('info', INFO)
  call print_matrix('V', V, LDV, N, M)
  call end_test()

  ! ===========================================================
  ! Test 10: M=0 (quick return)
  ! ===========================================================
  call DGEBAK('B', 'R', 4, 1, 4, SCALE, 0, V, LDV, INFO)

  call begin_test('m_zero')
  call print_int('info', INFO)
  call end_test()

  ! ===========================================================
  ! Test 11: Non-identity V with JOB='B', SIDE='R'
  ! ===========================================================
  A(1,1) = 1.0d0;  A(1,2) = 0.0d0;  A(1,3) = 0.0d0;  A(1,4) = 1000.0d0
  A(2,1) = 0.0d0;  A(2,2) = 2.0d0;  A(2,3) = 0.001d0; A(2,4) = 0.0d0
  A(3,1) = 0.0d0;  A(3,2) = 1000.0d0; A(3,3) = 3.0d0; A(3,4) = 0.0d0
  A(4,1) = 0.001d0; A(4,2) = 0.0d0;  A(4,3) = 0.0d0;  A(4,4) = 4.0d0

  call DGEBAL('B', N, A, 5, ILO, IHI, SCALE, INFO)

  V = 0.0d0
  V(1,1) = 2.0d0; V(1,2) = 0.5d0; V(1,3) = 1.0d0; V(1,4) = 0.0d0
  V(2,1) = 0.0d0; V(2,2) = 3.0d0; V(2,3) = 0.0d0; V(2,4) = 1.0d0
  V(3,1) = 1.0d0; V(3,2) = 0.0d0; V(3,3) = 2.0d0; V(3,4) = 0.5d0
  V(4,1) = 0.5d0; V(4,2) = 1.0d0; V(4,3) = 0.5d0; V(4,4) = 2.0d0

  call DGEBAK('B', 'R', N, ILO, IHI, SCALE, M, V, LDV, INFO)

  call begin_test('nonidentity_V')
  call print_int('info', INFO)
  call print_array('scale', SCALE, N)
  call print_matrix('V', V, LDV, N, M)
  call end_test()

end program
