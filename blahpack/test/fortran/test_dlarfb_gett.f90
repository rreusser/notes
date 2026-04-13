program test_dlarfb_gett
  use test_utils
  implicit none

  double precision :: A(8, 8), B(8, 8), T(6, 6), WORK(6, 8)
  integer :: i, j

  ! DLARFB_GETT applies a real block Householder H from the left to a
  ! (K+M)-by-N triangular-pentagonal matrix. A is K-by-N (upper
  ! trapezoidal; strict lower part of its leading K-by-K block contains
  ! V1 when IDENT != 'I'). B is M-by-N (left K columns contain V2,
  ! right N-K columns contain the rectangular B block).

  ! =========================================================
  ! Test 1: Basic, IDENT='N' (V1 stored), K=2, M=3, N=4
  ! =========================================================
  A = 0.0d0
  ! Upper-trapezoidal part (K-by-N), with strict lower = V1
  A(1,1) = 1.5d0; A(1,2) = 2.0d0; A(1,3) = 3.0d0; A(1,4) = 4.0d0
  A(2,1) = 0.5d0; A(2,2) = 2.5d0; A(2,3) = 3.5d0; A(2,4) = 4.5d0
  ! (A(2,1) = 0.5 is V1 lower-tri entry; diag of V1 is implicit 1)

  B = 0.0d0
  ! V2 in first K=2 columns
  B(1,1) = 0.1d0; B(1,2) = 0.2d0
  B(2,1) = 0.3d0; B(2,2) = 0.4d0
  B(3,1) = 0.5d0; B(3,2) = 0.6d0
  ! Rectangular B block in columns K+1..N
  B(1,3) = 1.1d0; B(1,4) = 1.2d0
  B(2,3) = 2.1d0; B(2,4) = 2.2d0
  B(3,3) = 3.1d0; B(3,4) = 3.2d0

  ! Upper-triangular K-by-K block T
  T = 0.0d0
  T(1,1) = 1.2d0; T(1,2) = -0.3d0
  T(2,2) = 0.9d0

  WORK = 0.0d0
  call DLARFB_GETT('N', 3, 4, 2, T, 6, A, 8, B, 8, WORK, 6)
  call begin_test('basic_notident_k2_m3_n4')
  call print_matrix('A', A, 8, 2, 4)
  call print_matrix('B', B, 8, 3, 4)
  call end_test()

  ! =========================================================
  ! Test 2: IDENT='I' (V1 is identity, not stored)
  ! =========================================================
  A = 0.0d0
  A(1,1) = 1.5d0; A(1,2) = 2.0d0; A(1,3) = 3.0d0; A(1,4) = 4.0d0
  A(2,1) = 0.0d0; A(2,2) = 2.5d0; A(2,3) = 3.5d0; A(2,4) = 4.5d0
  ! A(2,1) is ignored when IDENT='I'

  B = 0.0d0
  B(1,1) = 0.1d0; B(1,2) = 0.2d0
  B(2,1) = 0.3d0; B(2,2) = 0.4d0
  B(3,1) = 0.5d0; B(3,2) = 0.6d0
  B(1,3) = 1.1d0; B(1,4) = 1.2d0
  B(2,3) = 2.1d0; B(2,4) = 2.2d0
  B(3,3) = 3.1d0; B(3,4) = 3.2d0

  T = 0.0d0
  T(1,1) = 1.2d0; T(1,2) = -0.3d0
  T(2,2) = 0.9d0

  WORK = 0.0d0
  call DLARFB_GETT('I', 3, 4, 2, T, 6, A, 8, B, 8, WORK, 6)
  call begin_test('basic_ident_k2_m3_n4')
  call print_matrix('A', A, 8, 2, 4)
  call print_matrix('B', B, 8, 3, 4)
  call end_test()

  ! =========================================================
  ! Test 3: N == K (no N-K block), IDENT='N', K=3, M=4, N=3
  ! =========================================================
  A = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 0.5d0
  A(2,1) = 0.3d0; A(2,2) = 1.5d0; A(2,3) = 0.7d0
  A(3,1) = 0.2d0; A(3,2) = 0.1d0; A(3,3) = 2.5d0

  B = 0.0d0
  B(1,1) = 0.1d0; B(1,2) = 0.2d0; B(1,3) = 0.3d0
  B(2,1) = 0.4d0; B(2,2) = 0.5d0; B(2,3) = 0.6d0
  B(3,1) = 0.7d0; B(3,2) = 0.8d0; B(3,3) = 0.9d0
  B(4,1) = 1.0d0; B(4,2) = 1.1d0; B(4,3) = 1.2d0

  T = 0.0d0
  T(1,1) = 1.1d0; T(1,2) = -0.2d0; T(1,3) = 0.1d0
  T(2,2) = 0.8d0; T(2,3) = -0.3d0
  T(3,3) = 1.3d0

  WORK = 0.0d0
  call DLARFB_GETT('N', 4, 3, 3, T, 6, A, 8, B, 8, WORK, 6)
  call begin_test('n_eq_k_notident_k3_m4')
  call print_matrix('A', A, 8, 3, 3)
  call print_matrix('B', B, 8, 4, 3)
  call end_test()

  ! =========================================================
  ! Test 4: N == K, IDENT='I'
  ! =========================================================
  A = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 0.5d0
  A(2,2) = 1.5d0; A(2,3) = 0.7d0
  A(3,3) = 2.5d0

  B = 0.0d0
  B(1,1) = 0.1d0; B(1,2) = 0.2d0; B(1,3) = 0.3d0
  B(2,1) = 0.4d0; B(2,2) = 0.5d0; B(2,3) = 0.6d0
  B(3,1) = 0.7d0; B(3,2) = 0.8d0; B(3,3) = 0.9d0
  B(4,1) = 1.0d0; B(4,2) = 1.1d0; B(4,3) = 1.2d0

  T = 0.0d0
  T(1,1) = 1.1d0; T(1,2) = -0.2d0; T(1,3) = 0.1d0
  T(2,2) = 0.8d0; T(2,3) = -0.3d0
  T(3,3) = 1.3d0

  WORK = 0.0d0
  call DLARFB_GETT('I', 4, 3, 3, T, 6, A, 8, B, 8, WORK, 6)
  call begin_test('n_eq_k_ident_k3_m4')
  call print_matrix('A', A, 8, 3, 3)
  call print_matrix('B', B, 8, 4, 3)
  call end_test()

  ! =========================================================
  ! Test 5: M == 0 (no B rows), IDENT='N', K=2, N=4
  ! =========================================================
  A = 0.0d0
  A(1,1) = 1.5d0; A(1,2) = 2.0d0; A(1,3) = 3.0d0; A(1,4) = 4.0d0
  A(2,1) = 0.5d0; A(2,2) = 2.5d0; A(2,3) = 3.5d0; A(2,4) = 4.5d0

  B = 0.0d0

  T = 0.0d0
  T(1,1) = 1.2d0; T(1,2) = -0.3d0
  T(2,2) = 0.9d0

  WORK = 0.0d0
  call DLARFB_GETT('N', 0, 4, 2, T, 6, A, 8, B, 8, WORK, 6)
  call begin_test('m_zero_notident')
  call print_matrix('A', A, 8, 2, 4)
  call end_test()

  ! =========================================================
  ! Test 6: K=1, IDENT='N', M=2, N=3
  ! =========================================================
  A = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 0.5d0
  ! K=1, strict lower of 1x1 V1 is empty

  B = 0.0d0
  B(1,1) = 0.25d0
  B(2,1) = 0.5d0
  B(1,2) = 1.0d0; B(1,3) = 2.0d0
  B(2,2) = 3.0d0; B(2,3) = 4.0d0

  T = 0.0d0
  T(1,1) = 1.4d0

  WORK = 0.0d0
  call DLARFB_GETT('N', 2, 3, 1, T, 6, A, 8, B, 8, WORK, 6)
  call begin_test('k1_notident_m2_n3')
  call print_matrix('A', A, 8, 1, 3)
  call print_matrix('B', B, 8, 2, 3)
  call end_test()

  ! =========================================================
  ! Test 7: K=1, IDENT='I', M=2, N=3
  ! =========================================================
  A = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 0.5d0

  B = 0.0d0
  B(1,1) = 0.25d0
  B(2,1) = 0.5d0
  B(1,2) = 1.0d0; B(1,3) = 2.0d0
  B(2,2) = 3.0d0; B(2,3) = 4.0d0

  T = 0.0d0
  T(1,1) = 1.4d0

  WORK = 0.0d0
  call DLARFB_GETT('I', 2, 3, 1, T, 6, A, 8, B, 8, WORK, 6)
  call begin_test('k1_ident_m2_n3')
  call print_matrix('A', A, 8, 1, 3)
  call print_matrix('B', B, 8, 2, 3)
  call end_test()

end program
