program test_dggsvd3
  use test_utils
  implicit none
  integer, parameter :: MAXM=6, MAXN=6, MAXP=6, LWORK=200
  double precision :: A(MAXM, MAXN), B(MAXP, MAXN)
  double precision :: U(MAXM, MAXM), V(MAXP, MAXP), Q(MAXN, MAXN)
  double precision :: ALPHA(MAXN), BETA(MAXN), WORK(LWORK)
  integer :: IWORK(MAXN), INFO, K, L, M, N, P

  ! Test 1: 3x3 A, 2x3 B, compute all
  M = 3; N = 3; P = 2
  A = 0.0d0; B = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0; A(1,3) = 3.0d0
  A(2,1) = 4.0d0; A(2,2) = 5.0d0; A(2,3) = 6.0d0
  A(3,1) = 7.0d0; A(3,2) = 8.0d0; A(3,3) = 10.0d0
  B(1,1) = 1.0d0; B(1,2) = 0.0d0; B(1,3) = 1.0d0
  B(2,1) = 0.0d0; B(2,2) = 1.0d0; B(2,3) = 1.0d0
  call DGGSVD3('U', 'V', 'Q', M, N, P, K, L, A, MAXM, B, MAXP, &
               ALPHA, BETA, U, MAXM, V, MAXP, Q, MAXN, WORK, LWORK, &
               IWORK, INFO)
  call begin_test('basic_3x3_2x3')
  call print_int('info', INFO)
  call print_int('k', K)
  call print_int('l', L)
  call print_array('alpha', ALPHA, N)
  call print_array('beta', BETA, N)
  call print_matrix('A', A, MAXM, MIN(K+L,M), N)
  call print_matrix('U', U, MAXM, M, M)
  call print_matrix('V', V, MAXP, P, P)
  call print_matrix('Q', Q, MAXN, N, N)
  call print_int_array('iwork', IWORK, N)
  call end_test()

  ! Test 2: 2x3 A, 3x3 B (M < P)
  M = 2; N = 3; P = 3
  A = 0.0d0; B = 0.0d0
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 0.0d0
  A(2,1) = 0.0d0; A(2,2) = 3.0d0; A(2,3) = 1.0d0
  B(1,1) = 1.0d0; B(1,2) = 2.0d0; B(1,3) = 3.0d0
  B(2,1) = 4.0d0; B(2,2) = 5.0d0; B(2,3) = 6.0d0
  B(3,1) = 7.0d0; B(3,2) = 8.0d0; B(3,3) = 10.0d0
  call DGGSVD3('U', 'V', 'Q', M, N, P, K, L, A, MAXM, B, MAXP, &
               ALPHA, BETA, U, MAXM, V, MAXP, Q, MAXN, WORK, LWORK, &
               IWORK, INFO)
  call begin_test('2x3_3x3')
  call print_int('info', INFO)
  call print_int('k', K)
  call print_int('l', L)
  call print_array('alpha', ALPHA, N)
  call print_array('beta', BETA, N)
  call end_test()

  ! Test 3: No U/V/Q
  M = 3; N = 3; P = 2
  A = 0.0d0; B = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0; A(1,3) = 3.0d0
  A(2,1) = 4.0d0; A(2,2) = 5.0d0; A(2,3) = 6.0d0
  A(3,1) = 7.0d0; A(3,2) = 8.0d0; A(3,3) = 10.0d0
  B(1,1) = 1.0d0; B(1,2) = 0.0d0; B(1,3) = 1.0d0
  B(2,1) = 0.0d0; B(2,2) = 1.0d0; B(2,3) = 1.0d0
  call DGGSVD3('N', 'N', 'N', M, N, P, K, L, A, MAXM, B, MAXP, &
               ALPHA, BETA, U, MAXM, V, MAXP, Q, MAXN, WORK, LWORK, &
               IWORK, INFO)
  call begin_test('no_uvq')
  call print_int('info', INFO)
  call print_int('k', K)
  call print_int('l', L)
  call print_array('alpha', ALPHA, N)
  call print_array('beta', BETA, N)
  call end_test()

  ! Test 4: 4x4 A, 3x4 B - larger problem
  M = 4; N = 4; P = 3
  A = 0.0d0; B = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 0.0d0; A(1,3) = 0.0d0; A(1,4) = 0.0d0
  A(2,1) = 0.0d0; A(2,2) = 2.0d0; A(2,3) = 0.0d0; A(2,4) = 0.0d0
  A(3,1) = 0.0d0; A(3,2) = 0.0d0; A(3,3) = 3.0d0; A(3,4) = 0.0d0
  A(4,1) = 0.0d0; A(4,2) = 0.0d0; A(4,3) = 0.0d0; A(4,4) = 4.0d0
  B(1,1) = 1.0d0; B(1,2) = 1.0d0; B(1,3) = 0.0d0; B(1,4) = 0.0d0
  B(2,1) = 0.0d0; B(2,2) = 1.0d0; B(2,3) = 1.0d0; B(2,4) = 0.0d0
  B(3,1) = 0.0d0; B(3,2) = 0.0d0; B(3,3) = 1.0d0; B(3,4) = 1.0d0
  call DGGSVD3('U', 'V', 'Q', M, N, P, K, L, A, MAXM, B, MAXP, &
               ALPHA, BETA, U, MAXM, V, MAXP, Q, MAXN, WORK, LWORK, &
               IWORK, INFO)
  call begin_test('diag_4x4')
  call print_int('info', INFO)
  call print_int('k', K)
  call print_int('l', L)
  call print_array('alpha', ALPHA, N)
  call print_array('beta', BETA, N)
  call end_test()

  ! Test 5: Workspace query (LWORK=-1)
  M = 3; N = 3; P = 2
  call DGGSVD3('U', 'V', 'Q', M, N, P, K, L, A, MAXM, B, MAXP, &
               ALPHA, BETA, U, MAXM, V, MAXP, Q, MAXN, WORK, -1, &
               IWORK, INFO)
  call begin_test('workspace_query')
  call print_int('info', INFO)
  call print_scalar('optimal_lwork', WORK(1))
  call end_test()

end program
