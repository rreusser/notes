program test_zggsvd3
  use test_utils
  implicit none
  integer, parameter :: MAXM=6, MAXN=6, MAXP=6, LWORK=500
  complex*16 :: A(MAXM, MAXN), B(MAXP, MAXN)
  complex*16 :: U(MAXM, MAXM), V(MAXP, MAXP), Q(MAXN, MAXN)
  complex*16 :: WORK(LWORK)
  double precision :: ALPHA(MAXN), BETA(MAXN), RWORK(4*MAXN)
  double precision :: A_r(2*MAXM*MAXN), B_r(2*MAXP*MAXN)
  double precision :: U_r(2*MAXM*MAXM), V_r(2*MAXP*MAXP), Q_r(2*MAXN*MAXN)
  double precision :: WORK_r(2*LWORK)
  equivalence (A, A_r)
  equivalence (B, B_r)
  equivalence (U, U_r)
  equivalence (V, V_r)
  equivalence (Q, Q_r)
  equivalence (WORK, WORK_r)
  integer :: IWORK(MAXN), INFO, K, L, M, N, P

  ! Test 1: Basic 3x3 A, 2x3 B, complex, compute all
  M = 3; N = 3; P = 2
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.5d0); A(1,2) = (2.0d0, 0.0d0); A(1,3) = (3.0d0, -0.5d0)
  A(2,1) = (4.0d0, 0.0d0); A(2,2) = (5.0d0, 1.0d0); A(2,3) = (6.0d0, 0.0d0)
  A(3,1) = (7.0d0, -0.5d0); A(3,2) = (8.0d0, 0.0d0); A(3,3) = (10.0d0, 0.5d0)
  B(1,1) = (1.0d0, 0.0d0); B(1,2) = (0.0d0, 0.0d0); B(1,3) = (1.0d0, 0.5d0)
  B(2,1) = (0.0d0, 0.0d0); B(2,2) = (1.0d0, 0.0d0); B(2,3) = (1.0d0, -0.5d0)
  call ZGGSVD3('U', 'V', 'Q', M, N, P, K, L, A, MAXM, B, MAXP, &
               ALPHA, BETA, U, MAXM, V, MAXP, Q, MAXN, WORK, LWORK, &
               RWORK, IWORK, INFO)
  call begin_test('basic_3x3_2x3')
  call print_int('info', INFO)
  call print_int('k', K)
  call print_int('l', L)
  call print_array('alpha', ALPHA, N)
  call print_array('beta', BETA, N)
  call end_test()

  ! Test 2: 2x3 A, 3x3 B (M < P), complex
  M = 2; N = 3; P = 3
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  A(1,1) = (2.0d0, 0.5d0); A(1,2) = (1.0d0, 0.0d0); A(1,3) = (0.0d0, 0.0d0)
  A(2,1) = (0.0d0, 0.0d0); A(2,2) = (3.0d0, -0.5d0); A(2,3) = (1.0d0, 0.0d0)
  B(1,1) = (1.0d0, 0.0d0); B(1,2) = (2.0d0, 0.5d0); B(1,3) = (3.0d0, 0.0d0)
  B(2,1) = (4.0d0, -0.5d0); B(2,2) = (5.0d0, 0.0d0); B(2,3) = (6.0d0, 0.5d0)
  B(3,1) = (7.0d0, 0.0d0); B(3,2) = (8.0d0, -0.5d0); B(3,3) = (10.0d0, 0.0d0)
  call ZGGSVD3('U', 'V', 'Q', M, N, P, K, L, A, MAXM, B, MAXP, &
               ALPHA, BETA, U, MAXM, V, MAXP, Q, MAXN, WORK, LWORK, &
               RWORK, IWORK, INFO)
  call begin_test('m_lt_p')
  call print_int('info', INFO)
  call print_int('k', K)
  call print_int('l', L)
  call print_array('alpha', ALPHA, N)
  call print_array('beta', BETA, N)
  call end_test()

  ! Test 3: No U/V/Q
  M = 3; N = 3; P = 2
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.5d0); A(1,2) = (2.0d0, 0.0d0); A(1,3) = (3.0d0, -0.5d0)
  A(2,1) = (4.0d0, 0.0d0); A(2,2) = (5.0d0, 1.0d0); A(2,3) = (6.0d0, 0.0d0)
  A(3,1) = (7.0d0, -0.5d0); A(3,2) = (8.0d0, 0.0d0); A(3,3) = (10.0d0, 0.5d0)
  B(1,1) = (1.0d0, 0.0d0); B(1,2) = (0.0d0, 0.0d0); B(1,3) = (1.0d0, 0.5d0)
  B(2,1) = (0.0d0, 0.0d0); B(2,2) = (1.0d0, 0.0d0); B(2,3) = (1.0d0, -0.5d0)
  call ZGGSVD3('N', 'N', 'N', M, N, P, K, L, A, MAXM, B, MAXP, &
               ALPHA, BETA, U, MAXM, V, MAXP, Q, MAXN, WORK, LWORK, &
               RWORK, IWORK, INFO)
  call begin_test('no_uvq')
  call print_int('info', INFO)
  call print_int('k', K)
  call print_int('l', L)
  call print_array('alpha', ALPHA, N)
  call print_array('beta', BETA, N)
  call end_test()

  ! Test 4: 4x4 diagonal A, 3x4 bidiagonal B
  M = 4; N = 4; P = 3
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.0d0); A(2,2) = (2.0d0, 0.0d0); A(3,3) = (3.0d0, 0.0d0); A(4,4) = (4.0d0, 0.0d0)
  B(1,1) = (1.0d0, 0.0d0); B(1,2) = (1.0d0, 0.0d0)
  B(2,2) = (1.0d0, 0.0d0); B(2,3) = (1.0d0, 0.0d0)
  B(3,3) = (1.0d0, 0.0d0); B(3,4) = (1.0d0, 0.0d0)
  call ZGGSVD3('U', 'V', 'Q', M, N, P, K, L, A, MAXM, B, MAXP, &
               ALPHA, BETA, U, MAXM, V, MAXP, Q, MAXN, WORK, LWORK, &
               RWORK, IWORK, INFO)
  call begin_test('diag_4x4')
  call print_int('info', INFO)
  call print_int('k', K)
  call print_int('l', L)
  call print_array('alpha', ALPHA, N)
  call print_array('beta', BETA, N)
  call end_test()

  ! Test 5: Workspace query (LWORK=-1)
  M = 3; N = 3; P = 2
  call ZGGSVD3('U', 'V', 'Q', M, N, P, K, L, A, MAXM, B, MAXP, &
               ALPHA, BETA, U, MAXM, V, MAXP, Q, MAXN, WORK, -1, &
               RWORK, IWORK, INFO)
  call begin_test('workspace_query')
  call print_int('info', INFO)
  call print_scalar('optimal_lwork', dble(WORK(1)))
  call end_test()

end program
