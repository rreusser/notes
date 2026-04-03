program test_ztgsja
  use test_utils
  implicit none

  ! Use maximum sizes but match LDA to array dimensions
  integer, parameter :: MX=6
  complex*16 :: A(MX, MX), B(MX, MX)
  complex*16 :: U(MX, MX), V(MX, MX), Q(MX, MX)
  complex*16 :: WORK(2*MX)
  double precision :: ALPHA(MX), BETA(MX)
  double precision :: A_r(2*MX*MX), B_r(2*MX*MX)
  double precision :: U_r(2*MX*MX), V_r(2*MX*MX), Q_r(2*MX*MX)
  equivalence (A, A_r)
  equivalence (B, B_r)
  equivalence (U, U_r)
  equivalence (V, V_r)
  equivalence (Q, Q_r)
  integer :: INFO, NCYCLE, K, L, M, P, N

  ! Test 1: Basic 3x3, K=1, L=2 with M-K-L >= 0
  M = 3; P = 2; N = 3; K = 1; L = 2
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  A(1,2) = (3.0d0, 0.5d0); A(1,3) = (1.0d0, 0.25d0)
  A(2,3) = (4.0d0, 0.0d0)
  B(1,2) = (2.0d0, 0.3d0); B(1,3) = (0.5d0, 0.1d0)
  B(2,3) = (3.0d0, 0.0d0)
  call ZTGSJA('I', 'I', 'I', M, P, N, K, L, A, MX, B, MX, &
              1.0d-14, 1.0d-14, ALPHA, BETA, U, MX, V, MX, Q, MX, &
              WORK, NCYCLE, INFO)
  call begin_test('basic_3x3')
  call print_int('info', INFO)
  call print_int('ncycle', NCYCLE)
  call print_int('M', M)
  call print_int('P', P)
  call print_int('N', N)
  call print_int('K', K)
  call print_int('L', L)
  call print_int('LDA', MX)
  call print_array('alpha', ALPHA, N)
  call print_array('beta', BETA, N)
  call print_array('A', A_r, 2*MX*N)
  call print_array('B', B_r, 2*MX*N)
  call print_array('U', U_r, 2*MX*M)
  call print_array('V', V_r, 2*MX*P)
  call print_array('Q', Q_r, 2*MX*N)
  call end_test()

  ! Test 2: K=0, L=2, M=2, P=2, N=2 (all in "L" block)
  M = 2; P = 2; N = 2; K = 0; L = 2
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  A(1,1) = (5.0d0, 1.0d0); A(1,2) = (2.0d0, 0.5d0)
  A(2,2) = (3.0d0, 0.0d0)
  B(1,1) = (4.0d0, 0.0d0); B(1,2) = (1.0d0, 0.25d0)
  B(2,2) = (2.0d0, 0.0d0)
  call ZTGSJA('I', 'I', 'I', M, P, N, K, L, A, MX, B, MX, &
              1.0d-14, 1.0d-14, ALPHA, BETA, U, MX, V, MX, Q, MX, &
              WORK, NCYCLE, INFO)
  call begin_test('k0_l2')
  call print_int('info', INFO)
  call print_int('ncycle', NCYCLE)
  call print_int('M', M)
  call print_int('P', P)
  call print_int('N', N)
  call print_int('K', K)
  call print_int('L', L)
  call print_int('LDA', MX)
  call print_array('alpha', ALPHA, N)
  call print_array('beta', BETA, N)
  call print_array('A', A_r, 2*MX*N)
  call print_array('U', U_r, 2*MX*M)
  call print_array('V', V_r, 2*MX*P)
  call print_array('Q', Q_r, 2*MX*N)
  call end_test()

  ! Test 3: K=2, L=1, M=4, P=2, N=4 - large K block
  M = 4; P = 2; N = 4; K = 2; L = 1
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  A(1,3) = (1.0d0, 0.0d0)
  A(2,4) = (2.0d0, 0.5d0)
  A(3,4) = (5.0d0, 0.0d0)
  B(1,4) = (3.0d0, 0.0d0)
  call ZTGSJA('I', 'I', 'I', M, P, N, K, L, A, MX, B, MX, &
              1.0d-14, 1.0d-14, ALPHA, BETA, U, MX, V, MX, Q, MX, &
              WORK, NCYCLE, INFO)
  call begin_test('k2_l1')
  call print_int('info', INFO)
  call print_int('ncycle', NCYCLE)
  call print_int('M', M)
  call print_int('P', P)
  call print_int('N', N)
  call print_int('K', K)
  call print_int('L', L)
  call print_int('LDA', MX)
  call print_array('alpha', ALPHA, N)
  call print_array('beta', BETA, N)
  call end_test()

  ! Test 4: No U/V/Q computation
  M = 2; P = 2; N = 2; K = 0; L = 2
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  A(1,1) = (5.0d0, 1.0d0); A(1,2) = (2.0d0, 0.5d0)
  A(2,2) = (3.0d0, 0.0d0)
  B(1,1) = (4.0d0, 0.0d0); B(1,2) = (1.0d0, 0.25d0)
  B(2,2) = (2.0d0, 0.0d0)
  call ZTGSJA('N', 'N', 'N', M, P, N, K, L, A, MX, B, MX, &
              1.0d-14, 1.0d-14, ALPHA, BETA, U, MX, V, MX, Q, MX, &
              WORK, NCYCLE, INFO)
  call begin_test('no_uvq')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_int('P', P)
  call print_int('N', N)
  call print_int('K', K)
  call print_int('L', L)
  call print_int('LDA', MX)
  call print_array('alpha', ALPHA, N)
  call print_array('beta', BETA, N)
  call end_test()

  ! Test 5: M-K-L < 0 case: M=2, K=1, L=2 so M-K-L=-1
  M = 2; P = 3; N = 4; K = 1; L = 2
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  A(1,3) = (2.0d0, 0.0d0); A(1,4) = (1.0d0, 0.5d0)
  A(2,4) = (4.0d0, 0.0d0)
  B(1,3) = (3.0d0, 0.0d0); B(1,4) = (0.5d0, 0.1d0)
  B(2,4) = (2.0d0, 0.0d0)
  call ZTGSJA('I', 'I', 'I', M, P, N, K, L, A, MX, B, MX, &
              1.0d-14, 1.0d-14, ALPHA, BETA, U, MX, V, MX, Q, MX, &
              WORK, NCYCLE, INFO)
  call begin_test('m_k_l_negative')
  call print_int('info', INFO)
  call print_int('ncycle', NCYCLE)
  call print_int('M', M)
  call print_int('P', P)
  call print_int('N', N)
  call print_int('K', K)
  call print_int('L', L)
  call print_int('LDA', MX)
  call print_array('alpha', ALPHA, N)
  call print_array('beta', BETA, N)
  call end_test()

  ! Test 6: Larger complex test with significant complex parts
  M = 4; P = 3; N = 4; K = 1; L = 3
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  A(1,2) = (2.0d0, 0.5d0); A(1,3) = (1.0d0, 0.0d0); A(1,4) = (0.5d0, 0.3d0)
  A(2,3) = (3.0d0, 0.0d0); A(2,4) = (1.0d0, 0.2d0)
  A(3,4) = (4.0d0, 0.0d0)
  B(1,2) = (1.0d0, 0.0d0); B(1,3) = (0.5d0, 0.1d0); B(1,4) = (0.2d0, 0.0d0)
  B(2,3) = (2.0d0, 0.0d0); B(2,4) = (0.3d0, 0.15d0)
  B(3,4) = (1.5d0, 0.0d0)
  call ZTGSJA('I', 'I', 'I', M, P, N, K, L, A, MX, B, MX, &
              1.0d-14, 1.0d-14, ALPHA, BETA, U, MX, V, MX, Q, MX, &
              WORK, NCYCLE, INFO)
  call begin_test('larger_complex')
  call print_int('info', INFO)
  call print_int('ncycle', NCYCLE)
  call print_int('M', M)
  call print_int('P', P)
  call print_int('N', N)
  call print_int('K', K)
  call print_int('L', L)
  call print_int('LDA', MX)
  call print_array('alpha', ALPHA, N)
  call print_array('beta', BETA, N)
  call print_array('A', A_r, 2*MX*N)
  call print_array('B', B_r, 2*MX*N)
  call print_array('U', U_r, 2*MX*M)
  call print_array('V', V_r, 2*MX*P)
  call print_array('Q', Q_r, 2*MX*N)
  call end_test()

end program
