program test_dtgsja
  use test_utils
  implicit none
  integer, parameter :: MAXN=6, MAXM=6, MAXP=6
  double precision :: A(MAXM, MAXN), B(MAXP, MAXN)
  double precision :: U(MAXM, MAXM), V(MAXP, MAXP), Q(MAXN, MAXN)
  double precision :: ALPHA(MAXN), BETA(MAXN), WORK(2*MAXN)
  integer :: INFO, NCYCLE, K, L, M, P, N

  ! Test 1: Basic 3x3, K=1, L=2 with M-K-L >= 0
  ! A is 3x3 upper trapezoidal with K=1, L=2
  M = 3; P = 2; N = 3; K = 1; L = 2
  A = 0.0d0; B = 0.0d0
  A(1,1) = 0.0d0; A(1,2) = 3.0d0; A(1,3) = 1.0d0
  A(2,1) = 0.0d0; A(2,2) = 0.0d0; A(2,3) = 4.0d0
  A(3,1) = 0.0d0; A(3,2) = 0.0d0; A(3,3) = 0.0d0
  B(1,1) = 0.0d0; B(1,2) = 2.0d0; B(1,3) = 0.5d0
  B(2,1) = 0.0d0; B(2,2) = 0.0d0; B(2,3) = 3.0d0
  call DTGSJA('I', 'I', 'I', M, P, N, K, L, A, MAXM, B, MAXP, &
              1.0d-14, 1.0d-14, ALPHA, BETA, U, MAXM, V, MAXP, Q, MAXN, &
              WORK, NCYCLE, INFO)
  call begin_test('basic_3x3')
  call print_int('info', INFO)
  call print_int('ncycle', NCYCLE)
  call print_array('alpha', ALPHA, N)
  call print_array('beta', BETA, N)
  call print_matrix('A', A, MAXM, M, N)
  call print_matrix('B', B, MAXP, P, N)
  call print_matrix('U', U, MAXM, M, M)
  call print_matrix('V', V, MAXP, P, P)
  call print_matrix('Q', Q, MAXN, N, N)
  call end_test()

  ! Test 2: K=0, L=2, M=2, P=2, N=2 (all in "L" block)
  M = 2; P = 2; N = 2; K = 0; L = 2
  A = 0.0d0; B = 0.0d0
  A(1,1) = 5.0d0; A(1,2) = 2.0d0
  A(2,1) = 0.0d0; A(2,2) = 3.0d0
  B(1,1) = 4.0d0; B(1,2) = 1.0d0
  B(2,1) = 0.0d0; B(2,2) = 2.0d0
  call DTGSJA('I', 'I', 'I', M, P, N, K, L, A, MAXM, B, MAXP, &
              1.0d-14, 1.0d-14, ALPHA, BETA, U, MAXM, V, MAXP, Q, MAXN, &
              WORK, NCYCLE, INFO)
  call begin_test('k0_l2')
  call print_int('info', INFO)
  call print_int('ncycle', NCYCLE)
  call print_array('alpha', ALPHA, N)
  call print_array('beta', BETA, N)
  call print_matrix('A', A, MAXM, M, N)
  call print_matrix('U', U, MAXM, M, M)
  call print_matrix('V', V, MAXP, P, P)
  call print_matrix('Q', Q, MAXN, N, N)
  call end_test()

  ! Test 3: K=2, L=1, M=4, P=2, N=4 - large K block
  M = 4; P = 2; N = 4; K = 2; L = 1
  A = 0.0d0; B = 0.0d0
  A(1,1) = 0.0d0; A(1,2) = 0.0d0; A(1,3) = 1.0d0; A(1,4) = 0.0d0
  A(2,1) = 0.0d0; A(2,2) = 0.0d0; A(2,3) = 0.0d0; A(2,4) = 2.0d0
  A(3,1) = 0.0d0; A(3,2) = 0.0d0; A(3,3) = 0.0d0; A(3,4) = 5.0d0
  A(4,1) = 0.0d0; A(4,2) = 0.0d0; A(4,3) = 0.0d0; A(4,4) = 0.0d0
  B(1,1) = 0.0d0; B(1,2) = 0.0d0; B(1,3) = 0.0d0; B(1,4) = 3.0d0
  B(2,1) = 0.0d0; B(2,2) = 0.0d0; B(2,3) = 0.0d0; B(2,4) = 0.0d0
  call DTGSJA('I', 'I', 'I', M, P, N, K, L, A, MAXM, B, MAXP, &
              1.0d-14, 1.0d-14, ALPHA, BETA, U, MAXM, V, MAXP, Q, MAXN, &
              WORK, NCYCLE, INFO)
  call begin_test('k2_l1')
  call print_int('info', INFO)
  call print_int('ncycle', NCYCLE)
  call print_array('alpha', ALPHA, N)
  call print_array('beta', BETA, N)
  call end_test()

  ! Test 4: No U/V/Q computation
  M = 2; P = 2; N = 2; K = 0; L = 2
  A = 0.0d0; B = 0.0d0
  A(1,1) = 5.0d0; A(1,2) = 2.0d0
  A(2,1) = 0.0d0; A(2,2) = 3.0d0
  B(1,1) = 4.0d0; B(1,2) = 1.0d0
  B(2,1) = 0.0d0; B(2,2) = 2.0d0
  call DTGSJA('N', 'N', 'N', M, P, N, K, L, A, MAXM, B, MAXP, &
              1.0d-14, 1.0d-14, ALPHA, BETA, U, MAXM, V, MAXP, Q, MAXN, &
              WORK, NCYCLE, INFO)
  call begin_test('no_uvq')
  call print_int('info', INFO)
  call print_array('alpha', ALPHA, N)
  call print_array('beta', BETA, N)
  call end_test()

  ! Test 5: M-K-L < 0 case: M=2, K=1, L=2 so M-K-L=-1
  M = 2; P = 3; N = 4; K = 1; L = 2
  A = 0.0d0; B = 0.0d0
  A(1,1) = 0.0d0; A(1,2) = 0.0d0; A(1,3) = 2.0d0; A(1,4) = 1.0d0
  A(2,1) = 0.0d0; A(2,2) = 0.0d0; A(2,3) = 0.0d0; A(2,4) = 4.0d0
  B(1,1) = 0.0d0; B(1,2) = 0.0d0; B(1,3) = 3.0d0; B(1,4) = 0.5d0
  B(2,1) = 0.0d0; B(2,2) = 0.0d0; B(2,3) = 0.0d0; B(2,4) = 2.0d0
  B(3,1) = 0.0d0; B(3,2) = 0.0d0; B(3,3) = 0.0d0; B(3,4) = 0.0d0
  call DTGSJA('I', 'I', 'I', M, P, N, K, L, A, MAXM, B, MAXP, &
              1.0d-14, 1.0d-14, ALPHA, BETA, U, MAXM, V, MAXP, Q, MAXN, &
              WORK, NCYCLE, INFO)
  call begin_test('m_k_l_negative')
  call print_int('info', INFO)
  call print_int('ncycle', NCYCLE)
  call print_array('alpha', ALPHA, N)
  call print_array('beta', BETA, N)
  call end_test()

end program
