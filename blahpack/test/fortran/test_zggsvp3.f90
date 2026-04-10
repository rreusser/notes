program test_zggsvp3
  use test_utils
  implicit none

  integer, parameter :: MAXN = 8
  complex*16 :: A(MAXN, MAXN), B(MAXN, MAXN)
  complex*16 :: U(MAXN, MAXN), V(MAXN, MAXN), Q(MAXN, MAXN)
  complex*16 :: TAU(MAXN), WORK(5000)
  double precision :: RWORK(5*MAXN)
  double precision :: A_r(2*MAXN*MAXN), B_r(2*MAXN*MAXN)
  double precision :: U_r(2*MAXN*MAXN), V_r(2*MAXN*MAXN), Q_r(2*MAXN*MAXN)
  equivalence (A, A_r)
  equivalence (B, B_r)
  equivalence (U, U_r)
  equivalence (V, V_r)
  equivalence (Q, Q_r)
  integer :: IWORK(MAXN)
  integer :: info, K, L, lwork
  double precision :: tola, tolb

  lwork = 5000
  tola = 1.0d-8
  tolb = 1.0d-8

  ! Test 1: Basic 4x3 A, 3x3 B with JOBU='U', JOBV='V', JOBQ='Q'
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  U = (0.0d0, 0.0d0); V = (0.0d0, 0.0d0); Q = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.5d0); A(2,1) = (2.0d0, 0.0d0); A(3,1) = (3.0d0, 1.0d0); A(4,1) = (4.0d0, -0.5d0)
  A(1,2) = (5.0d0, 0.0d0); A(2,2) = (6.0d0, 1.0d0); A(3,2) = (7.0d0, 0.0d0); A(4,2) = (8.0d0, 0.5d0)
  A(1,3) = (9.0d0, 0.5d0); A(2,3) = (10.0d0, 0.0d0); A(3,3) = (11.0d0, -1.0d0); A(4,3) = (12.0d0, 0.0d0)
  B(1,1) = (10.0d0, 0.0d0); B(2,1) = (1.0d0, 0.5d0); B(3,1) = (1.0d0, -0.5d0)
  B(1,2) = (1.0d0, -0.5d0); B(2,2) = (10.0d0, 0.0d0); B(3,2) = (1.0d0, 0.5d0)
  B(1,3) = (1.0d0, 0.5d0); B(2,3) = (1.0d0, -0.5d0); B(3,3) = (10.0d0, 0.0d0)
  call ZGGSVP3('U', 'V', 'Q', 4, 3, 3, A, MAXN, B, MAXN, tola, tolb, &
               K, L, U, MAXN, V, MAXN, Q, MAXN, IWORK, RWORK, TAU, WORK, lwork, info)
  call begin_test('basic_4x3_3x3_UVQ')
  call print_int('info', info)
  call print_int('K', K)
  call print_int('L', L)
  call print_array('a', A_r, 2*MAXN*3)
  call print_array('b', B_r, 2*MAXN*3)
  call print_array('u', U_r, 2*MAXN*4)
  call print_array('v', V_r, 2*MAXN*3)
  call print_array('q', Q_r, 2*MAXN*3)
  call end_test()

  ! Test 2: JOBU='N', JOBV='N', JOBQ='N'
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  U = (0.0d0, 0.0d0); V = (0.0d0, 0.0d0); Q = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.5d0); A(2,1) = (2.0d0, 0.0d0); A(3,1) = (3.0d0, 1.0d0); A(4,1) = (4.0d0, -0.5d0)
  A(1,2) = (5.0d0, 0.0d0); A(2,2) = (6.0d0, 1.0d0); A(3,2) = (7.0d0, 0.0d0); A(4,2) = (8.0d0, 0.5d0)
  A(1,3) = (9.0d0, 0.5d0); A(2,3) = (10.0d0, 0.0d0); A(3,3) = (11.0d0, -1.0d0); A(4,3) = (12.0d0, 0.0d0)
  B(1,1) = (10.0d0, 0.0d0); B(2,1) = (1.0d0, 0.5d0); B(3,1) = (1.0d0, -0.5d0)
  B(1,2) = (1.0d0, -0.5d0); B(2,2) = (10.0d0, 0.0d0); B(3,2) = (1.0d0, 0.5d0)
  B(1,3) = (1.0d0, 0.5d0); B(2,3) = (1.0d0, -0.5d0); B(3,3) = (10.0d0, 0.0d0)
  call ZGGSVP3('N', 'N', 'N', 4, 3, 3, A, MAXN, B, MAXN, tola, tolb, &
               K, L, U, 1, V, 1, Q, 1, IWORK, RWORK, TAU, WORK, lwork, info)
  call begin_test('basic_4x3_3x3_NNN')
  call print_int('info', info)
  call print_int('K', K)
  call print_int('L', L)
  call print_array('a', A_r, 2*MAXN*3)
  call print_array('b', B_r, 2*MAXN*3)
  call end_test()

  ! Test 3: Rank-deficient B 3x3
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  U = (0.0d0, 0.0d0); V = (0.0d0, 0.0d0); Q = (0.0d0, 0.0d0)
  A(1,1) = (2.0d0, 0.0d0); A(2,1) = (1.0d0, 0.5d0); A(3,1) = (0.0d0, 0.0d0)
  A(1,2) = (1.0d0, -0.5d0); A(2,2) = (3.0d0, 0.0d0); A(3,2) = (1.0d0, 0.5d0)
  A(1,3) = (0.0d0, 0.0d0); A(2,3) = (1.0d0, -0.5d0); A(3,3) = (4.0d0, 0.0d0)
  B(1,1) = (5.0d0, 0.0d0); B(2,1) = (1.0d0, 0.5d0); B(3,1) = (0.0d0, 0.0d0)
  B(1,2) = (1.0d0, -0.5d0); B(2,2) = (5.0d0, 0.0d0); B(3,2) = (0.0d0, 0.0d0)
  B(1,3) = (1.0d0, 0.5d0); B(2,3) = (1.0d0, -0.5d0); B(3,3) = (0.0d0, 0.0d0)
  call ZGGSVP3('U', 'V', 'Q', 3, 3, 3, A, MAXN, B, MAXN, tola, tolb, &
               K, L, U, MAXN, V, MAXN, Q, MAXN, IWORK, RWORK, TAU, WORK, lwork, info)
  call begin_test('rank_deficient_B')
  call print_int('info', info)
  call print_int('K', K)
  call print_int('L', L)
  call print_array('a', A_r, 2*MAXN*3)
  call print_array('b', B_r, 2*MAXN*3)
  call print_array('u', U_r, 2*MAXN*3)
  call print_array('v', V_r, 2*MAXN*3)
  call print_array('q', Q_r, 2*MAXN*3)
  call end_test()

  ! Test 4: Square 3x3 diagonal
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  U = (0.0d0, 0.0d0); V = (0.0d0, 0.0d0); Q = (0.0d0, 0.0d0)
  A(1,1) = (10.0d0, 0.0d0); A(2,2) = (5.0d0, 0.0d0); A(3,3) = (1.0d0, 0.0d0)
  B(1,1) = (8.0d0, 0.0d0); B(2,2) = (4.0d0, 0.0d0); B(3,3) = (2.0d0, 0.0d0)
  call ZGGSVP3('U', 'V', 'Q', 3, 3, 3, A, MAXN, B, MAXN, tola, tolb, &
               K, L, U, MAXN, V, MAXN, Q, MAXN, IWORK, RWORK, TAU, WORK, lwork, info)
  call begin_test('diagonal_3x3')
  call print_int('info', info)
  call print_int('K', K)
  call print_int('L', L)
  call print_array('a', A_r, 2*MAXN*3)
  call print_array('b', B_r, 2*MAXN*3)
  call print_array('u', U_r, 2*MAXN*3)
  call print_array('v', V_r, 2*MAXN*3)
  call print_array('q', Q_r, 2*MAXN*3)
  call end_test()

  ! Test 5: Wide matrix M=2 P=2 N=5
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  U = (0.0d0, 0.0d0); V = (0.0d0, 0.0d0); Q = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.5d0); A(2,1) = (2.0d0, 0.0d0)
  A(1,2) = (3.0d0, -0.5d0); A(2,2) = (4.0d0, 1.0d0)
  A(1,3) = (5.0d0, 0.0d0); A(2,3) = (6.0d0, -0.5d0)
  A(1,4) = (7.0d0, 0.5d0); A(2,4) = (8.0d0, 0.0d0)
  A(1,5) = (9.0d0, -0.5d0); A(2,5) = (10.0d0, 0.5d0)
  B(1,1) = (10.0d0, 0.0d0); B(2,1) = (1.0d0, 0.5d0)
  B(1,2) = (1.0d0, -0.5d0); B(2,2) = (10.0d0, 0.0d0)
  B(1,3) = (2.0d0, 0.0d0); B(2,3) = (2.0d0, 0.5d0)
  B(1,4) = (3.0d0, -0.5d0); B(2,4) = (3.0d0, 0.0d0)
  B(1,5) = (1.0d0, 0.5d0); B(2,5) = (1.0d0, -0.5d0)
  call ZGGSVP3('U', 'V', 'Q', 2, 2, 5, A, MAXN, B, MAXN, tola, tolb, &
               K, L, U, MAXN, V, MAXN, Q, MAXN, IWORK, RWORK, TAU, WORK, lwork, info)
  call begin_test('wide_2x5_UVQ')
  call print_int('info', info)
  call print_int('K', K)
  call print_int('L', L)
  call print_array('a', A_r, 2*MAXN*5)
  call print_array('b', B_r, 2*MAXN*5)
  call print_array('u', U_r, 2*MAXN*2)
  call print_array('v', V_r, 2*MAXN*2)
  call print_array('q', Q_r, 2*MAXN*5)
  call end_test()

  ! Test 6: N=0 edge case
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  call ZGGSVP3('U', 'V', 'Q', 3, 2, 0, A, MAXN, B, MAXN, tola, tolb, &
               K, L, U, MAXN, V, MAXN, Q, 1, IWORK, RWORK, TAU, WORK, lwork, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call print_int('K', K)
  call print_int('L', L)
  call end_test()

  ! Test 7: M=0 edge case
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  B(1,1) = (5.0d0, 0.0d0); B(2,1) = (1.0d0, 0.5d0)
  B(1,2) = (1.0d0, -0.5d0); B(2,2) = (5.0d0, 0.0d0)
  call ZGGSVP3('U', 'V', 'Q', 0, 2, 2, A, 1, B, MAXN, tola, tolb, &
               K, L, U, 1, V, MAXN, Q, MAXN, IWORK, RWORK, TAU, WORK, lwork, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call print_int('K', K)
  call print_int('L', L)
  call end_test()

  ! Test 8: Tall B M=3 P=5 N=3 UVQ
  A = (0.0d0, 0.0d0); B = (0.0d0, 0.0d0)
  U = (0.0d0, 0.0d0); V = (0.0d0, 0.0d0); Q = (0.0d0, 0.0d0)
  A(1,1) = (2.0d0, 0.0d0); A(2,1) = (1.0d0, 0.5d0); A(3,1) = (0.5d0, 0.0d0)
  A(1,2) = (0.5d0, -0.5d0); A(2,2) = (3.0d0, 0.0d0); A(3,2) = (1.0d0, 0.5d0)
  A(1,3) = (1.0d0, 0.0d0); A(2,3) = (0.5d0, -0.5d0); A(3,3) = (4.0d0, 0.0d0)
  B(1,1) = (10.0d0, 0.0d0); B(2,1) = (1.0d0, 0.5d0); B(3,1) = (1.0d0, -0.5d0); B(4,1) = (1.0d0, 0.0d0); B(5,1) = (1.0d0, 0.5d0)
  B(1,2) = (1.0d0, -0.5d0); B(2,2) = (10.0d0, 0.0d0); B(3,2) = (1.0d0, 0.5d0); B(4,2) = (1.0d0, -0.5d0); B(5,2) = (1.0d0, 0.0d0)
  B(1,3) = (1.0d0, 0.5d0); B(2,3) = (1.0d0, -0.5d0); B(3,3) = (10.0d0, 0.0d0); B(4,3) = (1.0d0, 0.5d0); B(5,3) = (1.0d0, -0.5d0)
  call ZGGSVP3('U', 'V', 'Q', 3, 5, 3, A, MAXN, B, MAXN, tola, tolb, &
               K, L, U, MAXN, V, MAXN, Q, MAXN, IWORK, RWORK, TAU, WORK, lwork, info)
  call begin_test('tall_B_3x5x3')
  call print_int('info', info)
  call print_int('K', K)
  call print_int('L', L)
  call print_array('a', A_r, 2*MAXN*3)
  call print_array('b', B_r, 2*MAXN*3)
  call print_array('u', U_r, 2*MAXN*3)
  call print_array('v', V_r, 2*MAXN*5)
  call print_array('q', Q_r, 2*MAXN*3)
  call end_test()

end program
