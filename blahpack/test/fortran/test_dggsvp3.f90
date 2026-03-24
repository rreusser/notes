program test_dggsvp3
  use test_utils
  implicit none

  integer, parameter :: MAXN = 20
  double precision :: A(MAXN*MAXN), B(MAXN*MAXN)
  double precision :: U(MAXN*MAXN), V(MAXN*MAXN), Q(MAXN*MAXN)
  double precision :: TAU(MAXN), WORK(5000)
  integer :: IWORK(MAXN)
  integer :: info, K, L, lwork, i
  double precision :: tola, tolb
  double precision :: DLAMCH

  lwork = 5000
  tola = 1.0d-8
  tolb = 1.0d-8

  ! Test 1: Basic 4x3 A, 3x3 B with JOBU='U', JOBV='V', JOBQ='Q'
  ! A is 4x3, B is 3x3
  A = 0.0d0; B = 0.0d0; U = 0.0d0; V = 0.0d0; Q = 0.0d0
  ! A col-major: A(4,3) with LDA=4
  ! Column 1
  A(1) = 1.0d0; A(2) = 2.0d0; A(3) = 3.0d0; A(4) = 4.0d0
  ! Column 2
  A(5) = 5.0d0; A(6) = 6.0d0; A(7) = 7.0d0; A(8) = 8.0d0
  ! Column 3
  A(9) = 9.0d0; A(10) = 10.0d0; A(11) = 11.0d0; A(12) = 12.0d0
  ! B col-major: B(3,3) with LDB=3
  ! Column 1
  B(1) = 10.0d0; B(2) = 1.0d0; B(3) = 1.0d0
  ! Column 2
  B(4) = 1.0d0; B(5) = 10.0d0; B(6) = 1.0d0
  ! Column 3
  B(7) = 1.0d0; B(8) = 1.0d0; B(9) = 10.0d0
  call DGGSVP3('U', 'V', 'Q', 4, 3, 3, A, 4, B, 3, tola, tolb, &
               K, L, U, 4, V, 3, Q, 3, IWORK, TAU, WORK, lwork, info)
  call begin_test('basic_4x3_3x3_UVQ')
  call print_int('info', info)
  call print_int('K', K)
  call print_int('L', L)
  call print_matrix('A', A, 4, 4, 3)
  call print_matrix('B', B, 3, 3, 3)
  call print_matrix('U', U, 4, 4, 4)
  call print_matrix('V', V, 3, 3, 3)
  call print_matrix('Q', Q, 3, 3, 3)
  call end_test()

  ! Test 2: JOBU='N', JOBV='N', JOBQ='N' (no orthogonal matrices computed)
  A = 0.0d0; B = 0.0d0; U = 0.0d0; V = 0.0d0; Q = 0.0d0
  A(1) = 1.0d0; A(2) = 2.0d0; A(3) = 3.0d0; A(4) = 4.0d0
  A(5) = 5.0d0; A(6) = 6.0d0; A(7) = 7.0d0; A(8) = 8.0d0
  A(9) = 9.0d0; A(10) = 10.0d0; A(11) = 11.0d0; A(12) = 12.0d0
  B(1) = 10.0d0; B(2) = 1.0d0; B(3) = 1.0d0
  B(4) = 1.0d0; B(5) = 10.0d0; B(6) = 1.0d0
  B(7) = 1.0d0; B(8) = 1.0d0; B(9) = 10.0d0
  call DGGSVP3('N', 'N', 'N', 4, 3, 3, A, 4, B, 3, tola, tolb, &
               K, L, U, 1, V, 1, Q, 1, IWORK, TAU, WORK, lwork, info)
  call begin_test('basic_4x3_3x3_NNN')
  call print_int('info', info)
  call print_int('K', K)
  call print_int('L', L)
  call print_matrix('A', A, 4, 4, 3)
  call print_matrix('B', B, 3, 3, 3)
  call end_test()

  ! Test 3: M=0 edge case
  A = 0.0d0; B = 0.0d0; U = 0.0d0; V = 0.0d0; Q = 0.0d0
  B(1) = 5.0d0; B(2) = 1.0d0; B(3) = 1.0d0; B(4) = 5.0d0
  call DGGSVP3('U', 'V', 'Q', 0, 2, 2, A, 1, B, 2, tola, tolb, &
               K, L, U, 1, V, 2, Q, 2, IWORK, TAU, WORK, lwork, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call print_int('K', K)
  call print_int('L', L)
  call end_test()

  ! Test 4: N=0 edge case
  A = 0.0d0; B = 0.0d0; U = 0.0d0; V = 0.0d0; Q = 0.0d0
  call DGGSVP3('U', 'V', 'Q', 3, 2, 0, A, 3, B, 2, tola, tolb, &
               K, L, U, 3, V, 2, Q, 1, IWORK, TAU, WORK, lwork, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call print_int('K', K)
  call print_int('L', L)
  call end_test()

  ! Test 5: P=0 edge case
  A = 0.0d0; B = 0.0d0; U = 0.0d0; V = 0.0d0; Q = 0.0d0
  A(1) = 5.0d0; A(2) = 1.0d0; A(3) = 1.0d0; A(4) = 5.0d0
  call DGGSVP3('U', 'V', 'Q', 2, 0, 2, A, 2, B, 1, tola, tolb, &
               K, L, U, 2, V, 1, Q, 2, IWORK, TAU, WORK, lwork, info)
  call begin_test('p_zero')
  call print_int('info', info)
  call print_int('K', K)
  call print_int('L', L)
  call end_test()

  ! Test 6: Rank-deficient B (one zero row) 3x3 A, 3x3 B
  A = 0.0d0; B = 0.0d0; U = 0.0d0; V = 0.0d0; Q = 0.0d0
  ! A col-major 3x3 with LDA=3
  A(1) = 2.0d0; A(2) = 1.0d0; A(3) = 0.0d0
  A(4) = 1.0d0; A(5) = 3.0d0; A(6) = 1.0d0
  A(7) = 0.0d0; A(8) = 1.0d0; A(9) = 4.0d0
  ! B col-major 3x3 with LDB=3 - rank 2 (row 3 is zero)
  B(1) = 5.0d0; B(2) = 1.0d0; B(3) = 0.0d0
  B(4) = 1.0d0; B(5) = 5.0d0; B(6) = 0.0d0
  B(7) = 1.0d0; B(8) = 1.0d0; B(9) = 0.0d0
  call DGGSVP3('U', 'V', 'Q', 3, 3, 3, A, 3, B, 3, tola, tolb, &
               K, L, U, 3, V, 3, Q, 3, IWORK, TAU, WORK, lwork, info)
  call begin_test('rank_deficient_B')
  call print_int('info', info)
  call print_int('K', K)
  call print_int('L', L)
  call print_matrix('A', A, 3, 3, 3)
  call print_matrix('B', B, 3, 3, 3)
  call print_matrix('U', U, 3, 3, 3)
  call print_matrix('V', V, 3, 3, 3)
  call print_matrix('Q', Q, 3, 3, 3)
  call end_test()

  ! Test 7: Wide matrix: M=2, P=2, N=5 with UVQ
  A = 0.0d0; B = 0.0d0; U = 0.0d0; V = 0.0d0; Q = 0.0d0
  ! A col-major 2x5 with LDA=2
  A(1) = 1.0d0; A(2) = 2.0d0
  A(3) = 3.0d0; A(4) = 4.0d0
  A(5) = 5.0d0; A(6) = 6.0d0
  A(7) = 7.0d0; A(8) = 8.0d0
  A(9) = 9.0d0; A(10) = 10.0d0
  ! B col-major 2x5 with LDB=2
  B(1) = 10.0d0; B(2) = 1.0d0
  B(3) = 1.0d0; B(4) = 10.0d0
  B(5) = 2.0d0; B(6) = 2.0d0
  B(7) = 3.0d0; B(8) = 3.0d0
  B(9) = 1.0d0; B(10) = 1.0d0
  call DGGSVP3('U', 'V', 'Q', 2, 2, 5, A, 2, B, 2, tola, tolb, &
               K, L, U, 2, V, 2, Q, 5, IWORK, TAU, WORK, lwork, info)
  call begin_test('wide_2x5_UVQ')
  call print_int('info', info)
  call print_int('K', K)
  call print_int('L', L)
  call print_matrix('A', A, 2, 2, 5)
  call print_matrix('B', B, 2, 2, 5)
  call print_matrix('U', U, 2, 2, 2)
  call print_matrix('V', V, 2, 2, 2)
  call print_matrix('Q', Q, 5, 5, 5)
  call end_test()

  ! Test 8: Square 3x3, A and B both full rank, different tolerances
  A = 0.0d0; B = 0.0d0; U = 0.0d0; V = 0.0d0; Q = 0.0d0
  ! A = diag(10, 5, 1)
  A(1) = 10.0d0; A(2) = 0.0d0; A(3) = 0.0d0
  A(4) = 0.0d0; A(5) = 5.0d0; A(6) = 0.0d0
  A(7) = 0.0d0; A(8) = 0.0d0; A(9) = 1.0d0
  ! B = diag(8, 4, 2)
  B(1) = 8.0d0; B(2) = 0.0d0; B(3) = 0.0d0
  B(4) = 0.0d0; B(5) = 4.0d0; B(6) = 0.0d0
  B(7) = 0.0d0; B(8) = 0.0d0; B(9) = 2.0d0
  call DGGSVP3('U', 'V', 'Q', 3, 3, 3, A, 3, B, 3, tola, tolb, &
               K, L, U, 3, V, 3, Q, 3, IWORK, TAU, WORK, lwork, info)
  call begin_test('diagonal_3x3')
  call print_int('info', info)
  call print_int('K', K)
  call print_int('L', L)
  call print_matrix('A', A, 3, 3, 3)
  call print_matrix('B', B, 3, 3, 3)
  call print_matrix('U', U, 3, 3, 3)
  call print_matrix('V', V, 3, 3, 3)
  call print_matrix('Q', Q, 3, 3, 3)
  call end_test()

  ! Test 9: Tall B: M=3, P=5, N=3 with UVQ
  A = 0.0d0; B = 0.0d0; U = 0.0d0; V = 0.0d0; Q = 0.0d0
  ! A col-major 3x3 with LDA=3
  A(1) = 2.0d0; A(2) = 1.0d0; A(3) = 0.5d0
  A(4) = 0.5d0; A(5) = 3.0d0; A(6) = 1.0d0
  A(7) = 1.0d0; A(8) = 0.5d0; A(9) = 4.0d0
  ! B col-major 5x3 with LDB=5
  B(1) = 10.0d0; B(2) = 1.0d0; B(3) = 1.0d0; B(4) = 1.0d0; B(5) = 1.0d0
  B(6) = 1.0d0; B(7) = 10.0d0; B(8) = 1.0d0; B(9) = 1.0d0; B(10) = 1.0d0
  B(11) = 1.0d0; B(12) = 1.0d0; B(13) = 10.0d0; B(14) = 1.0d0; B(15) = 1.0d0
  call DGGSVP3('U', 'V', 'Q', 3, 5, 3, A, 3, B, 5, tola, tolb, &
               K, L, U, 3, V, 5, Q, 3, IWORK, TAU, WORK, lwork, info)
  call begin_test('tall_B_3x5x3')
  call print_int('info', info)
  call print_int('K', K)
  call print_int('L', L)
  call print_matrix('A', A, 3, 3, 3)
  call print_matrix('B', B, 5, 5, 3)
  call print_matrix('U', U, 3, 3, 3)
  call print_matrix('V', V, 5, 5, 5)
  call print_matrix('Q', Q, 3, 3, 3)
  call end_test()

end program
