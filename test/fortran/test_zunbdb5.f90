program test_zunbdb5
  use test_utils
  implicit none

  ! Workspace and arrays sized to fit the largest test
  integer, parameter :: NMAX = 8
  complex*16 :: X1(NMAX), X2(NMAX)
  complex*16 :: Q1(NMAX, NMAX), Q2(NMAX, NMAX)
  complex*16 :: WORK(NMAX)
  double precision :: X1_r(2*NMAX), X2_r(2*NMAX)
  equivalence (X1, X1_r)
  equivalence (X2, X2_r)
  integer :: m1, m2, n, info, i, j
  complex*16, parameter :: ZERO = (0.0d0, 0.0d0)
  double precision :: sq2

  sq2 = 1.0d0 / sqrt(2.0d0)

  ! ---------------------------------------------------------------------
  ! Test 1: basic — M1=4, M2=4, N=2; Q has orthonormal columns built from
  ! pairs of standard basis vectors (top half = e_i, bottom half = e_i),
  ! divided by sqrt(2). X is well outside range(Q), so the first
  ! projection succeeds and the routine returns.
  ! ---------------------------------------------------------------------
  m1 = 4
  m2 = 4
  n  = 2
  do j = 1, NMAX
     do i = 1, NMAX
        Q1(i,j) = ZERO
        Q2(i,j) = ZERO
     end do
  end do
  Q1(1,1) = (1.0d0, 0.0d0) * sq2
  Q2(1,1) = (1.0d0, 0.0d0) * sq2
  Q1(2,2) = (1.0d0, 0.0d0) * sq2
  Q2(2,2) = (1.0d0, 0.0d0) * sq2

  do i = 1, NMAX
     X1(i) = ZERO
     X2(i) = ZERO
  end do
  X1(1) = (1.0d0, 1.0d0); X1(2) = (2.0d0, 2.0d0); X1(3) = (3.0d0, 3.0d0); X1(4) = (4.0d0, 4.0d0)
  X2(1) = (5.0d0, 5.0d0); X2(2) = (6.0d0, 6.0d0); X2(3) = (7.0d0, 7.0d0); X2(4) = (8.0d0, 8.0d0)
  do i = 1, NMAX
     WORK(i) = ZERO
  end do
  call ZUNBDB5(m1, m2, n, X1, 1, X2, 1, Q1, NMAX, Q2, NMAX, WORK, NMAX, info)
  call begin_test('basic_4x4_n2')
  call print_int('info', info)
  call print_array('X1', X1_r, 2*m1)
  call print_array('X2', X2_r, 2*m2)
  call end_test()

  ! ---------------------------------------------------------------------
  ! Test 2: N=0 (no Q columns) — X is trivially in the orthogonal
  ! complement, so it is just normalized to unit length.
  ! ---------------------------------------------------------------------
  m1 = 3
  m2 = 3
  n  = 0
  do i = 1, NMAX
     X1(i) = ZERO
     X2(i) = ZERO
  end do
  X1(1) = (1.0d0, 0.0d0); X1(2) = (2.0d0, 0.0d0); X1(3) = (3.0d0, 0.0d0)
  X2(1) = (4.0d0, 0.0d0); X2(2) = (5.0d0, 0.0d0); X2(3) = (6.0d0, 0.0d0)
  call ZUNBDB5(m1, m2, n, X1, 1, X2, 1, Q1, max(1,m1), Q2, max(1,m2), WORK, max(1,n), info)
  call begin_test('n_zero')
  call print_int('info', info)
  call print_array('X1', X1_r, 2*m1)
  call print_array('X2', X2_r, 2*m2)
  call end_test()

  ! ---------------------------------------------------------------------
  ! Test 3: X is exactly zero — NORM .LE. N*EPS, so we fall through to
  ! the standard-basis search. Q = identity-like with cols e_1, e_2
  ! (from M1 part); e_3 is orthogonal to Q, so X1=e_3 is returned.
  ! ---------------------------------------------------------------------
  m1 = 4
  m2 = 4
  n  = 2
  do j = 1, NMAX
     do i = 1, NMAX
        Q1(i,j) = ZERO
        Q2(i,j) = ZERO
     end do
  end do
  Q1(1,1) = (1.0d0, 0.0d0)
  Q1(2,2) = (1.0d0, 0.0d0)
  do i = 1, NMAX
     X1(i) = ZERO
     X2(i) = ZERO
  end do
  call ZUNBDB5(m1, m2, n, X1, 1, X2, 1, Q1, NMAX, Q2, NMAX, WORK, NMAX, info)
  call begin_test('zero_x_finds_e3_in_x1')
  call print_int('info', info)
  call print_array('X1', X1_r, 2*m1)
  call print_array('X2', X2_r, 2*m2)
  call end_test()

  ! ---------------------------------------------------------------------
  ! Test 4: Force the M2 search path. m1=N=2; Q spans the entire X1
  ! partition. Every e_i in the X1 partition is in range(Q), so the
  ! routine falls through to the X2 search and returns e_1 of X2.
  ! ---------------------------------------------------------------------
  m1 = 2
  m2 = 3
  n  = 2
  do j = 1, NMAX
     do i = 1, NMAX
        Q1(i,j) = ZERO
        Q2(i,j) = ZERO
     end do
  end do
  Q1(1,1) = (1.0d0, 0.0d0)
  Q1(2,2) = (1.0d0, 0.0d0)
  do i = 1, NMAX
     X1(i) = ZERO
     X2(i) = ZERO
  end do
  call ZUNBDB5(m1, m2, n, X1, 1, X2, 1, Q1, NMAX, Q2, NMAX, WORK, NMAX, info)
  call begin_test('zero_x_finds_e_first_in_x2')
  call print_int('info', info)
  call print_array('X1', X1_r, 2*m1)
  call print_array('X2', X2_r, 2*m2)
  call end_test()

  ! ---------------------------------------------------------------------
  ! Test 5: M1=0 (X1 partition empty). Q1 has zero rows; entire vector
  ! lives in X2.
  ! ---------------------------------------------------------------------
  m1 = 0
  m2 = 4
  n  = 2
  do j = 1, NMAX
     do i = 1, NMAX
        Q1(i,j) = ZERO
        Q2(i,j) = ZERO
     end do
  end do
  Q2(1,1) = (1.0d0, 0.0d0)
  Q2(2,2) = (1.0d0, 0.0d0)
  do i = 1, NMAX
     X1(i) = ZERO
     X2(i) = ZERO
  end do
  X2(1) = (1.0d0, 0.0d0); X2(2) = (2.0d0, 0.0d0); X2(3) = (3.0d0, 0.0d0); X2(4) = (4.0d0, 0.0d0)
  call ZUNBDB5(m1, m2, n, X1, 1, X2, 1, Q1, max(1,m1), Q2, NMAX, WORK, NMAX, info)
  call begin_test('m1_zero')
  call print_int('info', info)
  call print_array('X2', X2_r, 2*m2)
  call end_test()

  ! ---------------------------------------------------------------------
  ! Test 6: M2=0 (X2 partition empty). Entire vector lives in X1.
  ! ---------------------------------------------------------------------
  m1 = 4
  m2 = 0
  n  = 2
  do j = 1, NMAX
     do i = 1, NMAX
        Q1(i,j) = ZERO
        Q2(i,j) = ZERO
     end do
  end do
  Q1(1,1) = (1.0d0, 0.0d0)
  Q1(2,2) = (1.0d0, 0.0d0)
  do i = 1, NMAX
     X1(i) = ZERO
     X2(i) = ZERO
  end do
  X1(1) = (1.0d0, 0.0d0); X1(2) = (2.0d0, 0.0d0); X1(3) = (3.0d0, 0.0d0); X1(4) = (4.0d0, 0.0d0)
  call ZUNBDB5(m1, m2, n, X1, 1, X2, 1, Q1, NMAX, Q2, max(1,m2), WORK, NMAX, info)
  call begin_test('m2_zero')
  call print_int('info', info)
  call print_array('X1', X1_r, 2*m1)
  call end_test()

  ! ---------------------------------------------------------------------
  ! Test 7: non-unit incx — pack X1, X2 with stride 2.
  ! ---------------------------------------------------------------------
  m1 = 3
  m2 = 3
  n  = 2
  do j = 1, NMAX
     do i = 1, NMAX
        Q1(i,j) = ZERO
        Q2(i,j) = ZERO
     end do
  end do
  Q1(1,1) = (1.0d0, 0.0d0)
  Q2(1,2) = (1.0d0, 0.0d0)
  do i = 1, NMAX
     X1(i) = ZERO
     X2(i) = ZERO
  end do
  X1(1) = (1.0d0, 0.0d0); X1(3) = (2.0d0, 0.0d0); X1(5) = (3.0d0, 0.0d0)
  X2(1) = (4.0d0, 0.0d0); X2(3) = (5.0d0, 0.0d0); X2(5) = (6.0d0, 0.0d0)
  call ZUNBDB5(m1, m2, n, X1, 2, X2, 2, Q1, NMAX, Q2, NMAX, WORK, NMAX, info)
  call begin_test('stride_2')
  call print_int('info', info)
  call print_array('X1', X1_r, 2*(2*m1))
  call print_array('X2', X2_r, 2*(2*m2))
  call end_test()

  ! ---------------------------------------------------------------------
  ! Test 8: M1=1, M2=1, N=1. Q=[(1,0); (0,0)]; X=[(3,4); (5,6)]; X is not in
  ! range(Q), so the routine scales then projects.
  ! ---------------------------------------------------------------------
  m1 = 1
  m2 = 1
  n  = 1
  do j = 1, NMAX
     do i = 1, NMAX
        Q1(i,j) = ZERO
        Q2(i,j) = ZERO
     end do
  end do
  Q1(1,1) = (1.0d0, 0.0d0)
  Q2(1,1) = (0.0d0, 0.0d0)
  do i = 1, NMAX
     X1(i) = ZERO
     X2(i) = ZERO
  end do
  X1(1) = (3.0d0, 4.0d0)
  X2(1) = (5.0d0, 6.0d0)
  call ZUNBDB5(m1, m2, n, X1, 1, X2, 1, Q1, NMAX, Q2, NMAX, WORK, NMAX, info)
  call begin_test('m1_m2_n_one')
  call print_int('info', info)
  call print_array('X1', X1_r, 2*m1)
  call print_array('X2', X2_r, 2*m2)
  call end_test()

  ! ---------------------------------------------------------------------
  ! Test 9: Fall-through to standard-basis path with an X that isn't
  ! literally zero, but whose norm is below N*EPS. Use very tiny X so
  ! NORM .LE. N*EPS triggers (skip the first scaled projection).
  ! ---------------------------------------------------------------------
  m1 = 3
  m2 = 3
  n  = 2
  do j = 1, NMAX
     do i = 1, NMAX
        Q1(i,j) = ZERO
        Q2(i,j) = ZERO
     end do
  end do
  Q1(1,1) = (1.0d0, 0.0d0)
  Q1(2,2) = (1.0d0, 0.0d0)
  do i = 1, NMAX
     X1(i) = ZERO
     X2(i) = ZERO
  end do
  X1(1) = (1.0d-300, 0.0d0)
  X1(2) = (1.0d-300, 0.0d0)
  call ZUNBDB5(m1, m2, n, X1, 1, X2, 1, Q1, NMAX, Q2, NMAX, WORK, NMAX, info)
  call begin_test('tiny_x_falls_through_to_basis_search')
  call print_int('info', info)
  call print_array('X1', X1_r, 2*m1)
  call print_array('X2', X2_r, 2*m2)
  call end_test()

  ! ---------------------------------------------------------------------
  ! Test 10: Genuinely complex Q (Q^H differs from Q^T) — exercises the
  ! complex conjugate-transpose path through zunbdb6.
  ! Q = [(0.5, 0.5); (0.5, -0.5); 0; 0] is unit-length (|z|^2 = 0.5 each).
  ! X = [(1, 0); (0, 1) | (2, 1); (1, -1)] — well outside range(Q).
  ! ---------------------------------------------------------------------
  m1 = 2
  m2 = 2
  n  = 1
  do j = 1, NMAX
     do i = 1, NMAX
        Q1(i,j) = ZERO
        Q2(i,j) = ZERO
     end do
  end do
  Q1(1,1) = (0.5d0, 0.5d0)
  Q1(2,1) = (0.5d0, -0.5d0)
  Q2(1,1) = (0.0d0, 0.0d0)
  Q2(2,1) = (0.0d0, 0.0d0)
  do i = 1, NMAX
     X1(i) = ZERO
     X2(i) = ZERO
  end do
  X1(1) = (1.0d0, 0.0d0)
  X1(2) = (0.0d0, 1.0d0)
  X2(1) = (2.0d0, 1.0d0)
  X2(2) = (1.0d0, -1.0d0)
  call ZUNBDB5(m1, m2, n, X1, 1, X2, 1, Q1, NMAX, Q2, NMAX, WORK, NMAX, info)
  call begin_test('complex_q_one_col')
  call print_int('info', info)
  call print_array('X1', X1_r, 2*m1)
  call print_array('X2', X2_r, 2*m2)
  call end_test()

end program
