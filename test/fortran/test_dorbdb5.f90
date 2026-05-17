program test_dorbdb5
  use test_utils
  implicit none

  ! Workspace and arrays sized to fit the largest test
  integer, parameter :: NMAX = 8
  double precision :: X1(NMAX), X2(NMAX)
  double precision :: Q1(NMAX, NMAX), Q2(NMAX, NMAX)
  double precision :: WORK(NMAX)
  integer :: m1, m2, n, info, i, j

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
        Q1(i,j) = 0.0d0
        Q2(i,j) = 0.0d0
     end do
  end do
  Q1(1,1) = 1.0d0 / sqrt(2.0d0)
  Q2(1,1) = 1.0d0 / sqrt(2.0d0)
  Q1(2,2) = 1.0d0 / sqrt(2.0d0)
  Q2(2,2) = 1.0d0 / sqrt(2.0d0)

  X1(1) = 1.0d0; X1(2) = 2.0d0; X1(3) = 3.0d0; X1(4) = 4.0d0
  X2(1) = 5.0d0; X2(2) = 6.0d0; X2(3) = 7.0d0; X2(4) = 8.0d0
  do i = 1, NMAX
     WORK(i) = 0.0d0
  end do
  call DORBDB5(m1, m2, n, X1, 1, X2, 1, Q1, NMAX, Q2, NMAX, WORK, NMAX, info)
  call begin_test('basic_4x4_n2')
  call print_int('info', info)
  call print_array('X1', X1, m1)
  call print_array('X2', X2, m2)
  call end_test()

  ! ---------------------------------------------------------------------
  ! Test 2: N=0 (no Q columns) — X is trivially in the orthogonal
  ! complement, so it is just normalized to unit length.
  ! ---------------------------------------------------------------------
  m1 = 3
  m2 = 3
  n  = 0
  X1(1) = 1.0d0; X1(2) = 2.0d0; X1(3) = 3.0d0
  X2(1) = 4.0d0; X2(2) = 5.0d0; X2(3) = 6.0d0
  call DORBDB5(m1, m2, n, X1, 1, X2, 1, Q1, max(1,m1), Q2, max(1,m2), WORK, max(1,n), info)
  call begin_test('n_zero')
  call print_int('info', info)
  call print_array('X1', X1, m1)
  call print_array('X2', X2, m2)
  call end_test()

  ! ---------------------------------------------------------------------
  ! Test 3: X is exactly zero — NORM .LE. N*EPS, so we fall through to
  ! the standard-basis search. e_1 is not in range(Q), so X1=e_1 path
  ! returns nonzero residual.
  ! Q = identity-like with cols e_1, e_2 (from M1 part).
  ! e_1 is fully aligned with Q(:,1), so it gets zeroed; e_2 with Q(:,2);
  ! continue to e_3 (in M1) which is orthogonal to Q.
  ! ---------------------------------------------------------------------
  m1 = 4
  m2 = 4
  n  = 2
  do j = 1, NMAX
     do i = 1, NMAX
        Q1(i,j) = 0.0d0
        Q2(i,j) = 0.0d0
     end do
  end do
  Q1(1,1) = 1.0d0
  Q1(2,2) = 1.0d0
  ! X starts as zero
  X1 = 0.0d0
  X2 = 0.0d0
  call DORBDB5(m1, m2, n, X1, 1, X2, 1, Q1, NMAX, Q2, NMAX, WORK, NMAX, info)
  call begin_test('zero_x_finds_e3_in_x1')
  call print_int('info', info)
  call print_array('X1', X1, m1)
  call print_array('X2', X2, m2)
  call end_test()

  ! ---------------------------------------------------------------------
  ! Test 4: Force the M2 search path. Set Q so its columns span the
  ! entire X1 partition (Q(:,1)=e_1 of M1 part, Q(:,2)=e_2, ...,
  ! Q(:,M1)=e_M1). With M1=N, every standard basis vector e_1..e_M1 in
  ! the X1 partition is fully in range(Q). The routine then tries
  ! e_(M1+1) (i.e. X2(1)=1) which is orthogonal — that returns.
  ! ---------------------------------------------------------------------
  m1 = 2
  m2 = 3
  n  = 2
  do j = 1, NMAX
     do i = 1, NMAX
        Q1(i,j) = 0.0d0
        Q2(i,j) = 0.0d0
     end do
  end do
  ! Q(:,1) = e_1 of M1 part; Q(:,2) = e_2 of M1 part
  Q1(1,1) = 1.0d0
  Q1(2,2) = 1.0d0
  X1 = 0.0d0
  X2 = 0.0d0
  call DORBDB5(m1, m2, n, X1, 1, X2, 1, Q1, NMAX, Q2, NMAX, WORK, NMAX, info)
  call begin_test('zero_x_finds_e_first_in_x2')
  call print_int('info', info)
  call print_array('X1', X1, m1)
  call print_array('X2', X2, m2)
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
        Q1(i,j) = 0.0d0
        Q2(i,j) = 0.0d0
     end do
  end do
  Q2(1,1) = 1.0d0
  Q2(2,2) = 1.0d0
  X2(1) = 1.0d0; X2(2) = 2.0d0; X2(3) = 3.0d0; X2(4) = 4.0d0
  call DORBDB5(m1, m2, n, X1, 1, X2, 1, Q1, max(1,m1), Q2, NMAX, WORK, NMAX, info)
  call begin_test('m1_zero')
  call print_int('info', info)
  call print_array('X2', X2, m2)
  call end_test()

  ! ---------------------------------------------------------------------
  ! Test 6: M2=0 (X2 partition empty). Entire vector lives in X1.
  ! ---------------------------------------------------------------------
  m1 = 4
  m2 = 0
  n  = 2
  do j = 1, NMAX
     do i = 1, NMAX
        Q1(i,j) = 0.0d0
        Q2(i,j) = 0.0d0
     end do
  end do
  Q1(1,1) = 1.0d0
  Q1(2,2) = 1.0d0
  X1(1) = 1.0d0; X1(2) = 2.0d0; X1(3) = 3.0d0; X1(4) = 4.0d0
  call DORBDB5(m1, m2, n, X1, 1, X2, 1, Q1, NMAX, Q2, max(1,m2), WORK, NMAX, info)
  call begin_test('m2_zero')
  call print_int('info', info)
  call print_array('X1', X1, m1)
  call end_test()

  ! ---------------------------------------------------------------------
  ! Test 7: non-unit incx — pack X1, X2 with stride 2.
  ! ---------------------------------------------------------------------
  m1 = 3
  m2 = 3
  n  = 2
  do j = 1, NMAX
     do i = 1, NMAX
        Q1(i,j) = 0.0d0
        Q2(i,j) = 0.0d0
     end do
  end do
  Q1(1,1) = 1.0d0
  Q2(1,2) = 1.0d0
  X1 = 0.0d0; X2 = 0.0d0
  X1(1) = 1.0d0; X1(3) = 2.0d0; X1(5) = 3.0d0
  X2(1) = 4.0d0; X2(3) = 5.0d0; X2(5) = 6.0d0
  call DORBDB5(m1, m2, n, X1, 2, X2, 2, Q1, NMAX, Q2, NMAX, WORK, NMAX, info)
  call begin_test('stride_2')
  call print_int('info', info)
  call print_array('X1', X1, 2*m1)
  call print_array('X2', X2, 2*m2)
  call end_test()

  ! ---------------------------------------------------------------------
  ! Test 8: M1=1, M2=1, N=1. Q=[1; 0]; X=[3; 4]; X is not in range(Q),
  ! so the routine scales then projects: result is the X-norm-1 unit
  ! vector minus its projection onto Q.
  ! ---------------------------------------------------------------------
  m1 = 1
  m2 = 1
  n  = 1
  do j = 1, NMAX
     do i = 1, NMAX
        Q1(i,j) = 0.0d0
        Q2(i,j) = 0.0d0
     end do
  end do
  Q1(1,1) = 1.0d0
  Q2(1,1) = 0.0d0
  X1(1) = 3.0d0
  X2(1) = 4.0d0
  call DORBDB5(m1, m2, n, X1, 1, X2, 1, Q1, NMAX, Q2, NMAX, WORK, NMAX, info)
  call begin_test('m1_m2_n_one')
  call print_int('info', info)
  call print_array('X1', X1, m1)
  call print_array('X2', X2, m2)
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
        Q1(i,j) = 0.0d0
        Q2(i,j) = 0.0d0
     end do
  end do
  Q1(1,1) = 1.0d0
  Q1(2,2) = 1.0d0
  X1 = 0.0d0
  X2 = 0.0d0
  X1(1) = 1.0d-300
  X1(2) = 1.0d-300
  call DORBDB5(m1, m2, n, X1, 1, X2, 1, Q1, NMAX, Q2, NMAX, WORK, NMAX, info)
  call begin_test('tiny_x_falls_through_to_basis_search')
  call print_int('info', info)
  call print_array('X1', X1, m1)
  call print_array('X2', X2, m2)
  call end_test()

end program
