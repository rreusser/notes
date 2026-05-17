program test_zunbdb6
  use test_utils
  implicit none

  ! Workspace and arrays sized to fit the largest test (NMAX=8 -> tests use N up to 4)
  ! Using NMAX=8 with EQUIVALENCE prints the FULL Q (including unused trailing entries)
  ! per the leading-dimension stride; so we keep our test sizes equal to the declared
  ! storage to avoid the LDA-vs-EQUIVALENCE pitfall. We only print X (1D vectors), not Q.
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
  ! divided by sqrt(2). Q is real-valued in this case but the projection
  ! still uses Q^H (= Q^T since Q is real).
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

  ! X = [(1,1), (2,2), (3,3), (4,4) | (5,5), (6,6), (7,7), (8,8)]
  do i = 1, NMAX
     X1(i) = ZERO
     X2(i) = ZERO
  end do
  X1(1) = (1.0d0, 1.0d0); X1(2) = (2.0d0, 2.0d0); X1(3) = (3.0d0, 3.0d0); X1(4) = (4.0d0, 4.0d0)
  X2(1) = (5.0d0, 5.0d0); X2(2) = (6.0d0, 6.0d0); X2(3) = (7.0d0, 7.0d0); X2(4) = (8.0d0, 8.0d0)
  do i = 1, NMAX
     WORK(i) = ZERO
  end do
  call ZUNBDB6(m1, m2, n, X1, 1, X2, 1, Q1, NMAX, Q2, NMAX, WORK, NMAX, info)
  call begin_test('basic_4x4_n2')
  call print_int('info', info)
  call print_array('X1', X1_r, 2*m1)
  call print_array('X2', X2_r, 2*m2)
  call end_test()

  ! ---------------------------------------------------------------------
  ! Test 2: N=0 (no projection — vector is already in orthogonal complement)
  ! ---------------------------------------------------------------------
  m1 = 3
  m2 = 3
  n  = 0
  do i = 1, NMAX
     X1(i) = ZERO
     X2(i) = ZERO
  end do
  X1(1) = (1.0d0, -1.0d0); X1(2) = (2.0d0, -2.0d0); X1(3) = (3.0d0, -3.0d0)
  X2(1) = (4.0d0, -4.0d0); X2(2) = (5.0d0, -5.0d0); X2(3) = (6.0d0, -6.0d0)
  call ZUNBDB6(m1, m2, n, X1, 1, X2, 1, Q1, NMAX, Q2, NMAX, WORK, NMAX, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call print_array('X1', X1_r, 2*m1)
  call print_array('X2', X2_r, 2*m2)
  call end_test()

  ! ---------------------------------------------------------------------
  ! Test 3: M1=0 (top half is empty — entire vector in X2)
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
  X2(1) = (1.0d0, 1.0d0); X2(2) = (2.0d0, 2.0d0); X2(3) = (3.0d0, 3.0d0); X2(4) = (4.0d0, 4.0d0)
  call ZUNBDB6(m1, m2, n, X1, 1, X2, 1, Q1, max(1,m1), Q2, NMAX, WORK, NMAX, info)
  call begin_test('m1_zero')
  call print_int('info', info)
  call print_array('X2', X2_r, 2*m2)
  call end_test()

  ! ---------------------------------------------------------------------
  ! Test 4: M2=0 (bottom half is empty — entire vector in X1)
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
  X1(1) = (1.0d0, 1.0d0); X1(2) = (2.0d0, 2.0d0); X1(3) = (3.0d0, 3.0d0); X1(4) = (4.0d0, 4.0d0)
  call ZUNBDB6(m1, m2, n, X1, 1, X2, 1, Q1, NMAX, Q2, max(1,m2), WORK, NMAX, info)
  call begin_test('m2_zero')
  call print_int('info', info)
  call print_array('X1', X1_r, 2*m1)
  call end_test()

  ! ---------------------------------------------------------------------
  ! Test 5: X already lies entirely in range(Q) — projection is exact zero
  ! Using same Q as test 1; X = Q(:,1) + 2*Q(:,2)
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
  ! X = 1 * Q(:,1) + 2 * Q(:,2)
  do i = 1, NMAX
     X1(i) = ZERO
     X2(i) = ZERO
  end do
  X1(1) = (1.0d0, 0.0d0) * sq2
  X1(2) = (2.0d0, 0.0d0) * sq2
  X2(1) = (1.0d0, 0.0d0) * sq2
  X2(2) = (2.0d0, 0.0d0) * sq2
  call ZUNBDB6(m1, m2, n, X1, 1, X2, 1, Q1, NMAX, Q2, NMAX, WORK, NMAX, info)
  call begin_test('x_in_range_truncated_to_zero')
  call print_int('info', info)
  call print_array('X1', X1_r, 2*m1)
  call print_array('X2', X2_r, 2*m2)
  call end_test()

  ! ---------------------------------------------------------------------
  ! Test 6: non-unit incx — pack X with stride 2 and test
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
  ! X with stride 2: X1 = [(1,1), _, (2,2), _, (3,3), _], X2 = [(4,4), _, (5,5), _, (6,6), _]
  do i = 1, NMAX
     X1(i) = ZERO
     X2(i) = ZERO
  end do
  X1(1) = (1.0d0, 1.0d0); X1(3) = (2.0d0, 2.0d0); X1(5) = (3.0d0, 3.0d0)
  X2(1) = (4.0d0, 4.0d0); X2(3) = (5.0d0, 5.0d0); X2(5) = (6.0d0, 6.0d0)
  call ZUNBDB6(m1, m2, n, X1, 2, X2, 2, Q1, NMAX, Q2, NMAX, WORK, NMAX, info)
  call begin_test('stride_2')
  call print_int('info', info)
  call print_array('X1', X1_r, 2*(2*m1))
  call print_array('X2', X2_r, 2*(2*m2))
  call end_test()

  ! ---------------------------------------------------------------------
  ! Test 7: M1=1, M2=1, N=1 — small case, x not in range(Q)
  ! Q = [(1,0); (0,0)] (orthonormal) — Q1=(1,0), Q2=(0,0)
  ! X = [(3,4); (5,6)]; expected projection: dot(Q^H, X) = (1-0i)*(3+4i) + 0 = (3+4i)
  ! X - (3+4i)*Q = [(3+4i) - (3+4i); (5+6i) - 0] = [(0,0); (5,6)]
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
  call ZUNBDB6(m1, m2, n, X1, 1, X2, 1, Q1, NMAX, Q2, NMAX, WORK, NMAX, info)
  call begin_test('m1_m2_n_one')
  call print_int('info', info)
  call print_array('X1', X1_r, 2*m1)
  call print_array('X2', X2_r, 2*m2)
  call end_test()

  ! ---------------------------------------------------------------------
  ! Test 8: Reorthogonalization path — vector with most magnitude in
  ! range(Q). After first projection the new norm is small relative to
  ! original norm so the second pass runs and the vector survives.
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
  X1(1) = (1.0d0, 0.0d0) * sq2
  X1(2) = (0.5d0, 0.0d0) * sq2
  X1(3) = (0.1d0, 0.0d0)
  X1(4) = (0.0d0, 0.0d0)
  X2(1) = (1.0d0, 0.0d0) * sq2
  X2(2) = (0.5d0, 0.0d0) * sq2
  X2(3) = (0.0d0, 0.0d0)
  X2(4) = (0.2d0, 0.0d0)
  call ZUNBDB6(m1, m2, n, X1, 1, X2, 1, Q1, NMAX, Q2, NMAX, WORK, NMAX, info)
  call begin_test('reortho_residual_survives')
  call print_int('info', info)
  call print_array('X1', X1_r, 2*m1)
  call print_array('X2', X2_r, 2*m2)
  call end_test()

  ! ---------------------------------------------------------------------
  ! Test 9: Genuinely complex Q (Q^H not equal to Q^T) — single column,
  ! Q = [(1+i)/2; (1-i)/2; (1+i)/2; (1-i)/2] (norm = sqrt(4*0.5) = sqrt(2))
  ! Normalize: divide by sqrt(2), so Q is unit-length.
  ! Use M1=M2=2, N=1, nontrivial complex X.
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
  ! Q = [(1+i)/2; (1-i)/2; (1+i)/2; (1-i)/2] — already unit length
  ! |Q|^2 = (1/2)^2 + (1/2)^2 + ... = 4 * 0.5 / 4 = 0.5; need 1, so use 1/sqrt(2)
  ! Actually each entry has |z|=sqrt(0.5)/... let me just use unit-norm direct values.
  ! Q1(1)=(0.5, 0.5), |z|^2 = 0.5; Q1(2)=(0.5,-0.5), |z|^2=0.5; sum so far = 1.0
  ! Q2(1)=(0.0, 0.0), Q2(2)=(0.0, 0.0). So Q is unit-length.
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
  call ZUNBDB6(m1, m2, n, X1, 1, X2, 1, Q1, NMAX, Q2, NMAX, WORK, NMAX, info)
  call begin_test('complex_q_one_col')
  call print_int('info', info)
  call print_array('X1', X1_r, 2*m1)
  call print_array('X2', X2_r, 2*m2)
  call end_test()

end program
