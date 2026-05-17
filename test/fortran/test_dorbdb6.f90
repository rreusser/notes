program test_dorbdb6
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
  ! divided by sqrt(2).
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

  ! X = [1, 2, 3, 4 | 5, 6, 7, 8]
  X1(1) = 1.0d0; X1(2) = 2.0d0; X1(3) = 3.0d0; X1(4) = 4.0d0
  X2(1) = 5.0d0; X2(2) = 6.0d0; X2(3) = 7.0d0; X2(4) = 8.0d0
  do i = 1, NMAX
     WORK(i) = 0.0d0
  end do
  call DORBDB6(m1, m2, n, X1, 1, X2, 1, Q1, NMAX, Q2, NMAX, WORK, NMAX, info)
  call begin_test('basic_4x4_n2')
  call print_int('info', info)
  call print_array('X1', X1, m1)
  call print_array('X2', X2, m2)
  call end_test()

  ! ---------------------------------------------------------------------
  ! Test 2: N=0 (no projection — vector is already in orthogonal complement)
  ! ---------------------------------------------------------------------
  m1 = 3
  m2 = 3
  n  = 0
  X1(1) = 1.0d0; X1(2) = 2.0d0; X1(3) = 3.0d0
  X2(1) = 4.0d0; X2(2) = 5.0d0; X2(3) = 6.0d0
  call DORBDB6(m1, m2, n, X1, 1, X2, 1, Q1, NMAX, Q2, NMAX, WORK, NMAX, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call print_array('X1', X1, m1)
  call print_array('X2', X2, m2)
  call end_test()

  ! ---------------------------------------------------------------------
  ! Test 3: M1=0 (top half is empty — entire vector in X2)
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
  call DORBDB6(m1, m2, n, X1, 1, X2, 1, Q1, max(1,m1), Q2, NMAX, WORK, NMAX, info)
  call begin_test('m1_zero')
  call print_int('info', info)
  call print_array('X2', X2, m2)
  call end_test()

  ! ---------------------------------------------------------------------
  ! Test 4: M2=0 (bottom half is empty — entire vector in X1)
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
  call DORBDB6(m1, m2, n, X1, 1, X2, 1, Q1, NMAX, Q2, max(1,m2), WORK, NMAX, info)
  call begin_test('m2_zero')
  call print_int('info', info)
  call print_array('X1', X1, m1)
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
        Q1(i,j) = 0.0d0
        Q2(i,j) = 0.0d0
     end do
  end do
  Q1(1,1) = 1.0d0 / sqrt(2.0d0)
  Q2(1,1) = 1.0d0 / sqrt(2.0d0)
  Q1(2,2) = 1.0d0 / sqrt(2.0d0)
  Q2(2,2) = 1.0d0 / sqrt(2.0d0)
  ! X = 1 * Q(:,1) + 2 * Q(:,2)
  X1(1) = 1.0d0/sqrt(2.0d0); X1(2) = 2.0d0/sqrt(2.0d0); X1(3) = 0.0d0; X1(4) = 0.0d0
  X2(1) = 1.0d0/sqrt(2.0d0); X2(2) = 2.0d0/sqrt(2.0d0); X2(3) = 0.0d0; X2(4) = 0.0d0
  call DORBDB6(m1, m2, n, X1, 1, X2, 1, Q1, NMAX, Q2, NMAX, WORK, NMAX, info)
  call begin_test('x_in_range_truncated_to_zero')
  call print_int('info', info)
  call print_array('X1', X1, m1)
  call print_array('X2', X2, m2)
  call end_test()

  ! ---------------------------------------------------------------------
  ! Test 6: non-unit incx — pack X with stride 2 and test
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
  ! Q1(:,1) = e1 (top), Q2(:,1) = 0  -> column 1 is e1 in 6-vector
  ! Q1(:,2) = 0, Q2(:,2) = e1 (bottom)  -> column 2 is e_{m1+1} in 6-vector
  Q1(1,1) = 1.0d0
  Q2(1,2) = 1.0d0
  ! X with stride 2: X1 = [1, _, 2, _, 3, _], X2 = [4, _, 5, _, 6, _]
  X1 = 0.0d0; X2 = 0.0d0
  X1(1) = 1.0d0; X1(3) = 2.0d0; X1(5) = 3.0d0
  X2(1) = 4.0d0; X2(3) = 5.0d0; X2(5) = 6.0d0
  call DORBDB6(m1, m2, n, X1, 2, X2, 2, Q1, NMAX, Q2, NMAX, WORK, NMAX, info)
  call begin_test('stride_2')
  call print_int('info', info)
  call print_array('X1', X1, 2*m1 - 1 + 1)
  call print_array('X2', X2, 2*m2 - 1 + 1)
  call end_test()

  ! ---------------------------------------------------------------------
  ! Test 7: M1=1, M2=1, N=1 — small case, x not in range(Q)
  ! Q = [1; 0] / 1 (orthonormal) — Q1=1, Q2=0
  ! X = [3; 4]; expected projection: dot(Q,X) = 3, X - 3*Q = [0; 4]
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
  call DORBDB6(m1, m2, n, X1, 1, X2, 1, Q1, NMAX, Q2, NMAX, WORK, NMAX, info)
  call begin_test('m1_m2_n_one')
  call print_int('info', info)
  call print_array('X1', X1, m1)
  call print_array('X2', X2, m2)
  call end_test()

  ! ---------------------------------------------------------------------
  ! Test 8: Reorthogonalization path — vector with most magnitude in
  ! range(Q). After first projection the new norm is small relative to
  ! original norm so the second pass runs and the vector survives.
  ! Q is two columns of 1/sqrt(2) * [e_i; e_i] for i=1,2.
  ! X = Q(:,1)*1.0 + Q(:,2)*0.5 + epsilon * orthogonal residual
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
  ! Construct X = a1*Q(:,1) + a2*Q(:,2) + r where r is in orthogonal
  ! complement (any nonzero entries in rows 3,4 of X1 and X2 are orthogonal
  ! to Q because Q only has support in rows 1,2 in this construction).
  X1(1) = 1.0d0/sqrt(2.0d0)
  X1(2) = 0.5d0/sqrt(2.0d0)
  X1(3) = 0.1d0
  X1(4) = 0.0d0
  X2(1) = 1.0d0/sqrt(2.0d0)
  X2(2) = 0.5d0/sqrt(2.0d0)
  X2(3) = 0.0d0
  X2(4) = 0.2d0
  call DORBDB6(m1, m2, n, X1, 1, X2, 1, Q1, NMAX, Q2, NMAX, WORK, NMAX, info)
  call begin_test('reortho_residual_survives')
  call print_int('info', info)
  call print_array('X1', X1, m1)
  call print_array('X2', X2, m2)
  call end_test()

end program
