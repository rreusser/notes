program test_zlacp2
  use test_utils
  implicit none

  integer, parameter :: NMAX = 4

  double precision :: A(NMAX, NMAX)
  complex*16 :: B(NMAX, NMAX)
  double precision :: B_r(2*NMAX*NMAX)
  double precision :: Bpk_r(2*NMAX*NMAX)
  complex*16 :: Bpk(NMAX*NMAX)
  equivalence (Bpk, Bpk_r)

  integer :: i, j, M, N

  ! ----------------------------------------------------------------
  ! Test 1: full copy 3x3
  ! ----------------------------------------------------------------
  M = 3
  N = 3
  A = 0.0d0
  A(1,1) = 1.0d0; A(2,1) = 2.0d0; A(3,1) = 3.0d0
  A(1,2) = 4.0d0; A(2,2) = 5.0d0; A(3,2) = 6.0d0
  A(1,3) = 7.0d0; A(2,3) = 8.0d0; A(3,3) = 9.0d0

  B = (0.0d0, 0.0d0)
  call ZLACP2( 'A', M, N, A, NMAX, B, NMAX )

  ! Pack B into contiguous array for printing
  do j = 1, N
    do i = 1, M
      Bpk(i + (j-1)*M) = B(i, j)
    end do
  end do

  call begin_test('full_3x3')
  call print_int('M', M)
  call print_int('N', N)
  call print_matrix('A', A, NMAX, M, N)
  call print_array('B', Bpk_r, 2*M*N)
  call end_test()

  ! ----------------------------------------------------------------
  ! Test 2: upper copy 3x3
  ! ----------------------------------------------------------------
  B = (0.0d0, 0.0d0)
  call ZLACP2( 'U', M, N, A, NMAX, B, NMAX )

  do j = 1, N
    do i = 1, M
      Bpk(i + (j-1)*M) = B(i, j)
    end do
  end do

  call begin_test('upper_3x3')
  call print_int('M', M)
  call print_int('N', N)
  call print_matrix('A', A, NMAX, M, N)
  call print_array('B', Bpk_r, 2*M*N)
  call end_test()

  ! ----------------------------------------------------------------
  ! Test 3: lower copy 3x3
  ! ----------------------------------------------------------------
  B = (0.0d0, 0.0d0)
  call ZLACP2( 'L', M, N, A, NMAX, B, NMAX )

  do j = 1, N
    do i = 1, M
      Bpk(i + (j-1)*M) = B(i, j)
    end do
  end do

  call begin_test('lower_3x3')
  call print_int('M', M)
  call print_int('N', N)
  call print_matrix('A', A, NMAX, M, N)
  call print_array('B', Bpk_r, 2*M*N)
  call end_test()

  ! ----------------------------------------------------------------
  ! Test 4: full copy rectangular 2x4
  ! ----------------------------------------------------------------
  M = 2
  N = 4
  A = 0.0d0
  A(1,1) = 1.1d0; A(2,1) = 2.2d0
  A(1,2) = 3.3d0; A(2,2) = 4.4d0
  A(1,3) = 5.5d0; A(2,3) = 6.6d0
  A(1,4) = 7.7d0; A(2,4) = 8.8d0

  B = (0.0d0, 0.0d0)
  call ZLACP2( 'A', M, N, A, NMAX, B, NMAX )

  do j = 1, N
    do i = 1, M
      Bpk(i + (j-1)*M) = B(i, j)
    end do
  end do

  call begin_test('full_2x4')
  call print_int('M', M)
  call print_int('N', N)
  call print_matrix('A', A, NMAX, M, N)
  call print_array('B', Bpk_r, 2*M*N)
  call end_test()

  ! ----------------------------------------------------------------
  ! Test 5: upper copy rectangular 4x2
  ! ----------------------------------------------------------------
  M = 4
  N = 2
  A = 0.0d0
  A(1,1) = 1.0d0; A(2,1) = 2.0d0; A(3,1) = 3.0d0; A(4,1) = 4.0d0
  A(1,2) = 5.0d0; A(2,2) = 6.0d0; A(3,2) = 7.0d0; A(4,2) = 8.0d0

  B = (0.0d0, 0.0d0)
  call ZLACP2( 'U', M, N, A, NMAX, B, NMAX )

  do j = 1, N
    do i = 1, M
      Bpk(i + (j-1)*M) = B(i, j)
    end do
  end do

  call begin_test('upper_4x2')
  call print_int('M', M)
  call print_int('N', N)
  call print_matrix('A', A, NMAX, M, N)
  call print_array('B', Bpk_r, 2*M*N)
  call end_test()

  ! ----------------------------------------------------------------
  ! Test 6: lower copy rectangular 2x4
  ! ----------------------------------------------------------------
  M = 2
  N = 4
  A = 0.0d0
  A(1,1) = 1.0d0; A(2,1) = 2.0d0
  A(1,2) = 3.0d0; A(2,2) = 4.0d0
  A(1,3) = 5.0d0; A(2,3) = 6.0d0
  A(1,4) = 7.0d0; A(2,4) = 8.0d0

  B = (0.0d0, 0.0d0)
  call ZLACP2( 'L', M, N, A, NMAX, B, NMAX )

  do j = 1, N
    do i = 1, M
      Bpk(i + (j-1)*M) = B(i, j)
    end do
  end do

  call begin_test('lower_2x4')
  call print_int('M', M)
  call print_int('N', N)
  call print_matrix('A', A, NMAX, M, N)
  call print_array('B', Bpk_r, 2*M*N)
  call end_test()

  ! ----------------------------------------------------------------
  ! Test 7: edge case M=0
  ! ----------------------------------------------------------------
  M = 0
  N = 3
  B = (99.0d0, 99.0d0)
  call ZLACP2( 'A', M, N, A, NMAX, B, NMAX )

  ! B should be unchanged (all 99+99i)
  call begin_test('edge_m0')
  call print_int('M', M)
  call print_int('N', N)
  call end_test()

  ! ----------------------------------------------------------------
  ! Test 8: edge case N=0
  ! ----------------------------------------------------------------
  M = 3
  N = 0
  B = (99.0d0, 99.0d0)
  call ZLACP2( 'A', M, N, A, NMAX, B, NMAX )

  call begin_test('edge_n0')
  call print_int('M', M)
  call print_int('N', N)
  call end_test()

  ! ----------------------------------------------------------------
  ! Test 9: 1x1 full copy
  ! ----------------------------------------------------------------
  M = 1
  N = 1
  A(1,1) = 42.5d0
  B = (0.0d0, 0.0d0)
  call ZLACP2( 'A', M, N, A, NMAX, B, NMAX )

  Bpk(1) = B(1, 1)

  call begin_test('full_1x1')
  call print_int('M', M)
  call print_int('N', N)
  call print_scalar('A11', A(1,1))
  call print_array('B', Bpk_r, 2)
  call end_test()

end program
