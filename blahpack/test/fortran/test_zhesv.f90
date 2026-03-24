program test_zhesv
  use test_utils
  implicit none

  integer, parameter :: NMAX = 6, LWMAX = 256
  complex*16 :: A(NMAX, NMAX), B(NMAX, 3), WORK(LWMAX)
  double precision :: A_r(2*NMAX*NMAX), B_r(2*NMAX*3)
  equivalence (A, A_r)
  equivalence (B, B_r)
  integer :: IPIV(NMAX), INFO, n, nrhs

  ! Test 1: Upper, 4x4 Hermitian, 1 RHS
  n = 4
  nrhs = 1
  A = (0.0d0, 0.0d0)
  A(1,1) = (4.0d0, 0.0d0)
  A(1,2) = (1.0d0, 2.0d0)
  A(1,3) = (3.0d0, -1.0d0)
  A(1,4) = (0.5d0, 0.5d0)
  A(2,2) = (5.0d0, 0.0d0)
  A(2,3) = (2.0d0, 1.0d0)
  A(2,4) = (1.0d0, -2.0d0)
  A(3,3) = (7.0d0, 0.0d0)
  A(3,4) = (3.0d0, 0.0d0)
  A(4,4) = (6.0d0, 0.0d0)
  B(1,1) = (1.0d0, 0.0d0)
  B(2,1) = (2.0d0, 1.0d0)
  B(3,1) = (-1.0d0, 3.0d0)
  B(4,1) = (0.5d0, -0.5d0)
  call ZHESV('U', n, nrhs, A, NMAX, IPIV, B, NMAX, WORK, LWMAX, INFO)
  call begin_test('upper_4x4_1rhs')
  call print_int('n', n)
  call print_int('nrhs', nrhs)
  call print_int('info', INFO)
  call print_array('B', B_r, 2*n*nrhs)
  call print_int_array('ipiv', IPIV, n)
  call end_test()

  ! Test 2: Lower, 4x4 Hermitian, 2 RHS
  n = 4
  nrhs = 2
  A = (0.0d0, 0.0d0)
  A(1,1) = (4.0d0, 0.0d0)
  A(2,1) = (1.0d0, -2.0d0)
  A(2,2) = (5.0d0, 0.0d0)
  A(3,1) = (3.0d0, 1.0d0)
  A(3,2) = (2.0d0, -1.0d0)
  A(3,3) = (7.0d0, 0.0d0)
  A(4,1) = (0.5d0, -0.5d0)
  A(4,2) = (1.0d0, 2.0d0)
  A(4,3) = (3.0d0, 0.0d0)
  A(4,4) = (6.0d0, 0.0d0)
  B(1,1) = (1.0d0, 0.0d0)
  B(2,1) = (2.0d0, 1.0d0)
  B(3,1) = (-1.0d0, 3.0d0)
  B(4,1) = (0.5d0, -0.5d0)
  B(1,2) = (0.0d0, 1.0d0)
  B(2,2) = (1.0d0, 0.0d0)
  B(3,2) = (2.0d0, -1.0d0)
  B(4,2) = (-1.0d0, 2.0d0)
  call ZHESV('L', n, nrhs, A, NMAX, IPIV, B, NMAX, WORK, LWMAX, INFO)
  call begin_test('lower_4x4_2rhs')
  call print_int('n', n)
  call print_int('nrhs', nrhs)
  call print_int('info', INFO)
  call print_array('B', B_r, 2*NMAX*nrhs)
  call print_int_array('ipiv', IPIV, n)
  call end_test()

  ! Test 3: N=0
  call ZHESV('U', 0, 1, A, NMAX, IPIV, B, NMAX, WORK, LWMAX, INFO)
  call begin_test('n0')
  call print_int('info', INFO)
  call end_test()

  ! Test 4: N=1
  n = 1
  nrhs = 1
  A(1,1) = (3.0d0, 0.0d0)
  B(1,1) = (6.0d0, 3.0d0)
  call ZHESV('U', n, nrhs, A, NMAX, IPIV, B, NMAX, WORK, LWMAX, INFO)
  call begin_test('n1')
  call print_int('info', INFO)
  call print_array('B', B_r, 2)
  call end_test()

end program
