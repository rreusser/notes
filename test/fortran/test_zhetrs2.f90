program test_zhetrs2
  use test_utils
  implicit none

  integer, parameter :: NMAX = 4, LWMAX = 256
  complex*16 :: A(NMAX, NMAX), B(NMAX, 2), WORK(NMAX)
  double precision :: B_r(2*NMAX*2)
  equivalence (B, B_r)
  integer :: IPIV(NMAX), INFO, n, nrhs

  ! Test 1: Upper, 4x4, 1 RHS
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
  call ZHETRF('U', n, A, NMAX, IPIV, WORK, NMAX, INFO)
  B(1,1) = (1.0d0, 0.0d0)
  B(2,1) = (2.0d0, 1.0d0)
  B(3,1) = (-1.0d0, 3.0d0)
  B(4,1) = (0.5d0, -0.5d0)
  call ZHETRS2('U', n, nrhs, A, NMAX, IPIV, B, NMAX, WORK, INFO)
  call begin_test('upper_4x4_1rhs')
  call print_int('info', INFO)
  call print_array('B', B_r, 2*n*nrhs)
  call end_test()

  ! Test 2: Lower, 4x4, 2 RHS
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
  call ZHETRF('L', n, A, NMAX, IPIV, WORK, NMAX, INFO)
  B(1,1) = (1.0d0, 0.0d0)
  B(2,1) = (2.0d0, 1.0d0)
  B(3,1) = (-1.0d0, 3.0d0)
  B(4,1) = (0.5d0, -0.5d0)
  B(1,2) = (0.0d0, 1.0d0)
  B(2,2) = (1.0d0, 0.0d0)
  B(3,2) = (2.0d0, -1.0d0)
  B(4,2) = (-1.0d0, 2.0d0)
  call ZHETRS2('L', n, nrhs, A, NMAX, IPIV, B, NMAX, WORK, INFO)
  call begin_test('lower_4x4_2rhs')
  call print_int('info', INFO)
  call print_array('B', B_r, 2*NMAX*nrhs)
  call end_test()

end program
