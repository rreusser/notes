program test_zhetrf
  use test_utils
  implicit none

  integer, parameter :: NMAX = 6, LWMAX = 256
  complex*16 :: A(NMAX, NMAX), WORK(LWMAX)
  double precision :: A_r(2*NMAX*NMAX)
  equivalence (A, A_r)
  integer :: IPIV(NMAX), INFO, n, LWORK

  ! Test 1: Upper, 4x4 Hermitian matrix
  n = 4
  LWORK = LWMAX
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
  call ZHETRF('U', n, A, NMAX, IPIV, WORK, LWORK, INFO)
  call begin_test('upper_4x4')
  call print_int('n', n)
  call print_int('info', INFO)
  call print_array('A', A_r, 2*NMAX*n)
  call print_int_array('ipiv', IPIV, n)
  call end_test()

  ! Test 2: Lower, 4x4 Hermitian matrix
  n = 4
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
  call ZHETRF('L', n, A, NMAX, IPIV, WORK, LWORK, INFO)
  call begin_test('lower_4x4')
  call print_int('n', n)
  call print_int('info', INFO)
  call print_array('A', A_r, 2*NMAX*n)
  call print_int_array('ipiv', IPIV, n)
  call end_test()

  ! Test 3: N=0
  call ZHETRF('U', 0, A, NMAX, IPIV, WORK, LWORK, INFO)
  call begin_test('n0')
  call print_int('info', INFO)
  call end_test()

  ! Test 4: Lower, 6x6 (forces blocked path with NB=2 internal)
  n = 6
  A = (0.0d0, 0.0d0)
  A(1,1) = (0.01d0, 0.0d0)
  A(2,1) = (5.0d0, -1.0d0)
  A(2,2) = (0.02d0, 0.0d0)
  A(3,1) = (1.0d0, 1.0d0)
  A(3,2) = (2.0d0, -1.0d0)
  A(3,3) = (8.0d0, 0.0d0)
  A(4,1) = (0.5d0, -0.5d0)
  A(4,2) = (1.0d0, 1.0d0)
  A(4,3) = (3.0d0, 0.0d0)
  A(4,4) = (7.0d0, 0.0d0)
  A(5,1) = (2.0d0, 0.0d0)
  A(5,2) = (1.5d0, -0.5d0)
  A(5,3) = (0.0d0, 2.0d0)
  A(5,4) = (1.0d0, 0.5d0)
  A(5,5) = (6.0d0, 0.0d0)
  A(6,1) = (1.0d0, -1.0d0)
  A(6,2) = (0.0d0, -3.0d0)
  A(6,3) = (1.0d0, 0.0d0)
  A(6,4) = (2.0d0, -2.0d0)
  A(6,5) = (0.5d0, 1.0d0)
  A(6,6) = (5.0d0, 0.0d0)
  call ZHETRF('L', n, A, NMAX, IPIV, WORK, LWORK, INFO)
  call begin_test('lower_6x6')
  call print_int('n', n)
  call print_int('info', INFO)
  call print_array('A', A_r, 2*NMAX*n)
  call print_int_array('ipiv', IPIV, n)
  call end_test()

end program
