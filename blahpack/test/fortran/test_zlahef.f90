program test_zlahef
  use test_utils
  implicit none

  integer, parameter :: NMAX = 6
  complex*16 :: A(NMAX, NMAX), W(NMAX, 3)
  double precision :: A_r(2*NMAX*NMAX)
  equivalence (A, A_r)
  integer :: IPIV(NMAX), INFO, n, nb, kb

  ! Test 1: Lower, 6x6 Hermitian, NB=3
  n = 6
  nb = 3
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
  call ZLAHEF('L', n, nb, kb, A, NMAX, IPIV, W, NMAX, INFO)
  call begin_test('lower_6x6_nb3')
  call print_int('n', n)
  call print_int('nb', nb)
  call print_int('kb', kb)
  call print_int('info', INFO)
  call print_array('A', A_r, 2*NMAX*n)
  call print_int_array('ipiv', IPIV, n)
  call end_test()

end program
