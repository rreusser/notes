program test_zsyr
  use test_utils
  implicit none

  integer, parameter :: NMAX = 4
  complex*16 :: A(NMAX, NMAX), x(NMAX), alpha
  double precision :: A_r(2*NMAX*NMAX), x_r(2*NMAX)
  equivalence (A, A_r)
  equivalence (x, x_r)
  integer :: n, i, j

  ! Test 1: Upper triangle, 3x3 with alpha=(1,0)
  n = 3
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.0d0)
  A(1,2) = (2.0d0, 1.0d0)
  A(1,3) = (3.0d0, -1.0d0)
  A(2,2) = (4.0d0, 0.0d0)
  A(2,3) = (5.0d0, 2.0d0)
  A(3,3) = (6.0d0, 0.0d0)
  x(1) = (1.0d0, 1.0d0)
  x(2) = (2.0d0, -1.0d0)
  x(3) = (0.0d0, 3.0d0)
  alpha = (1.0d0, 0.0d0)
  call ZSYR('U', n, alpha, x, 1, A, NMAX)
  call begin_test('upper_alpha1')
  call print_int('n', n)
  call print_array('A', A_r, 2*NMAX*n)
  call end_test()

  ! Test 2: Lower triangle, 3x3 with alpha=(2,-1)
  n = 3
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.0d0)
  A(2,1) = (2.0d0, 1.0d0)
  A(2,2) = (4.0d0, 0.0d0)
  A(3,1) = (3.0d0, -1.0d0)
  A(3,2) = (5.0d0, 2.0d0)
  A(3,3) = (6.0d0, 0.0d0)
  x(1) = (1.0d0, 1.0d0)
  x(2) = (2.0d0, -1.0d0)
  x(3) = (0.0d0, 3.0d0)
  alpha = (2.0d0, -1.0d0)
  call ZSYR('L', n, alpha, x, 1, A, NMAX)
  call begin_test('lower_alpha2m1')
  call print_int('n', n)
  call print_array('A', A_r, 2*NMAX*n)
  call end_test()

  ! Test 3: N=0 (quick return)
  n = 0
  alpha = (1.0d0, 0.0d0)
  call ZSYR('U', n, alpha, x, 1, A, NMAX)
  call begin_test('n0')
  call print_int('n', n)
  call end_test()

  ! Test 4: N=1 upper
  n = 1
  A(1,1) = (3.0d0, 2.0d0)
  x(1) = (1.0d0, -1.0d0)
  alpha = (1.0d0, 0.0d0)
  call ZSYR('U', n, alpha, x, 1, A, NMAX)
  call begin_test('n1_upper')
  call print_array('A', A_r, 2)
  call end_test()

  ! Test 5: alpha=0 (quick return)
  n = 3
  A(1,1) = (1.0d0, 0.0d0)
  alpha = (0.0d0, 0.0d0)
  call ZSYR('U', n, alpha, x, 1, A, NMAX)
  call begin_test('alpha0')
  call print_int('n', n)
  call end_test()

  ! Test 6: 4x4 upper with non-unit stride
  n = 4
  A = (0.0d0, 0.0d0)
  do j = 1, n
    do i = 1, j
      A(i,j) = dcmplx(dble(i+j), dble(i-j))
    end do
  end do
  x(1) = (1.0d0, 0.5d0)
  x(2) = (2.0d0, -0.5d0)
  x(3) = (-1.0d0, 1.0d0)
  x(4) = (0.5d0, 0.5d0)
  alpha = (0.5d0, 0.25d0)
  call ZSYR('U', n, alpha, x, 1, A, NMAX)
  call begin_test('upper_4x4')
  call print_int('n', n)
  call print_array('A', A_r, 2*NMAX*n)
  call end_test()

end program
