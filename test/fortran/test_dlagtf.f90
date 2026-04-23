program test_dlagtf
  use test_utils
  implicit none

  double precision :: a(5), b(4), c(4), d(3)
  integer :: in(5), info

  ! Test 1: 5x5 tridiagonal, lambda=2.0
  a(1) = 2.0d0
  a(2) = 2.0d0
  a(3) = 2.0d0
  a(4) = 2.0d0
  a(5) = 2.0d0
  b(1) = 1.0d0
  b(2) = 1.0d0
  b(3) = 1.0d0
  b(4) = 1.0d0
  c(1) = 1.0d0
  c(2) = 1.0d0
  c(3) = 1.0d0
  c(4) = 1.0d0

  call DLAGTF(5, a, 2.0d0, b, c, 0.0d0, d, in, info)

  call begin_test('basic_5x5')
  call print_int('info', info)
  call print_array('a', a, 5)
  call print_array('b', b, 4)
  call print_array('c', c, 4)
  call print_array('d', d, 3)
  call print_int_array('in', in, 5)
  call end_test()

  ! Test 2: N=1
  a(1) = 5.0d0
  call DLAGTF(1, a, 3.0d0, b, c, 0.0d0, d, in, info)

  call begin_test('n_equals_1')
  call print_int('info', info)
  call print_array('a', a, 1)
  call print_int_array('in', in, 1)
  call end_test()

  ! Test 3: N=0
  call DLAGTF(0, a, 0.0d0, b, c, 0.0d0, d, in, info)

  call begin_test('n_equals_0')
  call print_int('info', info)
  call end_test()

end program
