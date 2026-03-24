program test_zgeequ
  use test_utils
  implicit none
  double precision :: a_r(200)
  complex*16 :: a(100)
  equivalence (a, a_r)
  double precision :: r(10), c(10), rowcnd, colcnd, amax
  integer :: info, n, m

  ! Test 1: 3x3 well-conditioned complex matrix
  m = 3; n = 3
  a(1) = (4.0d0, 1.0d0); a(2) = (1.0d0, -1.0d0); a(3) = (0.5d0, 0.2d0)
  a(4) = (1.0d0, 0.5d0); a(5) = (3.0d0, 2.0d0);  a(6) = (1.0d0, -0.5d0)
  a(7) = (0.5d0, 0.1d0); a(8) = (1.0d0, 0.3d0);  a(9) = (2.0d0, 1.0d0)
  call zgeequ(m, n, a, m, r, c, rowcnd, colcnd, amax, info)
  call begin_test('basic_3x3')
  call print_array('r', r, m)
  call print_array('c', c, n)
  call print_scalar('rowcnd', rowcnd)
  call print_scalar('colcnd', colcnd)
  call print_scalar('amax', amax)
  call print_int('info', info)
  call end_test()

  ! Test 2: M=0 quick return
  call zgeequ(0, 3, a, 1, r, c, rowcnd, colcnd, amax, info)
  call begin_test('m_zero')
  call print_scalar('rowcnd', rowcnd)
  call print_scalar('colcnd', colcnd)
  call print_scalar('amax', amax)
  call print_int('info', info)
  call end_test()

  ! Test 3: N=0 quick return
  call zgeequ(3, 0, a, 3, r, c, rowcnd, colcnd, amax, info)
  call begin_test('n_zero')
  call print_scalar('rowcnd', rowcnd)
  call print_scalar('colcnd', colcnd)
  call print_scalar('amax', amax)
  call print_int('info', info)
  call end_test()

  ! Test 4: matrix with a zero row (info > 0)
  m = 3; n = 3
  a(1) = (4.0d0, 1.0d0); a(2) = (0.0d0, 0.0d0); a(3) = (1.0d0, 0.0d0)
  a(4) = (1.0d0, 0.5d0); a(5) = (0.0d0, 0.0d0); a(6) = (2.0d0, 0.0d0)
  a(7) = (0.5d0, 0.1d0); a(8) = (0.0d0, 0.0d0); a(9) = (3.0d0, 0.0d0)
  call zgeequ(m, n, a, m, r, c, rowcnd, colcnd, amax, info)
  call begin_test('zero_row')
  call print_int('info', info)
  call print_scalar('amax', amax)
  call end_test()

  ! Test 5: 1x1 matrix
  m = 1; n = 1
  a(1) = (5.0d0, 3.0d0)
  call zgeequ(m, n, a, m, r, c, rowcnd, colcnd, amax, info)
  call begin_test('1x1')
  call print_array('r', r, m)
  call print_array('c', c, n)
  call print_scalar('rowcnd', rowcnd)
  call print_scalar('colcnd', colcnd)
  call print_scalar('amax', amax)
  call print_int('info', info)
  call end_test()

  ! Test 6: poorly scaled 3x3 (large range of magnitudes)
  m = 3; n = 3
  a(1) = (1.0d6, 0.0d0);  a(2) = (1.0d0, 0.0d0);  a(3) = (1.0d0, 0.0d0)
  a(4) = (1.0d0, 0.0d0);  a(5) = (1.0d-3, 0.0d0); a(6) = (1.0d0, 0.0d0)
  a(7) = (1.0d0, 0.0d0);  a(8) = (1.0d0, 0.0d0);  a(9) = (1.0d3, 0.0d0)
  call zgeequ(m, n, a, m, r, c, rowcnd, colcnd, amax, info)
  call begin_test('poorly_scaled')
  call print_array('r', r, m)
  call print_array('c', c, n)
  call print_scalar('rowcnd', rowcnd)
  call print_scalar('colcnd', colcnd)
  call print_scalar('amax', amax)
  call print_int('info', info)
  call end_test()

  ! Test 7: rectangular 2x3 matrix
  m = 2; n = 3
  a(1) = (2.0d0, 1.0d0); a(2) = (1.0d0, -1.0d0)
  a(3) = (3.0d0, 0.0d0); a(4) = (0.5d0, 0.5d0)
  a(5) = (1.0d0, 1.0d0); a(6) = (4.0d0, 2.0d0)
  call zgeequ(m, n, a, m, r, c, rowcnd, colcnd, amax, info)
  call begin_test('rect_2x3')
  call print_array('r', r, m)
  call print_array('c', c, n)
  call print_scalar('rowcnd', rowcnd)
  call print_scalar('colcnd', colcnd)
  call print_scalar('amax', amax)
  call print_int('info', info)
  call end_test()

end program
