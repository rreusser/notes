program test_dgeequ
  use test_utils
  implicit none
  double precision :: a(100), r(10), c(10)
  double precision :: rowcnd, colcnd, amax
  integer :: info, m, n

  ! Test 1: 3x3 well-conditioned diag-dominant matrix
  m = 3
  n = 3
  a(1) = 4.0d0; a(2) = 1.0d0; a(3) = 0.5d0
  a(4) = 1.0d0; a(5) = 3.0d0; a(6) = 1.0d0
  a(7) = 0.5d0; a(8) = 1.0d0; a(9) = 2.0d0
  call dgeequ(m, n, a, m, r, c, rowcnd, colcnd, amax, info)
  call begin_test('basic')
  call print_array('r', r, m)
  call print_array('c', c, n)
  call print_scalar('rowcnd', rowcnd)
  call print_scalar('colcnd', colcnd)
  call print_scalar('amax', amax)
  call print_int('info', info)
  call end_test()

  ! Test 2: 3x3 diagonal matrix with varying scales
  m = 3
  n = 3
  a = 0.0d0
  a(1) = 100.0d0; a(5) = 1.0d0; a(9) = 0.01d0
  call dgeequ(m, n, a, m, r, c, rowcnd, colcnd, amax, info)
  call begin_test('diagonal_varied')
  call print_array('r', r, m)
  call print_array('c', c, n)
  call print_scalar('rowcnd', rowcnd)
  call print_scalar('colcnd', colcnd)
  call print_scalar('amax', amax)
  call print_int('info', info)
  call end_test()

  ! Test 3: Matrix with a zero row (should return info > 0)
  m = 3
  n = 3
  a(1) = 1.0d0; a(2) = 0.0d0; a(3) = 1.0d0
  a(4) = 2.0d0; a(5) = 0.0d0; a(6) = 3.0d0
  a(7) = 4.0d0; a(8) = 0.0d0; a(9) = 5.0d0
  call dgeequ(m, n, a, m, r, c, rowcnd, colcnd, amax, info)
  call begin_test('zero_row')
  call print_int('info', info)
  call end_test()

  ! Test 4: Matrix with a zero column (after row scaling, should return info = M+j)
  m = 3
  n = 3
  a(1) = 1.0d0; a(2) = 2.0d0; a(3) = 3.0d0
  a(4) = 0.0d0; a(5) = 0.0d0; a(6) = 0.0d0
  a(7) = 4.0d0; a(8) = 5.0d0; a(9) = 6.0d0
  call dgeequ(m, n, a, m, r, c, rowcnd, colcnd, amax, info)
  call begin_test('zero_col')
  call print_array('r', r, m)
  call print_int('info', info)
  call end_test()

  ! Test 5: Identity matrix
  m = 3
  n = 3
  a = 0.0d0
  a(1) = 1.0d0; a(5) = 1.0d0; a(9) = 1.0d0
  call dgeequ(m, n, a, m, r, c, rowcnd, colcnd, amax, info)
  call begin_test('identity')
  call print_array('r', r, m)
  call print_array('c', c, n)
  call print_scalar('rowcnd', rowcnd)
  call print_scalar('colcnd', colcnd)
  call print_scalar('amax', amax)
  call print_int('info', info)
  call end_test()

  ! Test 6: Quick return M=0
  call dgeequ(0, 3, a, 1, r, c, rowcnd, colcnd, amax, info)
  call begin_test('m_zero')
  call print_scalar('rowcnd', rowcnd)
  call print_scalar('colcnd', colcnd)
  call print_scalar('amax', amax)
  call end_test()

  ! Test 7: Quick return N=0
  call dgeequ(3, 0, a, 3, r, c, rowcnd, colcnd, amax, info)
  call begin_test('n_zero')
  call print_scalar('rowcnd', rowcnd)
  call print_scalar('colcnd', colcnd)
  call print_scalar('amax', amax)
  call end_test()

  ! Test 8: Non-square 2x4 matrix
  m = 2
  n = 4
  a(1) = 1.0d0; a(2) = 2.0d0
  a(3) = 3.0d0; a(4) = 4.0d0
  a(5) = 5.0d0; a(6) = 6.0d0
  a(7) = 7.0d0; a(8) = 8.0d0
  call dgeequ(m, n, a, m, r, c, rowcnd, colcnd, amax, info)
  call begin_test('nonsquare')
  call print_array('r', r, m)
  call print_array('c', c, n)
  call print_scalar('rowcnd', rowcnd)
  call print_scalar('colcnd', colcnd)
  call print_scalar('amax', amax)
  call print_int('info', info)
  call end_test()

end program
