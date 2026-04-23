program test_dlaqge
  use test_utils
  implicit none
  double precision :: a(100), r(10), c(10)
  double precision :: rowcnd, colcnd, amax
  character :: equed
  integer :: m, n, i
  double precision :: dlamch, small, large

  small = dlamch('S') / dlamch('P')
  large = 1.0d0 / small

  ! Test 1: No equilibration needed (rowcnd >= 0.1, colcnd >= 0.1, small <= amax <= large)
  m = 3
  n = 3
  a(1) = 2.0d0; a(2) = 1.0d0; a(3) = 0.5d0
  a(4) = 1.0d0; a(5) = 3.0d0; a(6) = 1.0d0
  a(7) = 0.5d0; a(8) = 1.0d0; a(9) = 4.0d0
  r(1) = 0.5d0; r(2) = 1.0d0; r(3) = 0.8d0
  c(1) = 0.6d0; c(2) = 1.0d0; c(3) = 0.7d0
  rowcnd = 0.5d0
  colcnd = 0.6d0
  amax = 4.0d0
  call dlaqge(m, n, a, m, r, c, rowcnd, colcnd, amax, equed)
  call begin_test('no_equil')
  call print_array('a', a, 9)
  call print_char('equed', equed)
  call end_test()

  ! Test 2: Row scaling only (rowcnd < 0.1, colcnd >= 0.1)
  a(1) = 2.0d0; a(2) = 1.0d0; a(3) = 0.5d0
  a(4) = 1.0d0; a(5) = 3.0d0; a(6) = 1.0d0
  a(7) = 0.5d0; a(8) = 1.0d0; a(9) = 4.0d0
  r(1) = 0.5d0; r(2) = 1.0d0; r(3) = 0.8d0
  c(1) = 0.6d0; c(2) = 1.0d0; c(3) = 0.7d0
  rowcnd = 0.01d0
  colcnd = 0.6d0
  amax = 4.0d0
  call dlaqge(m, n, a, m, r, c, rowcnd, colcnd, amax, equed)
  call begin_test('row_equil')
  call print_array('a', a, 9)
  call print_char('equed', equed)
  call end_test()

  ! Test 3: Column scaling only (rowcnd >= 0.1, colcnd < 0.1, small <= amax <= large)
  a(1) = 2.0d0; a(2) = 1.0d0; a(3) = 0.5d0
  a(4) = 1.0d0; a(5) = 3.0d0; a(6) = 1.0d0
  a(7) = 0.5d0; a(8) = 1.0d0; a(9) = 4.0d0
  r(1) = 0.5d0; r(2) = 1.0d0; r(3) = 0.8d0
  c(1) = 0.6d0; c(2) = 1.0d0; c(3) = 0.7d0
  rowcnd = 0.5d0
  colcnd = 0.01d0
  amax = 4.0d0
  call dlaqge(m, n, a, m, r, c, rowcnd, colcnd, amax, equed)
  call begin_test('col_equil')
  call print_array('a', a, 9)
  call print_char('equed', equed)
  call end_test()

  ! Test 4: Both row and column scaling (rowcnd < 0.1, colcnd < 0.1)
  a(1) = 2.0d0; a(2) = 1.0d0; a(3) = 0.5d0
  a(4) = 1.0d0; a(5) = 3.0d0; a(6) = 1.0d0
  a(7) = 0.5d0; a(8) = 1.0d0; a(9) = 4.0d0
  r(1) = 0.5d0; r(2) = 1.0d0; r(3) = 0.8d0
  c(1) = 0.6d0; c(2) = 1.0d0; c(3) = 0.7d0
  rowcnd = 0.01d0
  colcnd = 0.01d0
  amax = 4.0d0
  call dlaqge(m, n, a, m, r, c, rowcnd, colcnd, amax, equed)
  call begin_test('both_equil')
  call print_array('a', a, 9)
  call print_char('equed', equed)
  call end_test()

  ! Test 5: Row scaling triggered by amax > large
  a(1) = 2.0d0; a(2) = 1.0d0; a(3) = 0.5d0
  a(4) = 1.0d0; a(5) = 3.0d0; a(6) = 1.0d0
  a(7) = 0.5d0; a(8) = 1.0d0; a(9) = 4.0d0
  r(1) = 0.5d0; r(2) = 1.0d0; r(3) = 0.8d0
  c(1) = 0.6d0; c(2) = 1.0d0; c(3) = 0.7d0
  rowcnd = 0.5d0
  colcnd = 0.6d0
  amax = 1.0d+300
  call dlaqge(m, n, a, m, r, c, rowcnd, colcnd, amax, equed)
  call begin_test('amax_large')
  call print_array('a', a, 9)
  call print_char('equed', equed)
  call end_test()

  ! Test 6: Row scaling triggered by amax < small
  a(1) = 2.0d0; a(2) = 1.0d0; a(3) = 0.5d0
  a(4) = 1.0d0; a(5) = 3.0d0; a(6) = 1.0d0
  a(7) = 0.5d0; a(8) = 1.0d0; a(9) = 4.0d0
  r(1) = 0.5d0; r(2) = 1.0d0; r(3) = 0.8d0
  c(1) = 0.6d0; c(2) = 1.0d0; c(3) = 0.7d0
  rowcnd = 0.5d0
  colcnd = 0.6d0
  amax = 1.0d-320
  call dlaqge(m, n, a, m, r, c, rowcnd, colcnd, amax, equed)
  call begin_test('amax_small')
  call print_array('a', a, 9)
  call print_char('equed', equed)
  call end_test()

  ! Test 7: Quick return M=0
  m = 0
  n = 3
  call dlaqge(m, n, a, 1, r, c, rowcnd, colcnd, amax, equed)
  call begin_test('m_zero')
  call print_char('equed', equed)
  call end_test()

  ! Test 8: Quick return N=0
  m = 3
  n = 0
  call dlaqge(m, n, a, m, r, c, rowcnd, colcnd, amax, equed)
  call begin_test('n_zero')
  call print_char('equed', equed)
  call end_test()

end program
