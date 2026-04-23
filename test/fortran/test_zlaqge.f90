program test_zlaqge
  use test_utils
  implicit none
  double precision :: a_r(200)
  complex*16 :: a(100)
  equivalence (a, a_r)
  double precision :: r(10), c(10), rowcnd, colcnd, amax
  character :: equed
  integer :: m, n

  ! Test 1: No equilibration needed (well-scaled)
  m = 3; n = 3
  a(1) = (4.0d0, 1.0d0); a(2) = (1.0d0, -1.0d0); a(3) = (0.5d0, 0.2d0)
  a(4) = (1.0d0, 0.5d0); a(5) = (3.0d0, 2.0d0);  a(6) = (1.0d0, -0.5d0)
  a(7) = (0.5d0, 0.1d0); a(8) = (1.0d0, 0.3d0);  a(9) = (2.0d0, 1.0d0)
  r(1) = 1.0d0; r(2) = 1.0d0; r(3) = 1.0d0
  c(1) = 1.0d0; c(2) = 1.0d0; c(3) = 1.0d0
  rowcnd = 1.0d0; colcnd = 1.0d0; amax = 5.0d0
  call zlaqge(m, n, a, m, r, c, rowcnd, colcnd, amax, equed)
  call begin_test('no_equil')
  call print_char('equed', equed)
  call print_array('a', a_r, 2*m*n)
  call end_test()

  ! Test 2: Row equilibration only
  m = 3; n = 3
  a(1) = (4.0d0, 1.0d0); a(2) = (1.0d0, -1.0d0); a(3) = (0.5d0, 0.2d0)
  a(4) = (1.0d0, 0.5d0); a(5) = (3.0d0, 2.0d0);  a(6) = (1.0d0, -0.5d0)
  a(7) = (0.5d0, 0.1d0); a(8) = (1.0d0, 0.3d0);  a(9) = (2.0d0, 1.0d0)
  r(1) = 0.5d0; r(2) = 2.0d0; r(3) = 1.5d0
  c(1) = 1.0d0; c(2) = 1.0d0; c(3) = 1.0d0
  rowcnd = 0.01d0; colcnd = 1.0d0; amax = 5.0d0
  call zlaqge(m, n, a, m, r, c, rowcnd, colcnd, amax, equed)
  call begin_test('row_equil')
  call print_char('equed', equed)
  call print_array('a', a_r, 2*m*n)
  call end_test()

  ! Test 3: Column equilibration only
  m = 3; n = 3
  a(1) = (4.0d0, 1.0d0); a(2) = (1.0d0, -1.0d0); a(3) = (0.5d0, 0.2d0)
  a(4) = (1.0d0, 0.5d0); a(5) = (3.0d0, 2.0d0);  a(6) = (1.0d0, -0.5d0)
  a(7) = (0.5d0, 0.1d0); a(8) = (1.0d0, 0.3d0);  a(9) = (2.0d0, 1.0d0)
  r(1) = 1.0d0; r(2) = 1.0d0; r(3) = 1.0d0
  c(1) = 0.5d0; c(2) = 2.0d0; c(3) = 1.5d0
  rowcnd = 1.0d0; colcnd = 0.01d0; amax = 5.0d0
  call zlaqge(m, n, a, m, r, c, rowcnd, colcnd, amax, equed)
  call begin_test('col_equil')
  call print_char('equed', equed)
  call print_array('a', a_r, 2*m*n)
  call end_test()

  ! Test 4: Both row and column equilibration
  m = 3; n = 3
  a(1) = (4.0d0, 1.0d0); a(2) = (1.0d0, -1.0d0); a(3) = (0.5d0, 0.2d0)
  a(4) = (1.0d0, 0.5d0); a(5) = (3.0d0, 2.0d0);  a(6) = (1.0d0, -0.5d0)
  a(7) = (0.5d0, 0.1d0); a(8) = (1.0d0, 0.3d0);  a(9) = (2.0d0, 1.0d0)
  r(1) = 0.5d0; r(2) = 2.0d0; r(3) = 1.5d0
  c(1) = 0.5d0; c(2) = 2.0d0; c(3) = 1.5d0
  rowcnd = 0.01d0; colcnd = 0.01d0; amax = 5.0d0
  call zlaqge(m, n, a, m, r, c, rowcnd, colcnd, amax, equed)
  call begin_test('both_equil')
  call print_char('equed', equed)
  call print_array('a', a_r, 2*m*n)
  call end_test()

  ! Test 5: M=0 quick return
  call zlaqge(0, 3, a, 1, r, c, rowcnd, colcnd, amax, equed)
  call begin_test('m_zero')
  call print_char('equed', equed)
  call end_test()

  ! Test 6: N=0 quick return
  call zlaqge(3, 0, a, 3, r, c, rowcnd, colcnd, amax, equed)
  call begin_test('n_zero')
  call print_char('equed', equed)
  call end_test()

  ! Test 7: No equil due to small amax
  m = 2; n = 2
  a(1) = (1.0d-200, 0.0d0); a(2) = (1.0d-200, 0.0d0)
  a(3) = (1.0d-200, 0.0d0); a(4) = (1.0d-200, 0.0d0)
  r(1) = 1.0d0; r(2) = 1.0d0
  c(1) = 1.0d0; c(2) = 1.0d0
  rowcnd = 1.0d0; colcnd = 1.0d0; amax = 1.0d-200
  call zlaqge(m, n, a, m, r, c, rowcnd, colcnd, amax, equed)
  call begin_test('small_amax')
  call print_char('equed', equed)
  call end_test()

end program
