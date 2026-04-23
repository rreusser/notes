program test_zlaqhe
  use test_utils
  implicit none
  integer, parameter :: MAXN = 10
  complex*16 :: a(MAXN*MAXN)
  double precision :: a_r(2*MAXN*MAXN)
  equivalence (a, a_r)
  double precision :: s(MAXN)
  double precision :: scond, amax
  character :: equed
  integer :: n

  ! Test 1: upper triangle, equilibration needed (scond < 0.1)
  ! A = [[4, 1+i, 0], [1-i, 9, 1], [0, 1, 16]]
  n = 3
  a = (0.0d0, 0.0d0)
  a(1) = (4.0d0, 0.0d0);  a(4) = (1.0d0, 1.0d0);  a(7) = (0.0d0, 0.0d0)
  a(2) = (1.0d0, -1.0d0); a(5) = (9.0d0, 0.0d0);  a(8) = (1.0d0, 0.0d0)
  a(3) = (0.0d0, 0.0d0);  a(6) = (1.0d0, 0.0d0);  a(9) = (16.0d0, 0.0d0)
  s(1) = 0.5d0; s(2) = 1.0d0/3.0d0; s(3) = 0.25d0
  scond = 0.05d0
  amax = 16.0d0
  call zlaqhe('U', n, a, n, s, scond, amax, equed)
  call begin_test('upper_equil')
  call print_array('a', a_r, 2*n*n)
  call print_char('equed', equed)
  call end_test()

  ! Test 2: lower triangle, equilibration needed
  n = 3
  a = (0.0d0, 0.0d0)
  a(1) = (4.0d0, 0.0d0);  a(4) = (1.0d0, 1.0d0);  a(7) = (0.0d0, 0.0d0)
  a(2) = (1.0d0, -1.0d0); a(5) = (9.0d0, 0.0d0);  a(8) = (1.0d0, 0.0d0)
  a(3) = (0.0d0, 0.0d0);  a(6) = (1.0d0, 0.0d0);  a(9) = (16.0d0, 0.0d0)
  s(1) = 0.5d0; s(2) = 1.0d0/3.0d0; s(3) = 0.25d0
  scond = 0.05d0
  amax = 16.0d0
  call zlaqhe('L', n, a, n, s, scond, amax, equed)
  call begin_test('lower_equil')
  call print_array('a', a_r, 2*n*n)
  call print_char('equed', equed)
  call end_test()

  ! Test 3: no equilibration needed (scond >= 0.1, amax in range)
  n = 3
  a = (0.0d0, 0.0d0)
  a(1) = (4.0d0, 0.0d0);  a(4) = (1.0d0, 1.0d0);  a(7) = (0.0d0, 0.0d0)
  a(2) = (1.0d0, -1.0d0); a(5) = (9.0d0, 0.0d0);  a(8) = (1.0d0, 0.0d0)
  a(3) = (0.0d0, 0.0d0);  a(6) = (1.0d0, 0.0d0);  a(9) = (16.0d0, 0.0d0)
  s(1) = 1.0d0; s(2) = 1.0d0; s(3) = 1.0d0
  scond = 1.0d0
  amax = 16.0d0
  call zlaqhe('U', n, a, n, s, scond, amax, equed)
  call begin_test('no_equil')
  call print_array('a', a_r, 2*n*n)
  call print_char('equed', equed)
  call end_test()

  ! Test 4: n=0
  call zlaqhe('U', 0, a, 1, s, 1.0d0, 1.0d0, equed)
  call begin_test('n_zero')
  call print_char('equed', equed)
  call end_test()

  ! Test 5: n=1 upper, equilibration needed
  n = 1
  a(1) = (4.0d0, 0.0d0)
  s(1) = 0.5d0
  scond = 0.01d0
  amax = 4.0d0
  call zlaqhe('U', n, a, n, s, scond, amax, equed)
  call begin_test('n_one')
  call print_array('a', a_r, 2)
  call print_char('equed', equed)
  call end_test()

end program
