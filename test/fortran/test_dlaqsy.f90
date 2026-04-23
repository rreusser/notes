program test_dlaqsy
  use test_utils
  implicit none
  double precision :: a(100), s(10)
  double precision :: scond, amax
  character(1) :: equed
  integer :: n

  ! Test 1: Upper triangle, equilibration needed (poor scond)
  n = 3
  a(1) = 4.0d0; a(2) = 0.0d0; a(3) = 0.0d0
  a(4) = 1.0d0; a(5) = 9.0d0; a(6) = 0.0d0
  a(7) = 0.5d0; a(8) = 2.0d0; a(9) = 16.0d0
  s(1) = 0.5d0; s(2) = 1.0d0/3.0d0; s(3) = 0.25d0
  scond = 0.05d0
  amax = 16.0d0
  call dlaqsy('U', n, a, n, s, scond, amax, equed)
  call begin_test('upper_equilibrate')
  call print_array('a', a, n*n)
  call print_char('equed', equed)
  call end_test()

  ! Test 2: Lower triangle, equilibration needed
  n = 3
  a(1) = 4.0d0; a(2) = 1.0d0; a(3) = 0.5d0
  a(4) = 0.0d0; a(5) = 9.0d0; a(6) = 2.0d0
  a(7) = 0.0d0; a(8) = 0.0d0; a(9) = 16.0d0
  s(1) = 0.5d0; s(2) = 1.0d0/3.0d0; s(3) = 0.25d0
  scond = 0.05d0
  amax = 16.0d0
  call dlaqsy('L', n, a, n, s, scond, amax, equed)
  call begin_test('lower_equilibrate')
  call print_array('a', a, n*n)
  call print_char('equed', equed)
  call end_test()

  ! Test 3: No equilibration needed (good scond, amax in range)
  n = 3
  a(1) = 4.0d0; a(2) = 1.0d0; a(3) = 0.5d0
  a(4) = 1.0d0; a(5) = 9.0d0; a(6) = 2.0d0
  a(7) = 0.5d0; a(8) = 2.0d0; a(9) = 16.0d0
  s(1) = 1.0d0; s(2) = 1.0d0; s(3) = 1.0d0
  scond = 0.5d0
  amax = 16.0d0
  call dlaqsy('U', n, a, n, s, scond, amax, equed)
  call begin_test('no_equilibrate')
  call print_array('a', a, n*n)
  call print_char('equed', equed)
  call end_test()

  ! Test 4: N=0 quick return
  n = 0
  call dlaqsy('U', n, a, 1, s, scond, amax, equed)
  call begin_test('n_zero')
  call print_char('equed', equed)
  call end_test()

  ! Test 5: N=1, upper, equilibration needed
  n = 1
  a(1) = 100.0d0
  s(1) = 0.1d0
  scond = 0.01d0
  amax = 100.0d0
  call dlaqsy('U', n, a, n, s, scond, amax, equed)
  call begin_test('n_one_upper')
  call print_array('a', a, 1)
  call print_char('equed', equed)
  call end_test()

  ! Test 6: amax very small (triggers equilibration)
  n = 2
  a(1) = 1.0d-300; a(2) = 0.0d0
  a(3) = 0.0d0; a(4) = 1.0d-300
  s(1) = 1.0d150; s(2) = 1.0d150
  scond = 1.0d0
  amax = 1.0d-300
  call dlaqsy('U', n, a, n, s, scond, amax, equed)
  call begin_test('small_amax')
  call print_array('a', a, n*n)
  call print_char('equed', equed)
  call end_test()

end program
