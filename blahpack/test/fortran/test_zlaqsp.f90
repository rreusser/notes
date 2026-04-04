program test_zlaqsp
  use test_utils
  implicit none
  complex*16 :: ap(100)
  double precision :: ap_r(200)
  equivalence (ap, ap_r)
  double precision :: s(10)
  double precision :: scond, amax
  character(1) :: equed
  integer :: n

  ! Test 1: Upper triangle, equilibration needed (poor scond)
  ! Packed upper 3x3: col1: A(1,1); col2: A(1,2), A(2,2); col3: A(1,3), A(2,3), A(3,3)
  n = 3
  ap(1) = (4.0d0, 1.0d0)
  ap(2) = (1.0d0, 0.5d0);  ap(3) = (9.0d0, 2.0d0)
  ap(4) = (0.5d0, 0.25d0); ap(5) = (2.0d0, 1.0d0); ap(6) = (16.0d0, 3.0d0)
  s(1) = 0.5d0; s(2) = 1.0d0/3.0d0; s(3) = 0.25d0
  scond = 0.05d0
  amax = 16.0d0
  call zlaqsp('U', n, ap, s, scond, amax, equed)
  call begin_test('upper_equilibrate')
  call print_array('ap', ap_r, 2*n*(n+1)/2)
  call print_char('equed', equed)
  call end_test()

  ! Test 2: Lower triangle, equilibration needed
  ! Packed lower 3x3: col1: A(1,1), A(2,1), A(3,1); col2: A(2,2), A(3,2); col3: A(3,3)
  n = 3
  ap(1) = (4.0d0, 1.0d0);  ap(2) = (1.0d0, 0.5d0);  ap(3) = (0.5d0, 0.25d0)
  ap(4) = (9.0d0, 2.0d0);  ap(5) = (2.0d0, 1.0d0)
  ap(6) = (16.0d0, 3.0d0)
  s(1) = 0.5d0; s(2) = 1.0d0/3.0d0; s(3) = 0.25d0
  scond = 0.05d0
  amax = 16.0d0
  call zlaqsp('L', n, ap, s, scond, amax, equed)
  call begin_test('lower_equilibrate')
  call print_array('ap', ap_r, 2*n*(n+1)/2)
  call print_char('equed', equed)
  call end_test()

  ! Test 3: No equilibration needed (good scond, amax in range)
  n = 3
  ap(1) = (4.0d0, 1.0d0)
  ap(2) = (1.0d0, 0.5d0);  ap(3) = (9.0d0, 2.0d0)
  ap(4) = (0.5d0, 0.25d0); ap(5) = (2.0d0, 1.0d0); ap(6) = (16.0d0, 3.0d0)
  s(1) = 1.0d0; s(2) = 1.0d0; s(3) = 1.0d0
  scond = 0.5d0
  amax = 16.0d0
  call zlaqsp('U', n, ap, s, scond, amax, equed)
  call begin_test('no_equilibrate')
  call print_array('ap', ap_r, 2*n*(n+1)/2)
  call print_char('equed', equed)
  call end_test()

  ! Test 4: N=0 quick return
  n = 0
  call zlaqsp('U', n, ap, s, scond, amax, equed)
  call begin_test('n_zero')
  call print_char('equed', equed)
  call end_test()

  ! Test 5: N=1, upper, equilibration needed
  n = 1
  ap(1) = (100.0d0, 50.0d0)
  s(1) = 0.1d0
  scond = 0.01d0
  amax = 100.0d0
  call zlaqsp('U', n, ap, s, scond, amax, equed)
  call begin_test('n_one_upper')
  call print_array('ap', ap_r, 2*1)
  call print_char('equed', equed)
  call end_test()

  ! Test 6: amax very small (triggers equilibration via amax < SMALL)
  n = 2
  ap(1) = (1.0d-300, 2.0d-300)
  ap(2) = (0.0d0, 0.0d0)
  ap(3) = (1.0d-300, 3.0d-300)
  s(1) = 1.0d150; s(2) = 1.0d150
  scond = 1.0d0
  amax = 1.0d-300
  call zlaqsp('U', n, ap, s, scond, amax, equed)
  call begin_test('small_amax')
  call print_array('ap', ap_r, 2*n*(n+1)/2)
  call print_char('equed', equed)
  call end_test()

  ! Test 7: 4x4 upper, equilibration needed
  n = 4
  ap(1) = (16.0d0, 1.0d0)
  ap(2) = (4.0d0, 2.0d0);  ap(3) = (9.0d0, 3.0d0)
  ap(4) = (2.0d0, 0.5d0);  ap(5) = (3.0d0, 1.5d0);  ap(6) = (25.0d0, 4.0d0)
  ap(7) = (1.0d0, 0.25d0); ap(8) = (6.0d0, 3.0d0);  ap(9) = (5.0d0, 2.5d0); ap(10) = (36.0d0, 6.0d0)
  s(1) = 0.25d0; s(2) = 0.5d0; s(3) = 0.2d0; s(4) = 1.0d0/3.0d0
  scond = 0.01d0
  amax = 36.0d0
  call zlaqsp('U', n, ap, s, scond, amax, equed)
  call begin_test('upper_4x4')
  call print_array('ap', ap_r, 2*n*(n+1)/2)
  call print_char('equed', equed)
  call end_test()

  ! Test 8: 4x4 lower, equilibration needed
  n = 4
  ap(1) = (16.0d0, 1.0d0);  ap(2) = (4.0d0, 2.0d0);  ap(3) = (2.0d0, 0.5d0);  ap(4) = (1.0d0, 0.25d0)
  ap(5) = (9.0d0, 3.0d0);   ap(6) = (3.0d0, 1.5d0);   ap(7) = (6.0d0, 3.0d0)
  ap(8) = (25.0d0, 4.0d0);  ap(9) = (5.0d0, 2.5d0)
  ap(10) = (36.0d0, 6.0d0)
  s(1) = 0.25d0; s(2) = 0.5d0; s(3) = 0.2d0; s(4) = 1.0d0/3.0d0
  scond = 0.01d0
  amax = 36.0d0
  call zlaqsp('L', n, ap, s, scond, amax, equed)
  call begin_test('lower_4x4')
  call print_array('ap', ap_r, 2*n*(n+1)/2)
  call print_char('equed', equed)
  call end_test()

end program
