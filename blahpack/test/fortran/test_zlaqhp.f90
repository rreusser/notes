program test_zlaqhp
  use test_utils
  implicit none
  integer, parameter :: MAXN = 10
  complex*16 :: ap(MAXN*(MAXN+1)/2)
  double precision :: ap_r(2*MAXN*(MAXN+1)/2)
  equivalence (ap, ap_r)
  double precision :: s(MAXN)
  double precision :: scond, amax
  character :: equed
  integer :: n, nap

  ! Test 1: Upper triangle, equilibration needed (scond < 0.1)
  ! Hermitian 3x3 packed upper:
  !   col1: A(1,1)
  !   col2: A(1,2), A(2,2)
  !   col3: A(1,3), A(2,3), A(3,3)
  ! Hermitian => diagonal is real, A(i,j) = conj(A(j,i))
  n = 3
  nap = n*(n+1)/2
  ap(1) = (4.0d0, 0.0d0)
  ap(2) = (1.0d0, 2.0d0);  ap(3) = (9.0d0, 0.0d0)
  ap(4) = (0.5d0, -1.0d0); ap(5) = (2.0d0, 0.5d0); ap(6) = (16.0d0, 0.0d0)
  s(1) = 0.5d0; s(2) = 1.0d0/3.0d0; s(3) = 0.25d0
  scond = 0.05d0
  amax = 16.0d0
  call zlaqhp('U', n, ap, s, scond, amax, equed)
  call begin_test('upper_equilibrate')
  call print_array('ap', ap_r, 2*nap)
  call print_char('equed', equed)
  call end_test()

  ! Test 2: Lower triangle, equilibration needed
  ! Packed lower 3x3:
  !   col1: A(1,1), A(2,1), A(3,1)
  !   col2: A(2,2), A(3,2)
  !   col3: A(3,3)
  n = 3
  nap = n*(n+1)/2
  ap(1) = (4.0d0, 0.0d0);  ap(2) = (1.0d0, -2.0d0); ap(3) = (0.5d0, 1.0d0)
  ap(4) = (9.0d0, 0.0d0);  ap(5) = (2.0d0, -0.5d0)
  ap(6) = (16.0d0, 0.0d0)
  s(1) = 0.5d0; s(2) = 1.0d0/3.0d0; s(3) = 0.25d0
  scond = 0.05d0
  amax = 16.0d0
  call zlaqhp('L', n, ap, s, scond, amax, equed)
  call begin_test('lower_equilibrate')
  call print_array('ap', ap_r, 2*nap)
  call print_char('equed', equed)
  call end_test()

  ! Test 3: No equilibration needed (good scond, amax in range)
  n = 3
  nap = n*(n+1)/2
  ap(1) = (4.0d0, 0.0d0)
  ap(2) = (1.0d0, 1.0d0);  ap(3) = (9.0d0, 0.0d0)
  ap(4) = (0.5d0, -1.0d0); ap(5) = (2.0d0, 0.5d0); ap(6) = (16.0d0, 0.0d0)
  s(1) = 1.0d0; s(2) = 1.0d0; s(3) = 1.0d0
  scond = 1.0d0
  amax = 16.0d0
  call zlaqhp('U', n, ap, s, scond, amax, equed)
  call begin_test('no_equilibrate')
  call print_array('ap', ap_r, 2*nap)
  call print_char('equed', equed)
  call end_test()

  ! Test 4: N=0 quick return
  call zlaqhp('U', 0, ap, s, 1.0d0, 1.0d0, equed)
  call begin_test('n_zero')
  call print_char('equed', equed)
  call end_test()

  ! Test 5: N=1, upper, equilibration needed (only diagonal)
  n = 1
  nap = 1
  ap(1) = (100.0d0, 0.0d0)
  s(1) = 0.1d0
  scond = 0.01d0
  amax = 100.0d0
  call zlaqhp('U', n, ap, s, scond, amax, equed)
  call begin_test('n_one_upper')
  call print_array('ap', ap_r, 2*nap)
  call print_char('equed', equed)
  call end_test()

  ! Test 6: 4x4 upper, equilibration needed
  n = 4
  nap = n*(n+1)/2
  ap(1) = (16.0d0, 0.0d0)
  ap(2) = (4.0d0, 1.0d0);   ap(3) = (9.0d0, 0.0d0)
  ap(4) = (2.0d0, -1.0d0);  ap(5) = (3.0d0, 2.0d0);  ap(6) = (25.0d0, 0.0d0)
  ap(7) = (1.0d0, 0.5d0);   ap(8) = (6.0d0, -2.0d0); ap(9) = (5.0d0, 1.0d0)
  ap(10) = (36.0d0, 0.0d0)
  s(1) = 0.25d0; s(2) = 0.5d0; s(3) = 0.2d0; s(4) = 1.0d0/3.0d0
  scond = 0.01d0
  amax = 36.0d0
  call zlaqhp('U', n, ap, s, scond, amax, equed)
  call begin_test('upper_4x4')
  call print_array('ap', ap_r, 2*nap)
  call print_char('equed', equed)
  call end_test()

  ! Test 7: 4x4 lower, equilibration needed
  n = 4
  nap = n*(n+1)/2
  ap(1) = (16.0d0, 0.0d0);  ap(2) = (4.0d0, -1.0d0); ap(3) = (2.0d0, 1.0d0)
  ap(4) = (1.0d0, -0.5d0)
  ap(5) = (9.0d0, 0.0d0);   ap(6) = (3.0d0, -2.0d0); ap(7) = (6.0d0, 2.0d0)
  ap(8) = (25.0d0, 0.0d0);  ap(9) = (5.0d0, -1.0d0)
  ap(10) = (36.0d0, 0.0d0)
  s(1) = 0.25d0; s(2) = 0.5d0; s(3) = 0.2d0; s(4) = 1.0d0/3.0d0
  scond = 0.01d0
  amax = 36.0d0
  call zlaqhp('L', n, ap, s, scond, amax, equed)
  call begin_test('lower_4x4')
  call print_array('ap', ap_r, 2*nap)
  call print_char('equed', equed)
  call end_test()

  ! Test 8: Diagonal with imaginary part (should be zeroed by Hermitian property)
  ! Fortran ZLAQHP uses DBLE() which takes real part, so diagonal imag parts
  ! are discarded after scaling.
  n = 2
  nap = n*(n+1)/2
  ap(1) = (4.0d0, 0.5d0)
  ap(2) = (1.0d0, 2.0d0); ap(3) = (9.0d0, 0.3d0)
  s(1) = 0.5d0; s(2) = 0.25d0
  scond = 0.05d0
  amax = 9.0d0
  call zlaqhp('U', n, ap, s, scond, amax, equed)
  call begin_test('diag_imag_upper')
  call print_array('ap', ap_r, 2*nap)
  call print_char('equed', equed)
  call end_test()

  ! Test 9: N=1 lower, equilibration needed
  n = 1
  nap = 1
  ap(1) = (49.0d0, 0.0d0)
  s(1) = 0.2d0
  scond = 0.01d0
  amax = 49.0d0
  call zlaqhp('L', n, ap, s, scond, amax, equed)
  call begin_test('n_one_lower')
  call print_array('ap', ap_r, 2*nap)
  call print_char('equed', equed)
  call end_test()

end program
