program test_zlaqsy
  use test_utils
  implicit none
  complex*16 :: a(10, 10), apk(100)
  double precision :: a_r(200), apk_r(200)
  equivalence (apk, apk_r)
  double precision :: s(10)
  double precision :: scond, amax
  character(1) :: equed
  integer :: n, i, j

  ! Test 1: Upper triangle, equilibration needed (poor scond)
  n = 3
  a(1,1) = (4.0d0, 1.0d0);  a(1,2) = (1.0d0, 0.5d0);  a(1,3) = (0.5d0, 0.25d0)
  a(2,1) = (0.0d0, 0.0d0);  a(2,2) = (9.0d0, 2.0d0);  a(2,3) = (2.0d0, 1.0d0)
  a(3,1) = (0.0d0, 0.0d0);  a(3,2) = (0.0d0, 0.0d0);  a(3,3) = (16.0d0, 3.0d0)
  s(1) = 0.5d0; s(2) = 1.0d0/3.0d0; s(3) = 0.25d0
  scond = 0.05d0
  amax = 16.0d0
  call zlaqsy('U', n, a, 10, s, scond, amax, equed)
  do j = 1, n
    do i = 1, n
      apk(i + (j-1)*n) = a(i, j)
    end do
  end do
  call begin_test('upper_equilibrate')
  call print_array('a', apk_r, 2*n*n)
  call print_char('equed', equed)
  call end_test()

  ! Test 2: Lower triangle, equilibration needed
  n = 3
  a(1,1) = (4.0d0, 1.0d0);  a(1,2) = (0.0d0, 0.0d0);  a(1,3) = (0.0d0, 0.0d0)
  a(2,1) = (1.0d0, 0.5d0);  a(2,2) = (9.0d0, 2.0d0);  a(2,3) = (0.0d0, 0.0d0)
  a(3,1) = (0.5d0, 0.25d0); a(3,2) = (2.0d0, 1.0d0);  a(3,3) = (16.0d0, 3.0d0)
  s(1) = 0.5d0; s(2) = 1.0d0/3.0d0; s(3) = 0.25d0
  scond = 0.05d0
  amax = 16.0d0
  call zlaqsy('L', n, a, 10, s, scond, amax, equed)
  do j = 1, n
    do i = 1, n
      apk(i + (j-1)*n) = a(i, j)
    end do
  end do
  call begin_test('lower_equilibrate')
  call print_array('a', apk_r, 2*n*n)
  call print_char('equed', equed)
  call end_test()

  ! Test 3: No equilibration needed (good scond, amax in range)
  n = 3
  a(1,1) = (4.0d0, 1.0d0);  a(1,2) = (1.0d0, 0.5d0);  a(1,3) = (0.5d0, 0.25d0)
  a(2,1) = (1.0d0, 0.5d0);  a(2,2) = (9.0d0, 2.0d0);  a(2,3) = (2.0d0, 1.0d0)
  a(3,1) = (0.5d0, 0.25d0); a(3,2) = (2.0d0, 1.0d0);  a(3,3) = (16.0d0, 3.0d0)
  s(1) = 1.0d0; s(2) = 1.0d0; s(3) = 1.0d0
  scond = 0.5d0
  amax = 16.0d0
  call zlaqsy('U', n, a, 10, s, scond, amax, equed)
  do j = 1, n
    do i = 1, n
      apk(i + (j-1)*n) = a(i, j)
    end do
  end do
  call begin_test('no_equilibrate')
  call print_array('a', apk_r, 2*n*n)
  call print_char('equed', equed)
  call end_test()

  ! Test 4: N=0 quick return
  n = 0
  call zlaqsy('U', n, a, 10, s, 1.0d0, 1.0d0, equed)
  call begin_test('n_zero')
  call print_char('equed', equed)
  call end_test()

  ! Test 5: N=1, upper, equilibration needed
  n = 1
  a(1,1) = (100.0d0, 50.0d0)
  s(1) = 0.1d0
  scond = 0.01d0
  amax = 100.0d0
  call zlaqsy('U', n, a, 10, s, scond, amax, equed)
  do j = 1, n
    do i = 1, n
      apk(i + (j-1)*n) = a(i, j)
    end do
  end do
  call begin_test('n_one_upper')
  call print_array('a', apk_r, 2*n*n)
  call print_char('equed', equed)
  call end_test()

  ! Test 6: amax very small (triggers equilibration via amax < SMALL)
  n = 2
  a(1,1) = (1.0d-300, 2.0d-300); a(1,2) = (0.0d0, 0.0d0)
  a(2,1) = (0.0d0, 0.0d0);       a(2,2) = (1.0d-300, 3.0d-300)
  s(1) = 1.0d150; s(2) = 1.0d150
  scond = 1.0d0
  amax = 1.0d-300
  call zlaqsy('U', n, a, 10, s, scond, amax, equed)
  do j = 1, n
    do i = 1, n
      apk(i + (j-1)*n) = a(i, j)
    end do
  end do
  call begin_test('small_amax')
  call print_array('a', apk_r, 2*n*n)
  call print_char('equed', equed)
  call end_test()

end program
