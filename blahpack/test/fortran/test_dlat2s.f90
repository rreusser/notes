program test_dlat2s
  use test_utils
  implicit none
  double precision :: a(16)
  real :: sa(16)
  double precision :: sa_d(16)
  integer :: i, info

  ! Initialize A as a 3x3 matrix (column-major), values 1..9
  a = 0.0d0
  do i = 1, 9
    a(i) = dble(i) + 0.25d0 * dble(i)
  end do

  ! Test 1: upper, 3x3
  sa = 0.0; info = 0
  call dlat2s('U', 3, a, 3, sa, 3, info)
  sa_d = dble(sa)
  call begin_test('upper_3x3')
  call print_int('info', info)
  call print_array('sa', sa_d, 9)
  call end_test()

  ! Test 2: lower, 3x3
  sa = 0.0; info = 0
  call dlat2s('L', 3, a, 3, sa, 3, info)
  sa_d = dble(sa)
  call begin_test('lower_3x3')
  call print_int('info', info)
  call print_array('sa', sa_d, 9)
  call end_test()

  ! Test 3: n=0 (quick return)
  sa = 0.0; info = 0
  call dlat2s('U', 0, a, 3, sa, 3, info)
  sa_d = dble(sa)
  call begin_test('n_zero')
  call print_int('info', info)
  call print_array('sa', sa_d, 9)
  call end_test()

  ! Test 4: n=1
  sa = 0.0; info = 0
  call dlat2s('U', 1, a, 3, sa, 3, info)
  sa_d = dble(sa)
  call begin_test('n_one')
  call print_int('info', info)
  call print_array('sa', sa_d, 9)
  call end_test()

  ! Test 5: overflow upper. Put a huge value > slamch('O') single in upper.
  a = 0.0d0
  a(1) = 1.0d0
  a(4) = 1.0d300  ! A(1,2) huge
  a(5) = 2.0d0
  a(7) = 3.0d0
  a(8) = 4.0d0
  a(9) = 5.0d0
  sa = 0.0; info = 0
  call dlat2s('U', 3, a, 3, sa, 3, info)
  sa_d = dble(sa)
  call begin_test('overflow_upper')
  call print_int('info', info)
  call end_test()

  ! Test 6: overflow lower (negative huge)
  a = 0.0d0
  a(1) = 1.0d0
  a(2) = -1.0d300  ! A(2,1) huge negative
  a(3) = 2.0d0
  a(5) = 3.0d0
  a(6) = 4.0d0
  a(9) = 5.0d0
  sa = 0.0; info = 0
  call dlat2s('L', 3, a, 3, sa, 3, info)
  sa_d = dble(sa)
  call begin_test('overflow_lower')
  call print_int('info', info)
  call end_test()

  ! Test 7: lower 4x4, safe values
  a = 0.0d0
  do i = 1, 16
    a(i) = dble(i) * 0.5d0
  end do
  sa = 0.0; info = 0
  call dlat2s('L', 4, a, 4, sa, 4, info)
  sa_d = dble(sa)
  call begin_test('lower_4x4')
  call print_int('info', info)
  call print_array('sa', sa_d, 16)
  call end_test()

end program
