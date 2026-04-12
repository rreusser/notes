program test_zlat2c
  use test_utils
  implicit none
  complex*16 :: a(16)
  double precision :: a_r(32)
  equivalence (a, a_r)
  complex :: sa(16)
  double precision :: sa_d(32)
  integer :: i, info
  double precision :: re, im

  ! Test 1: upper 3x3, simple values
  a = (0.0d0, 0.0d0)
  do i = 1, 9
    re = dble(i) * 1.25d0
    im = dble(i) * 0.5d0
    a(i) = dcmplx(re, im)
  end do
  sa = (0.0, 0.0); info = 0
  call zlat2c('U', 3, a, 3, sa, 3, info)
  do i = 1, 9
    sa_d(2*i - 1) = dble(real(sa(i)))
    sa_d(2*i)     = dble(aimag(sa(i)))
  end do
  call begin_test('upper_3x3')
  call print_int('info', info)
  call print_array('sa', sa_d, 18)
  call end_test()

  ! Test 2: lower 3x3
  sa = (0.0, 0.0); info = 0
  call zlat2c('L', 3, a, 3, sa, 3, info)
  do i = 1, 9
    sa_d(2*i - 1) = dble(real(sa(i)))
    sa_d(2*i)     = dble(aimag(sa(i)))
  end do
  call begin_test('lower_3x3')
  call print_int('info', info)
  call print_array('sa', sa_d, 18)
  call end_test()

  ! Test 3: n=0 (quick return)
  sa = (0.0, 0.0); info = 0
  call zlat2c('U', 0, a, 3, sa, 3, info)
  do i = 1, 9
    sa_d(2*i - 1) = dble(real(sa(i)))
    sa_d(2*i)     = dble(aimag(sa(i)))
  end do
  call begin_test('n_zero')
  call print_int('info', info)
  call print_array('sa', sa_d, 18)
  call end_test()

  ! Test 4: n=1
  sa = (0.0, 0.0); info = 0
  call zlat2c('U', 1, a, 3, sa, 3, info)
  do i = 1, 9
    sa_d(2*i - 1) = dble(real(sa(i)))
    sa_d(2*i)     = dble(aimag(sa(i)))
  end do
  call begin_test('n_one')
  call print_int('info', info)
  call print_array('sa', sa_d, 18)
  call end_test()

  ! Test 5: overflow real part upper
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 0.5d0)
  a(4) = (1.0d300, 0.0d0)
  a(5) = (2.0d0, 0.0d0)
  a(7) = (3.0d0, 0.0d0)
  a(8) = (4.0d0, 0.0d0)
  a(9) = (5.0d0, 0.0d0)
  sa = (0.0, 0.0); info = 0
  call zlat2c('U', 3, a, 3, sa, 3, info)
  call begin_test('overflow_upper_real')
  call print_int('info', info)
  call end_test()

  ! Test 6: overflow imaginary part upper (negative huge)
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 0.5d0)
  a(4) = (0.0d0, -1.0d300)
  a(5) = (2.0d0, 0.0d0)
  a(7) = (3.0d0, 0.0d0)
  a(8) = (4.0d0, 0.0d0)
  a(9) = (5.0d0, 0.0d0)
  sa = (0.0, 0.0); info = 0
  call zlat2c('U', 3, a, 3, sa, 3, info)
  call begin_test('overflow_upper_imag')
  call print_int('info', info)
  call end_test()

  ! Test 7: overflow in lower
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 0.0d0)
  a(2) = (-1.0d300, 0.0d0)
  a(3) = (2.0d0, 0.0d0)
  a(5) = (3.0d0, 0.0d0)
  a(6) = (4.0d0, 0.0d0)
  a(9) = (5.0d0, 0.0d0)
  sa = (0.0, 0.0); info = 0
  call zlat2c('L', 3, a, 3, sa, 3, info)
  call begin_test('overflow_lower')
  call print_int('info', info)
  call end_test()

  ! Test 8: lower 4x4, safe values
  a = (0.0d0, 0.0d0)
  do i = 1, 16
    re = dble(i) * 0.5d0
    im = dble(i) * 0.25d0
    a(i) = dcmplx(re, im)
  end do
  sa = (0.0, 0.0); info = 0
  call zlat2c('L', 4, a, 4, sa, 4, info)
  do i = 1, 16
    sa_d(2*i - 1) = dble(real(sa(i)))
    sa_d(2*i)     = dble(aimag(sa(i)))
  end do
  call begin_test('lower_4x4')
  call print_int('info', info)
  call print_array('sa', sa_d, 32)
  call end_test()

end program
