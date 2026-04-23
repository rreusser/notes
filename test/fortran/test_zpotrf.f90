program test_zpotrf
  use test_utils
  implicit none
  complex*16 :: a(100)
  double precision :: a_r(200)
  equivalence (a, a_r)
  integer :: info

  ! Test 1: lower_3x3
  a = (0.0d0, 0.0d0)
  a(1) = (10.0d0, 0.0d0); a(2) = (3.0d0, 1.0d0); a(3) = (1.0d0, -2.0d0)
  a(4) = (3.0d0, -1.0d0); a(5) = (8.0d0, 0.0d0); a(6) = (2.0d0, 1.0d0)
  a(7) = (1.0d0, 2.0d0); a(8) = (2.0d0, -1.0d0); a(9) = (6.0d0, 0.0d0)
  call zpotrf('L', 3, a, 3, info)
  call begin_test('lower_3x3')
  call print_int('info', info)
  call print_array('a', a_r, 18)
  call end_test()

  ! Test 2: upper_3x3
  a = (0.0d0, 0.0d0)
  a(1) = (10.0d0, 0.0d0); a(2) = (3.0d0, 1.0d0); a(3) = (1.0d0, -2.0d0)
  a(4) = (3.0d0, -1.0d0); a(5) = (8.0d0, 0.0d0); a(6) = (2.0d0, 1.0d0)
  a(7) = (1.0d0, 2.0d0); a(8) = (2.0d0, -1.0d0); a(9) = (6.0d0, 0.0d0)
  call zpotrf('U', 3, a, 3, info)
  call begin_test('upper_3x3')
  call print_int('info', info)
  call print_array('a', a_r, 18)
  call end_test()

  ! Test 3: lower_4x4
  a = (0.0d0, 0.0d0)
  a(1)  = (14.0d0, 0.0d0); a(2)  = (4.0d0, 2.0d0);  a(3)  = (2.0d0, -1.0d0); a(4) = (1.0d0, 3.0d0)
  a(5)  = (4.0d0, -2.0d0); a(6)  = (12.0d0, 0.0d0);  a(7)  = (3.0d0, 1.0d0);  a(8) = (2.0d0, -2.0d0)
  a(9)  = (2.0d0, 1.0d0);  a(10) = (3.0d0, -1.0d0);  a(11) = (10.0d0, 0.0d0); a(12) = (1.0d0, 1.0d0)
  a(13) = (1.0d0, -3.0d0); a(14) = (2.0d0, 2.0d0);   a(15) = (1.0d0, -1.0d0); a(16) = (9.0d0, 0.0d0)
  call zpotrf('L', 4, a, 4, info)
  call begin_test('lower_4x4')
  call print_int('info', info)
  call print_array('a', a_r, 32)
  call end_test()

  ! Test 4: upper_4x4
  a = (0.0d0, 0.0d0)
  a(1)  = (14.0d0, 0.0d0); a(2)  = (4.0d0, 2.0d0);  a(3)  = (2.0d0, -1.0d0); a(4) = (1.0d0, 3.0d0)
  a(5)  = (4.0d0, -2.0d0); a(6)  = (12.0d0, 0.0d0);  a(7)  = (3.0d0, 1.0d0);  a(8) = (2.0d0, -2.0d0)
  a(9)  = (2.0d0, 1.0d0);  a(10) = (3.0d0, -1.0d0);  a(11) = (10.0d0, 0.0d0); a(12) = (1.0d0, 1.0d0)
  a(13) = (1.0d0, -3.0d0); a(14) = (2.0d0, 2.0d0);   a(15) = (1.0d0, -1.0d0); a(16) = (9.0d0, 0.0d0)
  call zpotrf('U', 4, a, 4, info)
  call begin_test('upper_4x4')
  call print_int('info', info)
  call print_array('a', a_r, 32)
  call end_test()

  ! Test 5: not positive definite
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 0.0d0); a(2) = (2.0d0, 1.0d0); a(3) = (3.0d0, 0.0d0)
  a(4) = (2.0d0, -1.0d0); a(5) = (1.0d0, 0.0d0); a(6) = (4.0d0, 0.0d0)
  a(7) = (3.0d0, 0.0d0); a(8) = (4.0d0, 0.0d0); a(9) = (1.0d0, 0.0d0)
  call zpotrf('L', 3, a, 3, info)
  call begin_test('not_posdef')
  call print_int('info', info)
  call end_test()

  ! Test 6: N=0
  call zpotrf('L', 0, a, 1, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

end program
