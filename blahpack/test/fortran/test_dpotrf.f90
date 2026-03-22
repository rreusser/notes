program test_dpotrf
  use test_utils
  implicit none
  double precision :: a(100)
  integer :: info

  ! Test 1: 3x3 SPD lower
  ! A = [4 2 1; 2 5 3; 1 3 9]
  a = 0.0d0
  a(1) = 4.0d0; a(2) = 2.0d0; a(3) = 1.0d0
  a(4) = 2.0d0; a(5) = 5.0d0; a(6) = 3.0d0
  a(7) = 1.0d0; a(8) = 3.0d0; a(9) = 9.0d0
  call dpotrf('L', 3, a, 3, info)
  call begin_test('lower_3x3')
  call print_matrix('L', a, 3, 3, 3)
  call print_int('info', info)
  call end_test()

  ! Test 2: 3x3 SPD upper
  a = 0.0d0
  a(1) = 4.0d0; a(2) = 2.0d0; a(3) = 1.0d0
  a(4) = 2.0d0; a(5) = 5.0d0; a(6) = 3.0d0
  a(7) = 1.0d0; a(8) = 3.0d0; a(9) = 9.0d0
  call dpotrf('U', 3, a, 3, info)
  call begin_test('upper_3x3')
  call print_matrix('U', a, 3, 3, 3)
  call print_int('info', info)
  call end_test()

  ! Test 3: 4x4 SPD lower
  a = 0.0d0
  a(1) = 4.0d0;  a(2) = 2.0d0;  a(3) = 1.0d0;  a(4) = 0.0d0
  a(5) = 2.0d0;  a(6) = 5.0d0;  a(7) = 3.0d0;  a(8) = 1.0d0
  a(9) = 1.0d0;  a(10) = 3.0d0; a(11) = 9.0d0; a(12) = 2.0d0
  a(13) = 0.0d0; a(14) = 1.0d0; a(15) = 2.0d0; a(16) = 8.0d0
  call dpotrf('L', 4, a, 4, info)
  call begin_test('lower_4x4')
  call print_matrix('L', a, 4, 4, 4)
  call print_int('info', info)
  call end_test()

  ! Test 4: 4x4 SPD upper
  a = 0.0d0
  a(1) = 4.0d0;  a(2) = 2.0d0;  a(3) = 1.0d0;  a(4) = 0.0d0
  a(5) = 2.0d0;  a(6) = 5.0d0;  a(7) = 3.0d0;  a(8) = 1.0d0
  a(9) = 1.0d0;  a(10) = 3.0d0; a(11) = 9.0d0; a(12) = 2.0d0
  a(13) = 0.0d0; a(14) = 1.0d0; a(15) = 2.0d0; a(16) = 8.0d0
  call dpotrf('U', 4, a, 4, info)
  call begin_test('upper_4x4')
  call print_matrix('U', a, 4, 4, 4)
  call print_int('info', info)
  call end_test()

  ! Test 5: not positive definite
  a = 0.0d0
  a(1) = 1.0d0; a(2) = 2.0d0; a(3) = 3.0d0
  a(4) = 2.0d0; a(5) = 1.0d0; a(6) = 4.0d0
  a(7) = 3.0d0; a(8) = 4.0d0; a(9) = 1.0d0
  call dpotrf('L', 3, a, 3, info)
  call begin_test('not_posdef')
  call print_int('info', info)
  call end_test()

  ! Test 6: N=0
  call dpotrf('L', 0, a, 1, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

end program
