program test_dpotf2
  use test_utils
  implicit none

  double precision :: a(16), a_save(16)
  integer :: info, i

  ! Test 1: 3x3 positive definite matrix, lower triangular
  ! A = [4 2 1; 2 5 3; 1 3 6] (column-major flat)
  a = 0.0d0
  a(1) = 4.0d0; a(2) = 2.0d0; a(3) = 1.0d0   ! col 1
  a(4) = 2.0d0; a(5) = 5.0d0; a(6) = 3.0d0   ! col 2
  a(7) = 1.0d0; a(8) = 3.0d0; a(9) = 6.0d0   ! col 3
  call dpotf2('L', 3, a, 3, info)
  call begin_test('lower_3x3')
  call print_int('info', info)
  call print_array('a', a, 9)
  call end_test()

  ! Test 2: same matrix, upper triangular
  a = 0.0d0
  a(1) = 4.0d0; a(2) = 2.0d0; a(3) = 1.0d0
  a(4) = 2.0d0; a(5) = 5.0d0; a(6) = 3.0d0
  a(7) = 1.0d0; a(8) = 3.0d0; a(9) = 6.0d0
  call dpotf2('U', 3, a, 3, info)
  call begin_test('upper_3x3')
  call print_int('info', info)
  call print_array('a', a, 9)
  call end_test()

  ! Test 3: 1x1 matrix
  a(1) = 9.0d0
  call dpotf2('L', 1, a, 1, info)
  call begin_test('n_one')
  call print_int('info', info)
  call print_array('a', a, 1)
  call end_test()

  ! Test 4: n=0 (quick return)
  call dpotf2('L', 0, a, 1, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 5: not positive definite (should return info > 0)
  a = 0.0d0
  a(1) = 1.0d0; a(2) = 2.0d0;  ! col 1
  a(3) = 2.0d0; a(4) = 1.0d0;  ! col 2 — not positive definite
  call dpotf2('L', 2, a, 2, info)
  call begin_test('not_posdef')
  call print_int('info', info)
  call print_array('a', a, 4)
  call end_test()

  ! Test 6: 4x4 identity matrix
  a = 0.0d0
  a(1) = 1.0d0; a(6) = 1.0d0; a(11) = 1.0d0; a(16) = 1.0d0
  call dpotf2('L', 4, a, 4, info)
  call begin_test('identity_4x4')
  call print_int('info', info)
  call print_array('a', a, 16)
  call end_test()

end program
