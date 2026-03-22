program test_dtrti2
  use test_utils
  implicit none
  double precision :: a(16)
  integer :: info

  ! Test 1: upper, non-unit, 3x3
  ! A = [2 1 3; 0 4 5; 0 0 6]
  a = 0.0d0
  a(1) = 2.0d0; a(4) = 1.0d0; a(7) = 3.0d0
  a(5) = 4.0d0; a(8) = 5.0d0
  a(9) = 6.0d0
  call dtrti2('U', 'N', 3, a, 3, info)
  call begin_test('upper_nonunit')
  call print_int('info', info)
  call print_matrix('a', a, 3, 3, 3)
  call end_test()

  ! Test 2: lower, non-unit, 3x3
  ! A = [2 0 0; 1 4 0; 3 5 6]
  a = 0.0d0
  a(1) = 2.0d0; a(2) = 1.0d0; a(3) = 3.0d0
  a(5) = 4.0d0; a(6) = 5.0d0
  a(9) = 6.0d0
  call dtrti2('L', 'N', 3, a, 3, info)
  call begin_test('lower_nonunit')
  call print_int('info', info)
  call print_matrix('a', a, 3, 3, 3)
  call end_test()

  ! Test 3: upper, unit diag, 3x3
  ! A = [1 1 3; 0 1 5; 0 0 1] (diagonal treated as 1)
  a = 0.0d0
  a(1) = 99.0d0; a(4) = 1.0d0; a(7) = 3.0d0
  a(5) = 99.0d0; a(8) = 5.0d0
  a(9) = 99.0d0
  call dtrti2('U', 'U', 3, a, 3, info)
  call begin_test('upper_unit')
  call print_int('info', info)
  call print_matrix('a', a, 3, 3, 3)
  call end_test()

  ! Test 4: lower, unit diag, 3x3
  a = 0.0d0
  a(1) = 99.0d0; a(2) = 1.0d0; a(3) = 3.0d0
  a(5) = 99.0d0; a(6) = 5.0d0
  a(9) = 99.0d0
  call dtrti2('L', 'U', 3, a, 3, info)
  call begin_test('lower_unit')
  call print_int('info', info)
  call print_matrix('a', a, 3, 3, 3)
  call end_test()

  ! Test 5: N=0 (quick return)
  call dtrti2('U', 'N', 0, a, 1, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: N=1, non-unit
  a(1) = 4.0d0
  call dtrti2('U', 'N', 1, a, 1, info)
  call begin_test('n_one')
  call print_int('info', info)
  call print_array('a', a, 1)
  call end_test()

  ! Test 7: identity 3x3, upper
  a = 0.0d0
  a(1) = 1.0d0; a(5) = 1.0d0; a(9) = 1.0d0
  call dtrti2('U', 'N', 3, a, 3, info)
  call begin_test('identity')
  call print_int('info', info)
  call print_matrix('a', a, 3, 3, 3)
  call end_test()

end program
