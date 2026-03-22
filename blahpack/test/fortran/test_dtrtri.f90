program test_dtrtri
  use test_utils
  implicit none
  double precision :: a(25)
  integer :: info

  ! Test 1: upper, non-unit, 3x3
  ! A = [2 1 3; 0 4 5; 0 0 6]
  a = 0.0d0
  a(1) = 2.0d0; a(4) = 1.0d0; a(7) = 3.0d0
  a(5) = 4.0d0; a(8) = 5.0d0
  a(9) = 6.0d0
  call dtrtri('U', 'N', 3, a, 3, info)
  call begin_test('upper_nonunit_3')
  call print_int('info', info)
  call print_matrix('a', a, 3, 3, 3)
  call end_test()

  ! Test 2: lower, non-unit, 3x3
  ! A = [2 0 0; 1 4 0; 3 5 6]
  a = 0.0d0
  a(1) = 2.0d0; a(2) = 1.0d0; a(3) = 3.0d0
  a(5) = 4.0d0; a(6) = 5.0d0
  a(9) = 6.0d0
  call dtrtri('L', 'N', 3, a, 3, info)
  call begin_test('lower_nonunit_3')
  call print_int('info', info)
  call print_matrix('a', a, 3, 3, 3)
  call end_test()

  ! Test 3: upper, non-unit, 4x4
  ! A = [1 2 3 4; 0 5 6 7; 0 0 8 9; 0 0 0 10]
  a = 0.0d0
  a(1)  = 1.0d0; a(5)  = 2.0d0; a(9)  = 3.0d0; a(13) = 4.0d0
  a(6)  = 5.0d0; a(10) = 6.0d0; a(14) = 7.0d0
  a(11) = 8.0d0; a(15) = 9.0d0
  a(16) = 10.0d0
  call dtrtri('U', 'N', 4, a, 4, info)
  call begin_test('upper_nonunit_4')
  call print_int('info', info)
  call print_matrix('a', a, 4, 4, 4)
  call end_test()

  ! Test 4: lower, non-unit, 4x4
  ! A = [1 0 0 0; 2 5 0 0; 3 6 8 0; 4 7 9 10]
  a = 0.0d0
  a(1)  = 1.0d0; a(2)  = 2.0d0; a(3)  = 3.0d0; a(4)  = 4.0d0
  a(6)  = 5.0d0; a(7)  = 6.0d0; a(8)  = 7.0d0
  a(11) = 8.0d0; a(12) = 9.0d0
  a(16) = 10.0d0
  call dtrtri('L', 'N', 4, a, 4, info)
  call begin_test('lower_nonunit_4')
  call print_int('info', info)
  call print_matrix('a', a, 4, 4, 4)
  call end_test()

  ! Test 5: N=0 quick return
  call dtrtri('U', 'N', 0, a, 1, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: singular (zero diagonal) — should return info > 0
  a = 0.0d0
  a(1) = 2.0d0; a(4) = 3.0d0
  a(5) = 0.0d0  ! zero diagonal at (2,2)
  a(9) = 6.0d0
  call dtrtri('U', 'N', 3, a, 3, info)
  call begin_test('singular')
  call print_int('info', info)
  call end_test()

  ! Test 7: unit diag, upper, 3x3
  a = 0.0d0
  a(1) = 99.0d0; a(4) = 1.0d0; a(7) = 3.0d0
  a(5) = 99.0d0; a(8) = 5.0d0
  a(9) = 99.0d0
  call dtrtri('U', 'U', 3, a, 3, info)
  call begin_test('upper_unit')
  call print_int('info', info)
  call print_matrix('a', a, 3, 3, 3)
  call end_test()

  ! Test 8: identity 3x3
  a = 0.0d0
  a(1) = 1.0d0; a(5) = 1.0d0; a(9) = 1.0d0
  call dtrtri('U', 'N', 3, a, 3, info)
  call begin_test('identity')
  call print_int('info', info)
  call print_matrix('a', a, 3, 3, 3)
  call end_test()

  ! Test 9: 5x5 upper to exercise blocking
  a = 0.0d0
  a(1)  = 2.0d0; a(6)  = 1.0d0; a(11) = 3.0d0; a(16) = 2.0d0; a(21) = 1.0d0
  a(7)  = 4.0d0; a(12) = 1.0d0; a(17) = 3.0d0; a(22) = 2.0d0
  a(13) = 5.0d0; a(18) = 1.0d0; a(23) = 4.0d0
  a(19) = 6.0d0; a(24) = 1.0d0
  a(25) = 3.0d0
  call dtrtri('U', 'N', 5, a, 5, info)
  call begin_test('upper_5x5')
  call print_int('info', info)
  call print_matrix('a', a, 5, 5, 5)
  call end_test()

  ! Test 10: 5x5 lower to exercise blocking
  a = 0.0d0
  a(1)  = 2.0d0; a(2)  = 1.0d0; a(3)  = 3.0d0; a(4)  = 2.0d0; a(5)  = 1.0d0
  a(7)  = 4.0d0; a(8)  = 1.0d0; a(9)  = 3.0d0; a(10) = 2.0d0
  a(13) = 5.0d0; a(14) = 1.0d0; a(15) = 4.0d0
  a(19) = 6.0d0; a(20) = 1.0d0
  a(25) = 3.0d0
  call dtrtri('L', 'N', 5, a, 5, info)
  call begin_test('lower_5x5')
  call print_int('info', info)
  call print_matrix('a', a, 5, 5, 5)
  call end_test()

end program
