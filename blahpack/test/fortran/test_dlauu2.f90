program test_dlauu2
  use test_utils
  implicit none
  double precision :: a(16)
  integer :: info

  ! Test 1: upper triangular 3x3
  ! A = [2 1 3; 0 4 5; 0 0 6]
  ! Result = U * U^T
  a = 0.0d0
  a(1) = 2.0d0; a(4) = 1.0d0; a(7) = 3.0d0
  a(5) = 4.0d0; a(8) = 5.0d0
  a(9) = 6.0d0
  call dlauu2('U', 3, a, 3, info)
  call begin_test('upper_3x3')
  call print_int('info', info)
  call print_matrix('a', a, 3, 3, 3)
  call end_test()

  ! Test 2: lower triangular 3x3
  ! A = [2 0 0; 1 4 0; 3 5 6]
  ! Result = L^T * L
  a = 0.0d0
  a(1) = 2.0d0; a(2) = 1.0d0; a(3) = 3.0d0
  a(5) = 4.0d0; a(6) = 5.0d0
  a(9) = 6.0d0
  call dlauu2('L', 3, a, 3, info)
  call begin_test('lower_3x3')
  call print_int('info', info)
  call print_matrix('a', a, 3, 3, 3)
  call end_test()

  ! Test 3: N=1 edge case
  a(1) = 5.0d0
  call dlauu2('U', 1, a, 1, info)
  call begin_test('n_one')
  call print_int('info', info)
  call print_array('a', a, 1)
  call end_test()

  ! Test 4: N=0 quick return
  call dlauu2('U', 0, a, 1, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 5: lower N=1 edge case
  a(1) = 3.0d0
  call dlauu2('L', 1, a, 1, info)
  call begin_test('lower_n_one')
  call print_int('info', info)
  call print_array('a', a, 1)
  call end_test()

  ! Test 6: upper 4x4 larger matrix
  ! A = [1 2 3 4; 0 5 6 7; 0 0 8 9; 0 0 0 10]
  a = 0.0d0
  a(1) = 1.0d0; a(5) = 2.0d0; a(9)  = 3.0d0; a(13) = 4.0d0
                 a(6) = 5.0d0; a(10) = 6.0d0; a(14) = 7.0d0
                                a(11) = 8.0d0; a(15) = 9.0d0
                                                a(16) = 10.0d0
  call dlauu2('U', 4, a, 4, info)
  call begin_test('upper_4x4')
  call print_int('info', info)
  call print_matrix('a', a, 4, 4, 4)
  call end_test()

  ! Test 7: identity 3x3 upper
  a = 0.0d0
  a(1) = 1.0d0; a(5) = 1.0d0; a(9) = 1.0d0
  call dlauu2('U', 3, a, 3, info)
  call begin_test('identity_upper')
  call print_int('info', info)
  call print_matrix('a', a, 3, 3, 3)
  call end_test()

  ! Test 8: identity 3x3 lower
  a = 0.0d0
  a(1) = 1.0d0; a(5) = 1.0d0; a(9) = 1.0d0
  call dlauu2('L', 3, a, 3, info)
  call begin_test('identity_lower')
  call print_int('info', info)
  call print_matrix('a', a, 3, 3, 3)
  call end_test()

end program
