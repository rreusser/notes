program test_zlauum
  use test_utils
  implicit none
  complex*16 :: a(25)
  double precision :: a_r(50)
  equivalence (a, a_r)
  integer :: info

  ! Test 1: upper triangular 3x3 (real diagonal)
  a = (0.0d0, 0.0d0)
  a(1) = (2.0d0, 0.0d0); a(4) = (1.0d0, 0.5d0); a(7) = (3.0d0, 1.0d0)
  a(5) = (4.0d0, 0.0d0); a(8) = (5.0d0, 1.0d0)
  a(9) = (6.0d0, 0.0d0)
  call zlauum('U', 3, a, 3, info)
  call begin_test('upper_3x3')
  call print_int('info', info)
  call print_array('a', a_r, 18)
  call end_test()

  ! Test 2: lower triangular 3x3 (real diagonal)
  a = (0.0d0, 0.0d0)
  a(1) = (2.0d0, 0.0d0); a(2) = (1.0d0, 0.5d0); a(3) = (3.0d0, 1.0d0)
  a(5) = (4.0d0, 0.0d0); a(6) = (5.0d0, 1.0d0)
  a(9) = (6.0d0, 0.0d0)
  call zlauum('L', 3, a, 3, info)
  call begin_test('lower_3x3')
  call print_int('info', info)
  call print_array('a', a_r, 18)
  call end_test()

  ! Test 3: N=1
  a(1) = (5.0d0, 0.0d0)
  call zlauum('U', 1, a, 1, info)
  call begin_test('n_one')
  call print_int('info', info)
  call print_array('a', a_r, 2)
  call end_test()

  ! Test 4: N=0 quick return
  call zlauum('U', 0, a, 1, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 5: upper 4x4 (real diagonal)
  a = (0.0d0, 0.0d0)
  a(1)  = (1.0d0, 0.0d0); a(5)  = (2.0d0, 1.0d0); a(9)  = (3.0d0, 0.0d0); a(13) = (4.0d0, 2.0d0)
                            a(6)  = (5.0d0, 0.0d0); a(10) = (6.0d0, 0.5d0); a(14) = (7.0d0, 3.0d0)
                                                      a(11) = (8.0d0, 0.0d0); a(15) = (9.0d0, 1.0d0)
                                                                                a(16) = (10.0d0, 0.0d0)
  call zlauum('U', 4, a, 4, info)
  call begin_test('upper_4x4')
  call print_int('info', info)
  call print_array('a', a_r, 32)
  call end_test()

  ! Test 6: lower 4x4 (real diagonal)
  a = (0.0d0, 0.0d0)
  a(1)  = (1.0d0, 0.0d0); a(2)  = (2.0d0, 1.0d0); a(3)  = (3.0d0, 0.0d0); a(4) = (4.0d0, 2.0d0)
                            a(6)  = (5.0d0, 0.0d0); a(7)  = (6.0d0, 0.5d0); a(8) = (7.0d0, 3.0d0)
                                                      a(11) = (8.0d0, 0.0d0); a(12) = (9.0d0, 1.0d0)
                                                                                a(16) = (10.0d0, 0.0d0)
  call zlauum('L', 4, a, 4, info)
  call begin_test('lower_4x4')
  call print_int('info', info)
  call print_array('a', a_r, 32)
  call end_test()

  ! Test 7: identity 3x3 upper
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 0.0d0); a(5) = (1.0d0, 0.0d0); a(9) = (1.0d0, 0.0d0)
  call zlauum('U', 3, a, 3, info)
  call begin_test('identity_upper')
  call print_int('info', info)
  call print_array('a', a_r, 18)
  call end_test()

end program
