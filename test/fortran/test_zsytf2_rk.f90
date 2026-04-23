program test_zsytf2_rk
  use test_utils
  implicit none

  complex*16 :: A4(4,4), A5(5,5), A3(3,3), A1(1,1)
  complex*16 :: E4(4), E5(5), E3(3), E1(1)
  double precision :: A4_r(32), A5_r(50), A3_r(18), A1_r(2)
  double precision :: E4_r(8), E5_r(10), E3_r(6), E1_r(2)
  equivalence (A4, A4_r)
  equivalence (A5, A5_r)
  equivalence (A3, A3_r)
  equivalence (A1, A1_r)
  equivalence (E4, E4_r)
  equivalence (E5, E5_r)
  equivalence (E3, E3_r)
  equivalence (E1, E1_r)
  integer :: ipiv4(4), ipiv5(5), ipiv3(3), ipiv1(1), info

  ! Test 1: Upper 4x4 complex symmetric
  A4 = (0.0d0, 0.0d0)
  A4(1,1) = (4.0d0, 0.5d0)
  A4(1,2) = (1.0d0, 2.0d0); A4(2,2) = (5.0d0, -0.5d0)
  A4(1,3) = (3.0d0, -1.0d0); A4(2,3) = (2.0d0, 1.0d0); A4(3,3) = (7.0d0, 1.0d0)
  A4(1,4) = (0.5d0, 0.5d0); A4(2,4) = (1.0d0, -2.0d0); A4(3,4) = (3.0d0, 0.0d0); A4(4,4) = (6.0d0, -1.0d0)
  E4 = (0.0d0, 0.0d0); ipiv4 = 0
  call zsytf2_rk('U', 4, A4, 4, E4, ipiv4, info)
  call begin_test('upper_4x4')
  call print_array('a', A4_r, 32)
  call print_array('e', E4_r, 8)
  call print_int_array('ipiv', ipiv4, 4)
  call print_int('info', info)
  call end_test()

  ! Test 2: Lower 4x4 complex symmetric
  A4 = (0.0d0, 0.0d0)
  A4(1,1) = (4.0d0, 0.5d0)
  A4(2,1) = (1.0d0, 2.0d0); A4(2,2) = (5.0d0, -0.5d0)
  A4(3,1) = (3.0d0, -1.0d0); A4(3,2) = (2.0d0, 1.0d0); A4(3,3) = (7.0d0, 1.0d0)
  A4(4,1) = (0.5d0, 0.5d0); A4(4,2) = (1.0d0, -2.0d0); A4(4,3) = (3.0d0, 0.0d0); A4(4,4) = (6.0d0, -1.0d0)
  E4 = (0.0d0, 0.0d0); ipiv4 = 0
  call zsytf2_rk('L', 4, A4, 4, E4, ipiv4, info)
  call begin_test('lower_4x4')
  call print_array('a', A4_r, 32)
  call print_array('e', E4_r, 8)
  call print_int_array('ipiv', ipiv4, 4)
  call print_int('info', info)
  call end_test()

  ! Test 3: Indefinite upper 4x4 with zero diagonal
  A4 = (0.0d0, 0.0d0)
  A4(1,2) = (1.0d0, 1.0d0)
  A4(1,3) = (2.0d0, -1.0d0)
  A4(2,3) = (4.0d0, 2.0d0)
  A4(1,4) = (3.0d0, 0.5d0)
  A4(2,4) = (5.0d0, -1.0d0)
  A4(3,4) = (6.0d0, 1.5d0)
  E4 = (0.0d0, 0.0d0); ipiv4 = 0
  call zsytf2_rk('U', 4, A4, 4, E4, ipiv4, info)
  call begin_test('indef_upper_4x4')
  call print_array('a', A4_r, 32)
  call print_array('e', E4_r, 8)
  call print_int_array('ipiv', ipiv4, 4)
  call print_int('info', info)
  call end_test()

  ! Test 4: Indefinite lower 4x4
  A4 = (0.0d0, 0.0d0)
  A4(2,1) = (1.0d0, 1.0d0)
  A4(3,1) = (2.0d0, -1.0d0)
  A4(3,2) = (4.0d0, 2.0d0)
  A4(4,1) = (3.0d0, 0.5d0)
  A4(4,2) = (5.0d0, -1.0d0)
  A4(4,3) = (6.0d0, 1.5d0)
  E4 = (0.0d0, 0.0d0); ipiv4 = 0
  call zsytf2_rk('L', 4, A4, 4, E4, ipiv4, info)
  call begin_test('indef_lower_4x4')
  call print_array('a', A4_r, 32)
  call print_array('e', E4_r, 8)
  call print_int_array('ipiv', ipiv4, 4)
  call print_int('info', info)
  call end_test()

  ! Test 5: N=1
  A1(1,1) = (5.0d0, 1.0d0)
  E1 = (0.0d0, 0.0d0); ipiv1 = 0
  call zsytf2_rk('L', 1, A1, 1, E1, ipiv1, info)
  call begin_test('n1_lower')
  call print_array('a', A1_r, 2)
  call print_array('e', E1_r, 2)
  call print_int_array('ipiv', ipiv1, 1)
  call print_int('info', info)
  call end_test()

  ! Test 6: Singular lower (info > 0)
  A3 = (0.0d0, 0.0d0)
  A3(1,1) = (1.0d0, 0.0d0)
  A3(3,3) = (2.0d0, 0.0d0)
  E3 = (0.0d0, 0.0d0); ipiv3 = 0
  call zsytf2_rk('L', 3, A3, 3, E3, ipiv3, info)
  call begin_test('singular_lower')
  call print_array('a', A3_r, 18)
  call print_array('e', E3_r, 6)
  call print_int_array('ipiv', ipiv3, 3)
  call print_int('info', info)
  call end_test()

  ! Test 7: Singular upper (info > 0)
  A3 = (0.0d0, 0.0d0)
  A3(1,1) = (1.0d0, 0.0d0)
  A3(3,3) = (2.0d0, 0.0d0)
  E3 = (0.0d0, 0.0d0); ipiv3 = 0
  call zsytf2_rk('U', 3, A3, 3, E3, ipiv3, info)
  call begin_test('singular_upper')
  call print_array('a', A3_r, 18)
  call print_array('e', E3_r, 6)
  call print_int_array('ipiv', ipiv3, 3)
  call print_int('info', info)
  call end_test()

  ! Test 8: Upper 5x5 tiny diagonal forcing rook cycles/swaps
  A5 = (0.0d0, 0.0d0)
  A5(1,1) = (0.03d0, 0.01d0)
  A5(1,2) = (0.06d0, 0.01d0); A5(2,2) = (3.0d0, -0.5d0)
  A5(1,3) = (0.05d0, -0.01d0); A5(2,3) = (0.1d0, 0.02d0); A5(3,3) = (2.0d0, 0.5d0)
  A5(1,4) = (200.0d0, 1.0d0); A5(2,4) = (0.2d0, -0.1d0); A5(3,4) = (0.4d0, 0.2d0); A5(4,4) = (0.02d0, 0.0d0)
  A5(1,5) = (100.0d0, -2.0d0); A5(2,5) = (0.3d0, 0.1d0); A5(3,5) = (0.5d0, -0.2d0); A5(4,5) = (100.0d0, 1.0d0); A5(5,5) = (0.01d0, 0.0d0)
  E5 = (0.0d0, 0.0d0); ipiv5 = 0
  call zsytf2_rk('U', 5, A5, 5, E5, ipiv5, info)
  call begin_test('rook_upper_5x5')
  call print_array('a', A5_r, 50)
  call print_array('e', E5_r, 10)
  call print_int_array('ipiv', ipiv5, 5)
  call print_int('info', info)
  call end_test()

  ! Test 9: Lower 5x5 mirror
  A5 = (0.0d0, 0.0d0)
  A5(1,1) = (0.01d0, 0.0d0); A5(2,1) = (0.3d0, -0.1d0); A5(3,1) = (0.5d0, 0.2d0); A5(4,1) = (100.0d0, -1.0d0); A5(5,1) = (100.0d0, 2.0d0)
  A5(2,2) = (0.02d0, 0.0d0); A5(3,2) = (0.4d0, -0.2d0); A5(4,2) = (0.2d0, 0.1d0); A5(5,2) = (200.0d0, -1.0d0)
  A5(3,3) = (2.0d0, 0.5d0); A5(4,3) = (0.1d0, -0.02d0); A5(5,3) = (0.05d0, 0.01d0)
  A5(4,4) = (3.0d0, -0.5d0); A5(5,4) = (0.06d0, -0.01d0)
  A5(5,5) = (0.03d0, 0.01d0)
  E5 = (0.0d0, 0.0d0); ipiv5 = 0
  call zsytf2_rk('L', 5, A5, 5, E5, ipiv5, info)
  call begin_test('rook_lower_5x5')
  call print_array('a', A5_r, 50)
  call print_array('e', E5_r, 10)
  call print_int_array('ipiv', ipiv5, 5)
  call print_int('info', info)
  call end_test()

end program test_zsytf2_rk
