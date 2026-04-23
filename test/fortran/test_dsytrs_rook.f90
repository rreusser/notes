program test_dsytrs_rook
  use test_utils
  implicit none
  double precision :: a(100), b(100), work(200)
  integer :: ipiv(10), info

  ! Test 1: 4x4 indefinite, UPLO='L', 1 RHS
  ! A = [ 0  1  2  3;
  !       1  0  4  5;
  !       2  4  0  6;
  !       3  5  6  0 ]
  ! b = A*[1;1;1;1] = [6; 10; 12; 14]
  a = 0.0d0
  a(1) = 0.0d0; a(2) = 1.0d0; a(3) = 2.0d0; a(4) = 3.0d0
  a(5) = 0.0d0; a(6) = 0.0d0; a(7) = 4.0d0; a(8) = 5.0d0
  a(9) = 0.0d0; a(10) = 0.0d0; a(11) = 0.0d0; a(12) = 6.0d0
  a(13) = 0.0d0; a(14) = 0.0d0; a(15) = 0.0d0; a(16) = 0.0d0
  b = 0.0d0
  b(1) = 6.0d0; b(2) = 10.0d0; b(3) = 12.0d0; b(4) = 14.0d0
  call dsytrf_rook('L', 4, a, 4, ipiv, work, 200, info)
  call dsytrs_rook('L', 4, 1, a, 4, ipiv, b, 4, info)
  call begin_test('4x4_indef_lower_1rhs')
  call print_matrix('A', a, 4, 4, 4)
  call print_int_array('ipiv', ipiv, 4)
  call print_array('b', b, 4)
  call print_int('info', info)
  call end_test()

  ! Test 2: 4x4 indefinite, UPLO='U', 1 RHS
  a = 0.0d0
  a(1) = 0.0d0
  a(5) = 1.0d0; a(6) = 0.0d0
  a(9) = 2.0d0; a(10) = 4.0d0; a(11) = 0.0d0
  a(13) = 3.0d0; a(14) = 5.0d0; a(15) = 6.0d0; a(16) = 0.0d0
  b = 0.0d0
  b(1) = 6.0d0; b(2) = 10.0d0; b(3) = 12.0d0; b(4) = 14.0d0
  call dsytrf_rook('U', 4, a, 4, ipiv, work, 200, info)
  call dsytrs_rook('U', 4, 1, a, 4, ipiv, b, 4, info)
  call begin_test('4x4_indef_upper_1rhs')
  call print_matrix('A', a, 4, 4, 4)
  call print_int_array('ipiv', ipiv, 4)
  call print_array('b', b, 4)
  call print_int('info', info)
  call end_test()

  ! Test 3: 4x4 SPD, UPLO='L', 1 RHS (only 1x1 pivots)
  a = 0.0d0
  a(1) = 4.0d0; a(2) = 2.0d0; a(3) = 1.0d0; a(4) = 0.0d0
  a(5) = 0.0d0; a(6) = 5.0d0; a(7) = 2.0d0; a(8) = 1.0d0
  a(9) = 0.0d0; a(10) = 0.0d0; a(11) = 6.0d0; a(12) = 3.0d0
  a(13) = 0.0d0; a(14) = 0.0d0; a(15) = 0.0d0; a(16) = 8.0d0
  b = 0.0d0
  b(1) = 7.0d0; b(2) = 10.0d0; b(3) = 12.0d0; b(4) = 12.0d0
  call dsytrf_rook('L', 4, a, 4, ipiv, work, 200, info)
  call dsytrs_rook('L', 4, 1, a, 4, ipiv, b, 4, info)
  call begin_test('4x4_spd_lower_1rhs')
  call print_matrix('A', a, 4, 4, 4)
  call print_int_array('ipiv', ipiv, 4)
  call print_array('b', b, 4)
  call print_int('info', info)
  call end_test()

  ! Test 4: 4x4 SPD, UPLO='U', 1 RHS
  a = 0.0d0
  a(1) = 4.0d0
  a(5) = 2.0d0; a(6) = 5.0d0
  a(9) = 1.0d0; a(10) = 2.0d0; a(11) = 6.0d0
  a(13) = 0.0d0; a(14) = 1.0d0; a(15) = 3.0d0; a(16) = 8.0d0
  b = 0.0d0
  b(1) = 7.0d0; b(2) = 10.0d0; b(3) = 12.0d0; b(4) = 12.0d0
  call dsytrf_rook('U', 4, a, 4, ipiv, work, 200, info)
  call dsytrs_rook('U', 4, 1, a, 4, ipiv, b, 4, info)
  call begin_test('4x4_spd_upper_1rhs')
  call print_matrix('A', a, 4, 4, 4)
  call print_int_array('ipiv', ipiv, 4)
  call print_array('b', b, 4)
  call print_int('info', info)
  call end_test()

  ! Test 5: 5x5 mixed pivots, UPLO='L', NRHS=2
  a = 0.0d0
  a(1) = 1.0d0; a(2) = -2.0d0; a(3) = 0.0d0; a(4) = 3.0d0; a(5) = 1.0d0
  a(6) = 0.0d0; a(7) = 0.0d0; a(8) = 4.0d0; a(9) = -1.0d0; a(10) = 2.0d0
  a(11) = 0.0d0; a(12) = 0.0d0; a(13) = -3.0d0; a(14) = 2.0d0; a(15) = 0.0d0
  a(16) = 0.0d0; a(17) = 0.0d0; a(18) = 0.0d0; a(19) = 1.0d0; a(20) = -2.0d0
  a(21) = 0.0d0; a(22) = 0.0d0; a(23) = 0.0d0; a(24) = 0.0d0; a(25) = 4.0d0
  ! x1 = [1;2;3;4;5], x2 = [2;1;0;-1;3]
  ! b1 = A*x1 = [14; 16; 7; 1; 17]
  ! b2 = A*x2 = compute
  ! row1: 1*2 + (-2)*1 + 0*0 + 3*(-1) + 1*3 = 2-2+0-3+3 = 0
  ! row2: -2*2 + 0*1 + 4*0 + (-1)*(-1) + 2*3 = -4+0+0+1+6 = 3
  ! row3: 0*2 + 4*1 + (-3)*0 + 2*(-1) + 0*3 = 0+4+0-2+0 = 2
  ! row4: 3*2 + (-1)*1 + 2*0 + 1*(-1) + (-2)*3 = 6-1+0-1-6 = -2
  ! row5: 1*2 + 2*1 + 0*0 + (-2)*(-1) + 4*3 = 2+2+0+2+12 = 18
  b = 0.0d0
  b(1) = 14.0d0; b(2) = 16.0d0; b(3) = 7.0d0; b(4) = 1.0d0; b(5) = 17.0d0
  b(6) = 0.0d0; b(7) = 3.0d0; b(8) = 2.0d0; b(9) = -2.0d0; b(10) = 18.0d0
  call dsytrf_rook('L', 5, a, 5, ipiv, work, 200, info)
  call dsytrs_rook('L', 5, 2, a, 5, ipiv, b, 5, info)
  call begin_test('5x5_lower_2rhs')
  call print_matrix('A', a, 5, 5, 5)
  call print_int_array('ipiv', ipiv, 5)
  call print_array('b', b, 10)
  call print_int('info', info)
  call end_test()

  ! Test 6: 5x5 mixed pivots, UPLO='U', NRHS=2
  a = 0.0d0
  a(1) = 1.0d0
  a(6) = -2.0d0; a(7) = 0.0d0
  a(11) = 0.0d0; a(12) = 4.0d0; a(13) = -3.0d0
  a(16) = 3.0d0; a(17) = -1.0d0; a(18) = 2.0d0; a(19) = 1.0d0
  a(21) = 1.0d0; a(22) = 2.0d0; a(23) = 0.0d0; a(24) = -2.0d0; a(25) = 4.0d0
  b = 0.0d0
  b(1) = 14.0d0; b(2) = 16.0d0; b(3) = 7.0d0; b(4) = 1.0d0; b(5) = 17.0d0
  b(6) = 0.0d0; b(7) = 3.0d0; b(8) = 2.0d0; b(9) = -2.0d0; b(10) = 18.0d0
  call dsytrf_rook('U', 5, a, 5, ipiv, work, 200, info)
  call dsytrs_rook('U', 5, 2, a, 5, ipiv, b, 5, info)
  call begin_test('5x5_upper_2rhs')
  call print_matrix('A', a, 5, 5, 5)
  call print_int_array('ipiv', ipiv, 5)
  call print_array('b', b, 10)
  call print_int('info', info)
  call end_test()

  ! Test 7: N=0
  call dsytrs_rook('L', 0, 1, a, 1, ipiv, b, 1, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 8: NRHS=0
  call dsytrs_rook('L', 3, 0, a, 3, ipiv, b, 3, info)
  call begin_test('nrhs_zero')
  call print_int('info', info)
  call end_test()

  ! Test 9: N=1 lower
  a(1) = 4.0d0
  b(1) = 8.0d0
  ipiv(1) = 1
  call dsytrs_rook('L', 1, 1, a, 1, ipiv, b, 1, info)
  call begin_test('n_one_lower')
  call print_matrix('A', a, 1, 1, 1)
  call print_int_array('ipiv', ipiv, 1)
  call print_array('b', b, 1)
  call print_int('info', info)
  call end_test()

end program
