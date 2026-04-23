program test_dsytrs2
  use test_utils
  implicit none
  double precision :: a(100), b(100), work(200), a_save(100), b_save(100)
  integer :: ipiv(10), info

  ! Test 1: 4x4 SPD system, UPLO='L', 1 RHS
  ! A = [ 4  2  1  0;
  !       2  5  2  1;
  !       1  2  6  3;
  !       0  1  3  8 ]
  ! b = [7; 10; 12; 12] (A*[1;1;1;1] = b)
  a = 0.0d0
  a(1) = 4.0d0; a(2) = 2.0d0; a(3) = 1.0d0; a(4) = 0.0d0
  a(5) = 0.0d0; a(6) = 5.0d0; a(7) = 2.0d0; a(8) = 1.0d0
  a(9) = 0.0d0; a(10) = 0.0d0; a(11) = 6.0d0; a(12) = 3.0d0
  a(13) = 0.0d0; a(14) = 0.0d0; a(15) = 0.0d0; a(16) = 8.0d0
  b = 0.0d0
  b(1) = 7.0d0; b(2) = 10.0d0; b(3) = 12.0d0; b(4) = 12.0d0
  call dsytrf('L', 4, a, 4, ipiv, work, 200, info)
  call dsytrs2('L', 4, 1, a, 4, ipiv, b, 4, work, info)
  call begin_test('4x4_lower_1rhs')
  call print_array('b', b, 4)
  call print_int('info', info)
  call end_test()

  ! Test 2: 4x4 SPD system, UPLO='U', 1 RHS
  a = 0.0d0
  a(1) = 4.0d0
  a(5) = 2.0d0; a(6) = 5.0d0
  a(9) = 1.0d0; a(10) = 2.0d0; a(11) = 6.0d0
  a(13) = 0.0d0; a(14) = 1.0d0; a(15) = 3.0d0; a(16) = 8.0d0
  b = 0.0d0
  b(1) = 7.0d0; b(2) = 10.0d0; b(3) = 12.0d0; b(4) = 12.0d0
  call dsytrf('U', 4, a, 4, ipiv, work, 200, info)
  call dsytrs2('U', 4, 1, a, 4, ipiv, b, 4, work, info)
  call begin_test('4x4_upper_1rhs')
  call print_array('b', b, 4)
  call print_int('info', info)
  call end_test()

  ! Test 3: 4x4 indefinite with 2x2 pivots, UPLO='L', 1 RHS
  ! A = [ 0  1  2  3;
  !       1  0  4  5;
  !       2  4  0  6;
  !       3  5  6  0 ]
  ! b = [6; 10; 12; 14] (A*[1;1;1;1] = b)
  a = 0.0d0
  a(1) = 0.0d0; a(2) = 1.0d0; a(3) = 2.0d0; a(4) = 3.0d0
  a(5) = 0.0d0; a(6) = 0.0d0; a(7) = 4.0d0; a(8) = 5.0d0
  a(9) = 0.0d0; a(10) = 0.0d0; a(11) = 0.0d0; a(12) = 6.0d0
  a(13) = 0.0d0; a(14) = 0.0d0; a(15) = 0.0d0; a(16) = 0.0d0
  b = 0.0d0
  b(1) = 6.0d0; b(2) = 10.0d0; b(3) = 12.0d0; b(4) = 14.0d0
  call dsytrf('L', 4, a, 4, ipiv, work, 200, info)
  call dsytrs2('L', 4, 1, a, 4, ipiv, b, 4, work, info)
  call begin_test('4x4_indef_lower_1rhs')
  call print_array('b', b, 4)
  call print_int('info', info)
  call end_test()

  ! Test 4: 4x4 indefinite, UPLO='U', 1 RHS
  a = 0.0d0
  a(1) = 0.0d0
  a(5) = 1.0d0; a(6) = 0.0d0
  a(9) = 2.0d0; a(10) = 4.0d0; a(11) = 0.0d0
  a(13) = 3.0d0; a(14) = 5.0d0; a(15) = 6.0d0; a(16) = 0.0d0
  b = 0.0d0
  b(1) = 6.0d0; b(2) = 10.0d0; b(3) = 12.0d0; b(4) = 14.0d0
  call dsytrf('U', 4, a, 4, ipiv, work, 200, info)
  call dsytrs2('U', 4, 1, a, 4, ipiv, b, 4, work, info)
  call begin_test('4x4_indef_upper_1rhs')
  call print_array('b', b, 4)
  call print_int('info', info)
  call end_test()

  ! Test 5: multiple RHS (NRHS=2), UPLO='L'
  ! A = [ 4  2  1;
  !       2  5  2;
  !       1  2  6 ]
  ! b = [7 14; 9 18; 9 18] (A*[1;1;1] = [7;9;9], A*[2;2;2] = [14;18;18])
  a = 0.0d0
  a(1) = 4.0d0; a(2) = 2.0d0; a(3) = 1.0d0
  a(4) = 0.0d0; a(5) = 5.0d0; a(6) = 2.0d0
  a(7) = 0.0d0; a(8) = 0.0d0; a(9) = 6.0d0
  b = 0.0d0
  b(1) = 7.0d0; b(2) = 9.0d0; b(3) = 9.0d0
  b(4) = 14.0d0; b(5) = 18.0d0; b(6) = 18.0d0
  call dsytrf('L', 3, a, 3, ipiv, work, 200, info)
  call dsytrs2('L', 3, 2, a, 3, ipiv, b, 3, work, info)
  call begin_test('3x3_lower_2rhs')
  call print_array('b', b, 6)
  call print_int('info', info)
  call end_test()

  ! Test 6: multiple RHS (NRHS=2), UPLO='U'
  a = 0.0d0
  a(1) = 4.0d0
  a(4) = 2.0d0; a(5) = 5.0d0
  a(7) = 1.0d0; a(8) = 2.0d0; a(9) = 6.0d0
  b = 0.0d0
  b(1) = 7.0d0; b(2) = 9.0d0; b(3) = 9.0d0
  b(4) = 14.0d0; b(5) = 18.0d0; b(6) = 18.0d0
  call dsytrf('U', 3, a, 3, ipiv, work, 200, info)
  call dsytrs2('U', 3, 2, a, 3, ipiv, b, 3, work, info)
  call begin_test('3x3_upper_2rhs')
  call print_array('b', b, 6)
  call print_int('info', info)
  call end_test()

  ! Test 7: N=0
  call dsytrs2('L', 0, 1, a, 1, ipiv, b, 1, work, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 8: NRHS=0
  call dsytrs2('L', 3, 0, a, 3, ipiv, b, 3, work, info)
  call begin_test('nrhs_zero')
  call print_int('info', info)
  call end_test()

  ! Test 9: N=1, UPLO='L'
  a(1) = 4.0d0
  b(1) = 8.0d0
  call dsytrf('L', 1, a, 1, ipiv, work, 200, info)
  call dsytrs2('L', 1, 1, a, 1, ipiv, b, 1, work, info)
  call begin_test('n_one_lower')
  call print_array('b', b, 1)
  call print_int('info', info)
  call end_test()

  ! Test 10: N=1, UPLO='U'
  a(1) = 4.0d0
  b(1) = 8.0d0
  call dsytrf('U', 1, a, 1, ipiv, work, 200, info)
  call dsytrs2('U', 1, 1, a, 1, ipiv, b, 1, work, info)
  call begin_test('n_one_upper')
  call print_array('b', b, 1)
  call print_int('info', info)
  call end_test()

  ! Test 11: 5x5 mixed pivots, UPLO='L'
  ! A = [ 1  -2   0   3   1;
  !      -2   0   4  -1   2;
  !       0   4  -3   2   0;
  !       3  -1   2   1  -2;
  !       1   2   0  -2   4 ]
  ! b = A*[1;2;3;4;5]
  a = 0.0d0
  a(1) = 1.0d0; a(2) = -2.0d0; a(3) = 0.0d0; a(4) = 3.0d0; a(5) = 1.0d0
  a(6) = 0.0d0; a(7) = 0.0d0; a(8) = 4.0d0; a(9) = -1.0d0; a(10) = 2.0d0
  a(11) = 0.0d0; a(12) = 0.0d0; a(13) = -3.0d0; a(14) = 2.0d0; a(15) = 0.0d0
  a(16) = 0.0d0; a(17) = 0.0d0; a(18) = 0.0d0; a(19) = 1.0d0; a(20) = -2.0d0
  a(21) = 0.0d0; a(22) = 0.0d0; a(23) = 0.0d0; a(24) = 0.0d0; a(25) = 4.0d0
  b = 0.0d0
  b(1) = 14.0d0; b(2) = 16.0d0; b(3) = 7.0d0; b(4) = 1.0d0; b(5) = 17.0d0
  call dsytrf('L', 5, a, 5, ipiv, work, 200, info)
  call dsytrs2('L', 5, 1, a, 5, ipiv, b, 5, work, info)
  call begin_test('5x5_lower_solve')
  call print_array('b', b, 5)
  call print_int('info', info)
  call end_test()

  ! Test 12: 5x5 mixed pivots, UPLO='U'
  a = 0.0d0
  a(1) = 1.0d0
  a(6) = -2.0d0; a(7) = 0.0d0
  a(11) = 0.0d0; a(12) = 4.0d0; a(13) = -3.0d0
  a(16) = 3.0d0; a(17) = -1.0d0; a(18) = 2.0d0; a(19) = 1.0d0
  a(21) = 1.0d0; a(22) = 2.0d0; a(23) = 0.0d0; a(24) = -2.0d0; a(25) = 4.0d0
  b = 0.0d0
  b(1) = 14.0d0; b(2) = 16.0d0; b(3) = 7.0d0; b(4) = 1.0d0; b(5) = 17.0d0
  call dsytrf('U', 5, a, 5, ipiv, work, 200, info)
  call dsytrs2('U', 5, 1, a, 5, ipiv, b, 5, work, info)
  call begin_test('5x5_upper_solve')
  call print_array('b', b, 5)
  call print_int('info', info)
  call end_test()

end program
