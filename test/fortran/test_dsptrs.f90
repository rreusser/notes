program test_dsptrs
  use test_utils
  implicit none
  double precision :: ap(100), b(100)
  integer :: ipiv(10), info

  ! Test 1: 3x3 SPD, UPLO='U', 1 RHS
  ! A = [ 4  2  1;
  !       2  5  3;
  !       1  3  6 ]
  ! Upper packed (col-major): A(1,1), A(1,2), A(2,2), A(1,3), A(2,3), A(3,3)
  ! b = A*[1;1;1] = [7; 10; 10]
  ap = 0.0d0
  ap(1) = 4.0d0; ap(2) = 2.0d0; ap(3) = 5.0d0
  ap(4) = 1.0d0; ap(5) = 3.0d0; ap(6) = 6.0d0
  b = 0.0d0
  b(1) = 7.0d0; b(2) = 10.0d0; b(3) = 10.0d0
  ipiv = 0
  call dsptrf('U', 3, ap, ipiv, info)
  call dsptrs('U', 3, 1, ap, ipiv, b, 3, info)
  call begin_test('3x3_upper_1rhs')
  call print_array('b', b, 3)
  call print_int('info', info)
  call end_test()

  ! Test 2: 3x3 SPD, UPLO='L', 1 RHS
  ! Same matrix, lower packed: A(1,1), A(2,1), A(3,1), A(2,2), A(3,2), A(3,3)
  ! b = A*[1;1;1] = [7; 10; 10]
  ap = 0.0d0
  ap(1) = 4.0d0; ap(2) = 2.0d0; ap(3) = 1.0d0
  ap(4) = 5.0d0; ap(5) = 3.0d0; ap(6) = 6.0d0
  b = 0.0d0
  b(1) = 7.0d0; b(2) = 10.0d0; b(3) = 10.0d0
  ipiv = 0
  call dsptrf('L', 3, ap, ipiv, info)
  call dsptrs('L', 3, 1, ap, ipiv, b, 3, info)
  call begin_test('3x3_lower_1rhs')
  call print_array('b', b, 3)
  call print_int('info', info)
  call end_test()

  ! Test 3: 3x3, UPLO='L', 2 RHS
  ! b(:,1) = A*[1;1;1] = [7;10;10], b(:,2) = A*[2;3;4] = [18;31;35]
  ap = 0.0d0
  ap(1) = 4.0d0; ap(2) = 2.0d0; ap(3) = 1.0d0
  ap(4) = 5.0d0; ap(5) = 3.0d0; ap(6) = 6.0d0
  b = 0.0d0
  b(1) = 7.0d0; b(2) = 10.0d0; b(3) = 10.0d0
  b(4) = 18.0d0; b(5) = 31.0d0; b(6) = 35.0d0
  ipiv = 0
  call dsptrf('L', 3, ap, ipiv, info)
  call dsptrs('L', 3, 2, ap, ipiv, b, 3, info)
  call begin_test('3x3_lower_2rhs')
  call print_array('b', b, 6)
  call print_int('info', info)
  call end_test()

  ! Test 4: 4x4 indefinite (forces 2x2 pivots), UPLO='L', 1 RHS
  ! A = [ 0  1  2  3;
  !       1  0  4  5;
  !       2  4  0  6;
  !       3  5  6  0 ]
  ! Lower packed: A(1,1), A(2,1), A(3,1), A(4,1), A(2,2), A(3,2), A(4,2), A(3,3), A(4,3), A(4,4)
  ! b = A*[1;1;1;1] = [6; 10; 12; 14]
  ap = 0.0d0
  ap(1) = 0.0d0; ap(2) = 1.0d0; ap(3) = 2.0d0; ap(4) = 3.0d0
  ap(5) = 0.0d0; ap(6) = 4.0d0; ap(7) = 5.0d0
  ap(8) = 0.0d0; ap(9) = 6.0d0
  ap(10) = 0.0d0
  b = 0.0d0
  b(1) = 6.0d0; b(2) = 10.0d0; b(3) = 12.0d0; b(4) = 14.0d0
  ipiv = 0
  call dsptrf('L', 4, ap, ipiv, info)
  call dsptrs('L', 4, 1, ap, ipiv, b, 4, info)
  call begin_test('4x4_indef_lower_1rhs')
  call print_array('b', b, 4)
  call print_int('info', info)
  call end_test()

  ! Test 5: 4x4 indefinite, UPLO='U', 1 RHS
  ! Same matrix, upper packed: A(1,1), A(1,2), A(2,2), A(1,3), A(2,3), A(3,3), A(1,4), A(2,4), A(3,4), A(4,4)
  ! b = A*[1;1;1;1] = [6; 10; 12; 14]
  ap = 0.0d0
  ap(1) = 0.0d0; ap(2) = 1.0d0; ap(3) = 0.0d0
  ap(4) = 2.0d0; ap(5) = 4.0d0; ap(6) = 0.0d0
  ap(7) = 3.0d0; ap(8) = 5.0d0; ap(9) = 6.0d0; ap(10) = 0.0d0
  b = 0.0d0
  b(1) = 6.0d0; b(2) = 10.0d0; b(3) = 12.0d0; b(4) = 14.0d0
  ipiv = 0
  call dsptrf('U', 4, ap, ipiv, info)
  call dsptrs('U', 4, 1, ap, ipiv, b, 4, info)
  call begin_test('4x4_indef_upper_1rhs')
  call print_array('b', b, 4)
  call print_int('info', info)
  call end_test()

  ! Test 6: N=0 quick return
  call dsptrs('L', 0, 1, ap, ipiv, b, 1, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 7: NRHS=0 quick return
  call dsptrs('L', 3, 0, ap, ipiv, b, 3, info)
  call begin_test('nrhs_zero')
  call print_int('info', info)
  call end_test()

  ! Test 8: N=1, UPLO='L'
  ap(1) = 4.0d0
  b(1) = 8.0d0
  ipiv(1) = 1
  call dsptrf('L', 1, ap, ipiv, info)
  call dsptrs('L', 1, 1, ap, ipiv, b, 1, info)
  call begin_test('n_one_lower')
  call print_array('b', b, 1)
  call print_int('info', info)
  call end_test()

  ! Test 9: N=1, UPLO='U'
  ap(1) = 5.0d0
  b(1) = 15.0d0
  ipiv(1) = 1
  call dsptrf('U', 1, ap, ipiv, info)
  call dsptrs('U', 1, 1, ap, ipiv, b, 1, info)
  call begin_test('n_one_upper')
  call print_array('b', b, 1)
  call print_int('info', info)
  call end_test()

  ! Test 10: 4x4 indefinite, UPLO='U', 2 RHS
  ! b(:,1) = A*[1;1;1;1] = [6;10;12;14]
  ! b(:,2) = A*[1;2;3;4] = [20;34;40;44]
  ap = 0.0d0
  ap(1) = 0.0d0; ap(2) = 1.0d0; ap(3) = 0.0d0
  ap(4) = 2.0d0; ap(5) = 4.0d0; ap(6) = 0.0d0
  ap(7) = 3.0d0; ap(8) = 5.0d0; ap(9) = 6.0d0; ap(10) = 0.0d0
  b = 0.0d0
  b(1) = 6.0d0; b(2) = 10.0d0; b(3) = 12.0d0; b(4) = 14.0d0
  b(5) = 20.0d0; b(6) = 34.0d0; b(7) = 40.0d0; b(8) = 44.0d0
  ipiv = 0
  call dsptrf('U', 4, ap, ipiv, info)
  call dsptrs('U', 4, 2, ap, ipiv, b, 4, info)
  call begin_test('4x4_indef_upper_2rhs')
  call print_array('b', b, 8)
  call print_int('info', info)
  call end_test()

  ! Test 11: 3x3 SPD, UPLO='U', 2 RHS
  ! b(:,1) = A*[1;1;1] = [7;10;10], b(:,2) = A*[2;3;4] = [18;31;35]
  ap = 0.0d0
  ap(1) = 4.0d0; ap(2) = 2.0d0; ap(3) = 5.0d0
  ap(4) = 1.0d0; ap(5) = 3.0d0; ap(6) = 6.0d0
  b = 0.0d0
  b(1) = 7.0d0; b(2) = 10.0d0; b(3) = 10.0d0
  b(4) = 18.0d0; b(5) = 31.0d0; b(6) = 35.0d0
  ipiv = 0
  call dsptrf('U', 3, ap, ipiv, info)
  call dsptrs('U', 3, 2, ap, ipiv, b, 3, info)
  call begin_test('3x3_upper_2rhs')
  call print_array('b', b, 6)
  call print_int('info', info)
  call end_test()

end program
