program test_dspsv
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
  call dspsv('U', 3, 1, ap, ipiv, b, 3, info)
  call begin_test('3x3_upper_1rhs')
  call print_array('b', b, 3)
  call print_int_array('ipiv', ipiv, 3)
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
  call dspsv('L', 3, 1, ap, ipiv, b, 3, info)
  call begin_test('3x3_lower_1rhs')
  call print_array('b', b, 3)
  call print_int_array('ipiv', ipiv, 3)
  call print_int('info', info)
  call end_test()

  ! Test 3: 3x3 SPD, UPLO='L', 2 RHS
  ! b(:,1) = A*[1;1;1] = [7;10;10], b(:,2) = A*[2;3;4] = [18;31;35]
  ap = 0.0d0
  ap(1) = 4.0d0; ap(2) = 2.0d0; ap(3) = 1.0d0
  ap(4) = 5.0d0; ap(5) = 3.0d0; ap(6) = 6.0d0
  b = 0.0d0
  b(1) = 7.0d0; b(2) = 10.0d0; b(3) = 10.0d0
  b(4) = 18.0d0; b(5) = 31.0d0; b(6) = 35.0d0
  ipiv = 0
  call dspsv('L', 3, 2, ap, ipiv, b, 3, info)
  call begin_test('3x3_lower_2rhs')
  call print_array('b', b, 6)
  call print_int_array('ipiv', ipiv, 3)
  call print_int('info', info)
  call end_test()

  ! Test 4: singular matrix (info > 0), UPLO='U'
  ! A = [ 1  2  3;
  !       2  4  6;
  !       3  6  9 ]   (rank 1 matrix)
  ! Upper packed: A(1,1), A(1,2), A(2,2), A(1,3), A(2,3), A(3,3)
  ap = 0.0d0
  ap(1) = 1.0d0; ap(2) = 2.0d0; ap(3) = 4.0d0
  ap(4) = 3.0d0; ap(5) = 6.0d0; ap(6) = 9.0d0
  b = 0.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  ipiv = 0
  call dspsv('U', 3, 1, ap, ipiv, b, 3, info)
  call begin_test('singular_upper')
  call print_int('info', info)
  call end_test()

  ! Test 5: N=0 quick return
  call dspsv('L', 0, 1, ap, ipiv, b, 1, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: NRHS=0 quick return
  ap(1) = 5.0d0
  call dspsv('L', 1, 0, ap, ipiv, b, 1, info)
  call begin_test('nrhs_zero')
  call print_int('info', info)
  call end_test()

  ! Test 7: N=1, UPLO='L'
  ap(1) = 4.0d0
  b(1) = 8.0d0
  ipiv = 0
  call dspsv('L', 1, 1, ap, ipiv, b, 1, info)
  call begin_test('n_one_lower')
  call print_array('b', b, 1)
  call print_int_array('ipiv', ipiv, 1)
  call print_int('info', info)
  call end_test()

  ! Test 8: N=1, UPLO='U'
  ap(1) = 5.0d0
  b(1) = 15.0d0
  ipiv = 0
  call dspsv('U', 1, 1, ap, ipiv, b, 1, info)
  call begin_test('n_one_upper')
  call print_array('b', b, 1)
  call print_int_array('ipiv', ipiv, 1)
  call print_int('info', info)
  call end_test()

  ! Test 9: 3x3 UPLO='U', 2 RHS
  ! b(:,1) = A*[1;1;1] = [7;10;10], b(:,2) = A*[2;3;4] = [18;31;35]
  ap = 0.0d0
  ap(1) = 4.0d0; ap(2) = 2.0d0; ap(3) = 5.0d0
  ap(4) = 1.0d0; ap(5) = 3.0d0; ap(6) = 6.0d0
  b = 0.0d0
  b(1) = 7.0d0; b(2) = 10.0d0; b(3) = 10.0d0
  b(4) = 18.0d0; b(5) = 31.0d0; b(6) = 35.0d0
  ipiv = 0
  call dspsv('U', 3, 2, ap, ipiv, b, 3, info)
  call begin_test('3x3_upper_2rhs')
  call print_array('b', b, 6)
  call print_int_array('ipiv', ipiv, 3)
  call print_int('info', info)
  call end_test()

end program
