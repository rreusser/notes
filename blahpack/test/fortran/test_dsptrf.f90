program test_dsptrf
  use test_utils
  implicit none
  double precision :: ap(100)
  integer :: ipiv(10), info

  ! Test 1: 3x3 symmetric positive definite, UPLO='U'
  ! A = [ 4  2  1;
  !       2  5  3;
  !       1  3  6 ]
  ! Upper packed (column-major): A(1,1), A(1,2), A(2,2), A(1,3), A(2,3), A(3,3)
  ap = 0.0d0
  ap(1) = 4.0d0  ! A(1,1)
  ap(2) = 2.0d0  ! A(1,2)
  ap(3) = 5.0d0  ! A(2,2)
  ap(4) = 1.0d0  ! A(1,3)
  ap(5) = 3.0d0  ! A(2,3)
  ap(6) = 6.0d0  ! A(3,3)
  ipiv = 0
  call dsptrf('U', 3, ap, ipiv, info)
  call begin_test('3x3_upper_spd')
  call print_array('ap', ap, 6)
  call print_int_array('ipiv', ipiv, 3)
  call print_int('info', info)
  call end_test()

  ! Test 2: 3x3 symmetric positive definite, UPLO='L'
  ! Same matrix A, lower packed: A(1,1), A(2,1), A(3,1), A(2,2), A(3,2), A(3,3)
  ap = 0.0d0
  ap(1) = 4.0d0  ! A(1,1)
  ap(2) = 2.0d0  ! A(2,1)
  ap(3) = 1.0d0  ! A(3,1)
  ap(4) = 5.0d0  ! A(2,2)
  ap(5) = 3.0d0  ! A(3,2)
  ap(6) = 6.0d0  ! A(3,3)
  ipiv = 0
  call dsptrf('L', 3, ap, ipiv, info)
  call begin_test('3x3_lower_spd')
  call print_array('ap', ap, 6)
  call print_int_array('ipiv', ipiv, 3)
  call print_int('info', info)
  call end_test()

  ! Test 3: 4x4 symmetric indefinite (forces 2x2 pivots), UPLO='U'
  ! A = [ 0  1  2  3;
  !       1  0  4  5;
  !       2  4  0  6;
  !       3  5  6  0 ]
  ! Upper packed: A(1,1), A(1,2), A(2,2), A(1,3), A(2,3), A(3,3), A(1,4), A(2,4), A(3,4), A(4,4)
  ap = 0.0d0
  ap(1) = 0.0d0   ! A(1,1)
  ap(2) = 1.0d0   ! A(1,2)
  ap(3) = 0.0d0   ! A(2,2)
  ap(4) = 2.0d0   ! A(1,3)
  ap(5) = 4.0d0   ! A(2,3)
  ap(6) = 0.0d0   ! A(3,3)
  ap(7) = 3.0d0   ! A(1,4)
  ap(8) = 5.0d0   ! A(2,4)
  ap(9) = 6.0d0   ! A(3,4)
  ap(10) = 0.0d0  ! A(4,4)
  ipiv = 0
  call dsptrf('U', 4, ap, ipiv, info)
  call begin_test('4x4_indef_upper')
  call print_array('ap', ap, 10)
  call print_int_array('ipiv', ipiv, 4)
  call print_int('info', info)
  call end_test()

  ! Test 4: 4x4 symmetric indefinite, UPLO='L'
  ! Same matrix, lower packed: A(1,1), A(2,1), A(3,1), A(4,1), A(2,2), A(3,2), A(4,2), A(3,3), A(4,3), A(4,4)
  ap = 0.0d0
  ap(1) = 0.0d0   ! A(1,1)
  ap(2) = 1.0d0   ! A(2,1)
  ap(3) = 2.0d0   ! A(3,1)
  ap(4) = 3.0d0   ! A(4,1)
  ap(5) = 0.0d0   ! A(2,2)
  ap(6) = 4.0d0   ! A(3,2)
  ap(7) = 5.0d0   ! A(4,2)
  ap(8) = 0.0d0   ! A(3,3)
  ap(9) = 6.0d0   ! A(4,3)
  ap(10) = 0.0d0  ! A(4,4)
  ipiv = 0
  call dsptrf('L', 4, ap, ipiv, info)
  call begin_test('4x4_indef_lower')
  call print_array('ap', ap, 10)
  call print_int_array('ipiv', ipiv, 4)
  call print_int('info', info)
  call end_test()

  ! Test 5: N=0 quick return
  ipiv = 0
  call dsptrf('L', 0, ap, ipiv, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: N=1
  ap(1) = 5.0d0
  ipiv = 0
  call dsptrf('L', 1, ap, ipiv, info)
  call begin_test('n_one')
  call print_array('ap', ap, 1)
  call print_int_array('ipiv', ipiv, 1)
  call print_int('info', info)
  call end_test()

  ! Test 7: singular matrix INFO>0
  ap(1) = 0.0d0
  ap(2) = 0.0d0
  ap(3) = 0.0d0
  ipiv = 0
  call dsptrf('L', 2, ap, ipiv, info)
  call begin_test('singular')
  call print_array('ap', ap, 3)
  call print_int_array('ipiv', ipiv, 2)
  call print_int('info', info)
  call end_test()

  ! Test 8: 4x4 tridiagonal, UPLO='L' (1x1 pivots expected)
  ! A = [ 2 -1  0  0;
  !      -1  2 -1  0;
  !       0 -1  2 -1;
  !       0  0 -1  2 ]
  ! Lower packed: A(1,1), A(2,1), A(3,1), A(4,1), A(2,2), A(3,2), A(4,2), A(3,3), A(4,3), A(4,4)
  ap = 0.0d0
  ap(1) = 2.0d0    ! A(1,1)
  ap(2) = -1.0d0   ! A(2,1)
  ap(3) = 0.0d0    ! A(3,1)
  ap(4) = 0.0d0    ! A(4,1)
  ap(5) = 2.0d0    ! A(2,2)
  ap(6) = -1.0d0   ! A(3,2)
  ap(7) = 0.0d0    ! A(4,2)
  ap(8) = 2.0d0    ! A(3,3)
  ap(9) = -1.0d0   ! A(4,3)
  ap(10) = 2.0d0   ! A(4,4)
  ipiv = 0
  call dsptrf('L', 4, ap, ipiv, info)
  call begin_test('4x4_tridiag_lower')
  call print_array('ap', ap, 10)
  call print_int_array('ipiv', ipiv, 4)
  call print_int('info', info)
  call end_test()

  ! Test 9: 4x4 tridiagonal, UPLO='U'
  ! Same matrix, upper packed: A(1,1), A(1,2), A(2,2), A(1,3), A(2,3), A(3,3), A(1,4), A(2,4), A(3,4), A(4,4)
  ap = 0.0d0
  ap(1) = 2.0d0    ! A(1,1)
  ap(2) = -1.0d0   ! A(1,2)
  ap(3) = 2.0d0    ! A(2,2)
  ap(4) = 0.0d0    ! A(1,3)
  ap(5) = -1.0d0   ! A(2,3)
  ap(6) = 2.0d0    ! A(3,3)
  ap(7) = 0.0d0    ! A(1,4)
  ap(8) = 0.0d0    ! A(2,4)
  ap(9) = -1.0d0   ! A(3,4)
  ap(10) = 2.0d0   ! A(4,4)
  ipiv = 0
  call dsptrf('U', 4, ap, ipiv, info)
  call begin_test('4x4_tridiag_upper')
  call print_array('ap', ap, 10)
  call print_int_array('ipiv', ipiv, 4)
  call print_int('info', info)
  call end_test()

  ! Test 10: N=1 singular
  ap(1) = 0.0d0
  ipiv = 0
  call dsptrf('U', 1, ap, ipiv, info)
  call begin_test('n_one_singular')
  call print_array('ap', ap, 1)
  call print_int_array('ipiv', ipiv, 1)
  call print_int('info', info)
  call end_test()

end program
