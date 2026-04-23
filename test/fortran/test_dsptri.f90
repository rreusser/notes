program test_dsptri
  use test_utils
  implicit none
  double precision :: ap(100), work(100), ap_save(100)
  integer :: ipiv(10), info

  ! Test 1: 3x3 symmetric positive definite, UPLO='U'
  ! A = [ 4  2  1;
  !       2  5  3;
  !       1  3  6 ]
  ! Upper packed (column-major): A(1,1), A(1,2), A(2,2), A(1,3), A(2,3), A(3,3)
  ap = 0.0d0
  ap(1) = 4.0d0
  ap(2) = 2.0d0
  ap(3) = 5.0d0
  ap(4) = 1.0d0
  ap(5) = 3.0d0
  ap(6) = 6.0d0
  ipiv = 0
  call dsptrf('U', 3, ap, ipiv, info)
  call dsptri('U', 3, ap, ipiv, work, info)
  call begin_test('3x3_upper')
  call print_array('ap', ap, 6)
  call print_int('info', info)
  call end_test()

  ! Test 2: 3x3 symmetric positive definite, UPLO='L'
  ap = 0.0d0
  ap(1) = 4.0d0
  ap(2) = 2.0d0
  ap(3) = 1.0d0
  ap(4) = 5.0d0
  ap(5) = 3.0d0
  ap(6) = 6.0d0
  ipiv = 0
  call dsptrf('L', 3, ap, ipiv, info)
  call dsptri('L', 3, ap, ipiv, work, info)
  call begin_test('3x3_lower')
  call print_array('ap', ap, 6)
  call print_int('info', info)
  call end_test()

  ! Test 3: 4x4 symmetric indefinite, UPLO='U'
  ! A = [ 2  1  0  3;
  !       1  4  2  1;
  !       0  2  6  5;
  !       3  1  5  8 ]
  ! Upper packed: A(1,1), A(1,2), A(2,2), A(1,3), A(2,3), A(3,3), A(1,4), A(2,4), A(3,4), A(4,4)
  ap = 0.0d0
  ap(1) = 2.0d0
  ap(2) = 1.0d0
  ap(3) = 4.0d0
  ap(4) = 0.0d0
  ap(5) = 2.0d0
  ap(6) = 6.0d0
  ap(7) = 3.0d0
  ap(8) = 1.0d0
  ap(9) = 5.0d0
  ap(10) = 8.0d0
  ipiv = 0
  call dsptrf('U', 4, ap, ipiv, info)
  call dsptri('U', 4, ap, ipiv, work, info)
  call begin_test('4x4_upper')
  call print_array('ap', ap, 10)
  call print_int('info', info)
  call end_test()

  ! Test 4: 4x4 symmetric indefinite, UPLO='L'
  ! Same matrix in lower packed
  ap = 0.0d0
  ap(1) = 2.0d0
  ap(2) = 1.0d0
  ap(3) = 0.0d0
  ap(4) = 3.0d0
  ap(5) = 4.0d0
  ap(6) = 2.0d0
  ap(7) = 1.0d0
  ap(8) = 6.0d0
  ap(9) = 5.0d0
  ap(10) = 8.0d0
  ipiv = 0
  call dsptrf('L', 4, ap, ipiv, info)
  call dsptri('L', 4, ap, ipiv, work, info)
  call begin_test('4x4_lower')
  call print_array('ap', ap, 10)
  call print_int('info', info)
  call end_test()

  ! Test 5: N=0 quick return
  call dsptri('U', 0, ap, ipiv, work, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: N=1
  ap(1) = 5.0d0
  ipiv(1) = 1
  call dsptrf('U', 1, ap, ipiv, info)
  call dsptri('U', 1, ap, ipiv, work, info)
  call begin_test('n_one')
  call print_array('ap', ap, 1)
  call print_int('info', info)
  call end_test()

  ! Test 7: 4x4 with 2x2 pivots (indefinite matrix that forces 2x2 blocks)
  ! A = [ 0  1  2  3;
  !       1  0  4  5;
  !       2  4  0  6;
  !       3  5  6  0 ]
  ! Upper packed
  ap = 0.0d0
  ap(1) = 0.0d0
  ap(2) = 1.0d0
  ap(3) = 0.0d0
  ap(4) = 2.0d0
  ap(5) = 4.0d0
  ap(6) = 0.0d0
  ap(7) = 3.0d0
  ap(8) = 5.0d0
  ap(9) = 6.0d0
  ap(10) = 0.0d0
  ipiv = 0
  call dsptrf('U', 4, ap, ipiv, info)
  if (info .eq. 0) then
    call dsptri('U', 4, ap, ipiv, work, info)
  end if
  call begin_test('4x4_indef_2x2_upper')
  call print_array('ap', ap, 10)
  call print_int('info', info)
  call end_test()

  ! Test 8: Same matrix lower packed
  ap = 0.0d0
  ap(1) = 0.0d0
  ap(2) = 1.0d0
  ap(3) = 2.0d0
  ap(4) = 3.0d0
  ap(5) = 0.0d0
  ap(6) = 4.0d0
  ap(7) = 5.0d0
  ap(8) = 0.0d0
  ap(9) = 6.0d0
  ap(10) = 0.0d0
  ipiv = 0
  call dsptrf('L', 4, ap, ipiv, info)
  if (info .eq. 0) then
    call dsptri('L', 4, ap, ipiv, work, info)
  end if
  call begin_test('4x4_indef_2x2_lower')
  call print_array('ap', ap, 10)
  call print_int('info', info)
  call end_test()

  ! Test 9: 3x3 indefinite matrix, UPLO='U'
  ! A = [ 1  2  3;
  !       2  0  1;
  !       3  1  1 ]
  ap = 0.0d0
  ap(1) = 1.0d0
  ap(2) = 2.0d0
  ap(3) = 0.0d0
  ap(4) = 3.0d0
  ap(5) = 1.0d0
  ap(6) = 1.0d0
  ipiv = 0
  call dsptrf('U', 3, ap, ipiv, info)
  if (info .eq. 0) then
    call dsptri('U', 3, ap, ipiv, work, info)
  end if
  call begin_test('3x3_indef_upper')
  call print_array('ap', ap, 6)
  call print_int('info', info)
  call end_test()

  ! Test 10: 3x3 indefinite matrix, UPLO='L'
  ap = 0.0d0
  ap(1) = 1.0d0
  ap(2) = 2.0d0
  ap(3) = 3.0d0
  ap(4) = 0.0d0
  ap(5) = 1.0d0
  ap(6) = 1.0d0
  ipiv = 0
  call dsptrf('L', 3, ap, ipiv, info)
  if (info .eq. 0) then
    call dsptri('L', 3, ap, ipiv, work, info)
  end if
  call begin_test('3x3_indef_lower')
  call print_array('ap', ap, 6)
  call print_int('info', info)
  call end_test()

end program
