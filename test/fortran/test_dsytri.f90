program test_dsytri
  use test_utils
  implicit none
  double precision :: a(4,4), a_fact(4,4), work(4)
  integer :: ipiv(4), info, n, lda

  lda = 4

  ! Test 1: 3x3 symmetric positive definite, UPLO='U'
  ! A = [ 4  2  1;
  !       2  5  3;
  !       1  3  6 ]
  n = 3
  a = 0.0d0
  a(1,1) = 4.0d0; a(1,2) = 2.0d0; a(1,3) = 1.0d0
  a(2,1) = 2.0d0; a(2,2) = 5.0d0; a(2,3) = 3.0d0
  a(3,1) = 1.0d0; a(3,2) = 3.0d0; a(3,3) = 6.0d0
  ipiv = 0
  call dsytf2('U', n, a, lda, ipiv, info)
  a_fact = a
  call dsytri('U', n, a, lda, ipiv, work, info)
  call begin_test('3x3_upper')
  call print_matrix('a_fact', a_fact, lda, n, n)
  call print_matrix('a', a, lda, n, n)
  call print_int_array('ipiv', ipiv, n)
  call print_int('info', info)
  call end_test()

  ! Test 2: 3x3 symmetric positive definite, UPLO='L'
  n = 3
  a = 0.0d0
  a(1,1) = 4.0d0; a(1,2) = 2.0d0; a(1,3) = 1.0d0
  a(2,1) = 2.0d0; a(2,2) = 5.0d0; a(2,3) = 3.0d0
  a(3,1) = 1.0d0; a(3,2) = 3.0d0; a(3,3) = 6.0d0
  ipiv = 0
  call dsytf2('L', n, a, lda, ipiv, info)
  a_fact = a
  call dsytri('L', n, a, lda, ipiv, work, info)
  call begin_test('3x3_lower')
  call print_matrix('a_fact', a_fact, lda, n, n)
  call print_matrix('a', a, lda, n, n)
  call print_int_array('ipiv', ipiv, n)
  call print_int('info', info)
  call end_test()

  ! Test 3: 4x4 symmetric indefinite, UPLO='U'
  n = 4
  a = 0.0d0
  a(1,1) = 2.0d0; a(1,2) = 1.0d0; a(1,3) = 0.0d0; a(1,4) = 3.0d0
  a(2,1) = 1.0d0; a(2,2) = 4.0d0; a(2,3) = 2.0d0; a(2,4) = 1.0d0
  a(3,1) = 0.0d0; a(3,2) = 2.0d0; a(3,3) = 6.0d0; a(3,4) = 5.0d0
  a(4,1) = 3.0d0; a(4,2) = 1.0d0; a(4,3) = 5.0d0; a(4,4) = 8.0d0
  ipiv = 0
  call dsytf2('U', n, a, lda, ipiv, info)
  a_fact = a
  call dsytri('U', n, a, lda, ipiv, work, info)
  call begin_test('4x4_upper')
  call print_matrix('a_fact', a_fact, lda, n, n)
  call print_matrix('a', a, lda, n, n)
  call print_int_array('ipiv', ipiv, n)
  call print_int('info', info)
  call end_test()

  ! Test 4: 4x4 symmetric indefinite, UPLO='L'
  n = 4
  a = 0.0d0
  a(1,1) = 2.0d0; a(1,2) = 1.0d0; a(1,3) = 0.0d0; a(1,4) = 3.0d0
  a(2,1) = 1.0d0; a(2,2) = 4.0d0; a(2,3) = 2.0d0; a(2,4) = 1.0d0
  a(3,1) = 0.0d0; a(3,2) = 2.0d0; a(3,3) = 6.0d0; a(3,4) = 5.0d0
  a(4,1) = 3.0d0; a(4,2) = 1.0d0; a(4,3) = 5.0d0; a(4,4) = 8.0d0
  ipiv = 0
  call dsytf2('L', n, a, lda, ipiv, info)
  a_fact = a
  call dsytri('L', n, a, lda, ipiv, work, info)
  call begin_test('4x4_lower')
  call print_matrix('a_fact', a_fact, lda, n, n)
  call print_matrix('a', a, lda, n, n)
  call print_int_array('ipiv', ipiv, n)
  call print_int('info', info)
  call end_test()

  ! Test 5: N=0 quick return
  call dsytri('U', 0, a, lda, ipiv, work, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: N=1
  n = 1
  a = 0.0d0
  a(1,1) = 5.0d0
  ipiv(1) = 1
  call dsytf2('U', n, a, lda, ipiv, info)
  a_fact = a
  call dsytri('U', n, a, lda, ipiv, work, info)
  call begin_test('n_one')
  call print_matrix('a_fact', a_fact, lda, n, n)
  call print_matrix('a', a, lda, n, n)
  call print_int_array('ipiv', ipiv, n)
  call print_int('info', info)
  call end_test()

  ! Test 7: 4x4 strongly indefinite that forces 2x2 blocks, UPLO='U'
  n = 4
  a = 0.0d0
  a(1,1) = 0.0d0; a(1,2) = 1.0d0; a(1,3) = 2.0d0; a(1,4) = 3.0d0
  a(2,1) = 1.0d0; a(2,2) = 0.0d0; a(2,3) = 4.0d0; a(2,4) = 5.0d0
  a(3,1) = 2.0d0; a(3,2) = 4.0d0; a(3,3) = 0.0d0; a(3,4) = 6.0d0
  a(4,1) = 3.0d0; a(4,2) = 5.0d0; a(4,3) = 6.0d0; a(4,4) = 0.0d0
  ipiv = 0
  call dsytf2('U', n, a, lda, ipiv, info)
  a_fact = a
  if (info .eq. 0) then
    call dsytri('U', n, a, lda, ipiv, work, info)
  end if
  call begin_test('4x4_indef_2x2_upper')
  call print_matrix('a_fact', a_fact, lda, n, n)
  call print_matrix('a', a, lda, n, n)
  call print_int_array('ipiv', ipiv, n)
  call print_int('info', info)
  call end_test()

  ! Test 8: Same matrix, UPLO='L'
  n = 4
  a = 0.0d0
  a(1,1) = 0.0d0; a(1,2) = 1.0d0; a(1,3) = 2.0d0; a(1,4) = 3.0d0
  a(2,1) = 1.0d0; a(2,2) = 0.0d0; a(2,3) = 4.0d0; a(2,4) = 5.0d0
  a(3,1) = 2.0d0; a(3,2) = 4.0d0; a(3,3) = 0.0d0; a(3,4) = 6.0d0
  a(4,1) = 3.0d0; a(4,2) = 5.0d0; a(4,3) = 6.0d0; a(4,4) = 0.0d0
  ipiv = 0
  call dsytf2('L', n, a, lda, ipiv, info)
  a_fact = a
  if (info .eq. 0) then
    call dsytri('L', n, a, lda, ipiv, work, info)
  end if
  call begin_test('4x4_indef_2x2_lower')
  call print_matrix('a_fact', a_fact, lda, n, n)
  call print_matrix('a', a, lda, n, n)
  call print_int_array('ipiv', ipiv, n)
  call print_int('info', info)
  call end_test()

  ! Test 9: 3x3 indefinite matrix, UPLO='U'
  n = 3
  a = 0.0d0
  a(1,1) = 1.0d0; a(1,2) = 2.0d0; a(1,3) = 3.0d0
  a(2,1) = 2.0d0; a(2,2) = 0.0d0; a(2,3) = 1.0d0
  a(3,1) = 3.0d0; a(3,2) = 1.0d0; a(3,3) = 1.0d0
  ipiv = 0
  call dsytf2('U', n, a, lda, ipiv, info)
  a_fact = a
  if (info .eq. 0) then
    call dsytri('U', n, a, lda, ipiv, work, info)
  end if
  call begin_test('3x3_indef_upper')
  call print_matrix('a_fact', a_fact, lda, n, n)
  call print_matrix('a', a, lda, n, n)
  call print_int_array('ipiv', ipiv, n)
  call print_int('info', info)
  call end_test()

  ! Test 10: 3x3 indefinite matrix, UPLO='L'
  n = 3
  a = 0.0d0
  a(1,1) = 1.0d0; a(1,2) = 2.0d0; a(1,3) = 3.0d0
  a(2,1) = 2.0d0; a(2,2) = 0.0d0; a(2,3) = 1.0d0
  a(3,1) = 3.0d0; a(3,2) = 1.0d0; a(3,3) = 1.0d0
  ipiv = 0
  call dsytf2('L', n, a, lda, ipiv, info)
  a_fact = a
  if (info .eq. 0) then
    call dsytri('L', n, a, lda, ipiv, work, info)
  end if
  call begin_test('3x3_indef_lower')
  call print_matrix('a_fact', a_fact, lda, n, n)
  call print_matrix('a', a, lda, n, n)
  call print_int_array('ipiv', ipiv, n)
  call print_int('info', info)
  call end_test()

end program
