program test_dsytri_3
  use test_utils
  implicit none
  double precision :: a(100), e(10), work_rk(200), work_3(4000)
  double precision :: afact(100)
  integer :: ipiv(10), info, lwork

  lwork = 4000

  ! Test 1: 4x4 definite, UPLO='L'
  a = 0.0d0
  a(1) = 4.0d0; a(2) = 2.0d0; a(3) = 1.0d0; a(4) = 0.0d0
  a(5) = 0.0d0; a(6) = 5.0d0; a(7) = 2.0d0; a(8) = 1.0d0
  a(9) = 0.0d0; a(10) = 0.0d0; a(11) = 6.0d0; a(12) = 3.0d0
  a(13) = 0.0d0; a(14) = 0.0d0; a(15) = 0.0d0; a(16) = 8.0d0
  call dsytrf_rk('L', 4, a, 4, e, ipiv, work_rk, 200, info)
  afact(1:16) = a(1:16)
  call dsytri_3('L', 4, a, 4, e, ipiv, work_3, lwork, info)
  call begin_test('4x4_lower_def')
  call print_array('a_factored', afact, 16)
  call print_array('e', e, 4)
  call print_int_array('ipiv', ipiv, 4)
  call print_array('a_inv', a, 16)
  call print_int('info', info)
  call end_test()

  ! Test 2: 4x4 definite, UPLO='U'
  a = 0.0d0
  a(1) = 4.0d0
  a(5) = 2.0d0; a(6) = 5.0d0
  a(9) = 1.0d0; a(10) = 2.0d0; a(11) = 6.0d0
  a(13) = 0.0d0; a(14) = 1.0d0; a(15) = 3.0d0; a(16) = 8.0d0
  call dsytrf_rk('U', 4, a, 4, e, ipiv, work_rk, 200, info)
  afact(1:16) = a(1:16)
  call dsytri_3('U', 4, a, 4, e, ipiv, work_3, lwork, info)
  call begin_test('4x4_upper_def')
  call print_array('a_factored', afact, 16)
  call print_array('e', e, 4)
  call print_int_array('ipiv', ipiv, 4)
  call print_array('a_inv', a, 16)
  call print_int('info', info)
  call end_test()

  ! Test 3: 5x5 indefinite, UPLO='L'
  a = 0.0d0
  a(1) = 1.0d0; a(2) = -2.0d0; a(3) = 0.0d0; a(4) = 3.0d0; a(5) = 1.0d0
  a(6) = 0.0d0; a(7) = 0.0d0; a(8) = 4.0d0; a(9) = -1.0d0; a(10) = 2.0d0
  a(11) = 0.0d0; a(12) = 0.0d0; a(13) = -3.0d0; a(14) = 2.0d0; a(15) = 0.0d0
  a(16) = 0.0d0; a(17) = 0.0d0; a(18) = 0.0d0; a(19) = 1.0d0; a(20) = -2.0d0
  a(21) = 0.0d0; a(22) = 0.0d0; a(23) = 0.0d0; a(24) = 0.0d0; a(25) = 4.0d0
  call dsytrf_rk('L', 5, a, 5, e, ipiv, work_rk, 200, info)
  afact(1:25) = a(1:25)
  call dsytri_3('L', 5, a, 5, e, ipiv, work_3, lwork, info)
  call begin_test('5x5_lower_indef')
  call print_array('a_factored', afact, 25)
  call print_array('e', e, 5)
  call print_int_array('ipiv', ipiv, 5)
  call print_array('a_inv', a, 25)
  call print_int('info', info)
  call end_test()

  ! Test 4: 5x5 indefinite, UPLO='U'
  a = 0.0d0
  a(1) = 1.0d0
  a(6) = -2.0d0; a(7) = 0.0d0
  a(11) = 0.0d0; a(12) = 4.0d0; a(13) = -3.0d0
  a(16) = 3.0d0; a(17) = -1.0d0; a(18) = 2.0d0; a(19) = 1.0d0
  a(21) = 1.0d0; a(22) = 2.0d0; a(23) = 0.0d0; a(24) = -2.0d0; a(25) = 4.0d0
  call dsytrf_rk('U', 5, a, 5, e, ipiv, work_rk, 200, info)
  afact(1:25) = a(1:25)
  call dsytri_3('U', 5, a, 5, e, ipiv, work_3, lwork, info)
  call begin_test('5x5_upper_indef')
  call print_array('a_factored', afact, 25)
  call print_array('e', e, 5)
  call print_int_array('ipiv', ipiv, 5)
  call print_array('a_inv', a, 25)
  call print_int('info', info)
  call end_test()

  ! Test 5: N=1
  a(1) = 5.0d0
  e(1) = 0.0d0
  ipiv(1) = 1
  call dsytri_3('L', 1, a, 1, e, ipiv, work_3, lwork, info)
  call begin_test('n_one_lower')
  call print_array('a_inv', a, 1)
  call print_int('info', info)
  call end_test()

  ! Test 6: N=0
  call dsytri_3('L', 0, a, 1, e, ipiv, work_3, lwork, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 7: workspace query
  call dsytri_3('L', 5, a, 5, e, ipiv, work_3, -1, info)
  call begin_test('lwork_query')
  call print_scalar('work1', work_3(1))
  call print_int('info', info)
  call end_test()

end program
