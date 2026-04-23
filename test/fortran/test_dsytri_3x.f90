program test_dsytri_3x
  use test_utils
  implicit none
  double precision :: a(100), e(10), work_rk(200), work_3x(2000)
  double precision :: afact(100)
  integer :: ipiv(10), info, nb

  ! Test 1: 4x4 SPD-like, UPLO='L' (1x1 pivots only)
  a = 0.0d0
  a(1) = 4.0d0; a(2) = 2.0d0; a(3) = 1.0d0; a(4) = 0.0d0
  a(5) = 0.0d0; a(6) = 5.0d0; a(7) = 2.0d0; a(8) = 1.0d0
  a(9) = 0.0d0; a(10) = 0.0d0; a(11) = 6.0d0; a(12) = 3.0d0
  a(13) = 0.0d0; a(14) = 0.0d0; a(15) = 0.0d0; a(16) = 8.0d0
  call dsytrf_rk('L', 4, a, 4, e, ipiv, work_rk, 200, info)
  ! Save factored copy for printing the input state to JS:
  afact(1:16) = a(1:16)
  nb = 2
  call dsytri_3x('L', 4, a, 4, e, ipiv, work_3x, nb, info)
  call begin_test('4x4_lower_def_nb2')
  call print_array('a_factored', afact, 16)
  call print_array('e', e, 4)
  call print_int_array('ipiv', ipiv, 4)
  call print_array('a_inv', a, 16)
  call print_int('info', info)
  call end_test()

  ! Test 2: 4x4 UPLO='U' definite
  a = 0.0d0
  a(1) = 4.0d0
  a(5) = 2.0d0; a(6) = 5.0d0
  a(9) = 1.0d0; a(10) = 2.0d0; a(11) = 6.0d0
  a(13) = 0.0d0; a(14) = 1.0d0; a(15) = 3.0d0; a(16) = 8.0d0
  call dsytrf_rk('U', 4, a, 4, e, ipiv, work_rk, 200, info)
  afact(1:16) = a(1:16)
  nb = 2
  call dsytri_3x('U', 4, a, 4, e, ipiv, work_3x, nb, info)
  call begin_test('4x4_upper_def_nb2')
  call print_array('a_factored', afact, 16)
  call print_array('e', e, 4)
  call print_int_array('ipiv', ipiv, 4)
  call print_array('a_inv', a, 16)
  call print_int('info', info)
  call end_test()

  ! Test 3: 4x4 indefinite (forces 2x2 pivots), UPLO='L'
  a = 0.0d0
  a(1) = 0.0d0; a(2) = 1.0d0; a(3) = 2.0d0; a(4) = 3.0d0
  a(5) = 0.0d0; a(6) = 0.0d0; a(7) = 4.0d0; a(8) = 5.0d0
  a(9) = 0.0d0; a(10) = 0.0d0; a(11) = 0.0d0; a(12) = 6.0d0
  a(13) = 0.0d0; a(14) = 0.0d0; a(15) = 0.0d0; a(16) = 0.0d0
  call dsytrf_rk('L', 4, a, 4, e, ipiv, work_rk, 200, info)
  afact(1:16) = a(1:16)
  nb = 2
  call dsytri_3x('L', 4, a, 4, e, ipiv, work_3x, nb, info)
  call begin_test('4x4_lower_indef_nb2')
  call print_array('a_factored', afact, 16)
  call print_array('e', e, 4)
  call print_int_array('ipiv', ipiv, 4)
  call print_array('a_inv', a, 16)
  call print_int('info', info)
  call end_test()

  ! Test 4: 4x4 indefinite, UPLO='U'
  a = 0.0d0
  a(1) = 0.0d0
  a(5) = 1.0d0; a(6) = 0.0d0
  a(9) = 2.0d0; a(10) = 4.0d0; a(11) = 0.0d0
  a(13) = 3.0d0; a(14) = 5.0d0; a(15) = 6.0d0; a(16) = 0.0d0
  call dsytrf_rk('U', 4, a, 4, e, ipiv, work_rk, 200, info)
  afact(1:16) = a(1:16)
  nb = 2
  call dsytri_3x('U', 4, a, 4, e, ipiv, work_3x, nb, info)
  call begin_test('4x4_upper_indef_nb2')
  call print_array('a_factored', afact, 16)
  call print_array('e', e, 4)
  call print_int_array('ipiv', ipiv, 4)
  call print_array('a_inv', a, 16)
  call print_int('info', info)
  call end_test()

  ! Test 5: 5x5 mixed pivots, UPLO='L', nb=2
  a = 0.0d0
  a(1) = 1.0d0; a(2) = -2.0d0; a(3) = 0.0d0; a(4) = 3.0d0; a(5) = 1.0d0
  a(6) = 0.0d0; a(7) = 0.0d0; a(8) = 4.0d0; a(9) = -1.0d0; a(10) = 2.0d0
  a(11) = 0.0d0; a(12) = 0.0d0; a(13) = -3.0d0; a(14) = 2.0d0; a(15) = 0.0d0
  a(16) = 0.0d0; a(17) = 0.0d0; a(18) = 0.0d0; a(19) = 1.0d0; a(20) = -2.0d0
  a(21) = 0.0d0; a(22) = 0.0d0; a(23) = 0.0d0; a(24) = 0.0d0; a(25) = 4.0d0
  call dsytrf_rk('L', 5, a, 5, e, ipiv, work_rk, 200, info)
  afact(1:25) = a(1:25)
  nb = 2
  call dsytri_3x('L', 5, a, 5, e, ipiv, work_3x, nb, info)
  call begin_test('5x5_lower_mixed_nb2')
  call print_array('a_factored', afact, 25)
  call print_array('e', e, 5)
  call print_int_array('ipiv', ipiv, 5)
  call print_array('a_inv', a, 25)
  call print_int('info', info)
  call end_test()

  ! Test 6: 5x5 mixed pivots, UPLO='U', nb=2
  a = 0.0d0
  a(1) = 1.0d0
  a(6) = -2.0d0; a(7) = 0.0d0
  a(11) = 0.0d0; a(12) = 4.0d0; a(13) = -3.0d0
  a(16) = 3.0d0; a(17) = -1.0d0; a(18) = 2.0d0; a(19) = 1.0d0
  a(21) = 1.0d0; a(22) = 2.0d0; a(23) = 0.0d0; a(24) = -2.0d0; a(25) = 4.0d0
  call dsytrf_rk('U', 5, a, 5, e, ipiv, work_rk, 200, info)
  afact(1:25) = a(1:25)
  nb = 2
  call dsytri_3x('U', 5, a, 5, e, ipiv, work_3x, nb, info)
  call begin_test('5x5_upper_mixed_nb2')
  call print_array('a_factored', afact, 25)
  call print_array('e', e, 5)
  call print_int_array('ipiv', ipiv, 5)
  call print_array('a_inv', a, 25)
  call print_int('info', info)
  call end_test()

  ! Test 7: 5x5 lower, nb=3 (NNB+1 path & remainder)
  a = 0.0d0
  a(1) = 1.0d0; a(2) = -2.0d0; a(3) = 0.0d0; a(4) = 3.0d0; a(5) = 1.0d0
  a(6) = 0.0d0; a(7) = 0.0d0; a(8) = 4.0d0; a(9) = -1.0d0; a(10) = 2.0d0
  a(11) = 0.0d0; a(12) = 0.0d0; a(13) = -3.0d0; a(14) = 2.0d0; a(15) = 0.0d0
  a(16) = 0.0d0; a(17) = 0.0d0; a(18) = 0.0d0; a(19) = 1.0d0; a(20) = -2.0d0
  a(21) = 0.0d0; a(22) = 0.0d0; a(23) = 0.0d0; a(24) = 0.0d0; a(25) = 4.0d0
  call dsytrf_rk('L', 5, a, 5, e, ipiv, work_rk, 200, info)
  afact(1:25) = a(1:25)
  nb = 3
  call dsytri_3x('L', 5, a, 5, e, ipiv, work_3x, nb, info)
  call begin_test('5x5_lower_mixed_nb3')
  call print_array('a_factored', afact, 25)
  call print_array('e', e, 5)
  call print_int_array('ipiv', ipiv, 5)
  call print_array('a_inv', a, 25)
  call print_int('info', info)
  call end_test()

  ! Test 8: N=1
  a(1) = 5.0d0
  e(1) = 0.0d0
  ipiv(1) = 1
  nb = 2
  call dsytri_3x('L', 1, a, 1, e, ipiv, work_3x, nb, info)
  call begin_test('n_one_lower')
  call print_array('a_inv', a, 1)
  call print_int('info', info)
  call end_test()

  ! Test 9: N=0
  nb = 2
  call dsytri_3x('L', 0, a, 1, e, ipiv, work_3x, nb, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 10: 6x6 lower, nb=4 (larger block)
  a = 0.0d0
  a(1) = 4.0d0; a(2) = 1.0d0; a(3) = 0.0d0; a(4) = 2.0d0; a(5) = 0.0d0; a(6) = 1.0d0
  a(8) = 5.0d0; a(9) = 1.0d0; a(10) = 0.0d0; a(11) = 1.0d0; a(12) = 0.0d0
  a(15) = 6.0d0; a(16) = 2.0d0; a(17) = 0.0d0; a(18) = 1.0d0
  a(22) = 7.0d0; a(23) = 1.0d0; a(24) = 0.0d0
  a(29) = 8.0d0; a(30) = 1.0d0
  a(36) = 9.0d0
  call dsytrf_rk('L', 6, a, 6, e, ipiv, work_rk, 200, info)
  afact(1:36) = a(1:36)
  nb = 4
  call dsytri_3x('L', 6, a, 6, e, ipiv, work_3x, nb, info)
  call begin_test('6x6_lower_def_nb4')
  call print_array('a_factored', afact, 36)
  call print_array('e', e, 6)
  call print_int_array('ipiv', ipiv, 6)
  call print_array('a_inv', a, 36)
  call print_int('info', info)
  call end_test()

end program
