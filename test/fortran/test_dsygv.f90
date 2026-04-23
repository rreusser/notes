program test_dsygv
  use test_utils
  implicit none
  double precision :: a(16), b(16), w(4), work(100)
  integer :: info

  ! Test 1: ITYPE=1, JOBZ='V', UPLO='U', N=3
  ! A = [4 2 1; 2 5 3; 1 3 6], B = [4 2 0; 2 5 1; 0 1 3]
  a = 0.0d0; b = 0.0d0; w = 0.0d0
  a(1) = 4.0d0; a(4) = 2.0d0; a(5) = 5.0d0
  a(7) = 1.0d0; a(8) = 3.0d0; a(9) = 6.0d0
  b(1) = 4.0d0; b(4) = 2.0d0; b(5) = 5.0d0
  b(8) = 1.0d0; b(9) = 3.0d0
  call dsygv(1, 'V', 'U', 3, a, 3, b, 3, w, work, 100, info)
  call begin_test('itype1_v_upper')
  call print_int('info', info)
  call print_array('w', w, 3)
  call print_matrix('A', a, 3, 3, 3)
  call end_test()

  ! Test 2: ITYPE=1, JOBZ='V', UPLO='L', N=3
  a = 0.0d0; b = 0.0d0; w = 0.0d0
  a(1) = 4.0d0; a(2) = 2.0d0; a(3) = 1.0d0
  a(5) = 5.0d0; a(6) = 3.0d0; a(9) = 6.0d0
  b(1) = 4.0d0; b(2) = 2.0d0
  b(5) = 5.0d0; b(6) = 1.0d0; b(9) = 3.0d0
  call dsygv(1, 'V', 'L', 3, a, 3, b, 3, w, work, 100, info)
  call begin_test('itype1_v_lower')
  call print_int('info', info)
  call print_array('w', w, 3)
  call print_matrix('A', a, 3, 3, 3)
  call end_test()

  ! Test 3: ITYPE=1, JOBZ='N', UPLO='L', N=3 (eigenvalues only)
  a = 0.0d0; b = 0.0d0; w = 0.0d0
  a(1) = 4.0d0; a(2) = 2.0d0; a(3) = 1.0d0
  a(5) = 5.0d0; a(6) = 3.0d0; a(9) = 6.0d0
  b(1) = 4.0d0; b(2) = 2.0d0
  b(5) = 5.0d0; b(6) = 1.0d0; b(9) = 3.0d0
  call dsygv(1, 'N', 'L', 3, a, 3, b, 3, w, work, 100, info)
  call begin_test('itype1_n_lower')
  call print_int('info', info)
  call print_array('w', w, 3)
  call end_test()

  ! Test 4: ITYPE=2, JOBZ='V', UPLO='U', N=3
  a = 0.0d0; b = 0.0d0; w = 0.0d0
  a(1) = 4.0d0; a(4) = 2.0d0; a(5) = 5.0d0
  a(7) = 1.0d0; a(8) = 3.0d0; a(9) = 6.0d0
  b(1) = 4.0d0; b(4) = 2.0d0; b(5) = 5.0d0
  b(8) = 1.0d0; b(9) = 3.0d0
  call dsygv(2, 'V', 'U', 3, a, 3, b, 3, w, work, 100, info)
  call begin_test('itype2_v_upper')
  call print_int('info', info)
  call print_array('w', w, 3)
  call end_test()

  ! Test 5: ITYPE=3, JOBZ='V', UPLO='L', N=3
  a = 0.0d0; b = 0.0d0; w = 0.0d0
  a(1) = 4.0d0; a(2) = 2.0d0; a(3) = 1.0d0
  a(5) = 5.0d0; a(6) = 3.0d0; a(9) = 6.0d0
  b(1) = 4.0d0; b(2) = 2.0d0
  b(5) = 5.0d0; b(6) = 1.0d0; b(9) = 3.0d0
  call dsygv(3, 'V', 'L', 3, a, 3, b, 3, w, work, 100, info)
  call begin_test('itype3_v_lower')
  call print_int('info', info)
  call print_array('w', w, 3)
  call end_test()

  ! Test 6: N=0 quick return
  call dsygv(1, 'V', 'U', 0, a, 1, b, 1, w, work, 100, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 7: N=1
  a(1) = 6.0d0; b(1) = 2.0d0; w(1) = 0.0d0
  call dsygv(1, 'V', 'U', 1, a, 1, b, 1, w, work, 100, info)
  call begin_test('n_one')
  call print_int('info', info)
  call print_scalar('w1', w(1))
  call print_scalar('A1', a(1))
  call end_test()

  ! Test 8: Non-positive definite B (should return info = N + k)
  a(1) = 1.0d0; a(2) = 0.0d0; a(3) = 0.0d0; a(4) = 1.0d0
  b(1) = -1.0d0; b(2) = 0.0d0; b(3) = 0.0d0; b(4) = 1.0d0
  call dsygv(1, 'V', 'L', 2, a, 2, b, 2, w, work, 100, info)
  call begin_test('not_posdef')
  call print_int('info', info)
  call end_test()

end program
