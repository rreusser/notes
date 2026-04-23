program test_zhegv
  use test_utils
  implicit none
  complex*16 :: a(16), b(16), work(200)
  double precision :: a_r(32), b_r(32), w(4), rwork(20)
  equivalence (a, a_r)
  equivalence (b, b_r)
  integer :: info

  ! A = [10 2+i 1-2i; 2-i 8 3+i; 1+2i 3-i 7] Hermitian
  ! B = [4 1+i 0; 1-i 5 2-i; 0 2+i 6] Hermitian positive definite

  ! Test 1: ITYPE=1, JOBZ='V', UPLO='U', N=3
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0); w = 0.0d0
  a(1) = (10.0d0, 0.0d0); a(4) = (2.0d0, 1.0d0); a(5) = (8.0d0, 0.0d0)
  a(7) = (1.0d0, -2.0d0); a(8) = (3.0d0, 1.0d0); a(9) = (7.0d0, 0.0d0)
  b(1) = (4.0d0, 0.0d0); b(4) = (1.0d0, 1.0d0); b(5) = (5.0d0, 0.0d0)
  b(8) = (2.0d0, -1.0d0); b(9) = (6.0d0, 0.0d0)
  call zhegv(1, 'V', 'U', 3, a, 3, b, 3, w, work, 200, rwork, info)
  call begin_test('itype1_v_upper')
  call print_int('info', info)
  call print_array('w', w, 3)
  call end_test()

  ! Test 2: ITYPE=1, JOBZ='V', UPLO='L', N=3
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0); w = 0.0d0
  a(1) = (10.0d0, 0.0d0); a(2) = (2.0d0, -1.0d0); a(3) = (1.0d0, 2.0d0)
  a(5) = (8.0d0, 0.0d0); a(6) = (3.0d0, -1.0d0); a(9) = (7.0d0, 0.0d0)
  b(1) = (4.0d0, 0.0d0); b(2) = (1.0d0, -1.0d0)
  b(5) = (5.0d0, 0.0d0); b(6) = (2.0d0, 1.0d0); b(9) = (6.0d0, 0.0d0)
  call zhegv(1, 'V', 'L', 3, a, 3, b, 3, w, work, 200, rwork, info)
  call begin_test('itype1_v_lower')
  call print_int('info', info)
  call print_array('w', w, 3)
  call end_test()

  ! Test 3: ITYPE=1, JOBZ='N', UPLO='L', N=3 (eigenvalues only)
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0); w = 0.0d0
  a(1) = (10.0d0, 0.0d0); a(2) = (2.0d0, -1.0d0); a(3) = (1.0d0, 2.0d0)
  a(5) = (8.0d0, 0.0d0); a(6) = (3.0d0, -1.0d0); a(9) = (7.0d0, 0.0d0)
  b(1) = (4.0d0, 0.0d0); b(2) = (1.0d0, -1.0d0)
  b(5) = (5.0d0, 0.0d0); b(6) = (2.0d0, 1.0d0); b(9) = (6.0d0, 0.0d0)
  call zhegv(1, 'N', 'L', 3, a, 3, b, 3, w, work, 200, rwork, info)
  call begin_test('itype1_n_lower')
  call print_int('info', info)
  call print_array('w', w, 3)
  call end_test()

  ! Test 4: ITYPE=2, JOBZ='V', UPLO='U', N=3
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0); w = 0.0d0
  a(1) = (10.0d0, 0.0d0); a(4) = (2.0d0, 1.0d0); a(5) = (8.0d0, 0.0d0)
  a(7) = (1.0d0, -2.0d0); a(8) = (3.0d0, 1.0d0); a(9) = (7.0d0, 0.0d0)
  b(1) = (4.0d0, 0.0d0); b(4) = (1.0d0, 1.0d0); b(5) = (5.0d0, 0.0d0)
  b(8) = (2.0d0, -1.0d0); b(9) = (6.0d0, 0.0d0)
  call zhegv(2, 'V', 'U', 3, a, 3, b, 3, w, work, 200, rwork, info)
  call begin_test('itype2_v_upper')
  call print_int('info', info)
  call print_array('w', w, 3)
  call end_test()

  ! Test 5: ITYPE=3, JOBZ='V', UPLO='L', N=3
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0); w = 0.0d0
  a(1) = (10.0d0, 0.0d0); a(2) = (2.0d0, -1.0d0); a(3) = (1.0d0, 2.0d0)
  a(5) = (8.0d0, 0.0d0); a(6) = (3.0d0, -1.0d0); a(9) = (7.0d0, 0.0d0)
  b(1) = (4.0d0, 0.0d0); b(2) = (1.0d0, -1.0d0)
  b(5) = (5.0d0, 0.0d0); b(6) = (2.0d0, 1.0d0); b(9) = (6.0d0, 0.0d0)
  call zhegv(3, 'V', 'L', 3, a, 3, b, 3, w, work, 200, rwork, info)
  call begin_test('itype3_v_lower')
  call print_int('info', info)
  call print_array('w', w, 3)
  call end_test()

  ! Test 6: N=0 quick return
  call zhegv(1, 'V', 'U', 0, a, 1, b, 1, w, work, 200, rwork, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 7: N=1
  a(1) = (6.0d0, 0.0d0); b(1) = (2.0d0, 0.0d0); w(1) = 0.0d0
  call zhegv(1, 'V', 'U', 1, a, 1, b, 1, w, work, 200, rwork, info)
  call begin_test('n_one')
  call print_int('info', info)
  call print_array('w', w, 1)
  call print_array('A', a_r, 2)
  call end_test()

  ! Test 8: Non-positive definite B
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 0.0d0); a(4) = (1.0d0, 0.0d0)
  b(1) = (-1.0d0, 0.0d0); b(4) = (1.0d0, 0.0d0)
  call zhegv(1, 'V', 'L', 2, a, 2, b, 2, w, work, 200, rwork, info)
  call begin_test('not_posdef')
  call print_int('info', info)
  call end_test()

end program
