program test_zhegvx
  use test_utils
  implicit none
  complex*16 :: a(16), b(16), z(16), work(200)
  double precision :: a_r(32), b_r(32), z_r(32), w(4), rwork(28)
  equivalence (a, a_r)
  equivalence (b, b_r)
  equivalence (z, z_r)
  integer :: info, m, iwork(20), ifail(4), lwork

  ! A = [10 2+i 1-2i; 2-i 8 3+i; 1+2i 3-i 7] Hermitian
  ! B = [4 1+i 0; 1-i 5 2-i; 0 2+i 6] Hermitian positive definite

  ! Test 1: ITYPE=1, JOBZ='V', RANGE='A', UPLO='U', N=3 (all eigenvalues, vectors)
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0); z = (0.0d0, 0.0d0); w = 0.0d0
  a(1) = (10.0d0, 0.0d0); a(4) = (2.0d0, 1.0d0); a(5) = (8.0d0, 0.0d0)
  a(7) = (1.0d0, -2.0d0); a(8) = (3.0d0, 1.0d0); a(9) = (7.0d0, 0.0d0)
  b(1) = (4.0d0, 0.0d0); b(4) = (1.0d0, 1.0d0); b(5) = (5.0d0, 0.0d0)
  b(8) = (2.0d0, -1.0d0); b(9) = (6.0d0, 0.0d0)
  lwork = 200
  call zhegvx(1, 'V', 'A', 'U', 3, a, 3, b, 3, &
              0.0d0, 0.0d0, 0, 0, 0.0d0, m, w, z, 3, &
              work, lwork, rwork, iwork, ifail, info)
  call begin_test('itype1_v_all_upper')
  call print_int('info', info)
  call print_int('m', m)
  call print_array('w', w, 3)
  call print_array('z', z_r, 18)
  call end_test()

  ! Test 2: ITYPE=1, JOBZ='V', RANGE='A', UPLO='L', N=3
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0); z = (0.0d0, 0.0d0); w = 0.0d0
  a(1) = (10.0d0, 0.0d0); a(2) = (2.0d0, -1.0d0); a(3) = (1.0d0, 2.0d0)
  a(5) = (8.0d0, 0.0d0); a(6) = (3.0d0, -1.0d0); a(9) = (7.0d0, 0.0d0)
  b(1) = (4.0d0, 0.0d0); b(2) = (1.0d0, -1.0d0)
  b(5) = (5.0d0, 0.0d0); b(6) = (2.0d0, 1.0d0); b(9) = (6.0d0, 0.0d0)
  call zhegvx(1, 'V', 'A', 'L', 3, a, 3, b, 3, &
              0.0d0, 0.0d0, 0, 0, 0.0d0, m, w, z, 3, &
              work, lwork, rwork, iwork, ifail, info)
  call begin_test('itype1_v_all_lower')
  call print_int('info', info)
  call print_int('m', m)
  call print_array('w', w, 3)
  call print_array('z', z_r, 18)
  call end_test()

  ! Test 3: ITYPE=1, JOBZ='N', RANGE='A', UPLO='U', N=3 (eigenvalues only)
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0); z = (0.0d0, 0.0d0); w = 0.0d0
  a(1) = (10.0d0, 0.0d0); a(4) = (2.0d0, 1.0d0); a(5) = (8.0d0, 0.0d0)
  a(7) = (1.0d0, -2.0d0); a(8) = (3.0d0, 1.0d0); a(9) = (7.0d0, 0.0d0)
  b(1) = (4.0d0, 0.0d0); b(4) = (1.0d0, 1.0d0); b(5) = (5.0d0, 0.0d0)
  b(8) = (2.0d0, -1.0d0); b(9) = (6.0d0, 0.0d0)
  call zhegvx(1, 'N', 'A', 'U', 3, a, 3, b, 3, &
              0.0d0, 0.0d0, 0, 0, 0.0d0, m, w, z, 1, &
              work, lwork, rwork, iwork, ifail, info)
  call begin_test('itype1_n_all_upper')
  call print_int('info', info)
  call print_int('m', m)
  call print_array('w', w, 3)
  call end_test()

  ! Test 4: ITYPE=1, JOBZ='V', RANGE='V', UPLO='U', N=3 (value range)
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0); z = (0.0d0, 0.0d0); w = 0.0d0
  a(1) = (10.0d0, 0.0d0); a(4) = (2.0d0, 1.0d0); a(5) = (8.0d0, 0.0d0)
  a(7) = (1.0d0, -2.0d0); a(8) = (3.0d0, 1.0d0); a(9) = (7.0d0, 0.0d0)
  b(1) = (4.0d0, 0.0d0); b(4) = (1.0d0, 1.0d0); b(5) = (5.0d0, 0.0d0)
  b(8) = (2.0d0, -1.0d0); b(9) = (6.0d0, 0.0d0)
  call zhegvx(1, 'V', 'V', 'U', 3, a, 3, b, 3, &
              0.5d0, 2.0d0, 0, 0, 0.0d0, m, w, z, 3, &
              work, lwork, rwork, iwork, ifail, info)
  call begin_test('itype1_v_value_upper')
  call print_int('info', info)
  call print_int('m', m)
  call print_array('w', w, m)
  call print_array('z', z_r, 6*m)
  call end_test()

  ! Test 5: ITYPE=1, JOBZ='V', RANGE='I', UPLO='U', N=3 (index range: 2nd and 3rd)
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0); z = (0.0d0, 0.0d0); w = 0.0d0
  a(1) = (10.0d0, 0.0d0); a(4) = (2.0d0, 1.0d0); a(5) = (8.0d0, 0.0d0)
  a(7) = (1.0d0, -2.0d0); a(8) = (3.0d0, 1.0d0); a(9) = (7.0d0, 0.0d0)
  b(1) = (4.0d0, 0.0d0); b(4) = (1.0d0, 1.0d0); b(5) = (5.0d0, 0.0d0)
  b(8) = (2.0d0, -1.0d0); b(9) = (6.0d0, 0.0d0)
  call zhegvx(1, 'V', 'I', 'U', 3, a, 3, b, 3, &
              0.0d0, 0.0d0, 2, 3, 0.0d0, m, w, z, 3, &
              work, lwork, rwork, iwork, ifail, info)
  call begin_test('itype1_v_index_upper')
  call print_int('info', info)
  call print_int('m', m)
  call print_array('w', w, m)
  call print_array('z', z_r, 6*m)
  call end_test()

  ! Test 6: ITYPE=2, JOBZ='V', RANGE='A', UPLO='U', N=3
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0); z = (0.0d0, 0.0d0); w = 0.0d0
  a(1) = (10.0d0, 0.0d0); a(4) = (2.0d0, 1.0d0); a(5) = (8.0d0, 0.0d0)
  a(7) = (1.0d0, -2.0d0); a(8) = (3.0d0, 1.0d0); a(9) = (7.0d0, 0.0d0)
  b(1) = (4.0d0, 0.0d0); b(4) = (1.0d0, 1.0d0); b(5) = (5.0d0, 0.0d0)
  b(8) = (2.0d0, -1.0d0); b(9) = (6.0d0, 0.0d0)
  call zhegvx(2, 'V', 'A', 'U', 3, a, 3, b, 3, &
              0.0d0, 0.0d0, 0, 0, 0.0d0, m, w, z, 3, &
              work, lwork, rwork, iwork, ifail, info)
  call begin_test('itype2_v_all_upper')
  call print_int('info', info)
  call print_int('m', m)
  call print_array('w', w, 3)
  call print_array('z', z_r, 18)
  call end_test()

  ! Test 7: ITYPE=3, JOBZ='V', RANGE='A', UPLO='L', N=3
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0); z = (0.0d0, 0.0d0); w = 0.0d0
  a(1) = (10.0d0, 0.0d0); a(2) = (2.0d0, -1.0d0); a(3) = (1.0d0, 2.0d0)
  a(5) = (8.0d0, 0.0d0); a(6) = (3.0d0, -1.0d0); a(9) = (7.0d0, 0.0d0)
  b(1) = (4.0d0, 0.0d0); b(2) = (1.0d0, -1.0d0)
  b(5) = (5.0d0, 0.0d0); b(6) = (2.0d0, 1.0d0); b(9) = (6.0d0, 0.0d0)
  call zhegvx(3, 'V', 'A', 'L', 3, a, 3, b, 3, &
              0.0d0, 0.0d0, 0, 0, 0.0d0, m, w, z, 3, &
              work, lwork, rwork, iwork, ifail, info)
  call begin_test('itype3_v_all_lower')
  call print_int('info', info)
  call print_int('m', m)
  call print_array('w', w, 3)
  call print_array('z', z_r, 18)
  call end_test()

  ! Test 8: N=0 quick return
  m = -1
  call zhegvx(1, 'V', 'A', 'U', 0, a, 1, b, 1, &
              0.0d0, 0.0d0, 0, 0, 0.0d0, m, w, z, 1, &
              work, lwork, rwork, iwork, ifail, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call print_int('m', m)
  call end_test()

  ! Test 9: N=1, ITYPE=1
  a(1) = (6.0d0, 0.0d0); b(1) = (2.0d0, 0.0d0)
  z = (0.0d0, 0.0d0); w = 0.0d0
  call zhegvx(1, 'V', 'A', 'U', 1, a, 1, b, 1, &
              0.0d0, 0.0d0, 0, 0, 0.0d0, m, w, z, 1, &
              work, lwork, rwork, iwork, ifail, info)
  call begin_test('n_one')
  call print_int('info', info)
  call print_int('m', m)
  call print_array('w', w, 1)
  call print_array('z', z_r, 2)
  call end_test()

  ! Test 10: Non-positive definite B
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 0.0d0); a(4) = (1.0d0, 0.0d0)
  b(1) = (-1.0d0, 0.0d0); b(4) = (1.0d0, 0.0d0)
  call zhegvx(1, 'V', 'A', 'L', 2, a, 2, b, 2, &
              0.0d0, 0.0d0, 0, 0, 0.0d0, m, w, z, 2, &
              work, lwork, rwork, iwork, ifail, info)
  call begin_test('not_posdef')
  call print_int('info', info)
  call end_test()

  ! Test 11: ITYPE=2, RANGE='I', UPLO='L', N=3 (index range: 1st only)
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0); z = (0.0d0, 0.0d0); w = 0.0d0
  a(1) = (10.0d0, 0.0d0); a(2) = (2.0d0, -1.0d0); a(3) = (1.0d0, 2.0d0)
  a(5) = (8.0d0, 0.0d0); a(6) = (3.0d0, -1.0d0); a(9) = (7.0d0, 0.0d0)
  b(1) = (4.0d0, 0.0d0); b(2) = (1.0d0, -1.0d0)
  b(5) = (5.0d0, 0.0d0); b(6) = (2.0d0, 1.0d0); b(9) = (6.0d0, 0.0d0)
  call zhegvx(2, 'V', 'I', 'L', 3, a, 3, b, 3, &
              0.0d0, 0.0d0, 1, 1, 0.0d0, m, w, z, 3, &
              work, lwork, rwork, iwork, ifail, info)
  call begin_test('itype2_v_index_lower')
  call print_int('info', info)
  call print_int('m', m)
  call print_array('w', w, m)
  call print_array('z', z_r, 6*m)
  call end_test()

  ! Test 12: ITYPE=3, RANGE='V', UPLO='U', N=3 (value range)
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0); z = (0.0d0, 0.0d0); w = 0.0d0
  a(1) = (10.0d0, 0.0d0); a(4) = (2.0d0, 1.0d0); a(5) = (8.0d0, 0.0d0)
  a(7) = (1.0d0, -2.0d0); a(8) = (3.0d0, 1.0d0); a(9) = (7.0d0, 0.0d0)
  b(1) = (4.0d0, 0.0d0); b(4) = (1.0d0, 1.0d0); b(5) = (5.0d0, 0.0d0)
  b(8) = (2.0d0, -1.0d0); b(9) = (6.0d0, 0.0d0)
  call zhegvx(3, 'V', 'V', 'U', 3, a, 3, b, 3, &
              1.0d0, 100.0d0, 0, 0, 0.0d0, m, w, z, 3, &
              work, lwork, rwork, iwork, ifail, info)
  call begin_test('itype3_v_value_upper')
  call print_int('info', info)
  call print_int('m', m)
  call print_array('w', w, m)
  call print_array('z', z_r, 6*m)
  call end_test()

  ! Test 13: ITYPE=1, JOBZ='N', RANGE='V', UPLO='L', N=3 (no vectors, value range)
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0); w = 0.0d0
  a(1) = (10.0d0, 0.0d0); a(2) = (2.0d0, -1.0d0); a(3) = (1.0d0, 2.0d0)
  a(5) = (8.0d0, 0.0d0); a(6) = (3.0d0, -1.0d0); a(9) = (7.0d0, 0.0d0)
  b(1) = (4.0d0, 0.0d0); b(2) = (1.0d0, -1.0d0)
  b(5) = (5.0d0, 0.0d0); b(6) = (2.0d0, 1.0d0); b(9) = (6.0d0, 0.0d0)
  call zhegvx(1, 'N', 'V', 'L', 3, a, 3, b, 3, &
              0.5d0, 2.0d0, 0, 0, 0.0d0, m, w, z, 1, &
              work, lwork, rwork, iwork, ifail, info)
  call begin_test('itype1_n_value_lower')
  call print_int('info', info)
  call print_int('m', m)
  call print_array('w', w, m)
  call end_test()

  ! Test 14: ITYPE=1, RANGE='I', single eigenvalue (IL=IU=2)
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0); z = (0.0d0, 0.0d0); w = 0.0d0
  a(1) = (10.0d0, 0.0d0); a(4) = (2.0d0, 1.0d0); a(5) = (8.0d0, 0.0d0)
  a(7) = (1.0d0, -2.0d0); a(8) = (3.0d0, 1.0d0); a(9) = (7.0d0, 0.0d0)
  b(1) = (4.0d0, 0.0d0); b(4) = (1.0d0, 1.0d0); b(5) = (5.0d0, 0.0d0)
  b(8) = (2.0d0, -1.0d0); b(9) = (6.0d0, 0.0d0)
  call zhegvx(1, 'V', 'I', 'U', 3, a, 3, b, 3, &
              0.0d0, 0.0d0, 2, 2, 0.0d0, m, w, z, 3, &
              work, lwork, rwork, iwork, ifail, info)
  call begin_test('itype1_v_index_single')
  call print_int('info', info)
  call print_int('m', m)
  call print_array('w', w, m)
  call print_array('z', z_r, 6*m)
  call end_test()

end program
