program test_dpbsv
  use test_utils
  implicit none

  double precision :: ab(100), b(100)
  integer :: info, ldab, ldb

  ! ===== Test 1: UPLO='U', N=5, KD=1 (tridiagonal), NRHS=1 =====
  ! A = tridiag(-1, 2, -1), b = [1, 0, 0, 0, 1]
  ldab = 2
  ldb = 5
  ab = 0.0d0
  ab(1) = 0.0d0;  ab(2) = 2.0d0
  ab(3) = -1.0d0; ab(4) = 2.0d0
  ab(5) = -1.0d0; ab(6) = 2.0d0
  ab(7) = -1.0d0; ab(8) = 2.0d0
  ab(9) = -1.0d0; ab(10) = 2.0d0

  b(1) = 1.0d0; b(2) = 0.0d0; b(3) = 0.0d0; b(4) = 0.0d0; b(5) = 1.0d0

  call dpbsv('U', 5, 1, 1, ab, ldab, b, ldb, info)
  call begin_test('upper_tridiag_nrhs1')
  call print_int('info', info)
  call print_array('x', b, 5)
  call print_array('ab_factored', ab, 10)
  call end_test()

  ! ===== Test 2: UPLO='L', N=5, KD=1 (tridiagonal), NRHS=1 =====
  ldab = 2
  ldb = 5
  ab = 0.0d0
  ab(1) = 2.0d0;  ab(2) = -1.0d0
  ab(3) = 2.0d0;  ab(4) = -1.0d0
  ab(5) = 2.0d0;  ab(6) = -1.0d0
  ab(7) = 2.0d0;  ab(8) = -1.0d0
  ab(9) = 2.0d0;  ab(10) = 0.0d0

  b(1) = 1.0d0; b(2) = 0.0d0; b(3) = 0.0d0; b(4) = 0.0d0; b(5) = 1.0d0

  call dpbsv('L', 5, 1, 1, ab, ldab, b, ldb, info)
  call begin_test('lower_tridiag_nrhs1')
  call print_int('info', info)
  call print_array('x', b, 5)
  call end_test()

  ! ===== Test 3: UPLO='U', NRHS=2 =====
  ldab = 2
  ldb = 5
  ab = 0.0d0
  ab(1) = 0.0d0;  ab(2) = 2.0d0
  ab(3) = -1.0d0; ab(4) = 2.0d0
  ab(5) = -1.0d0; ab(6) = 2.0d0
  ab(7) = -1.0d0; ab(8) = 2.0d0
  ab(9) = -1.0d0; ab(10) = 2.0d0

  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0; b(4) = 4.0d0; b(5) = 5.0d0
  b(6) = 5.0d0; b(7) = 4.0d0; b(8) = 3.0d0; b(9) = 2.0d0; b(10) = 1.0d0

  call dpbsv('U', 5, 1, 2, ab, ldab, b, ldb, info)
  call begin_test('upper_tridiag_nrhs2')
  call print_int('info', info)
  call print_array('x', b, 10)
  call end_test()

  ! ===== Test 4: N=0 (quick return) =====
  call dpbsv('U', 0, 0, 1, ab, 1, b, 1, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! ===== Test 5: NRHS=0 (quick return) =====
  ab(1) = 4.0d0
  call dpbsv('L', 1, 0, 0, ab, 1, b, 1, info)
  call begin_test('nrhs_zero')
  call print_int('info', info)
  call end_test()

  ! ===== Test 6: N=1 =====
  ab(1) = 4.0d0
  b(1) = 8.0d0
  call dpbsv('U', 1, 0, 1, ab, 1, b, 1, info)
  call begin_test('n_one')
  call print_int('info', info)
  call print_array('x', b, 1)
  call end_test()

  ! ===== Test 7: Not positive definite (INFO > 0) =====
  ldab = 2
  ab = 0.0d0
  ab(1) = 1.0d0; ab(2) = 2.0d0   ! col 1: diag=1, sub=2 (or super=2)
  ab(3) = 1.0d0; ab(4) = 0.0d0   ! col 2: diag=0, sub=0
  b(1) = 1.0d0; b(2) = 1.0d0
  call dpbsv('L', 2, 1, 1, ab, ldab, b, 2, info)
  call begin_test('not_posdef')
  call print_int('info', info)
  call end_test()

  ! ===== Test 8: UPLO='L', N=4, KD=2 (pentadiagonal), NRHS=1 =====
  ldab = 3
  ldb = 4
  ab = 0.0d0
  ab(1) = 6.0d0; ab(2) = -1.0d0; ab(3) = 0.5d0
  ab(4) = 6.0d0; ab(5) = -1.0d0; ab(6) = 0.5d0
  ab(7) = 6.0d0; ab(8) = -1.0d0; ab(9) = 0.0d0
  ab(10) = 6.0d0; ab(11) = 0.0d0; ab(12) = 0.0d0

  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0; b(4) = 4.0d0

  call dpbsv('L', 4, 2, 1, ab, ldab, b, ldb, info)
  call begin_test('lower_penta_nrhs1')
  call print_int('info', info)
  call print_array('x', b, 4)
  call end_test()

  ! ===== Test 9: UPLO='U', N=4, KD=2 (pentadiagonal), NRHS=1 =====
  ldab = 3
  ldb = 4
  ab = 0.0d0
  ab(1) = 0.0d0; ab(2) = 0.0d0; ab(3) = 6.0d0
  ab(4) = 0.0d0; ab(5) = -1.0d0; ab(6) = 6.0d0
  ab(7) = 0.5d0; ab(8) = -1.0d0; ab(9) = 6.0d0
  ab(10) = 0.5d0; ab(11) = -1.0d0; ab(12) = 6.0d0

  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0; b(4) = 4.0d0

  call dpbsv('U', 4, 2, 1, ab, ldab, b, ldb, info)
  call begin_test('upper_penta_nrhs1')
  call print_int('info', info)
  call print_array('x', b, 4)
  call end_test()

end program
