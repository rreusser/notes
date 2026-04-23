program test_zpbsv
  use test_utils
  implicit none

  complex*16 :: ab(200), b(200)
  double precision :: ab_r(400), b_r(400)
  equivalence (ab, ab_r)
  equivalence (b, b_r)
  integer :: info, ldab, ldb

  ! ===== Test 1: UPLO='U', N=5, KD=1 (tridiagonal), NRHS=1 =====
  ! Hermitian positive definite banded: diag=4, super=-1+0.5i
  ldab = 2
  ldb = 5
  ab = (0.0d0, 0.0d0)
  ! Column 1: superdiag=*, diag=4
  ab(1) = (0.0d0, 0.0d0); ab(2) = (4.0d0, 0.0d0)
  ! Column 2: superdiag=(-1,0.5), diag=4
  ab(3) = (-1.0d0, 0.5d0); ab(4) = (4.0d0, 0.0d0)
  ! Column 3: superdiag=(-1,0.5), diag=4
  ab(5) = (-1.0d0, 0.5d0); ab(6) = (4.0d0, 0.0d0)
  ! Column 4: superdiag=(-1,0.5), diag=4
  ab(7) = (-1.0d0, 0.5d0); ab(8) = (4.0d0, 0.0d0)
  ! Column 5: superdiag=(-1,0.5), diag=4
  ab(9) = (-1.0d0, 0.5d0); ab(10) = (4.0d0, 0.0d0)

  b(1) = (1.0d0, 1.0d0); b(2) = (2.0d0, -1.0d0); b(3) = (0.0d0, 3.0d0)
  b(4) = (-1.0d0, 2.0d0); b(5) = (1.0d0, 0.0d0)

  call zpbsv('U', 5, 1, 1, ab, ldab, b, ldb, info)
  call begin_test('upper_tridiag_nrhs1')
  call print_int('info', info)
  call print_array('x', b_r, 10)
  call end_test()

  ! ===== Test 2: UPLO='L', N=5, KD=1 (tridiagonal), NRHS=1 =====
  ldab = 2
  ldb = 5
  ab = (0.0d0, 0.0d0)
  ! Lower band: row 0 = diag, row 1 = subdiag
  ! Column 1: diag=4, subdiag=(-1,-0.5)
  ab(1) = (4.0d0, 0.0d0); ab(2) = (-1.0d0, -0.5d0)
  ! Column 2: diag=4, subdiag=(-1,-0.5)
  ab(3) = (4.0d0, 0.0d0); ab(4) = (-1.0d0, -0.5d0)
  ! Column 3: diag=4, subdiag=(-1,-0.5)
  ab(5) = (4.0d0, 0.0d0); ab(6) = (-1.0d0, -0.5d0)
  ! Column 4: diag=4, subdiag=(-1,-0.5)
  ab(7) = (4.0d0, 0.0d0); ab(8) = (-1.0d0, -0.5d0)
  ! Column 5: diag=4, subdiag=*
  ab(9) = (4.0d0, 0.0d0); ab(10) = (0.0d0, 0.0d0)

  b(1) = (1.0d0, 1.0d0); b(2) = (2.0d0, -1.0d0); b(3) = (0.0d0, 3.0d0)
  b(4) = (-1.0d0, 2.0d0); b(5) = (1.0d0, 0.0d0)

  call zpbsv('L', 5, 1, 1, ab, ldab, b, ldb, info)
  call begin_test('lower_tridiag_nrhs1')
  call print_int('info', info)
  call print_array('x', b_r, 10)
  call end_test()

  ! ===== Test 3: UPLO='U', NRHS=2 =====
  ldab = 2
  ldb = 5
  ab = (0.0d0, 0.0d0)
  ab(1) = (0.0d0, 0.0d0); ab(2) = (4.0d0, 0.0d0)
  ab(3) = (-1.0d0, 0.5d0); ab(4) = (4.0d0, 0.0d0)
  ab(5) = (-1.0d0, 0.5d0); ab(6) = (4.0d0, 0.0d0)
  ab(7) = (-1.0d0, 0.5d0); ab(8) = (4.0d0, 0.0d0)
  ab(9) = (-1.0d0, 0.5d0); ab(10) = (4.0d0, 0.0d0)

  ! RHS column 1
  b(1) = (1.0d0, 0.0d0); b(2) = (2.0d0, 1.0d0); b(3) = (3.0d0, -1.0d0)
  b(4) = (4.0d0, 2.0d0); b(5) = (5.0d0, 0.0d0)
  ! RHS column 2
  b(6) = (5.0d0, 1.0d0); b(7) = (4.0d0, -1.0d0); b(8) = (3.0d0, 0.0d0)
  b(9) = (2.0d0, 1.0d0); b(10) = (1.0d0, -1.0d0)

  call zpbsv('U', 5, 1, 2, ab, ldab, b, ldb, info)
  call begin_test('upper_tridiag_nrhs2')
  call print_int('info', info)
  call print_array('x', b_r, 20)
  call end_test()

  ! ===== Test 4: N=0 (quick return) =====
  call zpbsv('U', 0, 0, 1, ab, 1, b, 1, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! ===== Test 5: NRHS=0 (quick return) =====
  ab(1) = (4.0d0, 0.0d0)
  call zpbsv('L', 1, 0, 0, ab, 1, b, 1, info)
  call begin_test('nrhs_zero')
  call print_int('info', info)
  call end_test()

  ! ===== Test 6: N=1 =====
  ab(1) = (4.0d0, 0.0d0)
  b(1) = (8.0d0, 4.0d0)
  call zpbsv('U', 1, 0, 1, ab, 1, b, 1, info)
  call begin_test('n_one')
  call print_int('info', info)
  call print_array('x', b_r, 2)
  call end_test()

  ! ===== Test 7: Not positive definite (INFO > 0) =====
  ldab = 2
  ab = (0.0d0, 0.0d0)
  ! Lower band: diag=1, subdiag=(2,1)
  ab(1) = (1.0d0, 0.0d0); ab(2) = (2.0d0, 1.0d0)
  ! diag=0
  ab(3) = (0.0d0, 0.0d0); ab(4) = (0.0d0, 0.0d0)
  b(1) = (1.0d0, 0.0d0); b(2) = (1.0d0, 0.0d0)
  call zpbsv('L', 2, 1, 1, ab, ldab, b, 2, info)
  call begin_test('not_posdef')
  call print_int('info', info)
  call end_test()

  ! ===== Test 8: UPLO='L', N=4, KD=2 (pentadiagonal), NRHS=1 =====
  ldab = 3
  ldb = 4
  ab = (0.0d0, 0.0d0)
  ! Column 1: diag=6, sub1=(-0.5,0.5), sub2=(0.25,0)
  ab(1) = (6.0d0, 0.0d0); ab(2) = (-0.5d0, 0.5d0); ab(3) = (0.25d0, 0.0d0)
  ! Column 2: diag=6, sub1=(-0.5,0.5), sub2=(0.25,0)
  ab(4) = (6.0d0, 0.0d0); ab(5) = (-0.5d0, 0.5d0); ab(6) = (0.25d0, 0.0d0)
  ! Column 3: diag=6, sub1=(-0.5,0.5), sub2=*
  ab(7) = (6.0d0, 0.0d0); ab(8) = (-0.5d0, 0.5d0); ab(9) = (0.0d0, 0.0d0)
  ! Column 4: diag=6, sub1=*, sub2=*
  ab(10) = (6.0d0, 0.0d0); ab(11) = (0.0d0, 0.0d0); ab(12) = (0.0d0, 0.0d0)

  b(1) = (1.0d0, 2.0d0); b(2) = (2.0d0, -1.0d0)
  b(3) = (3.0d0, 0.0d0); b(4) = (4.0d0, 1.0d0)

  call zpbsv('L', 4, 2, 1, ab, ldab, b, ldb, info)
  call begin_test('lower_penta_nrhs1')
  call print_int('info', info)
  call print_array('x', b_r, 8)
  call end_test()

  ! ===== Test 9: UPLO='U', N=4, KD=2 (pentadiagonal), NRHS=1 =====
  ldab = 3
  ldb = 4
  ab = (0.0d0, 0.0d0)
  ! Column 1: row0=*, row1=*, row2(diag)=6
  ab(1) = (0.0d0, 0.0d0); ab(2) = (0.0d0, 0.0d0); ab(3) = (6.0d0, 0.0d0)
  ! Column 2: row0=*, row1=(-0.5,-0.5), row2(diag)=6
  ab(4) = (0.0d0, 0.0d0); ab(5) = (-0.5d0, -0.5d0); ab(6) = (6.0d0, 0.0d0)
  ! Column 3: row0=(0.25,0), row1=(-0.5,-0.5), row2(diag)=6
  ab(7) = (0.25d0, 0.0d0); ab(8) = (-0.5d0, -0.5d0); ab(9) = (6.0d0, 0.0d0)
  ! Column 4: row0=(0.25,0), row1=(-0.5,-0.5), row2(diag)=6
  ab(10) = (0.25d0, 0.0d0); ab(11) = (-0.5d0, -0.5d0); ab(12) = (6.0d0, 0.0d0)

  b(1) = (1.0d0, 2.0d0); b(2) = (2.0d0, -1.0d0)
  b(3) = (3.0d0, 0.0d0); b(4) = (4.0d0, 1.0d0)

  call zpbsv('U', 4, 2, 1, ab, ldab, b, ldb, info)
  call begin_test('upper_penta_nrhs1')
  call print_int('info', info)
  call print_array('x', b_r, 8)
  call end_test()

end program
