program test_zlags2
  use test_utils
  implicit none
  double precision :: csu, csv, csq
  complex*16 :: snu, snv, snq

  ! For printing complex scalars via EQUIVALENCE
  double precision :: snu_r(2), snv_r(2), snq_r(2)
  equivalence (snu, snu_r)
  equivalence (snv, snv_r)
  equivalence (snq, snq_r)

  ! Test 1: Upper triangular, basic case with complex off-diagonals
  call ZLAGS2(.TRUE., 4.0d0, dcmplx(2.0d0, 1.0d0), 3.0d0, &
              1.0d0, dcmplx(0.5d0, 0.25d0), 2.0d0, &
              csu, snu, csv, snv, csq, snq)
  call begin_test('upper_basic')
  call print_scalar('csu', csu)
  call print_array('snu', snu_r, 2)
  call print_scalar('csv', csv)
  call print_array('snv', snv_r, 2)
  call print_scalar('csq', csq)
  call print_array('snq', snq_r, 2)
  call end_test()

  ! Test 2: Lower triangular, basic case with complex off-diagonals
  call ZLAGS2(.FALSE., 4.0d0, dcmplx(2.0d0, 1.0d0), 3.0d0, &
              1.0d0, dcmplx(0.5d0, 0.25d0), 2.0d0, &
              csu, snu, csv, snv, csq, snq)
  call begin_test('lower_basic')
  call print_scalar('csu', csu)
  call print_array('snu', snu_r, 2)
  call print_scalar('csv', csv)
  call print_array('snv', snv_r, 2)
  call print_scalar('csq', csq)
  call print_array('snq', snq_r, 2)
  call end_test()

  ! Test 3: Upper triangular, diagonal B (B2 = 0)
  call ZLAGS2(.TRUE., 5.0d0, dcmplx(3.0d0, 2.0d0), 2.0d0, &
              1.0d0, dcmplx(0.0d0, 0.0d0), 1.0d0, &
              csu, snu, csv, snv, csq, snq)
  call begin_test('upper_diagonal_b')
  call print_scalar('csu', csu)
  call print_array('snu', snu_r, 2)
  call print_scalar('csv', csv)
  call print_array('snv', snv_r, 2)
  call print_scalar('csq', csq)
  call print_array('snq', snq_r, 2)
  call end_test()

  ! Test 4: Lower triangular, diagonal B (B2 = 0)
  call ZLAGS2(.FALSE., 5.0d0, dcmplx(3.0d0, 2.0d0), 2.0d0, &
              1.0d0, dcmplx(0.0d0, 0.0d0), 1.0d0, &
              csu, snu, csv, snv, csq, snq)
  call begin_test('lower_diagonal_b')
  call print_scalar('csu', csu)
  call print_array('snu', snu_r, 2)
  call print_scalar('csv', csv)
  call print_array('snv', snv_r, 2)
  call print_scalar('csq', csq)
  call print_array('snq', snq_r, 2)
  call end_test()

  ! Test 5: Upper triangular, purely imaginary off-diagonals
  call ZLAGS2(.TRUE., 10.0d0, dcmplx(0.0d0, 5.0d0), 5.0d0, &
              3.0d0, dcmplx(0.0d0, 1.5d0), 2.0d0, &
              csu, snu, csv, snv, csq, snq)
  call begin_test('upper_imag_offdiag')
  call print_scalar('csu', csu)
  call print_array('snu', snu_r, 2)
  call print_scalar('csv', csv)
  call print_array('snv', snv_r, 2)
  call print_scalar('csq', csq)
  call print_array('snq', snq_r, 2)
  call end_test()

  ! Test 6: Lower triangular, negative elements
  call ZLAGS2(.FALSE., -3.0d0, dcmplx(4.0d0, -1.0d0), -2.0d0, &
              1.0d0, dcmplx(-0.5d0, 0.3d0), 3.0d0, &
              csu, snu, csv, snv, csq, snq)
  call begin_test('lower_negative')
  call print_scalar('csu', csu)
  call print_array('snu', snu_r, 2)
  call print_scalar('csv', csv)
  call print_array('snv', snv_r, 2)
  call print_scalar('csq', csq)
  call print_array('snq', snq_r, 2)
  call end_test()

  ! Test 7: Upper triangular, identity-like (A2=B2=0)
  call ZLAGS2(.TRUE., 1.0d0, dcmplx(0.0d0, 0.0d0), 1.0d0, &
              1.0d0, dcmplx(0.0d0, 0.0d0), 1.0d0, &
              csu, snu, csv, snv, csq, snq)
  call begin_test('upper_identity')
  call print_scalar('csu', csu)
  call print_array('snu', snu_r, 2)
  call print_scalar('csv', csv)
  call print_array('snv', snv_r, 2)
  call print_scalar('csq', csq)
  call print_array('snq', snq_r, 2)
  call end_test()

  ! Test 8: Lower triangular, large values
  call ZLAGS2(.FALSE., 1.0d10, dcmplx(5.0d9, 3.0d9), 2.0d10, &
              3.0d10, dcmplx(1.0d10, 0.5d10), 4.0d10, &
              csu, snu, csv, snv, csq, snq)
  call begin_test('lower_large')
  call print_scalar('csu', csu)
  call print_array('snu', snu_r, 2)
  call print_scalar('csv', csv)
  call print_array('snv', snv_r, 2)
  call print_scalar('csq', csq)
  call print_array('snq', snq_r, 2)
  call end_test()

  ! Test 9: Upper, else branch (|csl|<|snl| AND |csr|<|snr|)
  ! Large off-diagonal relative to diagonal to force large rotation angles
  call ZLAGS2(.TRUE., 73.99d0, dcmplx(90.035d0, 45.0d0), 99.59d0, &
              1.0d0, dcmplx(0.5d0, 0.25d0), 1.0d0, &
              csu, snu, csv, snv, csq, snq)
  call begin_test('upper_else_branch')
  call print_scalar('csu', csu)
  call print_array('snu', snu_r, 2)
  call print_scalar('csv', csv)
  call print_array('snv', snv_r, 2)
  call print_scalar('csq', csq)
  call print_array('snq', snq_r, 2)
  call end_test()

  ! Test 10: Lower, else branch (|csr|<|snr| AND |csl|<|snl|)
  call ZLAGS2(.FALSE., 73.99d0, dcmplx(102.835d0, 51.0d0), 99.59d0, &
              1.0d0, dcmplx(0.5d0, 0.25d0), 1.0d0, &
              csu, snu, csv, snv, csq, snq)
  call begin_test('lower_else_branch')
  call print_scalar('csu', csu)
  call print_array('snu', snu_r, 2)
  call print_scalar('csv', csv)
  call print_array('snv', snv_r, 2)
  call print_scalar('csq', csq)
  call print_array('snq', snq_r, 2)
  call end_test()

  ! Test 11: Upper, zero A (triggers VB fallback in first branch)
  call ZLAGS2(.TRUE., 0.0d0, dcmplx(0.0d0, 0.0d0), 0.0d0, &
              1.0d0, dcmplx(0.5d0, 0.3d0), 2.0d0, &
              csu, snu, csv, snv, csq, snq)
  call begin_test('upper_zero_a')
  call print_scalar('csu', csu)
  call print_array('snu', snu_r, 2)
  call print_scalar('csv', csv)
  call print_array('snv', snv_r, 2)
  call print_scalar('csq', csq)
  call print_array('snq', snq_r, 2)
  call end_test()

  ! Test 12: Lower, zero A (triggers VB fallback)
  call ZLAGS2(.FALSE., 0.0d0, dcmplx(0.0d0, 0.0d0), 0.0d0, &
              1.0d0, dcmplx(0.5d0, 0.3d0), 2.0d0, &
              csu, snu, csv, snv, csq, snq)
  call begin_test('lower_zero_a')
  call print_scalar('csu', csu)
  call print_array('snu', snu_r, 2)
  call print_scalar('csv', csv)
  call print_array('snv', snv_r, 2)
  call print_scalar('csq', csq)
  call print_array('snq', snq_r, 2)
  call end_test()

  ! Test 13: Upper, VB11R + ABS1(VB12) = 0 triggers UA path
  ! B = 0 forces VB11R = 0 and VB12 = 0
  call ZLAGS2(.TRUE., 4.0d0, dcmplx(2.0d0, 1.0d0), 3.0d0, &
              0.0d0, dcmplx(0.0d0, 0.0d0), 0.0d0, &
              csu, snu, csv, snv, csq, snq)
  call begin_test('upper_zero_b')
  call print_scalar('csu', csu)
  call print_array('snu', snu_r, 2)
  call print_scalar('csv', csv)
  call print_array('snv', snv_r, 2)
  call print_scalar('csq', csq)
  call print_array('snq', snq_r, 2)
  call end_test()

  ! Test 14: Lower, B = 0 triggers UA path
  call ZLAGS2(.FALSE., 4.0d0, dcmplx(2.0d0, 1.0d0), 3.0d0, &
              0.0d0, dcmplx(0.0d0, 0.0d0), 0.0d0, &
              csu, snu, csv, snv, csq, snq)
  call begin_test('lower_zero_b')
  call print_scalar('csu', csu)
  call print_array('snu', snu_r, 2)
  call print_scalar('csv', csv)
  call print_array('snv', snv_r, 2)
  call print_scalar('csq', csq)
  call print_array('snq', snq_r, 2)
  call end_test()

  ! Test 15: Upper, else branch with VB-driven ratio comparison
  call ZLAGS2(.TRUE., -54.5637d0, dcmplx(-12.628d0, 8.0d0), 111.0266d0, &
              90.743d0, dcmplx(83.6416d0, 40.0d0), 48.053d0, &
              csu, snu, csv, snv, csq, snq)
  call begin_test('upper_else_vb_driven')
  call print_scalar('csu', csu)
  call print_array('snu', snu_r, 2)
  call print_scalar('csv', csv)
  call print_array('snv', snv_r, 2)
  call print_scalar('csq', csq)
  call print_array('snq', snq_r, 2)
  call end_test()

  ! Test 16: Lower, else branch with VB-driven ratio comparison
  call ZLAGS2(.FALSE., -19.992d0, dcmplx(-17.6713d0, 9.0d0), -59.3108d0, &
              96.5235d0, dcmplx(-54.6987d0, 30.0d0), 74.2924d0, &
              csu, snu, csv, snv, csq, snq)
  call begin_test('lower_else_vb_driven')
  call print_scalar('csu', csu)
  call print_array('snu', snu_r, 2)
  call print_scalar('csv', csv)
  call print_array('snv', snv_r, 2)
  call print_scalar('csq', csq)
  call print_array('snq', snq_r, 2)
  call end_test()

end program
