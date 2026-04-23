program test_dlags2
  use test_utils
  implicit none
  double precision :: csu, snu, csv, snv, csq, snq

  ! Test 1: Upper triangular, basic case
  call DLAGS2(.TRUE., 4.0d0, 2.0d0, 3.0d0, 1.0d0, 0.5d0, 2.0d0, &
              csu, snu, csv, snv, csq, snq)
  call begin_test('upper_basic')
  call print_scalar('csu', csu)
  call print_scalar('snu', snu)
  call print_scalar('csv', csv)
  call print_scalar('snv', snv)
  call print_scalar('csq', csq)
  call print_scalar('snq', snq)
  call end_test()

  ! Test 2: Lower triangular, basic case
  call DLAGS2(.FALSE., 4.0d0, 2.0d0, 3.0d0, 1.0d0, 0.5d0, 2.0d0, &
              csu, snu, csv, snv, csq, snq)
  call begin_test('lower_basic')
  call print_scalar('csu', csu)
  call print_scalar('snu', snu)
  call print_scalar('csv', csv)
  call print_scalar('snv', snv)
  call print_scalar('csq', csq)
  call print_scalar('snq', snq)
  call end_test()

  ! Test 3: Upper triangular, diagonal B
  call DLAGS2(.TRUE., 5.0d0, 3.0d0, 2.0d0, 1.0d0, 0.0d0, 1.0d0, &
              csu, snu, csv, snv, csq, snq)
  call begin_test('upper_diagonal_b')
  call print_scalar('csu', csu)
  call print_scalar('snu', snu)
  call print_scalar('csv', csv)
  call print_scalar('snv', snv)
  call print_scalar('csq', csq)
  call print_scalar('snq', snq)
  call end_test()

  ! Test 4: Lower triangular, diagonal B
  call DLAGS2(.FALSE., 5.0d0, 3.0d0, 2.0d0, 1.0d0, 0.0d0, 1.0d0, &
              csu, snu, csv, snv, csq, snq)
  call begin_test('lower_diagonal_b')
  call print_scalar('csu', csu)
  call print_scalar('snu', snu)
  call print_scalar('csv', csv)
  call print_scalar('snv', snv)
  call print_scalar('csq', csq)
  call print_scalar('snq', snq)
  call end_test()

  ! Test 5: Upper triangular, small off-diagonal
  call DLAGS2(.TRUE., 10.0d0, 1.0d-10, 5.0d0, 3.0d0, 1.0d-10, 2.0d0, &
              csu, snu, csv, snv, csq, snq)
  call begin_test('upper_small_offdiag')
  call print_scalar('csu', csu)
  call print_scalar('snu', snu)
  call print_scalar('csv', csv)
  call print_scalar('snv', snv)
  call print_scalar('csq', csq)
  call print_scalar('snq', snq)
  call end_test()

  ! Test 6: Lower triangular, negative elements
  call DLAGS2(.FALSE., -3.0d0, 4.0d0, -2.0d0, 1.0d0, -0.5d0, 3.0d0, &
              csu, snu, csv, snv, csq, snq)
  call begin_test('lower_negative')
  call print_scalar('csu', csu)
  call print_scalar('snu', snu)
  call print_scalar('csv', csv)
  call print_scalar('snv', snv)
  call print_scalar('csq', csq)
  call print_scalar('snq', snq)
  call end_test()

  ! Test 7: Upper triangular, identity-like
  call DLAGS2(.TRUE., 1.0d0, 0.0d0, 1.0d0, 1.0d0, 0.0d0, 1.0d0, &
              csu, snu, csv, snv, csq, snq)
  call begin_test('upper_identity')
  call print_scalar('csu', csu)
  call print_scalar('snu', snu)
  call print_scalar('csv', csv)
  call print_scalar('snv', snv)
  call print_scalar('csq', csq)
  call print_scalar('snq', snq)
  call end_test()

  ! Test 8: Lower triangular, large values
  call DLAGS2(.FALSE., 1.0d10, 5.0d9, 2.0d10, 3.0d10, 1.0d10, 4.0d10, &
              csu, snu, csv, snv, csq, snq)
  call begin_test('lower_large')
  call print_scalar('csu', csu)
  call print_scalar('snu', snu)
  call print_scalar('csv', csv)
  call print_scalar('snv', snv)
  call print_scalar('csq', csq)
  call print_scalar('snq', snq)
  call end_test()

end program
