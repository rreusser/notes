program test_dlaln2
  use test_utils
  implicit none
  double precision :: A(2,2), B(2,2), X(2,2), SCALE, XNORM, SMIN, CA
  double precision :: D1, D2, WR, WI
  integer :: INFO, NA, NW
  logical :: LTRANS

  ! Test 1: 1x1 real system (na=1, nw=1, ltrans=false)
  ! (ca*A(1,1) - wr*d1) * x = scale * b
  ! (2*3 - 1*1) * x = scale * 10  => 5*x = 10 => x=2
  NA = 1; NW = 1; LTRANS = .FALSE.
  CA = 2.0d0; SMIN = 0.0d0
  A = 0.0d0; A(1,1) = 3.0d0
  D1 = 1.0d0; D2 = 0.0d0
  B = 0.0d0; B(1,1) = 10.0d0
  WR = 1.0d0; WI = 0.0d0
  X = 0.0d0
  call DLALN2(LTRANS, NA, NW, SMIN, CA, A, 2, D1, D2, B, 2, WR, WI, X, 2, SCALE, XNORM, INFO)
  call begin_test('1x1_real_basic')
  call print_scalar('X11', X(1,1))
  call print_scalar('SCALE', SCALE)
  call print_scalar('XNORM', XNORM)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 2: 1x1 complex system (na=1, nw=2)
  NA = 1; NW = 2; LTRANS = .FALSE.
  CA = 1.0d0; SMIN = 0.0d0
  A = 0.0d0; A(1,1) = 2.0d0
  D1 = 1.0d0; D2 = 0.0d0
  B = 0.0d0; B(1,1) = 1.0d0; B(1,2) = 0.0d0
  WR = 1.0d0; WI = 1.0d0
  X = 0.0d0
  call DLALN2(LTRANS, NA, NW, SMIN, CA, A, 2, D1, D2, B, 2, WR, WI, X, 2, SCALE, XNORM, INFO)
  call begin_test('1x1_complex_basic')
  call print_scalar('Xre', X(1,1))
  call print_scalar('Xim', X(1,2))
  call print_scalar('SCALE', SCALE)
  call print_scalar('XNORM', XNORM)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 3: 2x2 real system, no transpose (na=2, nw=1)
  NA = 2; NW = 1; LTRANS = .FALSE.
  CA = 1.0d0; SMIN = 0.0d0
  A = 0.0d0; A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(2,2) = 3.0d0
  D1 = 1.0d0; D2 = 1.0d0
  B = 0.0d0; B(1,1) = 7.0d0; B(2,1) = 6.0d0
  WR = 0.0d0; WI = 0.0d0
  X = 0.0d0
  call DLALN2(LTRANS, NA, NW, SMIN, CA, A, 2, D1, D2, B, 2, WR, WI, X, 2, SCALE, XNORM, INFO)
  call begin_test('2x2_real_notrans')
  call print_scalar('X1', X(1,1))
  call print_scalar('X2', X(2,1))
  call print_scalar('SCALE', SCALE)
  call print_scalar('XNORM', XNORM)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 4: 2x2 real system, transposed
  NA = 2; NW = 1; LTRANS = .TRUE.
  CA = 1.0d0; SMIN = 0.0d0
  A = 0.0d0; A(1,1) = 2.0d0; A(2,1) = 1.0d0; A(2,2) = 3.0d0
  D1 = 1.0d0; D2 = 1.0d0
  B = 0.0d0; B(1,1) = 5.0d0; B(2,1) = 9.0d0
  WR = 0.0d0; WI = 0.0d0
  X = 0.0d0
  call DLALN2(LTRANS, NA, NW, SMIN, CA, A, 2, D1, D2, B, 2, WR, WI, X, 2, SCALE, XNORM, INFO)
  call begin_test('2x2_real_trans')
  call print_scalar('X1', X(1,1))
  call print_scalar('X2', X(2,1))
  call print_scalar('SCALE', SCALE)
  call print_scalar('XNORM', XNORM)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 5: 1x1 near-singular
  NA = 1; NW = 1; LTRANS = .FALSE.
  CA = 1.0d0; SMIN = 0.0d0
  A = 0.0d0; A(1,1) = 1.0d0
  D1 = 1.0d0; D2 = 0.0d0
  B = 0.0d0; B(1,1) = 1.0d0
  WR = 1.0d0; WI = 0.0d0
  X = 0.0d0
  call DLALN2(LTRANS, NA, NW, SMIN, CA, A, 2, D1, D2, B, 2, WR, WI, X, 2, SCALE, XNORM, INFO)
  call begin_test('1x1_real_singular')
  call print_scalar('X11', X(1,1))
  call print_scalar('SCALE', SCALE)
  call print_scalar('XNORM', XNORM)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 6: 2x2 complex system (na=2, nw=2, ltrans=false)
  NA = 2; NW = 2; LTRANS = .FALSE.
  CA = 1.0d0; SMIN = 0.0d0
  A = 0.0d0; A(1,1) = 4.0d0; A(1,2) = 1.0d0; A(2,2) = 3.0d0
  D1 = 1.0d0; D2 = 1.0d0
  B = 0.0d0; B(1,1) = 1.0d0; B(2,1) = 2.0d0; B(1,2) = 0.5d0; B(2,2) = 1.0d0
  WR = 1.0d0; WI = 2.0d0
  X = 0.0d0
  call DLALN2(LTRANS, NA, NW, SMIN, CA, A, 2, D1, D2, B, 2, WR, WI, X, 2, SCALE, XNORM, INFO)
  call begin_test('2x2_complex_notrans')
  call print_scalar('X1re', X(1,1))
  call print_scalar('X2re', X(2,1))
  call print_scalar('X1im', X(1,2))
  call print_scalar('X2im', X(2,2))
  call print_scalar('SCALE', SCALE)
  call print_scalar('XNORM', XNORM)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 7: 2x2 complex system, transposed
  NA = 2; NW = 2; LTRANS = .TRUE.
  CA = 1.0d0; SMIN = 0.0d0
  A = 0.0d0; A(1,1) = 4.0d0; A(2,1) = 1.0d0; A(2,2) = 3.0d0
  D1 = 1.0d0; D2 = 1.0d0
  B = 0.0d0; B(1,1) = 1.0d0; B(2,1) = 2.0d0; B(1,2) = 0.5d0; B(2,2) = 1.0d0
  WR = 1.0d0; WI = 2.0d0
  X = 0.0d0
  call DLALN2(LTRANS, NA, NW, SMIN, CA, A, 2, D1, D2, B, 2, WR, WI, X, 2, SCALE, XNORM, INFO)
  call begin_test('2x2_complex_trans')
  call print_scalar('X1re', X(1,1))
  call print_scalar('X2re', X(2,1))
  call print_scalar('X1im', X(1,2))
  call print_scalar('X2im', X(2,2))
  call print_scalar('SCALE', SCALE)
  call print_scalar('XNORM', XNORM)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 8: 1x1 real with non-trivial ca and d1
  NA = 1; NW = 1; LTRANS = .FALSE.
  CA = 3.0d0; SMIN = 0.0d0
  A = 0.0d0; A(1,1) = 5.0d0
  D1 = 4.0d0; D2 = 0.0d0
  B = 0.0d0; B(1,1) = 7.0d0
  WR = 2.0d0; WI = 0.0d0
  X = 0.0d0
  call DLALN2(LTRANS, NA, NW, SMIN, CA, A, 2, D1, D2, B, 2, WR, WI, X, 2, SCALE, XNORM, INFO)
  call begin_test('1x1_real_nontrivial_ca_d1')
  call print_scalar('X11', X(1,1))
  call print_scalar('SCALE', SCALE)
  call print_scalar('XNORM', XNORM)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 9: 2x2 real with wr shift
  NA = 2; NW = 1; LTRANS = .FALSE.
  CA = 1.0d0; SMIN = 0.0d0
  A = 0.0d0; A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(2,2) = 3.0d0
  D1 = 1.0d0; D2 = 2.0d0
  B = 0.0d0; B(1,1) = 5.0d0; B(2,1) = 9.0d0
  WR = 3.0d0; WI = 0.0d0
  X = 0.0d0
  call DLALN2(LTRANS, NA, NW, SMIN, CA, A, 2, D1, D2, B, 2, WR, WI, X, 2, SCALE, XNORM, INFO)
  call begin_test('2x2_real_wr_shift')
  call print_scalar('X1', X(1,1))
  call print_scalar('X2', X(2,1))
  call print_scalar('SCALE', SCALE)
  call print_scalar('XNORM', XNORM)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 10: 1x1 complex near-singular
  NA = 1; NW = 2; LTRANS = .FALSE.
  CA = 1.0d0; SMIN = 0.0d0
  A = 0.0d0; A(1,1) = 1.0d0
  D1 = 1.0d0; D2 = 0.0d0
  B = 0.0d0; B(1,1) = 1.0d0; B(1,2) = 0.0d0
  WR = 1.0d0; WI = 0.0d0
  X = 0.0d0
  call DLALN2(LTRANS, NA, NW, SMIN, CA, A, 2, D1, D2, B, 2, WR, WI, X, 2, SCALE, XNORM, INFO)
  call begin_test('1x1_complex_singular')
  call print_scalar('Xre', X(1,1))
  call print_scalar('Xim', X(1,2))
  call print_scalar('SCALE', SCALE)
  call print_scalar('XNORM', XNORM)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 11: 2x2 real with smin > 0
  NA = 2; NW = 1; LTRANS = .FALSE.
  CA = 1.0d0; SMIN = 1.0d0
  A = 0.0d0; A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(2,2) = 3.0d0
  D1 = 1.0d0; D2 = 1.0d0
  B = 0.0d0; B(1,1) = 7.0d0; B(2,1) = 6.0d0
  WR = 0.0d0; WI = 0.0d0
  X = 0.0d0
  call DLALN2(LTRANS, NA, NW, SMIN, CA, A, 2, D1, D2, B, 2, WR, WI, X, 2, SCALE, XNORM, INFO)
  call begin_test('2x2_real_smin_positive')
  call print_scalar('X1', X(1,1))
  call print_scalar('X2', X(2,1))
  call print_scalar('SCALE', SCALE)
  call print_scalar('XNORM', XNORM)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 12: 2x2 near-singular (all coefficients small)
  NA = 2; NW = 1; LTRANS = .FALSE.
  CA = 1.0d-300; SMIN = 0.0d0
  A = 0.0d0; A(1,1) = 1.0d0; A(2,2) = 1.0d0
  D1 = 1.0d0; D2 = 1.0d0
  B = 0.0d0; B(1,1) = 1.0d0; B(2,1) = 1.0d0
  WR = 0.0d0; WI = 0.0d0
  X = 0.0d0
  call DLALN2(LTRANS, NA, NW, SMIN, CA, A, 2, D1, D2, B, 2, WR, WI, X, 2, SCALE, XNORM, INFO)
  call begin_test('2x2_real_all_small')
  call print_scalar('X1', X(1,1))
  call print_scalar('X2', X(2,1))
  call print_scalar('SCALE', SCALE)
  call print_scalar('XNORM', XNORM)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 13: 2x2 complex near-singular
  NA = 2; NW = 2; LTRANS = .FALSE.
  CA = 1.0d-300; SMIN = 0.0d0
  A = 0.0d0; A(1,1) = 1.0d0; A(2,2) = 1.0d0
  D1 = 1.0d0; D2 = 1.0d0
  B = 0.0d0; B(1,1) = 1.0d0; B(2,1) = 1.0d0
  WR = 0.0d0; WI = 0.0d0
  X = 0.0d0
  call DLALN2(LTRANS, NA, NW, SMIN, CA, A, 2, D1, D2, B, 2, WR, WI, X, 2, SCALE, XNORM, INFO)
  call begin_test('2x2_complex_all_small')
  call print_scalar('X1re', X(1,1))
  call print_scalar('X2re', X(2,1))
  call print_scalar('X1im', X(1,2))
  call print_scalar('X2im', X(2,2))
  call print_scalar('SCALE', SCALE)
  call print_scalar('XNORM', XNORM)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 14: 2x2 real with different D values and full A
  NA = 2; NW = 1; LTRANS = .FALSE.
  CA = 2.0d0; SMIN = 0.0d0
  A = 0.0d0; A(1,1) = 5.0d0; A(1,2) = 3.0d0; A(2,1) = 1.0d0; A(2,2) = 4.0d0
  D1 = 2.0d0; D2 = 3.0d0
  B = 0.0d0; B(1,1) = 10.0d0; B(2,1) = 20.0d0
  WR = 1.0d0; WI = 0.0d0
  X = 0.0d0
  call DLALN2(LTRANS, NA, NW, SMIN, CA, A, 2, D1, D2, B, 2, WR, WI, X, 2, SCALE, XNORM, INFO)
  call begin_test('2x2_real_diff_D')
  call print_scalar('X1', X(1,1))
  call print_scalar('X2', X(2,1))
  call print_scalar('SCALE', SCALE)
  call print_scalar('XNORM', XNORM)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 15: 1x1 real scaling trigger (bnorm > BIGNUM * cnorm)
  ! Set up: small coefficient, large B
  NA = 1; NW = 1; LTRANS = .FALSE.
  CA = 1.0d0; SMIN = 0.0d0
  A = 0.0d0; A(1,1) = 1.0d-305
  D1 = 0.0d0; D2 = 0.0d0
  B = 0.0d0; B(1,1) = 1.0d+300
  WR = 0.0d0; WI = 0.0d0
  X = 0.0d0
  call DLALN2(LTRANS, NA, NW, SMIN, CA, A, 2, D1, D2, B, 2, WR, WI, X, 2, SCALE, XNORM, INFO)
  call begin_test('1x1_real_scaling')
  call print_scalar('X11', X(1,1))
  call print_scalar('SCALE', SCALE)
  call print_scalar('XNORM', XNORM)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 16: 1x1 complex scaling trigger
  NA = 1; NW = 2; LTRANS = .FALSE.
  CA = 1.0d0; SMIN = 0.0d0
  A = 0.0d0; A(1,1) = 1.0d-305
  D1 = 0.0d0; D2 = 0.0d0
  B = 0.0d0; B(1,1) = 1.0d+300; B(1,2) = 1.0d+300
  WR = 0.0d0; WI = 0.0d0
  X = 0.0d0
  call DLALN2(LTRANS, NA, NW, SMIN, CA, A, 2, D1, D2, B, 2, WR, WI, X, 2, SCALE, XNORM, INFO)
  call begin_test('1x1_complex_scaling')
  call print_scalar('Xre', X(1,1))
  call print_scalar('Xim', X(1,2))
  call print_scalar('SCALE', SCALE)
  call print_scalar('XNORM', XNORM)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 17: 2x2 real with pivot on off-diagonal (icmax=1 or 2)
  ! Make cr21 the largest element
  NA = 2; NW = 1; LTRANS = .FALSE.
  CA = 1.0d0; SMIN = 0.0d0
  A = 0.0d0; A(1,1) = 1.0d0; A(1,2) = 0.5d0; A(2,1) = 10.0d0; A(2,2) = 1.0d0
  D1 = 1.0d0; D2 = 1.0d0
  B = 0.0d0; B(1,1) = 3.0d0; B(2,1) = 5.0d0
  WR = 0.0d0; WI = 0.0d0
  X = 0.0d0
  call DLALN2(LTRANS, NA, NW, SMIN, CA, A, 2, D1, D2, B, 2, WR, WI, X, 2, SCALE, XNORM, INFO)
  call begin_test('2x2_real_offdiag_pivot')
  call print_scalar('X1', X(1,1))
  call print_scalar('X2', X(2,1))
  call print_scalar('SCALE', SCALE)
  call print_scalar('XNORM', XNORM)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 18: 2x2 complex with pivot on off-diagonal
  NA = 2; NW = 2; LTRANS = .FALSE.
  CA = 1.0d0; SMIN = 0.0d0
  A = 0.0d0; A(1,1) = 1.0d0; A(1,2) = 0.5d0; A(2,1) = 10.0d0; A(2,2) = 1.0d0
  D1 = 1.0d0; D2 = 1.0d0
  B = 0.0d0; B(1,1) = 3.0d0; B(2,1) = 5.0d0; B(1,2) = 1.0d0; B(2,2) = 2.0d0
  WR = 0.0d0; WI = 1.0d0
  X = 0.0d0
  call DLALN2(LTRANS, NA, NW, SMIN, CA, A, 2, D1, D2, B, 2, WR, WI, X, 2, SCALE, XNORM, INFO)
  call begin_test('2x2_complex_offdiag_pivot')
  call print_scalar('X1re', X(1,1))
  call print_scalar('X2re', X(2,1))
  call print_scalar('X1im', X(1,2))
  call print_scalar('X2im', X(2,2))
  call print_scalar('SCALE', SCALE)
  call print_scalar('XNORM', XNORM)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 19: 1x1 real with ltrans=true (should be same as false for 1x1)
  NA = 1; NW = 1; LTRANS = .TRUE.
  CA = 2.0d0; SMIN = 0.0d0
  A = 0.0d0; A(1,1) = 3.0d0
  D1 = 1.0d0; D2 = 0.0d0
  B = 0.0d0; B(1,1) = 10.0d0
  WR = 1.0d0; WI = 0.0d0
  X = 0.0d0
  call DLALN2(LTRANS, NA, NW, SMIN, CA, A, 2, D1, D2, B, 2, WR, WI, X, 2, SCALE, XNORM, INFO)
  call begin_test('1x1_real_ltrans')
  call print_scalar('X11', X(1,1))
  call print_scalar('SCALE', SCALE)
  call print_scalar('XNORM', XNORM)
  call print_int('INFO', INFO)
  call end_test()

  ! Test 20: 2x2 real where icmax=2 (cr[2] largest: upper-right of coefficient)
  ! Make A(1,2) large so cr[2] = ca*A(1,2) is biggest
  NA = 2; NW = 1; LTRANS = .FALSE.
  CA = 1.0d0; SMIN = 0.0d0
  A = 0.0d0; A(1,1) = 0.1d0; A(1,2) = 20.0d0; A(2,1) = 0.1d0; A(2,2) = 0.2d0
  D1 = 1.0d0; D2 = 1.0d0
  B = 0.0d0; B(1,1) = 4.0d0; B(2,1) = 8.0d0
  WR = 0.0d0; WI = 0.0d0
  X = 0.0d0
  call DLALN2(LTRANS, NA, NW, SMIN, CA, A, 2, D1, D2, B, 2, WR, WI, X, 2, SCALE, XNORM, INFO)
  call begin_test('2x2_real_icmax2')
  call print_scalar('X1', X(1,1))
  call print_scalar('X2', X(2,1))
  call print_scalar('SCALE', SCALE)
  call print_scalar('XNORM', XNORM)
  call print_int('INFO', INFO)
  call end_test()

end program
