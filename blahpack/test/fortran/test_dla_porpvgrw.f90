program test_dla_porpvgrw
  use test_utils
  implicit none
  double precision :: a(4, 4), af(4, 4), work(8)
  double precision :: result
  double precision :: dla_porpvgrw
  external :: dla_porpvgrw
  integer :: ncols, lda, ldaf, info

  lda = 4
  ldaf = 4

  ! =========================================================
  ! Test 1: upper triangle, 3x3 SPD matrix with Cholesky factor
  ! A (upper triangle stored):
  !   [ 4   2  -2 ]
  !   [ .   5   1 ]
  !   [ .   .  14 ]
  ! AF = chol(A) (upper triangle):
  !   [ 2   1  -1 ]
  !   [ .   2   1 ]
  !   [ .   .   3 ] (approximately, hand-computed: not exact but good for testing)
  ! We use dpotrf to get exact AF
  ! =========================================================
  ncols = 3
  a = 0.0d0
  a(1,1) = 4.0d0
  a(1,2) = 2.0d0
  a(1,3) = -2.0d0
  a(2,2) = 5.0d0
  a(2,3) = 1.0d0
  a(3,3) = 14.0d0

  ! Compute Cholesky factorization via dpotrf
  af = a
  call dpotrf('U', ncols, af, ldaf, info)

  work = 0.0d0
  result = dla_porpvgrw('U', ncols, a, lda, af, ldaf, work)
  call begin_test('upper_3x3')
  call print_scalar('result', result)
  call print_array('work', work, 2*ncols)
  call end_test()

  ! =========================================================
  ! Test 2: lower triangle, 3x3 SPD matrix with Cholesky factor
  ! Same matrix but stored in lower triangle
  ! =========================================================
  ncols = 3
  a = 0.0d0
  a(1,1) = 4.0d0
  a(2,1) = 2.0d0
  a(3,1) = -2.0d0
  a(2,2) = 5.0d0
  a(3,2) = 1.0d0
  a(3,3) = 14.0d0

  af = a
  call dpotrf('L', ncols, af, ldaf, info)

  work = 0.0d0
  result = dla_porpvgrw('L', ncols, a, lda, af, ldaf, work)
  call begin_test('lower_3x3')
  call print_scalar('result', result)
  call print_array('work', work, 2*ncols)
  call end_test()

  ! =========================================================
  ! Test 3: ncols=0 edge case — should return 1.0
  ! =========================================================
  ncols = 0
  work = 0.0d0
  result = dla_porpvgrw('U', ncols, a, lda, af, ldaf, work)
  call begin_test('ncols_0')
  call print_scalar('result', result)
  call end_test()

  ! =========================================================
  ! Test 4: ncols=1 edge case, upper
  ! =========================================================
  ncols = 1
  a = 0.0d0
  a(1,1) = 9.0d0
  af = 0.0d0
  af(1,1) = 3.0d0  ! sqrt(9) = 3

  work = 0.0d0
  result = dla_porpvgrw('U', ncols, a, lda, af, ldaf, work)
  call begin_test('ncols_1_upper')
  call print_scalar('result', result)
  call print_array('work', work, 2*ncols)
  call end_test()

  ! =========================================================
  ! Test 5: ncols=1 edge case, lower
  ! =========================================================
  ncols = 1
  a = 0.0d0
  a(1,1) = 9.0d0
  af = 0.0d0
  af(1,1) = 3.0d0

  work = 0.0d0
  result = dla_porpvgrw('L', ncols, a, lda, af, ldaf, work)
  call begin_test('ncols_1_lower')
  call print_scalar('result', result)
  call print_array('work', work, 2*ncols)
  call end_test()

  ! =========================================================
  ! Test 6: upper, 4x4 larger matrix
  ! =========================================================
  ncols = 4
  a = 0.0d0
  a(1,1) = 10.0d0
  a(1,2) = 1.0d0
  a(1,3) = 2.0d0
  a(1,4) = 0.5d0
  a(2,2) = 10.0d0
  a(2,3) = 1.0d0
  a(2,4) = 1.5d0
  a(3,3) = 10.0d0
  a(3,4) = 2.0d0
  a(4,4) = 10.0d0

  af = a
  call dpotrf('U', ncols, af, ldaf, info)

  work = 0.0d0
  result = dla_porpvgrw('U', ncols, a, lda, af, ldaf, work)
  call begin_test('upper_4x4')
  call print_scalar('result', result)
  call print_array('work', work, 2*ncols)
  call end_test()

  ! =========================================================
  ! Test 7: lower, 4x4 larger matrix
  ! =========================================================
  ncols = 4
  a = 0.0d0
  a(1,1) = 10.0d0
  a(2,1) = 1.0d0
  a(3,1) = 2.0d0
  a(4,1) = 0.5d0
  a(2,2) = 10.0d0
  a(3,2) = 1.0d0
  a(4,2) = 1.5d0
  a(3,3) = 10.0d0
  a(4,3) = 2.0d0
  a(4,4) = 10.0d0

  af = a
  call dpotrf('L', ncols, af, ldaf, info)

  work = 0.0d0
  result = dla_porpvgrw('L', ncols, a, lda, af, ldaf, work)
  call begin_test('lower_4x4')
  call print_scalar('result', result)
  call print_array('work', work, 2*ncols)
  call end_test()

  ! =========================================================
  ! Test 8: upper, zero column in AF (umax=0 path)
  ! AF has a zero column so the UMAX /= 0 check is exercised
  ! =========================================================
  ncols = 3
  a = 0.0d0
  a(1,1) = 4.0d0
  a(1,2) = 2.0d0
  a(1,3) = 1.0d0
  a(2,2) = 5.0d0
  a(2,3) = 3.0d0
  a(3,3) = 7.0d0

  af = 0.0d0
  af(1,1) = 2.0d0
  af(1,2) = 1.0d0
  af(1,3) = 0.5d0
  af(2,2) = 0.0d0   ! zero column 2 in AF (upper tri)
  af(2,3) = 0.0d0
  af(3,3) = 2.5d0

  work = 0.0d0
  result = dla_porpvgrw('U', ncols, a, lda, af, ldaf, work)
  call begin_test('upper_zero_col')
  call print_scalar('result', result)
  call print_array('work', work, 2*ncols)
  call end_test()

  ! =========================================================
  ! Test 9: upper, manual A and AF with rpvgrw < 1
  ! A has small elements, AF has large elements to get ratio < 1
  ! =========================================================
  ncols = 3
  a = 0.0d0
  a(1,1) = 2.0d0
  a(1,2) = 1.0d0
  a(1,3) = 0.5d0
  a(2,2) = 3.0d0
  a(2,3) = 1.0d0
  a(3,3) = 4.0d0

  af = 0.0d0
  af(1,1) = 4.0d0
  af(1,2) = 2.0d0
  af(1,3) = 1.0d0
  af(2,2) = 6.0d0
  af(2,3) = 3.0d0
  af(3,3) = 8.0d0

  work = 0.0d0
  result = dla_porpvgrw('U', ncols, a, lda, af, ldaf, work)
  call begin_test('upper_rpvgrw_lt1')
  call print_scalar('result', result)
  call print_array('work', work, 2*ncols)
  call end_test()

  ! =========================================================
  ! Test 10: lower, manual A and AF with rpvgrw < 1
  ! Same values but stored in lower triangle
  ! =========================================================
  ncols = 3
  a = 0.0d0
  a(1,1) = 2.0d0
  a(2,1) = 1.0d0
  a(3,1) = 0.5d0
  a(2,2) = 3.0d0
  a(3,2) = 1.0d0
  a(3,3) = 4.0d0

  af = 0.0d0
  af(1,1) = 4.0d0
  af(2,1) = 2.0d0
  af(3,1) = 1.0d0
  af(2,2) = 6.0d0
  af(3,2) = 3.0d0
  af(3,3) = 8.0d0

  work = 0.0d0
  result = dla_porpvgrw('L', ncols, a, lda, af, ldaf, work)
  call begin_test('lower_rpvgrw_lt1')
  call print_scalar('result', result)
  call print_array('work', work, 2*ncols)
  call end_test()

  ! =========================================================
  ! Test 11: lower, zero column in AF
  ! =========================================================
  ncols = 3
  a = 0.0d0
  a(1,1) = 4.0d0
  a(2,1) = 2.0d0
  a(3,1) = 1.0d0
  a(2,2) = 5.0d0
  a(3,2) = 3.0d0
  a(3,3) = 7.0d0

  af = 0.0d0
  af(1,1) = 2.0d0
  af(2,1) = 1.0d0
  af(3,1) = 0.5d0
  af(2,2) = 0.0d0
  af(3,2) = 0.0d0
  af(3,3) = 2.5d0

  work = 0.0d0
  result = dla_porpvgrw('L', ncols, a, lda, af, ldaf, work)
  call begin_test('lower_zero_col')
  call print_scalar('result', result)
  call print_array('work', work, 2*ncols)
  call end_test()

end program
