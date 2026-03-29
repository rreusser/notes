program test_zla_porpvgrw
  use test_utils
  implicit none
  integer, parameter :: NMAX = 4
  complex*16 :: a(NMAX, NMAX), af(NMAX, NMAX)
  double precision :: work(2*NMAX)
  double precision :: result
  double precision :: zla_porpvgrw
  external :: zla_porpvgrw
  integer :: ncols, lda, ldaf, info

  lda = NMAX
  ldaf = NMAX

  ! =========================================================
  ! Test 1: upper triangle, 3x3 Hermitian PD matrix
  ! A (upper triangle stored):
  !   [ 4+0i   2+1i  -2+0.5i ]
  !   [  .     5+0i   1+0.3i  ]
  !   [  .      .    14+0i    ]
  ! AF = chol(A) (upper triangle) via zpotrf
  ! =========================================================
  ncols = 3
  a = (0.0d0, 0.0d0)
  a(1,1) = (4.0d0, 0.0d0)
  a(1,2) = (2.0d0, 1.0d0)
  a(1,3) = (-2.0d0, 0.5d0)
  a(2,2) = (5.0d0, 0.0d0)
  a(2,3) = (1.0d0, 0.3d0)
  a(3,3) = (14.0d0, 0.0d0)

  af = a
  call zpotrf('U', ncols, af, ldaf, info)

  work = 0.0d0
  result = zla_porpvgrw('U', ncols, a, lda, af, ldaf, work)
  call begin_test('upper_3x3')
  call print_scalar('result', result)
  call print_array('work', work, 2*ncols)
  call end_test()

  ! =========================================================
  ! Test 2: lower triangle, 3x3 Hermitian PD matrix
  ! Same matrix stored in lower triangle (conjugate-transposed)
  ! =========================================================
  ncols = 3
  a = (0.0d0, 0.0d0)
  a(1,1) = (4.0d0, 0.0d0)
  a(2,1) = (2.0d0, -1.0d0)
  a(3,1) = (-2.0d0, -0.5d0)
  a(2,2) = (5.0d0, 0.0d0)
  a(3,2) = (1.0d0, -0.3d0)
  a(3,3) = (14.0d0, 0.0d0)

  af = a
  call zpotrf('L', ncols, af, ldaf, info)

  work = 0.0d0
  result = zla_porpvgrw('L', ncols, a, lda, af, ldaf, work)
  call begin_test('lower_3x3')
  call print_scalar('result', result)
  call print_array('work', work, 2*ncols)
  call end_test()

  ! =========================================================
  ! Test 3: ncols=0 edge case — should return 1.0
  ! =========================================================
  ncols = 0
  work = 0.0d0
  result = zla_porpvgrw('U', ncols, a, lda, af, ldaf, work)
  call begin_test('ncols_0')
  call print_scalar('result', result)
  call end_test()

  ! =========================================================
  ! Test 4: ncols=1 edge case, upper
  ! =========================================================
  ncols = 1
  a = (0.0d0, 0.0d0)
  a(1,1) = (9.0d0, 0.0d0)
  af = (0.0d0, 0.0d0)
  af(1,1) = (3.0d0, 0.0d0)

  work = 0.0d0
  result = zla_porpvgrw('U', ncols, a, lda, af, ldaf, work)
  call begin_test('ncols_1_upper')
  call print_scalar('result', result)
  call print_array('work', work, 2*ncols)
  call end_test()

  ! =========================================================
  ! Test 5: ncols=1 edge case, lower
  ! =========================================================
  ncols = 1
  a = (0.0d0, 0.0d0)
  a(1,1) = (9.0d0, 0.0d0)
  af = (0.0d0, 0.0d0)
  af(1,1) = (3.0d0, 0.0d0)

  work = 0.0d0
  result = zla_porpvgrw('L', ncols, a, lda, af, ldaf, work)
  call begin_test('ncols_1_lower')
  call print_scalar('result', result)
  call print_array('work', work, 2*ncols)
  call end_test()

  ! =========================================================
  ! Test 6: upper, 4x4 larger Hermitian PD matrix
  ! =========================================================
  ncols = 4
  a = (0.0d0, 0.0d0)
  a(1,1) = (10.0d0, 0.0d0)
  a(1,2) = (1.0d0, 0.5d0)
  a(1,3) = (2.0d0, -0.3d0)
  a(1,4) = (0.5d0, 0.1d0)
  a(2,2) = (10.0d0, 0.0d0)
  a(2,3) = (1.0d0, 0.2d0)
  a(2,4) = (1.5d0, -0.4d0)
  a(3,3) = (10.0d0, 0.0d0)
  a(3,4) = (2.0d0, 0.7d0)
  a(4,4) = (10.0d0, 0.0d0)

  af = a
  call zpotrf('U', ncols, af, ldaf, info)

  work = 0.0d0
  result = zla_porpvgrw('U', ncols, a, lda, af, ldaf, work)
  call begin_test('upper_4x4')
  call print_scalar('result', result)
  call print_array('work', work, 2*ncols)
  call end_test()

  ! =========================================================
  ! Test 7: lower, 4x4 larger Hermitian PD matrix
  ! Same matrix but stored in lower triangle
  ! =========================================================
  ncols = 4
  a = (0.0d0, 0.0d0)
  a(1,1) = (10.0d0, 0.0d0)
  a(2,1) = (1.0d0, -0.5d0)
  a(3,1) = (2.0d0, 0.3d0)
  a(4,1) = (0.5d0, -0.1d0)
  a(2,2) = (10.0d0, 0.0d0)
  a(3,2) = (1.0d0, -0.2d0)
  a(4,2) = (1.5d0, 0.4d0)
  a(3,3) = (10.0d0, 0.0d0)
  a(4,3) = (2.0d0, -0.7d0)
  a(4,4) = (10.0d0, 0.0d0)

  af = a
  call zpotrf('L', ncols, af, ldaf, info)

  work = 0.0d0
  result = zla_porpvgrw('L', ncols, a, lda, af, ldaf, work)
  call begin_test('lower_4x4')
  call print_scalar('result', result)
  call print_array('work', work, 2*ncols)
  call end_test()

  ! =========================================================
  ! Test 8: upper, zero column in AF (umax=0 path)
  ! AF has a zero column so the UMAX /= 0 check is exercised
  ! =========================================================
  ncols = 3
  a = (0.0d0, 0.0d0)
  a(1,1) = (4.0d0, 0.0d0)
  a(1,2) = (2.0d0, 1.0d0)
  a(1,3) = (1.0d0, 0.5d0)
  a(2,2) = (5.0d0, 0.0d0)
  a(2,3) = (3.0d0, -0.2d0)
  a(3,3) = (7.0d0, 0.0d0)

  af = (0.0d0, 0.0d0)
  af(1,1) = (2.0d0, 0.0d0)
  af(1,2) = (1.0d0, 0.5d0)
  af(1,3) = (0.5d0, 0.3d0)
  af(2,2) = (0.0d0, 0.0d0)   ! zero column 2 in AF (upper tri)
  af(2,3) = (0.0d0, 0.0d0)
  af(3,3) = (2.5d0, 0.0d0)

  work = 0.0d0
  result = zla_porpvgrw('U', ncols, a, lda, af, ldaf, work)
  call begin_test('upper_zero_col')
  call print_scalar('result', result)
  call print_array('work', work, 2*ncols)
  call end_test()

  ! =========================================================
  ! Test 9: upper, rpvgrw < 1 (small A, large AF)
  ! =========================================================
  ncols = 3
  a = (0.0d0, 0.0d0)
  a(1,1) = (2.0d0, 0.0d0)
  a(1,2) = (1.0d0, 0.1d0)
  a(1,3) = (0.5d0, 0.2d0)
  a(2,2) = (3.0d0, 0.0d0)
  a(2,3) = (1.0d0, -0.1d0)
  a(3,3) = (4.0d0, 0.0d0)

  af = (0.0d0, 0.0d0)
  af(1,1) = (4.0d0, 0.0d0)
  af(1,2) = (2.0d0, 0.3d0)
  af(1,3) = (1.0d0, -0.5d0)
  af(2,2) = (6.0d0, 0.0d0)
  af(2,3) = (3.0d0, 0.4d0)
  af(3,3) = (8.0d0, 0.0d0)

  work = 0.0d0
  result = zla_porpvgrw('U', ncols, a, lda, af, ldaf, work)
  call begin_test('upper_rpvgrw_lt1')
  call print_scalar('result', result)
  call print_array('work', work, 2*ncols)
  call end_test()

  ! =========================================================
  ! Test 10: lower, rpvgrw < 1
  ! =========================================================
  ncols = 3
  a = (0.0d0, 0.0d0)
  a(1,1) = (2.0d0, 0.0d0)
  a(2,1) = (1.0d0, -0.1d0)
  a(3,1) = (0.5d0, -0.2d0)
  a(2,2) = (3.0d0, 0.0d0)
  a(3,2) = (1.0d0, 0.1d0)
  a(3,3) = (4.0d0, 0.0d0)

  af = (0.0d0, 0.0d0)
  af(1,1) = (4.0d0, 0.0d0)
  af(2,1) = (2.0d0, -0.3d0)
  af(3,1) = (1.0d0, 0.5d0)
  af(2,2) = (6.0d0, 0.0d0)
  af(3,2) = (3.0d0, -0.4d0)
  af(3,3) = (8.0d0, 0.0d0)

  work = 0.0d0
  result = zla_porpvgrw('L', ncols, a, lda, af, ldaf, work)
  call begin_test('lower_rpvgrw_lt1')
  call print_scalar('result', result)
  call print_array('work', work, 2*ncols)
  call end_test()

  ! =========================================================
  ! Test 11: lower, zero column in AF
  ! =========================================================
  ncols = 3
  a = (0.0d0, 0.0d0)
  a(1,1) = (4.0d0, 0.0d0)
  a(2,1) = (2.0d0, -1.0d0)
  a(3,1) = (1.0d0, -0.5d0)
  a(2,2) = (5.0d0, 0.0d0)
  a(3,2) = (3.0d0, 0.2d0)
  a(3,3) = (7.0d0, 0.0d0)

  af = (0.0d0, 0.0d0)
  af(1,1) = (2.0d0, 0.0d0)
  af(2,1) = (1.0d0, -0.5d0)
  af(3,1) = (0.5d0, 0.3d0)
  af(2,2) = (0.0d0, 0.0d0)
  af(3,2) = (0.0d0, 0.0d0)
  af(3,3) = (2.5d0, 0.0d0)

  work = 0.0d0
  result = zla_porpvgrw('L', ncols, a, lda, af, ldaf, work)
  call begin_test('lower_zero_col')
  call print_scalar('result', result)
  call print_array('work', work, 2*ncols)
  call end_test()

end program
