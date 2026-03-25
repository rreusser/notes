program test_zgbcon
  use test_utils
  implicit none
  double precision :: ab_r(240), work_r(400), rwork(20)
  complex*16 :: ab(6, 10), work(200)
  equivalence (ab, ab_r)
  equivalence (work, work_r)
  integer :: ipiv(10), info, n, kl, ku
  double precision :: anorm, rcond

  ! ============================================================
  ! Test 1: 4x4 complex tridiag (KL=1, KU=1), 1-norm
  ! LDAB = 2*KL+KU+1 = 4
  n = 4; kl = 1; ku = 1
  ab = (0.0d0, 0.0d0)
  ! Row 1=fill, Row 2=super(ku=1), Row 3=main, Row 4=sub(1)
  ab(3,1) = (4.0d0,1.0d0); ab(4,1) = (-1.0d0,0.0d0)
  ab(2,2) = (-1.0d0,0.0d0); ab(3,2) = (4.0d0,1.0d0); ab(4,2) = (-1.0d0,0.0d0)
  ab(2,3) = (-1.0d0,0.0d0); ab(3,3) = (4.0d0,1.0d0); ab(4,3) = (-1.0d0,0.0d0)
  ab(2,4) = (-1.0d0,0.0d0); ab(3,4) = (4.0d0,1.0d0)
  anorm = 7.0d0
  call ZGBTRF(n, n, kl, ku, ab, 6, ipiv, info)
  call ZGBCON('1', n, kl, ku, ab, 6, ipiv, anorm, rcond, work, rwork, info)
  call begin_test('tridiag_1norm')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 2: Same matrix, infinity-norm
  ab = (0.0d0, 0.0d0)
  ab(3,1) = (4.0d0,1.0d0); ab(4,1) = (-1.0d0,0.0d0)
  ab(2,2) = (-1.0d0,0.0d0); ab(3,2) = (4.0d0,1.0d0); ab(4,2) = (-1.0d0,0.0d0)
  ab(2,3) = (-1.0d0,0.0d0); ab(3,3) = (4.0d0,1.0d0); ab(4,3) = (-1.0d0,0.0d0)
  ab(2,4) = (-1.0d0,0.0d0); ab(3,4) = (4.0d0,1.0d0)
  anorm = 7.0d0
  call ZGBTRF(n, n, kl, ku, ab, 6, ipiv, info)
  call ZGBCON('I', n, kl, ku, ab, 6, ipiv, anorm, rcond, work, rwork, info)
  call begin_test('tridiag_Inorm')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 3: 4x4 with KL=2, KU=1, LDAB=6
  n = 4; kl = 2; ku = 1
  ab = (0.0d0, 0.0d0)
  ab(4,1) = (5.0d0,1.0d0); ab(5,1) = (2.0d0,0.5d0); ab(6,1) = (1.0d0,0.0d0)
  ab(3,2) = (1.0d0,0.0d0); ab(4,2) = (6.0d0,1.0d0); ab(5,2) = (1.0d0,0.5d0); ab(6,2) = (2.0d0,1.0d0)
  ab(3,3) = (2.0d0,0.5d0); ab(4,3) = (7.0d0,2.0d0); ab(5,3) = (3.0d0,0.0d0)
  ab(3,4) = (1.0d0,1.0d0); ab(4,4) = (8.0d0,1.0d0)
  anorm = 14.5d0
  call ZGBTRF(n, n, kl, ku, ab, 6, ipiv, info)
  call ZGBCON('1', n, kl, ku, ab, 6, ipiv, anorm, rcond, work, rwork, info)
  call begin_test('kl2_ku1_1norm')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 4: N=0 (rcond = 1)
  n = 0; kl = 0; ku = 0; anorm = 0.0d0
  call ZGBCON('1', n, kl, ku, ab, 6, ipiv, anorm, rcond, work, rwork, info)
  call begin_test('n_zero')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 5: N=1
  n = 1; kl = 0; ku = 0
  ab = (0.0d0, 0.0d0)
  ab(1,1) = (3.0d0, 1.0d0)
  anorm = 4.0d0
  call ZGBTRF(n, n, kl, ku, ab, 6, ipiv, info)
  call ZGBCON('1', n, kl, ku, ab, 6, ipiv, anorm, rcond, work, rwork, info)
  call begin_test('n_one')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 6: anorm = 0 (rcond = 0)
  n = 4; kl = 1; ku = 1
  ab = (0.0d0, 0.0d0)
  ab(3,1) = (4.0d0,1.0d0); ab(4,1) = (-1.0d0,0.0d0)
  ab(2,2) = (-1.0d0,0.0d0); ab(3,2) = (4.0d0,1.0d0); ab(4,2) = (-1.0d0,0.0d0)
  ab(2,3) = (-1.0d0,0.0d0); ab(3,3) = (4.0d0,1.0d0); ab(4,3) = (-1.0d0,0.0d0)
  ab(2,4) = (-1.0d0,0.0d0); ab(3,4) = (4.0d0,1.0d0)
  call ZGBTRF(n, n, kl, ku, ab, 6, ipiv, info)
  anorm = 0.0d0
  call ZGBCON('1', n, kl, ku, ab, 6, ipiv, anorm, rcond, work, rwork, info)
  call begin_test('anorm_zero')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

end program
