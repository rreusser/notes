program test_zgtcon
  use test_utils
  implicit none
  double precision :: dl_r(20), d_r(20), du_r(20), du2_r(20), work_r(200)
  complex*16 :: dl(10), d(10), du(10), du2(10), work(100)
  equivalence (dl, dl_r)
  equivalence (d, d_r)
  equivalence (du, du_r)
  equivalence (du2, du2_r)
  equivalence (work, work_r)
  integer :: ipiv(10), info, n
  double precision :: anorm, rcond

  ! ============================================================
  ! Test 1: 5x5 well-conditioned complex tridiag, 1-norm
  n = 5
  dl(1) = (-1.0d0, 0.0d0); dl(2) = (-1.0d0, 0.0d0)
  dl(3) = (-1.0d0, 0.0d0); dl(4) = (-1.0d0, 0.0d0)
  d(1) = (4.0d0, 1.0d0); d(2) = (4.0d0, 1.0d0)
  d(3) = (4.0d0, 1.0d0); d(4) = (4.0d0, 1.0d0); d(5) = (4.0d0, 1.0d0)
  du(1) = (-1.0d0, 0.0d0); du(2) = (-1.0d0, 0.0d0)
  du(3) = (-1.0d0, 0.0d0); du(4) = (-1.0d0, 0.0d0)
  ! 1-norm: max col sum (cabs1)
  ! Col 1: |4+i| + |-1| = 5 + 1 = 6
  ! Col 2: |-1| + |4+i| + |-1| = 1 + 5 + 1 = 7
  ! Col 3: same = 7, Col 4: same = 7, Col 5: 1+5 = 6
  anorm = 7.0d0

  call ZGTTRF(n, dl, d, du, du2, ipiv, info)
  call ZGTCON('1', n, dl, d, du, du2, ipiv, anorm, rcond, work, info)
  call begin_test('tridiag_1norm')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 2: Same matrix, infinity-norm
  dl(1) = (-1.0d0, 0.0d0); dl(2) = (-1.0d0, 0.0d0)
  dl(3) = (-1.0d0, 0.0d0); dl(4) = (-1.0d0, 0.0d0)
  d(1) = (4.0d0, 1.0d0); d(2) = (4.0d0, 1.0d0)
  d(3) = (4.0d0, 1.0d0); d(4) = (4.0d0, 1.0d0); d(5) = (4.0d0, 1.0d0)
  du(1) = (-1.0d0, 0.0d0); du(2) = (-1.0d0, 0.0d0)
  du(3) = (-1.0d0, 0.0d0); du(4) = (-1.0d0, 0.0d0)
  anorm = 7.0d0

  call ZGTTRF(n, dl, d, du, du2, ipiv, info)
  call ZGTCON('I', n, dl, d, du, du2, ipiv, anorm, rcond, work, info)
  call begin_test('tridiag_Inorm')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 3: N=0 (rcond = 1)
  n = 0
  anorm = 0.0d0
  call ZGTCON('1', n, dl, d, du, du2, ipiv, anorm, rcond, work, info)
  call begin_test('n_zero')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 4: N=1
  n = 1
  d(1) = (3.0d0, 1.0d0)
  anorm = 4.0d0  ! cabs1(3+i) = 4
  call ZGTTRF(n, dl, d, du, du2, ipiv, info)
  call ZGTCON('1', n, dl, d, du, du2, ipiv, anorm, rcond, work, info)
  call begin_test('n_one')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 5: anorm = 0 (rcond = 0)
  n = 5
  dl(1) = (-1.0d0, 0.0d0); dl(2) = (-1.0d0, 0.0d0)
  dl(3) = (-1.0d0, 0.0d0); dl(4) = (-1.0d0, 0.0d0)
  d(1) = (4.0d0, 1.0d0); d(2) = (4.0d0, 1.0d0)
  d(3) = (4.0d0, 1.0d0); d(4) = (4.0d0, 1.0d0); d(5) = (4.0d0, 1.0d0)
  du(1) = (-1.0d0, 0.0d0); du(2) = (-1.0d0, 0.0d0)
  du(3) = (-1.0d0, 0.0d0); du(4) = (-1.0d0, 0.0d0)
  call ZGTTRF(n, dl, d, du, du2, ipiv, info)
  anorm = 0.0d0
  call ZGTCON('1', n, dl, d, du, du2, ipiv, anorm, rcond, work, info)
  call begin_test('anorm_zero')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 6: 4x4 with non-trivial complex entries
  n = 4
  dl(1) = (2.0d0, 1.0d0); dl(2) = (1.0d0, 3.0d0); dl(3) = (0.5d0, 0.5d0)
  d(1) = (5.0d0, 2.0d0); d(2) = (6.0d0, 1.0d0)
  d(3) = (7.0d0, 3.0d0); d(4) = (4.0d0, 2.0d0)
  du(1) = (1.0d0, 0.5d0); du(2) = (2.0d0, 1.0d0); du(3) = (1.5d0, 0.5d0)
  ! 1-norm: max col sum by cabs1
  ! Col 1: cabs1(5+2i) + cabs1(2+i) = 7 + 3 = 10
  ! Col 2: cabs1(1+0.5i) + cabs1(6+i) + cabs1(1+3i) = 1.5+7+4 = 12.5
  ! Col 3: cabs1(2+i) + cabs1(7+3i) + cabs1(0.5+0.5i) = 3+10+1 = 14
  ! Col 4: cabs1(1.5+0.5i) + cabs1(4+2i) = 2+6 = 8
  anorm = 14.0d0
  call ZGTTRF(n, dl, d, du, du2, ipiv, info)
  call ZGTCON('1', n, dl, d, du, du2, ipiv, anorm, rcond, work, info)
  call begin_test('complex_4x4_1norm')
  call print_scalar('anorm', anorm)
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

end program
