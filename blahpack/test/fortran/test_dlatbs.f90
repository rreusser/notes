program test_dlatbs
  use test_utils
  implicit none
  ! For banded storage: AB(LDAB, N) where LDAB >= KD+1
  double precision :: ab3(3,10), ab2(2,10), ab1(1,10)
  double precision :: x(10), cnorm(10), scale
  integer :: info, n, kd

  ! ========================================================================
  ! Test 1: Upper triangular banded, no transpose, non-unit, 4x4, KD=2
  ! ========================================================================
  n = 4
  kd = 2
  ab3 = 0.0d0
  ab3(3,1) = 4.0d0
  ab3(2,2) = 2.0d0; ab3(3,2) = 3.0d0
  ab3(1,3) = 1.0d0; ab3(2,3) = 1.0d0; ab3(3,3) = 5.0d0
  ab3(1,4) = 2.0d0; ab3(2,4) = 3.0d0; ab3(3,4) = 6.0d0

  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0; x(4) = 4.0d0
  scale = 0.0d0
  cnorm = 0.0d0
  call dlatbs('U', 'N', 'N', 'N', n, kd, ab3, kd+1, x, scale, cnorm, info)
  call begin_test('upper_N_nonunit')
  call print_array('x', x, n)
  call print_scalar('scale', scale)
  call print_array('cnorm', cnorm, n)
  call print_int('info', info)
  call end_test()

  ! ========================================================================
  ! Test 2: Lower triangular banded, no transpose, non-unit, 4x4, KD=2
  ! ========================================================================
  n = 4
  kd = 2
  ab3 = 0.0d0
  ab3(1,1) = 4.0d0; ab3(2,1) = 2.0d0; ab3(3,1) = 1.0d0
  ab3(1,2) = 3.0d0; ab3(2,2) = 1.0d0; ab3(3,2) = 2.0d0
  ab3(1,3) = 5.0d0; ab3(2,3) = 3.0d0
  ab3(1,4) = 6.0d0

  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0; x(4) = 4.0d0
  scale = 0.0d0
  cnorm = 0.0d0
  call dlatbs('L', 'N', 'N', 'N', n, kd, ab3, kd+1, x, scale, cnorm, info)
  call begin_test('lower_N_nonunit')
  call print_array('x', x, n)
  call print_scalar('scale', scale)
  call print_array('cnorm', cnorm, n)
  call print_int('info', info)
  call end_test()

  ! ========================================================================
  ! Test 3: Upper triangular banded, transpose, non-unit, 4x4, KD=2
  ! ========================================================================
  n = 4
  kd = 2
  ab3 = 0.0d0
  ab3(3,1) = 4.0d0
  ab3(2,2) = 2.0d0; ab3(3,2) = 3.0d0
  ab3(1,3) = 1.0d0; ab3(2,3) = 1.0d0; ab3(3,3) = 5.0d0
  ab3(1,4) = 2.0d0; ab3(2,4) = 3.0d0; ab3(3,4) = 6.0d0

  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0; x(4) = 4.0d0
  scale = 0.0d0
  cnorm = 0.0d0
  call dlatbs('U', 'T', 'N', 'N', n, kd, ab3, kd+1, x, scale, cnorm, info)
  call begin_test('upper_T_nonunit')
  call print_array('x', x, n)
  call print_scalar('scale', scale)
  call print_array('cnorm', cnorm, n)
  call print_int('info', info)
  call end_test()

  ! ========================================================================
  ! Test 4: Lower triangular banded, transpose, non-unit, 4x4, KD=2
  ! ========================================================================
  n = 4
  kd = 2
  ab3 = 0.0d0
  ab3(1,1) = 4.0d0; ab3(2,1) = 2.0d0; ab3(3,1) = 1.0d0
  ab3(1,2) = 3.0d0; ab3(2,2) = 1.0d0; ab3(3,2) = 2.0d0
  ab3(1,3) = 5.0d0; ab3(2,3) = 3.0d0
  ab3(1,4) = 6.0d0

  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0; x(4) = 4.0d0
  scale = 0.0d0
  cnorm = 0.0d0
  call dlatbs('L', 'T', 'N', 'N', n, kd, ab3, kd+1, x, scale, cnorm, info)
  call begin_test('lower_T_nonunit')
  call print_array('x', x, n)
  call print_scalar('scale', scale)
  call print_array('cnorm', cnorm, n)
  call print_int('info', info)
  call end_test()

  ! ========================================================================
  ! Test 5: Upper, unit diagonal, no transpose
  ! ========================================================================
  n = 4
  kd = 2
  ab3 = 0.0d0
  ab3(3,1) = 99.0d0
  ab3(2,2) = 2.0d0; ab3(3,2) = 99.0d0
  ab3(1,3) = 1.0d0; ab3(2,3) = 1.0d0; ab3(3,3) = 99.0d0
  ab3(1,4) = 2.0d0; ab3(2,4) = 3.0d0; ab3(3,4) = 99.0d0

  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0; x(4) = 4.0d0
  scale = 0.0d0
  cnorm = 0.0d0
  call dlatbs('U', 'N', 'U', 'N', n, kd, ab3, kd+1, x, scale, cnorm, info)
  call begin_test('upper_N_unit')
  call print_array('x', x, n)
  call print_scalar('scale', scale)
  call print_array('cnorm', cnorm, n)
  call print_int('info', info)
  call end_test()

  ! ========================================================================
  ! Test 6: Lower, unit diagonal, no transpose
  ! ========================================================================
  n = 4
  kd = 2
  ab3 = 0.0d0
  ab3(1,1) = 99.0d0; ab3(2,1) = 2.0d0; ab3(3,1) = 1.0d0
  ab3(1,2) = 99.0d0; ab3(2,2) = 1.0d0; ab3(3,2) = 2.0d0
  ab3(1,3) = 99.0d0; ab3(2,3) = 3.0d0
  ab3(1,4) = 99.0d0

  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0; x(4) = 4.0d0
  scale = 0.0d0
  cnorm = 0.0d0
  call dlatbs('L', 'N', 'U', 'N', n, kd, ab3, kd+1, x, scale, cnorm, info)
  call begin_test('lower_N_unit')
  call print_array('x', x, n)
  call print_scalar('scale', scale)
  call print_array('cnorm', cnorm, n)
  call print_int('info', info)
  call end_test()

  ! ========================================================================
  ! Test 7: N=0 (quick return)
  ! ========================================================================
  n = 0
  kd = 0
  scale = 0.0d0
  call dlatbs('U', 'N', 'N', 'N', n, kd, ab1, 1, x, scale, cnorm, info)
  call begin_test('n_zero')
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! ========================================================================
  ! Test 8: N=1
  ! ========================================================================
  n = 1
  kd = 0
  ab1(1,1) = 5.0d0
  x(1) = 10.0d0
  scale = 0.0d0
  cnorm = 0.0d0
  call dlatbs('U', 'N', 'N', 'N', n, kd, ab1, kd+1, x, scale, cnorm, info)
  call begin_test('n_one')
  call print_array('x', x, n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! ========================================================================
  ! Test 9: Pre-computed CNORM (normin='Y')
  ! ========================================================================
  n = 4
  kd = 2
  ab3 = 0.0d0
  ab3(3,1) = 4.0d0
  ab3(2,2) = 2.0d0; ab3(3,2) = 3.0d0
  ab3(1,3) = 1.0d0; ab3(2,3) = 1.0d0; ab3(3,3) = 5.0d0
  ab3(1,4) = 2.0d0; ab3(2,4) = 3.0d0; ab3(3,4) = 6.0d0

  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0; x(4) = 4.0d0
  scale = 0.0d0
  cnorm(1) = 0.0d0; cnorm(2) = 2.0d0; cnorm(3) = 2.0d0; cnorm(4) = 5.0d0
  call dlatbs('U', 'N', 'N', 'Y', n, kd, ab3, kd+1, x, scale, cnorm, info)
  call begin_test('normin_Y')
  call print_array('x', x, n)
  call print_scalar('scale', scale)
  call print_array('cnorm', cnorm, n)
  call print_int('info', info)
  call end_test()

  ! ========================================================================
  ! Test 10: KD=1 (tridiagonal band), upper
  ! ========================================================================
  n = 4
  kd = 1
  ab2 = 0.0d0
  ab2(2,1) = 3.0d0
  ab2(1,2) = 1.0d0; ab2(2,2) = 4.0d0
  ab2(1,3) = 2.0d0; ab2(2,3) = 5.0d0
  ab2(1,4) = 1.0d0; ab2(2,4) = 6.0d0

  x(1) = 2.0d0; x(2) = 3.0d0; x(3) = 1.0d0; x(4) = 5.0d0
  scale = 0.0d0
  cnorm = 0.0d0
  call dlatbs('U', 'N', 'N', 'N', n, kd, ab2, kd+1, x, scale, cnorm, info)
  call begin_test('upper_kd1')
  call print_array('x', x, n)
  call print_scalar('scale', scale)
  call print_array('cnorm', cnorm, n)
  call print_int('info', info)
  call end_test()

  ! ========================================================================
  ! Test 11: Upper, transpose, unit diagonal
  ! ========================================================================
  n = 4
  kd = 2
  ab3 = 0.0d0
  ab3(3,1) = 99.0d0
  ab3(2,2) = 2.0d0; ab3(3,2) = 99.0d0
  ab3(1,3) = 1.0d0; ab3(2,3) = 1.0d0; ab3(3,3) = 99.0d0
  ab3(1,4) = 2.0d0; ab3(2,4) = 3.0d0; ab3(3,4) = 99.0d0

  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0; x(4) = 4.0d0
  scale = 0.0d0
  cnorm = 0.0d0
  call dlatbs('U', 'T', 'U', 'N', n, kd, ab3, kd+1, x, scale, cnorm, info)
  call begin_test('upper_T_unit')
  call print_array('x', x, n)
  call print_scalar('scale', scale)
  call print_array('cnorm', cnorm, n)
  call print_int('info', info)
  call end_test()

  ! ========================================================================
  ! Test 12: Lower, transpose, unit diagonal
  ! ========================================================================
  n = 4
  kd = 2
  ab3 = 0.0d0
  ab3(1,1) = 99.0d0; ab3(2,1) = 2.0d0; ab3(3,1) = 1.0d0
  ab3(1,2) = 99.0d0; ab3(2,2) = 1.0d0; ab3(3,2) = 2.0d0
  ab3(1,3) = 99.0d0; ab3(2,3) = 3.0d0
  ab3(1,4) = 99.0d0

  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0; x(4) = 4.0d0
  scale = 0.0d0
  cnorm = 0.0d0
  call dlatbs('L', 'T', 'U', 'N', n, kd, ab3, kd+1, x, scale, cnorm, info)
  call begin_test('lower_T_unit')
  call print_array('x', x, n)
  call print_scalar('scale', scale)
  call print_array('cnorm', cnorm, n)
  call print_int('info', info)
  call end_test()

  ! ========================================================================
  ! Test 13: Lower, transpose, non-unit, normin='Y'
  ! ========================================================================
  n = 4
  kd = 2
  ab3 = 0.0d0
  ab3(1,1) = 4.0d0; ab3(2,1) = 2.0d0; ab3(3,1) = 1.0d0
  ab3(1,2) = 3.0d0; ab3(2,2) = 1.0d0; ab3(3,2) = 2.0d0
  ab3(1,3) = 5.0d0; ab3(2,3) = 3.0d0
  ab3(1,4) = 6.0d0

  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0; x(4) = 4.0d0
  scale = 0.0d0
  cnorm(1) = 3.0d0; cnorm(2) = 3.0d0; cnorm(3) = 3.0d0; cnorm(4) = 0.0d0
  call dlatbs('L', 'T', 'N', 'Y', n, kd, ab3, kd+1, x, scale, cnorm, info)
  call begin_test('lower_T_nonunit_normin_Y')
  call print_array('x', x, n)
  call print_scalar('scale', scale)
  call print_array('cnorm', cnorm, n)
  call print_int('info', info)
  call end_test()

  ! ========================================================================
  ! Test 14: Upper, transpose, non-unit, normin='Y'
  ! ========================================================================
  n = 4
  kd = 2
  ab3 = 0.0d0
  ab3(3,1) = 4.0d0
  ab3(2,2) = 2.0d0; ab3(3,2) = 3.0d0
  ab3(1,3) = 1.0d0; ab3(2,3) = 1.0d0; ab3(3,3) = 5.0d0
  ab3(1,4) = 2.0d0; ab3(2,4) = 3.0d0; ab3(3,4) = 6.0d0

  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0; x(4) = 4.0d0
  scale = 0.0d0
  cnorm(1) = 0.0d0; cnorm(2) = 2.0d0; cnorm(3) = 2.0d0; cnorm(4) = 5.0d0
  call dlatbs('U', 'T', 'N', 'Y', n, kd, ab3, kd+1, x, scale, cnorm, info)
  call begin_test('upper_T_nonunit_normin_Y')
  call print_array('x', x, n)
  call print_scalar('scale', scale)
  call print_array('cnorm', cnorm, n)
  call print_int('info', info)
  call end_test()

  ! ========================================================================
  ! Test 15: Lower, no-transpose, unit, normin='Y'
  ! ========================================================================
  n = 4
  kd = 2
  ab3 = 0.0d0
  ab3(1,1) = 99.0d0; ab3(2,1) = 2.0d0; ab3(3,1) = 1.0d0
  ab3(1,2) = 99.0d0; ab3(2,2) = 1.0d0; ab3(3,2) = 2.0d0
  ab3(1,3) = 99.0d0; ab3(2,3) = 3.0d0
  ab3(1,4) = 99.0d0

  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0; x(4) = 4.0d0
  scale = 0.0d0
  cnorm(1) = 3.0d0; cnorm(2) = 3.0d0; cnorm(3) = 3.0d0; cnorm(4) = 0.0d0
  call dlatbs('L', 'N', 'U', 'Y', n, kd, ab3, kd+1, x, scale, cnorm, info)
  call begin_test('lower_N_unit_normin_Y')
  call print_array('x', x, n)
  call print_scalar('scale', scale)
  call print_array('cnorm', cnorm, n)
  call print_int('info', info)
  call end_test()

  ! ========================================================================
  ! Test 16: Lower, KD=1, transpose, non-unit
  ! ========================================================================
  n = 4
  kd = 1
  ab2 = 0.0d0
  ab2(1,1) = 3.0d0; ab2(2,1) = 1.0d0
  ab2(1,2) = 4.0d0; ab2(2,2) = 2.0d0
  ab2(1,3) = 5.0d0; ab2(2,3) = 1.0d0
  ab2(1,4) = 6.0d0

  x(1) = 2.0d0; x(2) = 3.0d0; x(3) = 1.0d0; x(4) = 5.0d0
  scale = 0.0d0
  cnorm = 0.0d0
  call dlatbs('L', 'T', 'N', 'N', n, kd, ab2, kd+1, x, scale, cnorm, info)
  call begin_test('lower_T_kd1')
  call print_array('x', x, n)
  call print_scalar('scale', scale)
  call print_array('cnorm', cnorm, n)
  call print_int('info', info)
  call end_test()

  ! ========================================================================
  ! Test 17: Singular matrix (zero diagonal), upper
  ! This triggers the careful-solve path with tjj=0
  ! ========================================================================
  n = 3
  kd = 1
  ab2 = 0.0d0
  ab2(2,1) = 2.0d0
  ab2(1,2) = 1.0d0; ab2(2,2) = 0.0d0  ! zero diagonal
  ab2(1,3) = 1.0d0; ab2(2,3) = 3.0d0

  x(1) = 1.0d0; x(2) = 1.0d0; x(3) = 1.0d0
  scale = 0.0d0
  cnorm = 0.0d0
  call dlatbs('U', 'N', 'N', 'N', n, kd, ab2, kd+1, x, scale, cnorm, info)
  call begin_test('singular_upper')
  call print_array('x', x, n)
  call print_scalar('scale', scale)
  call print_array('cnorm', cnorm, n)
  call print_int('info', info)
  call end_test()

  ! ========================================================================
  ! Test 18: Very small diagonal (near-singular), triggers careful-solve
  ! ========================================================================
  n = 3
  kd = 1
  ab2 = 0.0d0
  ab2(2,1) = 1.0d0
  ab2(1,2) = 1.0d0; ab2(2,2) = 1.0d-300  ! very small diagonal
  ab2(1,3) = 1.0d0; ab2(2,3) = 1.0d0

  x(1) = 1.0d0; x(2) = 1.0d0; x(3) = 1.0d0
  scale = 0.0d0
  cnorm = 0.0d0
  call dlatbs('U', 'N', 'N', 'N', n, kd, ab2, kd+1, x, scale, cnorm, info)
  call begin_test('near_singular_upper')
  call print_array('x', x, n)
  call print_scalar('scale', scale)
  call print_array('cnorm', cnorm, n)
  call print_int('info', info)
  call end_test()

end program
