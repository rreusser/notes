program test_dbdsqr
  use test_utils
  implicit none

  double precision :: d(10), e(10), work(100)
  double precision :: vt(100), u(100), c(100)
  integer :: info, n, ncvt, nru, ncc, ldvt, ldu, ldc

  ! Test 1: Upper bidiagonal 4x4, values only (no vectors)
  n = 4
  ncvt = 0
  nru = 0
  ncc = 0
  ldvt = 1
  ldu = 1
  ldc = 1
  d(1) = 4.0d0
  d(2) = 3.0d0
  d(3) = 2.0d0
  d(4) = 1.0d0
  e(1) = 1.0d0
  e(2) = 1.0d0
  e(3) = 1.0d0
  work = 0.0d0

  call dbdsqr('U', n, ncvt, nru, ncc, d, e, vt, ldvt, u, ldu, c, ldc, work, info)

  call begin_test('upper_4x4_values_only')
  call print_int('info', info)
  call print_array('d', d, n)
  call end_test()

  ! Test 2: Upper bidiagonal 3x3 with right singular vectors (VT)
  n = 3
  ncvt = 3
  nru = 0
  ncc = 0
  ldvt = 3
  ldu = 1
  ldc = 1
  d(1) = 3.0d0
  d(2) = 2.0d0
  d(3) = 1.0d0
  e(1) = 0.5d0
  e(2) = 0.5d0
  work = 0.0d0

  ! Initialize VT to 3x3 identity
  vt = 0.0d0
  vt(1) = 1.0d0
  vt(5) = 1.0d0
  vt(9) = 1.0d0

  call dbdsqr('U', n, ncvt, nru, ncc, d, e, vt, ldvt, u, ldu, c, ldc, work, info)

  call begin_test('upper_3x3_with_vt')
  call print_int('info', info)
  call print_array('d', d, n)
  call print_array('vt', vt, n*n)
  call end_test()

  ! Test 3: Upper bidiagonal 3x3 with both VT and U
  n = 3
  ncvt = 3
  nru = 3
  ncc = 0
  ldvt = 3
  ldu = 3
  ldc = 1
  d(1) = 5.0d0
  d(2) = 3.0d0
  d(3) = 1.0d0
  e(1) = 2.0d0
  e(2) = 1.0d0
  work = 0.0d0

  ! Initialize VT and U to identity
  vt = 0.0d0
  vt(1) = 1.0d0
  vt(5) = 1.0d0
  vt(9) = 1.0d0

  u = 0.0d0
  u(1) = 1.0d0
  u(5) = 1.0d0
  u(9) = 1.0d0

  call dbdsqr('U', n, ncvt, nru, ncc, d, e, vt, ldvt, u, ldu, c, ldc, work, info)

  call begin_test('upper_3x3_with_vt_and_u')
  call print_int('info', info)
  call print_array('d', d, n)
  call print_array('vt', vt, n*n)
  call print_array('u', u, n*n)
  call end_test()

  ! Test 4: Lower bidiagonal 3x3
  n = 3
  ncvt = 0
  nru = 0
  ncc = 0
  ldvt = 1
  ldu = 1
  ldc = 1
  d(1) = 4.0d0
  d(2) = 3.0d0
  d(3) = 2.0d0
  e(1) = 1.5d0
  e(2) = 0.5d0
  work = 0.0d0

  call dbdsqr('L', n, ncvt, nru, ncc, d, e, vt, ldvt, u, ldu, c, ldc, work, info)

  call begin_test('lower_3x3_values_only')
  call print_int('info', info)
  call print_array('d', d, n)
  call end_test()

  ! Test 5: Lower bidiagonal 3x3 with U
  n = 3
  ncvt = 0
  nru = 3
  ncc = 0
  ldvt = 1
  ldu = 3
  ldc = 1
  d(1) = 4.0d0
  d(2) = 3.0d0
  d(3) = 2.0d0
  e(1) = 1.5d0
  e(2) = 0.5d0
  work = 0.0d0

  u = 0.0d0
  u(1) = 1.0d0
  u(5) = 1.0d0
  u(9) = 1.0d0

  call dbdsqr('L', n, ncvt, nru, ncc, d, e, vt, ldvt, u, ldu, c, ldc, work, info)

  call begin_test('lower_3x3_with_u')
  call print_int('info', info)
  call print_array('d', d, n)
  call print_array('u', u, n*n)
  call end_test()

  ! Test 6: N=1 edge case
  n = 1
  ncvt = 0
  nru = 0
  ncc = 0
  ldvt = 1
  ldu = 1
  ldc = 1
  d(1) = -5.0d0
  work = 0.0d0

  call dbdsqr('U', n, ncvt, nru, ncc, d, e, vt, ldvt, u, ldu, c, ldc, work, info)

  call begin_test('n_1')
  call print_int('info', info)
  call print_array('d', d, n)
  call end_test()

  ! Test 7: N=0 quick return
  n = 0
  ncvt = 0
  nru = 0
  ncc = 0
  ldvt = 1
  ldu = 1
  ldc = 1

  call dbdsqr('U', n, ncvt, nru, ncc, d, e, vt, ldvt, u, ldu, c, ldc, work, info)

  call begin_test('n_0')
  call print_int('info', info)
  call end_test()

  ! Test 8: N=2 upper bidiagonal with vectors
  n = 2
  ncvt = 2
  nru = 2
  ncc = 0
  ldvt = 2
  ldu = 2
  ldc = 1
  d(1) = 3.0d0
  d(2) = 1.0d0
  e(1) = 2.0d0
  work = 0.0d0

  vt = 0.0d0
  vt(1) = 1.0d0
  vt(4) = 1.0d0

  u = 0.0d0
  u(1) = 1.0d0
  u(4) = 1.0d0

  call dbdsqr('U', n, ncvt, nru, ncc, d, e, vt, ldvt, u, ldu, c, ldc, work, info)

  call begin_test('upper_2x2_with_vectors')
  call print_int('info', info)
  call print_array('d', d, n)
  call print_array('vt', vt, n*n)
  call print_array('u', u, n*n)
  call end_test()

  ! Test 9: N=1 with negative value and VT (tests dscal path)
  n = 1
  ncvt = 2
  nru = 0
  ncc = 0
  ldvt = 1
  ldu = 1
  ldc = 1
  d(1) = -3.0d0
  work = 0.0d0

  vt = 0.0d0
  vt(1) = 1.0d0
  vt(2) = 3.0d0

  call dbdsqr('U', n, ncvt, nru, ncc, d, e, vt, ldvt, u, ldu, c, ldc, work, info)

  call begin_test('n_1_neg_with_vt')
  call print_int('info', info)
  call print_array('d', d, n)
  call print_array('vt', vt, ncvt)
  call end_test()

  ! Test 10: Upper bidiagonal 3x3 with C matrix
  n = 3
  ncvt = 0
  nru = 0
  ncc = 2
  ldvt = 1
  ldu = 1
  ldc = 3
  d(1) = 4.0d0
  d(2) = 2.0d0
  d(3) = 1.0d0
  e(1) = 1.0d0
  e(2) = 0.5d0
  work = 0.0d0

  ! Initialize C to a 3x2 real matrix
  c = 0.0d0
  c(1) = 1.0d0
  c(2) = 0.5d0
  c(3) = 1.5d0
  c(4) = 2.0d0
  c(5) = 0.25d0
  c(6) = 2.5d0

  call dbdsqr('U', n, ncvt, nru, ncc, d, e, vt, ldvt, u, ldu, c, ldc, work, info)

  call begin_test('upper_3x3_with_c')
  call print_int('info', info)
  call print_array('d', d, n)
  call print_array('c', c, n*ncc)
  call end_test()

  ! Test 11: Upper bidiagonal where |d(1)| < |d(n)| to trigger idir=2
  n = 4
  ncvt = 0
  nru = 0
  ncc = 0
  ldvt = 1
  ldu = 1
  ldc = 1
  d(1) = 0.5d0
  d(2) = 1.0d0
  d(3) = 2.0d0
  d(4) = 4.0d0
  e(1) = 0.1d0
  e(2) = 0.1d0
  e(3) = 0.1d0
  work = 0.0d0

  call dbdsqr('U', n, ncvt, nru, ncc, d, e, vt, ldvt, u, ldu, c, ldc, work, info)

  call begin_test('upper_4x4_idir2')
  call print_int('info', info)
  call print_array('d', d, n)
  call end_test()

  ! Test 12: Upper bidiagonal with very small element to trigger zero shift
  n = 3
  ncvt = 3
  nru = 3
  ncc = 0
  ldvt = 3
  ldu = 3
  ldc = 1
  d(1) = 1.0d0
  d(2) = 1.0d-15
  d(3) = 1.0d0
  e(1) = 1.0d0
  e(2) = 1.0d0
  work = 0.0d0

  vt = 0.0d0
  vt(1) = 1.0d0
  vt(5) = 1.0d0
  vt(9) = 1.0d0

  u = 0.0d0
  u(1) = 1.0d0
  u(5) = 1.0d0
  u(9) = 1.0d0

  call dbdsqr('U', n, ncvt, nru, ncc, d, e, vt, ldvt, u, ldu, c, ldc, work, info)

  call begin_test('upper_3x3_zero_shift')
  call print_int('info', info)
  call print_array('d', d, n)
  call print_array('vt', vt, n*n)
  call print_array('u', u, n*n)
  call end_test()

  ! Test 13: Lower bidiagonal with C matrix
  n = 3
  ncvt = 0
  nru = 0
  ncc = 2
  ldvt = 1
  ldu = 1
  ldc = 3
  d(1) = 3.0d0
  d(2) = 2.0d0
  d(3) = 1.0d0
  e(1) = 0.5d0
  e(2) = 0.5d0
  work = 0.0d0

  c = 0.0d0
  c(1) = 1.0d0
  c(2) = 0.0d0
  c(3) = 0.0d0
  c(4) = 0.0d0
  c(5) = 1.0d0
  c(6) = 0.0d0

  call dbdsqr('L', n, ncvt, nru, ncc, d, e, vt, ldvt, u, ldu, c, ldc, work, info)

  call begin_test('lower_3x3_with_c')
  call print_int('info', info)
  call print_array('d', d, n)
  call print_array('c', c, n*ncc)
  call end_test()

  ! Test 14: Reverse-ordered (idir=2) with vectors
  n = 3
  ncvt = 3
  nru = 3
  ncc = 0
  ldvt = 3
  ldu = 3
  ldc = 1
  d(1) = 0.1d0
  d(2) = 0.5d0
  d(3) = 3.0d0
  e(1) = 0.2d0
  e(2) = 0.3d0
  work = 0.0d0

  vt = 0.0d0
  vt(1) = 1.0d0
  vt(5) = 1.0d0
  vt(9) = 1.0d0

  u = 0.0d0
  u(1) = 1.0d0
  u(5) = 1.0d0
  u(9) = 1.0d0

  call dbdsqr('U', n, ncvt, nru, ncc, d, e, vt, ldvt, u, ldu, c, ldc, work, info)

  call begin_test('upper_3x3_idir2_with_vectors')
  call print_int('info', info)
  call print_array('d', d, n)
  call print_array('vt', vt, n*n)
  call print_array('u', u, n*n)
  call end_test()

  ! Test 15: Negative singular values to test sign flip and VT negate
  n = 3
  ncvt = 3
  nru = 0
  ncc = 0
  ldvt = 3
  ldu = 1
  ldc = 1
  d(1) = -3.0d0
  d(2) = 2.0d0
  d(3) = -1.0d0
  e(1) = 0.5d0
  e(2) = 0.5d0
  work = 0.0d0

  vt = 0.0d0
  vt(1) = 1.0d0
  vt(5) = 1.0d0
  vt(9) = 1.0d0

  call dbdsqr('U', n, ncvt, nru, ncc, d, e, vt, ldvt, u, ldu, c, ldc, work, info)

  call begin_test('upper_3x3_negative_d')
  call print_int('info', info)
  call print_array('d', d, n)
  call print_array('vt', vt, n*n)
  call end_test()

  ! Test 16: Nearly diagonal (very small off-diagonal)
  n = 4
  ncvt = 0
  nru = 0
  ncc = 0
  ldvt = 1
  ldu = 1
  ldc = 1
  d(1) = 5.0d0
  d(2) = 3.0d0
  d(3) = 2.0d0
  d(4) = 1.0d0
  e(1) = 1.0d-16
  e(2) = 1.0d-16
  e(3) = 1.0d-16
  work = 0.0d0

  call dbdsqr('U', n, ncvt, nru, ncc, d, e, vt, ldvt, u, ldu, c, ldc, work, info)

  call begin_test('nearly_diagonal')
  call print_int('info', info)
  call print_array('d', d, n)
  call end_test()

  ! Test 17: Lower bidiagonal with VT and U
  n = 3
  ncvt = 3
  nru = 3
  ncc = 0
  ldvt = 3
  ldu = 3
  ldc = 1
  d(1) = 3.0d0
  d(2) = 2.0d0
  d(3) = 1.0d0
  e(1) = 0.5d0
  e(2) = 0.5d0
  work = 0.0d0

  vt = 0.0d0
  vt(1) = 1.0d0
  vt(5) = 1.0d0
  vt(9) = 1.0d0

  u = 0.0d0
  u(1) = 1.0d0
  u(5) = 1.0d0
  u(9) = 1.0d0

  call dbdsqr('L', n, ncvt, nru, ncc, d, e, vt, ldvt, u, ldu, c, ldc, work, info)

  call begin_test('lower_3x3_with_vt_and_u')
  call print_int('info', info)
  call print_array('d', d, n)
  call print_array('vt', vt, n*n)
  call print_array('u', u, n*n)
  call end_test()

  ! Test 18: Lower bidiagonal with VT, U, and C
  n = 3
  ncvt = 3
  nru = 3
  ncc = 2
  ldvt = 3
  ldu = 3
  ldc = 3
  d(1) = 4.0d0
  d(2) = 2.0d0
  d(3) = 1.0d0
  e(1) = 1.0d0
  e(2) = 0.5d0
  work = 0.0d0

  vt = 0.0d0
  vt(1) = 1.0d0
  vt(5) = 1.0d0
  vt(9) = 1.0d0

  u = 0.0d0
  u(1) = 1.0d0
  u(5) = 1.0d0
  u(9) = 1.0d0

  c = 0.0d0
  c(1) = 1.0d0
  c(2) = 0.5d0
  c(3) = 1.5d0
  c(4) = 2.0d0
  c(5) = 0.25d0
  c(6) = 2.5d0

  call dbdsqr('L', n, ncvt, nru, ncc, d, e, vt, ldvt, u, ldu, c, ldc, work, info)

  call begin_test('lower_3x3_all_vectors')
  call print_int('info', info)
  call print_array('d', d, n)
  call print_array('vt', vt, n*n)
  call print_array('u', u, n*n)
  call print_array('c', c, n*ncc)
  call end_test()

  ! Test 19: Upper bidiag with d=0 entry (triggers sminoa=0 path)
  n = 3
  ncvt = 3
  nru = 3
  ncc = 0
  ldvt = 3
  ldu = 3
  ldc = 1
  d(1) = 2.0d0
  d(2) = 0.0d0
  d(3) = 3.0d0
  e(1) = 1.0d0
  e(2) = 1.0d0
  work = 0.0d0

  vt = 0.0d0
  vt(1) = 1.0d0
  vt(5) = 1.0d0
  vt(9) = 1.0d0

  u = 0.0d0
  u(1) = 1.0d0
  u(5) = 1.0d0
  u(9) = 1.0d0

  call dbdsqr('U', n, ncvt, nru, ncc, d, e, vt, ldvt, u, ldu, c, ldc, work, info)

  call begin_test('upper_3x3_zero_d')
  call print_int('info', info)
  call print_array('d', d, n)
  call print_array('vt', vt, n*n)
  call print_array('u', u, n*n)
  call end_test()

  ! Test 20: Upper bidiag idir=2 zero-shift with all vectors
  n = 3
  ncvt = 3
  nru = 3
  ncc = 2
  ldvt = 3
  ldu = 3
  ldc = 3
  d(1) = 1.0d-15
  d(2) = 1.0d0
  d(3) = 1.0d0
  e(1) = 1.0d0
  e(2) = 1.0d0
  work = 0.0d0

  vt = 0.0d0
  vt(1) = 1.0d0
  vt(5) = 1.0d0
  vt(9) = 1.0d0

  u = 0.0d0
  u(1) = 1.0d0
  u(5) = 1.0d0
  u(9) = 1.0d0

  c = 0.0d0
  c(1) = 1.0d0
  c(2) = 0.5d0
  c(3) = 1.5d0
  c(4) = 2.0d0
  c(5) = 0.25d0
  c(6) = 2.5d0

  call dbdsqr('U', n, ncvt, nru, ncc, d, e, vt, ldvt, u, ldu, c, ldc, work, info)

  call begin_test('upper_3x3_zero_shift_all_vecs')
  call print_int('info', info)
  call print_array('d', d, n)
  call print_array('vt', vt, n*n)
  call print_array('u', u, n*n)
  call print_array('c', c, n*ncc)
  call end_test()

  ! Test 21: idir=2 with vectors (ascending singular values, ncvt/nru/ncc > 0)
  n = 4
  ncvt = 4
  nru = 4
  ncc = 2
  ldvt = 4
  ldu = 4
  ldc = 4
  d(1) = 0.5d0
  d(2) = 1.0d0
  d(3) = 2.0d0
  d(4) = 4.0d0
  e(1) = 0.1d0
  e(2) = 0.1d0
  e(3) = 0.1d0
  work = 0.0d0

  vt = 0.0d0
  vt(1) = 1.0d0
  vt(6) = 1.0d0
  vt(11) = 1.0d0
  vt(16) = 1.0d0

  u = 0.0d0
  u(1) = 1.0d0
  u(6) = 1.0d0
  u(11) = 1.0d0
  u(16) = 1.0d0

  c = 0.0d0
  c(1) = 1.0d0
  c(2) = 0.5d0
  c(3) = 1.5d0
  c(4) = 0.25d0
  c(5) = 2.0d0
  c(6) = 0.75d0
  c(7) = 2.5d0
  c(8) = 1.0d0

  call dbdsqr('U', n, ncvt, nru, ncc, d, e, vt, ldvt, u, ldu, c, ldc, work, info)

  call begin_test('upper_4x4_idir2_all_vecs')
  call print_int('info', info)
  call print_array('d', d, n)
  call print_array('vt', vt, n*n)
  call print_array('u', u, n*n)
  call print_array('c', c, n*ncc)
  call end_test()

  ! Test 22: idir=1 zero-shift with all vectors
  n = 4
  ncvt = 4
  nru = 4
  ncc = 2
  ldvt = 4
  ldu = 4
  ldc = 4
  d(1) = 10.0d0
  d(2) = 1.0d-15
  d(3) = 5.0d0
  d(4) = 1.0d0
  e(1) = 0.1d0
  e(2) = 0.1d0
  e(3) = 0.1d0
  work = 0.0d0

  vt = 0.0d0
  vt(1) = 1.0d0
  vt(6) = 1.0d0
  vt(11) = 1.0d0
  vt(16) = 1.0d0

  u = 0.0d0
  u(1) = 1.0d0
  u(6) = 1.0d0
  u(11) = 1.0d0
  u(16) = 1.0d0

  c = 0.0d0
  c(1) = 1.0d0
  c(2) = 0.5d0
  c(3) = 1.5d0
  c(4) = 0.25d0
  c(5) = 2.0d0
  c(6) = 0.75d0
  c(7) = 2.5d0
  c(8) = 1.0d0

  call dbdsqr('U', n, ncvt, nru, ncc, d, e, vt, ldvt, u, ldu, c, ldc, work, info)

  call begin_test('upper_4x4_idir1_zero_shift_all_vecs')
  call print_int('info', info)
  call print_array('d', d, n)
  call print_array('vt', vt, n*n)
  call print_array('u', u, n*n)
  call print_array('c', c, n*ncc)
  call end_test()

  ! Test 23: (shift/sll)^2 < eps for the near-zero shift
  n = 3
  ncvt = 3
  nru = 3
  ncc = 0
  ldvt = 3
  ldu = 3
  ldc = 1
  d(1) = 1.0d8
  d(2) = 1.0d0
  d(3) = 1.0d0
  e(1) = 0.5d0
  e(2) = 0.5d0
  work = 0.0d0

  vt = 0.0d0
  vt(1) = 1.0d0
  vt(5) = 1.0d0
  vt(9) = 1.0d0

  u = 0.0d0
  u(1) = 1.0d0
  u(5) = 1.0d0
  u(9) = 1.0d0

  call dbdsqr('U', n, ncvt, nru, ncc, d, e, vt, ldvt, u, ldu, c, ldc, work, info)

  call begin_test('upper_3x3_near_zero_shift')
  call print_int('info', info)
  call print_array('d', d, n)
  call print_array('vt', vt, n*n)
  call print_array('u', u, n*n)
  call end_test()

end program
