program test_dsterf
  use test_utils
  implicit none
  integer :: info, n
  double precision :: d(20), e(20)

  ! Test 1: N=0, quick return
  n = 0
  call dsterf(n, d, e, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 2: N=1, quick return
  n = 1
  d(1) = 5.0d0
  call dsterf(n, d, e, info)
  call begin_test('n_one')
  call print_int('info', info)
  call print_array('d', d, 1)
  call end_test()

  ! Test 3: 2x2 tridiagonal
  ! T = [[2, 1], [1, 3]]
  ! eigenvalues: (5 +/- sqrt(5))/2 = 3.618..., 1.381...
  n = 2
  d(1) = 2.0d0
  d(2) = 3.0d0
  e(1) = 1.0d0
  call dsterf(n, d, e, info)
  call begin_test('two_by_two')
  call print_int('info', info)
  call print_array('d', d, 2)
  call end_test()

  ! Test 4: 4x4 tridiagonal (Wilkinson-type)
  ! T = [[4, 1, 0, 0],
  !      [1, 3, 1, 0],
  !      [0, 1, 2, 1],
  !      [0, 0, 1, 1]]
  n = 4
  d(1) = 4.0d0
  d(2) = 3.0d0
  d(3) = 2.0d0
  d(4) = 1.0d0
  e(1) = 1.0d0
  e(2) = 1.0d0
  e(3) = 1.0d0
  call dsterf(n, d, e, info)
  call begin_test('four_by_four')
  call print_int('info', info)
  call print_array('d', d, 4)
  call end_test()

  ! Test 5: already diagonal matrix (all off-diag zero)
  n = 4
  d(1) = 3.0d0
  d(2) = 1.0d0
  d(3) = 4.0d0
  d(4) = 2.0d0
  e(1) = 0.0d0
  e(2) = 0.0d0
  e(3) = 0.0d0
  call dsterf(n, d, e, info)
  call begin_test('already_diagonal')
  call print_int('info', info)
  call print_array('d', d, 4)
  call end_test()

  ! Test 6: 6x6 tridiagonal with negative elements
  ! Tests QR path (abs(D(LEND)) >= abs(D(L)))
  n = 6
  d(1) = -2.0d0
  d(2) = 1.0d0
  d(3) = -3.0d0
  d(4) = 4.0d0
  d(5) = -1.0d0
  d(6) = 2.0d0
  e(1) = 1.0d0
  e(2) = 2.0d0
  e(3) = 1.0d0
  e(4) = 3.0d0
  e(5) = 1.0d0
  call dsterf(n, d, e, info)
  call begin_test('six_by_six_mixed')
  call print_int('info', info)
  call print_array('d', d, 6)
  call end_test()

  ! Test 7: matrix with split (zero off-diagonal in the middle)
  n = 4
  d(1) = 2.0d0
  d(2) = 3.0d0
  d(3) = 5.0d0
  d(4) = 7.0d0
  e(1) = 1.0d0
  e(2) = 0.0d0
  e(3) = 2.0d0
  call dsterf(n, d, e, info)
  call begin_test('split_matrix')
  call print_int('info', info)
  call print_array('d', d, 4)
  call end_test()

  ! Test 8: 3x3 identity-like
  n = 3
  d(1) = 1.0d0
  d(2) = 1.0d0
  d(3) = 1.0d0
  e(1) = 0.0d0
  e(2) = 0.0d0
  call dsterf(n, d, e, info)
  call begin_test('identity_tridiag')
  call print_int('info', info)
  call print_array('d', d, 3)
  call end_test()

  ! Test 9: 5x5 with equal diagonal entries and equal off-diagonal
  ! Tests Toeplitz-like tridiagonal
  n = 5
  d(1) = 2.0d0
  d(2) = 2.0d0
  d(3) = 2.0d0
  d(4) = 2.0d0
  d(5) = 2.0d0
  e(1) = 1.0d0
  e(2) = 1.0d0
  e(3) = 1.0d0
  e(4) = 1.0d0
  call dsterf(n, d, e, info)
  call begin_test('toeplitz')
  call print_int('info', info)
  call print_array('d', d, 5)
  call end_test()

  ! Test 10: 8x8 tridiagonal to exercise more iterations
  n = 8
  d(1) = 10.0d0
  d(2) = 1.0d0
  d(3) = 8.0d0
  d(4) = 3.0d0
  d(5) = 6.0d0
  d(6) = 5.0d0
  d(7) = 4.0d0
  d(8) = 7.0d0
  e(1) = 2.0d0
  e(2) = 3.0d0
  e(3) = 1.0d0
  e(4) = 4.0d0
  e(5) = 2.0d0
  e(6) = 1.0d0
  e(7) = 3.0d0
  call dsterf(n, d, e, info)
  call begin_test('eight_by_eight')
  call print_int('info', info)
  call print_array('d', d, 8)
  call end_test()

  ! Test 11: 3x3 that exercises QR path (small first diag, large last diag)
  ! With abs(D(LEND)) > abs(D(L)), should take QR branch
  n = 3
  d(1) = 0.1d0
  d(2) = 0.5d0
  d(3) = 10.0d0
  e(1) = 1.0d0
  e(2) = 1.0d0
  call dsterf(n, d, e, info)
  call begin_test('qr_path')
  call print_int('info', info)
  call print_array('d', d, 3)
  call end_test()

  ! Test 12: very large values to exercise upscaling (iscale=1)
  ! Need anorm > ssfmax ~ 2.23e153
  n = 3
  d(1) = 1.0d154
  d(2) = 2.0d154
  d(3) = 3.0d154
  e(1) = 0.5d154
  e(2) = 0.5d154
  call dsterf(n, d, e, info)
  call begin_test('large_values')
  call print_int('info', info)
  call print_array('d', d, 3)
  call end_test()

  ! Test 13: very small values to exercise downscaling
  n = 3
  d(1) = 1.0d-155
  d(2) = 2.0d-155
  d(3) = 3.0d-155
  e(1) = 0.5d-155
  e(2) = 0.5d-155
  call dsterf(n, d, e, info)
  call begin_test('small_values')
  call print_int('info', info)
  call print_array('d', d, 3)
  call end_test()

  ! Test 14: 4x4 that forces QR path (increasing diagonal)
  n = 4
  d(1) = 1.0d0
  d(2) = 2.0d0
  d(3) = 3.0d0
  d(4) = 100.0d0
  e(1) = 5.0d0
  e(2) = 5.0d0
  e(3) = 5.0d0
  call dsterf(n, d, e, info)
  call begin_test('qr_four_by_four')
  call print_int('info', info)
  call print_array('d', d, 4)
  call end_test()

end program
