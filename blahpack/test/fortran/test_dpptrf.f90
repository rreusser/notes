program test_dpptrf
  use test_utils
  implicit none

  ! Packed storage: AP has length N*(N+1)/2
  ! Upper: AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j
  ! Lower: AP(i + (j-1)*(2n-j)/2) = A(i,j) for j<=i<=n

  double precision :: ap(50), ap_save(50)
  integer :: info, n

  ! Test 1: UPLO='U', N=4, well-conditioned SPD matrix
  ! A = [4  2  1   0.5]   Upper packed: [4, 2, 5, 1, 1, 5, 0.5, 0.5, 1, 5]
  !     [2  5  1   0.5]   Positions:
  !     [1  1  5   1  ]   AP(1)=4, AP(2)=2, AP(3)=5, AP(4)=1, AP(5)=1,
  !     [0.5 0.5 1  5 ]   AP(6)=5, AP(7)=0.5, AP(8)=0.5, AP(9)=1, AP(10)=5
  ap = 0.0d0
  ap(1) = 4.0d0
  ap(2) = 2.0d0
  ap(3) = 5.0d0
  ap(4) = 1.0d0
  ap(5) = 1.0d0
  ap(6) = 5.0d0
  ap(7) = 0.5d0
  ap(8) = 0.5d0
  ap(9) = 1.0d0
  ap(10) = 5.0d0
  n = 4

  call dpptrf('U', n, ap, info)
  call begin_test('upper_basic')
  call print_array('ap', ap, n*(n+1)/2)
  call print_int('info', info)
  call end_test()

  ! Test 2: UPLO='L', N=4, same SPD matrix in lower packed
  ! Lower packed: col 1 = [4, 2, 1, 0.5], col 2 = [5, 1, 0.5], col 3 = [5, 1], col 4 = [5]
  ap = 0.0d0
  ap(1) = 4.0d0
  ap(2) = 2.0d0
  ap(3) = 1.0d0
  ap(4) = 0.5d0
  ap(5) = 5.0d0
  ap(6) = 1.0d0
  ap(7) = 0.5d0
  ap(8) = 5.0d0
  ap(9) = 1.0d0
  ap(10) = 5.0d0
  n = 4

  call dpptrf('L', n, ap, info)
  call begin_test('lower_basic')
  call print_array('ap', ap, n*(n+1)/2)
  call print_int('info', info)
  call end_test()

  ! Test 3: N=0 (quick return)
  info = -999
  call dpptrf('U', 0, ap, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 4: N=1 (single element, upper)
  ap(1) = 9.0d0
  call dpptrf('U', 1, ap, info)
  call begin_test('n_one_upper')
  call print_array('ap', ap, 1)
  call print_int('info', info)
  call end_test()

  ! Test 5: N=1 (single element, lower)
  ap(1) = 16.0d0
  call dpptrf('L', 1, ap, info)
  call begin_test('n_one_lower')
  call print_array('ap', ap, 1)
  call print_int('info', info)
  call end_test()

  ! Test 6: INFO > 0, not positive definite (upper)
  ! Matrix: [1  2]  which has eigenvalues 1 +/- 2 -> not SPD
  !         [2  1]
  ! Upper packed: AP(1)=1, AP(2)=2, AP(3)=1
  ap = 0.0d0
  ap(1) = 1.0d0
  ap(2) = 2.0d0
  ap(3) = 1.0d0
  call dpptrf('U', 2, ap, info)
  call begin_test('not_spd_upper')
  call print_array('ap', ap, 3)
  call print_int('info', info)
  call end_test()

  ! Test 7: INFO > 0, not positive definite (lower)
  ! Same matrix in lower packed: AP(1)=1, AP(2)=2, AP(3)=1
  ap = 0.0d0
  ap(1) = 1.0d0
  ap(2) = 2.0d0
  ap(3) = 1.0d0
  call dpptrf('L', 2, ap, info)
  call begin_test('not_spd_lower')
  call print_array('ap', ap, 3)
  call print_int('info', info)
  call end_test()

  ! Test 8: UPLO='U', N=3, identity matrix
  ap = 0.0d0
  ap(1) = 1.0d0
  ap(2) = 0.0d0
  ap(3) = 1.0d0
  ap(4) = 0.0d0
  ap(5) = 0.0d0
  ap(6) = 1.0d0
  call dpptrf('U', 3, ap, info)
  call begin_test('identity_upper')
  call print_array('ap', ap, 6)
  call print_int('info', info)
  call end_test()

  ! Test 9: UPLO='L', N=3, identity matrix
  ap = 0.0d0
  ap(1) = 1.0d0
  ap(2) = 0.0d0
  ap(3) = 0.0d0
  ap(4) = 1.0d0
  ap(5) = 0.0d0
  ap(6) = 1.0d0
  call dpptrf('L', 3, ap, info)
  call begin_test('identity_lower')
  call print_array('ap', ap, 6)
  call print_int('info', info)
  call end_test()

  ! Test 10: UPLO='U', N=3, larger SPD matrix with varied values
  ! A = [25   5  -5]
  !     [ 5  10   2]
  !     [-5   2   6]
  ! Upper packed: [25, 5, 10, -5, 2, 6]
  ap = 0.0d0
  ap(1) = 25.0d0
  ap(2) = 5.0d0
  ap(3) = 10.0d0
  ap(4) = -5.0d0
  ap(5) = 2.0d0
  ap(6) = 6.0d0
  call dpptrf('U', 3, ap, info)
  call begin_test('upper_3x3')
  call print_array('ap', ap, 6)
  call print_int('info', info)
  call end_test()

  ! Test 11: UPLO='L', N=3, same SPD matrix in lower packed
  ! Lower packed: [25, 5, -5, 10, 2, 6]
  ap = 0.0d0
  ap(1) = 25.0d0
  ap(2) = 5.0d0
  ap(3) = -5.0d0
  ap(4) = 10.0d0
  ap(5) = 2.0d0
  ap(6) = 6.0d0
  call dpptrf('L', 3, ap, info)
  call begin_test('lower_3x3')
  call print_array('ap', ap, 6)
  call print_int('info', info)
  call end_test()

  ! Test 12: INFO > 0 at first diagonal element (not positive definite)
  ap(1) = -4.0d0
  ap(2) = 1.0d0
  ap(3) = 5.0d0
  call dpptrf('U', 2, ap, info)
  call begin_test('not_spd_first_upper')
  call print_int('info', info)
  call end_test()

  ! Test 13: INFO > 0 at first diagonal element (lower)
  ap(1) = 0.0d0
  ap(2) = 1.0d0
  ap(3) = 5.0d0
  call dpptrf('L', 2, ap, info)
  call begin_test('not_spd_first_lower')
  call print_int('info', info)
  call end_test()

  ! Test 14: N=2, basic SPD (upper)
  ap(1) = 4.0d0
  ap(2) = 2.0d0
  ap(3) = 5.0d0
  call dpptrf('U', 2, ap, info)
  call begin_test('upper_2x2')
  call print_array('ap', ap, 3)
  call print_int('info', info)
  call end_test()

  ! Test 15: N=2, basic SPD (lower)
  ap(1) = 4.0d0
  ap(2) = 2.0d0
  ap(3) = 5.0d0
  call dpptrf('L', 2, ap, info)
  call begin_test('lower_2x2')
  call print_array('ap', ap, 3)
  call print_int('info', info)
  call end_test()

end program
