program test_zpptrf
  use test_utils
  implicit none

  ! Packed storage for complex Hermitian matrices
  ! AP has length N*(N+1)/2 complex elements = N*(N+1) doubles
  ! Upper packed: AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j
  ! Lower packed: AP(i + (j-1)*(2n-j)/2) = A(i,j) for j<=i<=n

  complex*16 :: ap(50)
  double precision :: ap_r(100)
  equivalence (ap, ap_r)
  integer :: info, n

  ! =================================================================
  ! Test 1: UPLO='U', N=3, well-conditioned HPD matrix
  ! A = [10       (2+i)     (3-2i) ]
  !     [(2-i)     8        (1+i)  ]
  !     [(3+2i)   (1-i)      6     ]
  ! Upper packed (column-major): a11, a12, a22, a13, a23, a33
  ! = [10, (2+i), 8, (3-2i), (1+i), 6]
  ap = (0.0d0, 0.0d0)
  ap(1) = (10.0d0, 0.0d0)
  ap(2) = (2.0d0, 1.0d0)
  ap(3) = (8.0d0, 0.0d0)
  ap(4) = (3.0d0, -2.0d0)
  ap(5) = (1.0d0, 1.0d0)
  ap(6) = (6.0d0, 0.0d0)
  n = 3

  call zpptrf('U', n, ap, info)
  call begin_test('upper_3x3')
  call print_array('ap', ap_r, n*(n+1))
  call print_int('info', info)
  call end_test()

  ! =================================================================
  ! Test 2: UPLO='L', N=3, same HPD matrix in lower packed
  ! Lower packed (column-major): a11, a21, a31, a22, a32, a33
  ! = [10, (2-i), (3+2i), 8, (1-i), 6]
  ap = (0.0d0, 0.0d0)
  ap(1) = (10.0d0, 0.0d0)
  ap(2) = (2.0d0, -1.0d0)
  ap(3) = (3.0d0, 2.0d0)
  ap(4) = (8.0d0, 0.0d0)
  ap(5) = (1.0d0, -1.0d0)
  ap(6) = (6.0d0, 0.0d0)
  n = 3

  call zpptrf('L', n, ap, info)
  call begin_test('lower_3x3')
  call print_array('ap', ap_r, n*(n+1))
  call print_int('info', info)
  call end_test()

  ! =================================================================
  ! Test 3: UPLO='U', N=4, HPD matrix
  ! A = [20       (3+i)     (1-2i)     (4+i)  ]
  !     [(3-i)     15       (2+3i)     (1-i)  ]
  !     [(1+2i)   (2-3i)     12        (3+2i) ]
  !     [(4-i)    (1+i)     (3-2i)      10    ]
  ! Upper packed: a11, a12, a22, a13, a23, a33, a14, a24, a34, a44
  ap = (0.0d0, 0.0d0)
  ap(1) = (20.0d0, 0.0d0)
  ap(2) = (3.0d0, 1.0d0)
  ap(3) = (15.0d0, 0.0d0)
  ap(4) = (1.0d0, -2.0d0)
  ap(5) = (2.0d0, 3.0d0)
  ap(6) = (12.0d0, 0.0d0)
  ap(7) = (4.0d0, 1.0d0)
  ap(8) = (1.0d0, -1.0d0)
  ap(9) = (3.0d0, 2.0d0)
  ap(10) = (10.0d0, 0.0d0)
  n = 4

  call zpptrf('U', n, ap, info)
  call begin_test('upper_4x4')
  call print_array('ap', ap_r, n*(n+1))
  call print_int('info', info)
  call end_test()

  ! =================================================================
  ! Test 4: UPLO='L', N=4, same HPD matrix in lower packed
  ! Lower packed: a11, a21, a31, a41, a22, a32, a42, a33, a43, a44
  ap = (0.0d0, 0.0d0)
  ap(1) = (20.0d0, 0.0d0)
  ap(2) = (3.0d0, -1.0d0)
  ap(3) = (1.0d0, 2.0d0)
  ap(4) = (4.0d0, -1.0d0)
  ap(5) = (15.0d0, 0.0d0)
  ap(6) = (2.0d0, -3.0d0)
  ap(7) = (1.0d0, 1.0d0)
  ap(8) = (12.0d0, 0.0d0)
  ap(9) = (3.0d0, -2.0d0)
  ap(10) = (10.0d0, 0.0d0)
  n = 4

  call zpptrf('L', n, ap, info)
  call begin_test('lower_4x4')
  call print_array('ap', ap_r, n*(n+1))
  call print_int('info', info)
  call end_test()

  ! =================================================================
  ! Test 5: N=0 (quick return)
  info = -999
  call zpptrf('U', 0, ap, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! =================================================================
  ! Test 6: N=1, upper
  ap(1) = (9.0d0, 0.0d0)
  call zpptrf('U', 1, ap, info)
  call begin_test('n_one_upper')
  call print_array('ap', ap_r, 2)
  call print_int('info', info)
  call end_test()

  ! =================================================================
  ! Test 7: N=1, lower
  ap(1) = (16.0d0, 0.0d0)
  call zpptrf('L', 1, ap, info)
  call begin_test('n_one_lower')
  call print_array('ap', ap_r, 2)
  call print_int('info', info)
  call end_test()

  ! =================================================================
  ! Test 8: INFO > 0, not positive definite (upper)
  ! A = [1       (2+i)]
  !     [(2-i)    1   ]
  ! Not HPD: eigenvalues approx 1 +/- |2+i| = 1 +/- sqrt(5) -> negative
  ap = (0.0d0, 0.0d0)
  ap(1) = (1.0d0, 0.0d0)
  ap(2) = (2.0d0, 1.0d0)
  ap(3) = (1.0d0, 0.0d0)
  n = 2

  call zpptrf('U', n, ap, info)
  call begin_test('not_hpd_upper')
  call print_array('ap', ap_r, n*(n+1))
  call print_int('info', info)
  call end_test()

  ! =================================================================
  ! Test 9: INFO > 0, not positive definite (lower)
  ! Same matrix in lower packed
  ap = (0.0d0, 0.0d0)
  ap(1) = (1.0d0, 0.0d0)
  ap(2) = (2.0d0, -1.0d0)
  ap(3) = (1.0d0, 0.0d0)
  n = 2

  call zpptrf('L', n, ap, info)
  call begin_test('not_hpd_lower')
  call print_array('ap', ap_r, n*(n+1))
  call print_int('info', info)
  call end_test()

  ! =================================================================
  ! Test 10: INFO > 0 at first diagonal (negative diagonal)
  ap(1) = (-4.0d0, 0.0d0)
  ap(2) = (1.0d0, 0.0d0)
  ap(3) = (5.0d0, 0.0d0)
  call zpptrf('U', 2, ap, info)
  call begin_test('not_hpd_first_upper')
  call print_int('info', info)
  call end_test()

  ! =================================================================
  ! Test 11: INFO > 0 at first diagonal (lower)
  ap(1) = (0.0d0, 0.0d0)
  ap(2) = (1.0d0, 0.0d0)
  ap(3) = (5.0d0, 0.0d0)
  call zpptrf('L', 2, ap, info)
  call begin_test('not_hpd_first_lower')
  call print_int('info', info)
  call end_test()

  ! =================================================================
  ! Test 12: UPLO='U', N=3, identity
  ap = (0.0d0, 0.0d0)
  ap(1) = (1.0d0, 0.0d0)
  ap(2) = (0.0d0, 0.0d0)
  ap(3) = (1.0d0, 0.0d0)
  ap(4) = (0.0d0, 0.0d0)
  ap(5) = (0.0d0, 0.0d0)
  ap(6) = (1.0d0, 0.0d0)
  call zpptrf('U', 3, ap, info)
  call begin_test('identity_upper')
  call print_array('ap', ap_r, 12)
  call print_int('info', info)
  call end_test()

  ! =================================================================
  ! Test 13: UPLO='L', N=3, identity
  ap = (0.0d0, 0.0d0)
  ap(1) = (1.0d0, 0.0d0)
  ap(2) = (0.0d0, 0.0d0)
  ap(3) = (0.0d0, 0.0d0)
  ap(4) = (1.0d0, 0.0d0)
  ap(5) = (0.0d0, 0.0d0)
  ap(6) = (1.0d0, 0.0d0)
  call zpptrf('L', 3, ap, info)
  call begin_test('identity_lower')
  call print_array('ap', ap_r, 12)
  call print_int('info', info)
  call end_test()

  ! =================================================================
  ! Test 14: UPLO='U', N=2, basic HPD
  ap = (0.0d0, 0.0d0)
  ap(1) = (4.0d0, 0.0d0)
  ap(2) = (1.0d0, 2.0d0)
  ap(3) = (10.0d0, 0.0d0)
  call zpptrf('U', 2, ap, info)
  call begin_test('upper_2x2')
  call print_array('ap', ap_r, 6)
  call print_int('info', info)
  call end_test()

  ! =================================================================
  ! Test 15: UPLO='L', N=2, basic HPD
  ap = (0.0d0, 0.0d0)
  ap(1) = (4.0d0, 0.0d0)
  ap(2) = (1.0d0, -2.0d0)
  ap(3) = (10.0d0, 0.0d0)
  call zpptrf('L', 2, ap, info)
  call begin_test('lower_2x2')
  call print_array('ap', ap_r, 6)
  call print_int('info', info)
  call end_test()

end program
