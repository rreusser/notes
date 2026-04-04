program test_zhpgst
  use test_utils
  implicit none

  ! Packed storage for complex Hermitian matrices
  ! AP has length N*(N+1)/2 complex elements = N*(N+1) doubles
  ! Upper packed: AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j
  ! Lower packed: AP(i + (j-1)*(2n-j)/2) = A(i,j) for j<=i<=n

  complex*16 :: ap(50), bp(50)
  double precision :: ap_r(100), bp_r(100)
  equivalence (ap, ap_r)
  equivalence (bp, bp_r)
  integer :: info, n

  ! =================================================================
  ! Hermitian positive-definite B (3x3):
  ! B = [10       (2+i)     (3-2i) ]
  !     [(2-i)     8        (1+i)  ]
  !     [(3+2i)   (1-i)      6     ]
  ! Hermitian A (3x3):
  ! A = [12       (3+2i)    (1-i)  ]
  !     [(3-2i)    9        (2+3i) ]
  !     [(1+i)    (2-3i)     7     ]
  ! =================================================================

  ! Test 1: ITYPE=1, UPLO='U', N=3
  ! B upper packed: b11, b12, b22, b13, b23, b33
  bp = (0.0d0, 0.0d0)
  bp(1) = (10.0d0, 0.0d0)
  bp(2) = (2.0d0, 1.0d0)
  bp(3) = (8.0d0, 0.0d0)
  bp(4) = (3.0d0, -2.0d0)
  bp(5) = (1.0d0, 1.0d0)
  bp(6) = (6.0d0, 0.0d0)
  call zpptrf('U', 3, bp, info)

  ! A upper packed: a11, a12, a22, a13, a23, a33
  ap = (0.0d0, 0.0d0)
  ap(1) = (12.0d0, 0.0d0)
  ap(2) = (3.0d0, 2.0d0)
  ap(3) = (9.0d0, 0.0d0)
  ap(4) = (1.0d0, -1.0d0)
  ap(5) = (2.0d0, 3.0d0)
  ap(6) = (7.0d0, 0.0d0)
  call zhpgst(1, 'U', 3, ap, bp, info)
  call begin_test('itype1_upper_3')
  call print_int('info', info)
  call print_array('AP', ap_r, 12)
  call print_array('BP', bp_r, 12)
  call end_test()

  ! Test 2: ITYPE=1, UPLO='L', N=3
  ! B lower packed: b11, b21, b31, b22, b32, b33
  bp = (0.0d0, 0.0d0)
  bp(1) = (10.0d0, 0.0d0)
  bp(2) = (2.0d0, -1.0d0)
  bp(3) = (3.0d0, 2.0d0)
  bp(4) = (8.0d0, 0.0d0)
  bp(5) = (1.0d0, -1.0d0)
  bp(6) = (6.0d0, 0.0d0)
  call zpptrf('L', 3, bp, info)

  ! A lower packed: a11, a21, a31, a22, a32, a33
  ap = (0.0d0, 0.0d0)
  ap(1) = (12.0d0, 0.0d0)
  ap(2) = (3.0d0, -2.0d0)
  ap(3) = (1.0d0, 1.0d0)
  ap(4) = (9.0d0, 0.0d0)
  ap(5) = (2.0d0, -3.0d0)
  ap(6) = (7.0d0, 0.0d0)
  call zhpgst(1, 'L', 3, ap, bp, info)
  call begin_test('itype1_lower_3')
  call print_int('info', info)
  call print_array('AP', ap_r, 12)
  call print_array('BP', bp_r, 12)
  call end_test()

  ! Test 3: ITYPE=2, UPLO='U', N=3
  bp = (0.0d0, 0.0d0)
  bp(1) = (10.0d0, 0.0d0)
  bp(2) = (2.0d0, 1.0d0)
  bp(3) = (8.0d0, 0.0d0)
  bp(4) = (3.0d0, -2.0d0)
  bp(5) = (1.0d0, 1.0d0)
  bp(6) = (6.0d0, 0.0d0)
  call zpptrf('U', 3, bp, info)

  ap = (0.0d0, 0.0d0)
  ap(1) = (12.0d0, 0.0d0)
  ap(2) = (3.0d0, 2.0d0)
  ap(3) = (9.0d0, 0.0d0)
  ap(4) = (1.0d0, -1.0d0)
  ap(5) = (2.0d0, 3.0d0)
  ap(6) = (7.0d0, 0.0d0)
  call zhpgst(2, 'U', 3, ap, bp, info)
  call begin_test('itype2_upper_3')
  call print_int('info', info)
  call print_array('AP', ap_r, 12)
  call end_test()

  ! Test 4: ITYPE=2, UPLO='L', N=3
  bp = (0.0d0, 0.0d0)
  bp(1) = (10.0d0, 0.0d0)
  bp(2) = (2.0d0, -1.0d0)
  bp(3) = (3.0d0, 2.0d0)
  bp(4) = (8.0d0, 0.0d0)
  bp(5) = (1.0d0, -1.0d0)
  bp(6) = (6.0d0, 0.0d0)
  call zpptrf('L', 3, bp, info)

  ap = (0.0d0, 0.0d0)
  ap(1) = (12.0d0, 0.0d0)
  ap(2) = (3.0d0, -2.0d0)
  ap(3) = (1.0d0, 1.0d0)
  ap(4) = (9.0d0, 0.0d0)
  ap(5) = (2.0d0, -3.0d0)
  ap(6) = (7.0d0, 0.0d0)
  call zhpgst(2, 'L', 3, ap, bp, info)
  call begin_test('itype2_lower_3')
  call print_int('info', info)
  call print_array('AP', ap_r, 12)
  call end_test()

  ! Test 5: ITYPE=3, UPLO='U', N=3
  bp = (0.0d0, 0.0d0)
  bp(1) = (10.0d0, 0.0d0)
  bp(2) = (2.0d0, 1.0d0)
  bp(3) = (8.0d0, 0.0d0)
  bp(4) = (3.0d0, -2.0d0)
  bp(5) = (1.0d0, 1.0d0)
  bp(6) = (6.0d0, 0.0d0)
  call zpptrf('U', 3, bp, info)

  ap = (0.0d0, 0.0d0)
  ap(1) = (12.0d0, 0.0d0)
  ap(2) = (3.0d0, 2.0d0)
  ap(3) = (9.0d0, 0.0d0)
  ap(4) = (1.0d0, -1.0d0)
  ap(5) = (2.0d0, 3.0d0)
  ap(6) = (7.0d0, 0.0d0)
  call zhpgst(3, 'U', 3, ap, bp, info)
  call begin_test('itype3_upper_3')
  call print_int('info', info)
  call print_array('AP', ap_r, 12)
  call end_test()

  ! Test 6: ITYPE=3, UPLO='L', N=3
  bp = (0.0d0, 0.0d0)
  bp(1) = (10.0d0, 0.0d0)
  bp(2) = (2.0d0, -1.0d0)
  bp(3) = (3.0d0, 2.0d0)
  bp(4) = (8.0d0, 0.0d0)
  bp(5) = (1.0d0, -1.0d0)
  bp(6) = (6.0d0, 0.0d0)
  call zpptrf('L', 3, bp, info)

  ap = (0.0d0, 0.0d0)
  ap(1) = (12.0d0, 0.0d0)
  ap(2) = (3.0d0, -2.0d0)
  ap(3) = (1.0d0, 1.0d0)
  ap(4) = (9.0d0, 0.0d0)
  ap(5) = (2.0d0, -3.0d0)
  ap(6) = (7.0d0, 0.0d0)
  call zhpgst(3, 'L', 3, ap, bp, info)
  call begin_test('itype3_lower_3')
  call print_int('info', info)
  call print_array('AP', ap_r, 12)
  call end_test()

  ! =================================================================
  ! HPD B (4x4):
  ! B = [20       (3+i)     (1-2i)     (4+i)  ]
  !     [(3-i)     15       (2+3i)     (1-i)  ]
  !     [(1+2i)   (2-3i)     12        (3+2i) ]
  !     [(4-i)    (1+i)     (3-2i)      10    ]
  ! Hermitian A (4x4):
  ! A = [18       (5+2i)    (2-i)      (3+i)  ]
  !     [(5-2i)    14       (4+3i)     (1-2i) ]
  !     [(2+i)    (4-3i)     11        (2+i)  ]
  !     [(3-i)    (1+2i)    (2-i)       8     ]
  ! =================================================================

  ! Test 7: ITYPE=1, UPLO='U', N=4
  ! B upper packed: b11, b12, b22, b13, b23, b33, b14, b24, b34, b44
  bp = (0.0d0, 0.0d0)
  bp(1) = (20.0d0, 0.0d0)
  bp(2) = (3.0d0, 1.0d0)
  bp(3) = (15.0d0, 0.0d0)
  bp(4) = (1.0d0, -2.0d0)
  bp(5) = (2.0d0, 3.0d0)
  bp(6) = (12.0d0, 0.0d0)
  bp(7) = (4.0d0, 1.0d0)
  bp(8) = (1.0d0, -1.0d0)
  bp(9) = (3.0d0, 2.0d0)
  bp(10) = (10.0d0, 0.0d0)
  call zpptrf('U', 4, bp, info)

  ! A upper packed
  ap = (0.0d0, 0.0d0)
  ap(1) = (18.0d0, 0.0d0)
  ap(2) = (5.0d0, 2.0d0)
  ap(3) = (14.0d0, 0.0d0)
  ap(4) = (2.0d0, -1.0d0)
  ap(5) = (4.0d0, 3.0d0)
  ap(6) = (11.0d0, 0.0d0)
  ap(7) = (3.0d0, 1.0d0)
  ap(8) = (1.0d0, -2.0d0)
  ap(9) = (2.0d0, 1.0d0)
  ap(10) = (8.0d0, 0.0d0)
  call zhpgst(1, 'U', 4, ap, bp, info)
  call begin_test('itype1_upper_4')
  call print_int('info', info)
  call print_array('AP', ap_r, 20)
  call end_test()

  ! Test 8: ITYPE=1, UPLO='L', N=4
  ! B lower packed: b11, b21, b31, b41, b22, b32, b42, b33, b43, b44
  bp = (0.0d0, 0.0d0)
  bp(1) = (20.0d0, 0.0d0)
  bp(2) = (3.0d0, -1.0d0)
  bp(3) = (1.0d0, 2.0d0)
  bp(4) = (4.0d0, -1.0d0)
  bp(5) = (15.0d0, 0.0d0)
  bp(6) = (2.0d0, -3.0d0)
  bp(7) = (1.0d0, 1.0d0)
  bp(8) = (12.0d0, 0.0d0)
  bp(9) = (3.0d0, -2.0d0)
  bp(10) = (10.0d0, 0.0d0)
  call zpptrf('L', 4, bp, info)

  ! A lower packed
  ap = (0.0d0, 0.0d0)
  ap(1) = (18.0d0, 0.0d0)
  ap(2) = (5.0d0, -2.0d0)
  ap(3) = (2.0d0, 1.0d0)
  ap(4) = (3.0d0, -1.0d0)
  ap(5) = (14.0d0, 0.0d0)
  ap(6) = (4.0d0, -3.0d0)
  ap(7) = (1.0d0, 2.0d0)
  ap(8) = (11.0d0, 0.0d0)
  ap(9) = (2.0d0, -1.0d0)
  ap(10) = (8.0d0, 0.0d0)
  call zhpgst(1, 'L', 4, ap, bp, info)
  call begin_test('itype1_lower_4')
  call print_int('info', info)
  call print_array('AP', ap_r, 20)
  call end_test()

  ! Test 9: ITYPE=2, UPLO='U', N=4
  bp = (0.0d0, 0.0d0)
  bp(1) = (20.0d0, 0.0d0)
  bp(2) = (3.0d0, 1.0d0)
  bp(3) = (15.0d0, 0.0d0)
  bp(4) = (1.0d0, -2.0d0)
  bp(5) = (2.0d0, 3.0d0)
  bp(6) = (12.0d0, 0.0d0)
  bp(7) = (4.0d0, 1.0d0)
  bp(8) = (1.0d0, -1.0d0)
  bp(9) = (3.0d0, 2.0d0)
  bp(10) = (10.0d0, 0.0d0)
  call zpptrf('U', 4, bp, info)

  ap = (0.0d0, 0.0d0)
  ap(1) = (18.0d0, 0.0d0)
  ap(2) = (5.0d0, 2.0d0)
  ap(3) = (14.0d0, 0.0d0)
  ap(4) = (2.0d0, -1.0d0)
  ap(5) = (4.0d0, 3.0d0)
  ap(6) = (11.0d0, 0.0d0)
  ap(7) = (3.0d0, 1.0d0)
  ap(8) = (1.0d0, -2.0d0)
  ap(9) = (2.0d0, 1.0d0)
  ap(10) = (8.0d0, 0.0d0)
  call zhpgst(2, 'U', 4, ap, bp, info)
  call begin_test('itype2_upper_4')
  call print_int('info', info)
  call print_array('AP', ap_r, 20)
  call end_test()

  ! Test 10: ITYPE=2, UPLO='L', N=4
  bp = (0.0d0, 0.0d0)
  bp(1) = (20.0d0, 0.0d0)
  bp(2) = (3.0d0, -1.0d0)
  bp(3) = (1.0d0, 2.0d0)
  bp(4) = (4.0d0, -1.0d0)
  bp(5) = (15.0d0, 0.0d0)
  bp(6) = (2.0d0, -3.0d0)
  bp(7) = (1.0d0, 1.0d0)
  bp(8) = (12.0d0, 0.0d0)
  bp(9) = (3.0d0, -2.0d0)
  bp(10) = (10.0d0, 0.0d0)
  call zpptrf('L', 4, bp, info)

  ap = (0.0d0, 0.0d0)
  ap(1) = (18.0d0, 0.0d0)
  ap(2) = (5.0d0, -2.0d0)
  ap(3) = (2.0d0, 1.0d0)
  ap(4) = (3.0d0, -1.0d0)
  ap(5) = (14.0d0, 0.0d0)
  ap(6) = (4.0d0, -3.0d0)
  ap(7) = (1.0d0, 2.0d0)
  ap(8) = (11.0d0, 0.0d0)
  ap(9) = (2.0d0, -1.0d0)
  ap(10) = (8.0d0, 0.0d0)
  call zhpgst(2, 'L', 4, ap, bp, info)
  call begin_test('itype2_lower_4')
  call print_int('info', info)
  call print_array('AP', ap_r, 20)
  call end_test()

  ! Test 11: ITYPE=3, UPLO='U', N=4
  bp = (0.0d0, 0.0d0)
  bp(1) = (20.0d0, 0.0d0)
  bp(2) = (3.0d0, 1.0d0)
  bp(3) = (15.0d0, 0.0d0)
  bp(4) = (1.0d0, -2.0d0)
  bp(5) = (2.0d0, 3.0d0)
  bp(6) = (12.0d0, 0.0d0)
  bp(7) = (4.0d0, 1.0d0)
  bp(8) = (1.0d0, -1.0d0)
  bp(9) = (3.0d0, 2.0d0)
  bp(10) = (10.0d0, 0.0d0)
  call zpptrf('U', 4, bp, info)

  ap = (0.0d0, 0.0d0)
  ap(1) = (18.0d0, 0.0d0)
  ap(2) = (5.0d0, 2.0d0)
  ap(3) = (14.0d0, 0.0d0)
  ap(4) = (2.0d0, -1.0d0)
  ap(5) = (4.0d0, 3.0d0)
  ap(6) = (11.0d0, 0.0d0)
  ap(7) = (3.0d0, 1.0d0)
  ap(8) = (1.0d0, -2.0d0)
  ap(9) = (2.0d0, 1.0d0)
  ap(10) = (8.0d0, 0.0d0)
  call zhpgst(3, 'U', 4, ap, bp, info)
  call begin_test('itype3_upper_4')
  call print_int('info', info)
  call print_array('AP', ap_r, 20)
  call end_test()

  ! Test 12: ITYPE=3, UPLO='L', N=4
  bp = (0.0d0, 0.0d0)
  bp(1) = (20.0d0, 0.0d0)
  bp(2) = (3.0d0, -1.0d0)
  bp(3) = (1.0d0, 2.0d0)
  bp(4) = (4.0d0, -1.0d0)
  bp(5) = (15.0d0, 0.0d0)
  bp(6) = (2.0d0, -3.0d0)
  bp(7) = (1.0d0, 1.0d0)
  bp(8) = (12.0d0, 0.0d0)
  bp(9) = (3.0d0, -2.0d0)
  bp(10) = (10.0d0, 0.0d0)
  call zpptrf('L', 4, bp, info)

  ap = (0.0d0, 0.0d0)
  ap(1) = (18.0d0, 0.0d0)
  ap(2) = (5.0d0, -2.0d0)
  ap(3) = (2.0d0, 1.0d0)
  ap(4) = (3.0d0, -1.0d0)
  ap(5) = (14.0d0, 0.0d0)
  ap(6) = (4.0d0, -3.0d0)
  ap(7) = (1.0d0, 2.0d0)
  ap(8) = (11.0d0, 0.0d0)
  ap(9) = (2.0d0, -1.0d0)
  ap(10) = (8.0d0, 0.0d0)
  call zhpgst(3, 'L', 4, ap, bp, info)
  call begin_test('itype3_lower_4')
  call print_int('info', info)
  call print_array('AP', ap_r, 20)
  call end_test()

  ! Test 13: N=0 (quick return)
  call zhpgst(1, 'U', 0, ap, bp, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 14: N=1, ITYPE=1, UPLO='U'
  bp(1) = (9.0d0, 0.0d0)
  call zpptrf('U', 1, bp, info)
  ap(1) = (12.0d0, 0.0d0)
  call zhpgst(1, 'U', 1, ap, bp, info)
  call begin_test('n_one_itype1_upper')
  call print_int('info', info)
  call print_array('AP', ap_r, 2)
  call end_test()

  ! Test 15: N=1, ITYPE=1, UPLO='L'
  bp(1) = (16.0d0, 0.0d0)
  call zpptrf('L', 1, bp, info)
  ap(1) = (25.0d0, 0.0d0)
  call zhpgst(1, 'L', 1, ap, bp, info)
  call begin_test('n_one_itype1_lower')
  call print_int('info', info)
  call print_array('AP', ap_r, 2)
  call end_test()

  ! Test 16: N=1, ITYPE=2
  bp(1) = (4.0d0, 0.0d0)
  call zpptrf('U', 1, bp, info)
  ap(1) = (5.0d0, 0.0d0)
  call zhpgst(2, 'U', 1, ap, bp, info)
  call begin_test('n_one_itype2')
  call print_int('info', info)
  call print_array('AP', ap_r, 2)
  call end_test()

  ! Test 17: N=1, ITYPE=3
  bp(1) = (4.0d0, 0.0d0)
  call zpptrf('L', 1, bp, info)
  ap(1) = (5.0d0, 0.0d0)
  call zhpgst(3, 'L', 1, ap, bp, info)
  call begin_test('n_one_itype3')
  call print_int('info', info)
  call print_array('AP', ap_r, 2)
  call end_test()

end program
