program test_dspgst
  use test_utils
  implicit none

  ! Packed storage: AP has length N*(N+1)/2
  ! Upper: AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j
  ! Lower: AP(i + (j-1)*(2n-j)/2) = A(i,j) for j<=i<=n

  double precision :: ap(50), bp(50)
  integer :: info, n

  ! ==========================================================
  ! SPD matrix B (3x3) for tests:
  ! B_full = [4  2  1]
  !          [2  5  1]
  !          [1  1  3]
  ! Symmetric matrix A (3x3):
  ! A_full = [4  2  1]
  !          [2  5  3]
  !          [1  3  6]
  ! ==========================================================

  ! Test 1: ITYPE=1, UPLO='U', N=3
  ! B upper packed: [4, 2, 5, 1, 1, 3]
  bp = 0.0d0
  bp(1) = 4.0d0; bp(2) = 2.0d0; bp(3) = 5.0d0
  bp(4) = 1.0d0; bp(5) = 1.0d0; bp(6) = 3.0d0
  call dpptrf('U', 3, bp, info)

  ! A upper packed: [4, 2, 5, 1, 3, 6]
  ap = 0.0d0
  ap(1) = 4.0d0; ap(2) = 2.0d0; ap(3) = 5.0d0
  ap(4) = 1.0d0; ap(5) = 3.0d0; ap(6) = 6.0d0
  call dspgst(1, 'U', 3, ap, bp, info)
  call begin_test('itype1_upper_3')
  call print_int('info', info)
  call print_array('AP', ap, 6)
  call end_test()

  ! Test 2: ITYPE=1, UPLO='L', N=3
  ! B lower packed: [4, 2, 1, 5, 1, 3]
  bp = 0.0d0
  bp(1) = 4.0d0; bp(2) = 2.0d0; bp(3) = 1.0d0
  bp(4) = 5.0d0; bp(5) = 1.0d0; bp(6) = 3.0d0
  call dpptrf('L', 3, bp, info)

  ! A lower packed: [4, 2, 1, 5, 3, 6]
  ap = 0.0d0
  ap(1) = 4.0d0; ap(2) = 2.0d0; ap(3) = 1.0d0
  ap(4) = 5.0d0; ap(5) = 3.0d0; ap(6) = 6.0d0
  call dspgst(1, 'L', 3, ap, bp, info)
  call begin_test('itype1_lower_3')
  call print_int('info', info)
  call print_array('AP', ap, 6)
  call end_test()

  ! Test 3: ITYPE=2, UPLO='U', N=3
  bp = 0.0d0
  bp(1) = 4.0d0; bp(2) = 2.0d0; bp(3) = 5.0d0
  bp(4) = 1.0d0; bp(5) = 1.0d0; bp(6) = 3.0d0
  call dpptrf('U', 3, bp, info)

  ap = 0.0d0
  ap(1) = 4.0d0; ap(2) = 2.0d0; ap(3) = 5.0d0
  ap(4) = 1.0d0; ap(5) = 3.0d0; ap(6) = 6.0d0
  call dspgst(2, 'U', 3, ap, bp, info)
  call begin_test('itype2_upper_3')
  call print_int('info', info)
  call print_array('AP', ap, 6)
  call end_test()

  ! Test 4: ITYPE=2, UPLO='L', N=3
  bp = 0.0d0
  bp(1) = 4.0d0; bp(2) = 2.0d0; bp(3) = 1.0d0
  bp(4) = 5.0d0; bp(5) = 1.0d0; bp(6) = 3.0d0
  call dpptrf('L', 3, bp, info)

  ap = 0.0d0
  ap(1) = 4.0d0; ap(2) = 2.0d0; ap(3) = 1.0d0
  ap(4) = 5.0d0; ap(5) = 3.0d0; ap(6) = 6.0d0
  call dspgst(2, 'L', 3, ap, bp, info)
  call begin_test('itype2_lower_3')
  call print_int('info', info)
  call print_array('AP', ap, 6)
  call end_test()

  ! Test 5: ITYPE=3, UPLO='U', N=3
  bp = 0.0d0
  bp(1) = 4.0d0; bp(2) = 2.0d0; bp(3) = 5.0d0
  bp(4) = 1.0d0; bp(5) = 1.0d0; bp(6) = 3.0d0
  call dpptrf('U', 3, bp, info)

  ap = 0.0d0
  ap(1) = 4.0d0; ap(2) = 2.0d0; ap(3) = 5.0d0
  ap(4) = 1.0d0; ap(5) = 3.0d0; ap(6) = 6.0d0
  call dspgst(3, 'U', 3, ap, bp, info)
  call begin_test('itype3_upper_3')
  call print_int('info', info)
  call print_array('AP', ap, 6)
  call end_test()

  ! Test 6: ITYPE=3, UPLO='L', N=3
  bp = 0.0d0
  bp(1) = 4.0d0; bp(2) = 2.0d0; bp(3) = 1.0d0
  bp(4) = 5.0d0; bp(5) = 1.0d0; bp(6) = 3.0d0
  call dpptrf('L', 3, bp, info)

  ap = 0.0d0
  ap(1) = 4.0d0; ap(2) = 2.0d0; ap(3) = 1.0d0
  ap(4) = 5.0d0; ap(5) = 3.0d0; ap(6) = 6.0d0
  call dspgst(3, 'L', 3, ap, bp, info)
  call begin_test('itype3_lower_3')
  call print_int('info', info)
  call print_array('AP', ap, 6)
  call end_test()

  ! ==========================================================
  ! SPD matrix B (4x4):
  ! B_full = [9  3  1  0]
  !          [3  5  2  1]
  !          [1  2  4  1]
  !          [0  1  1  3]
  ! Symmetric matrix A (4x4):
  ! A_full = [10  4  2  1]
  !          [ 4  8  3  2]
  !          [ 2  3  7  4]
  !          [ 1  2  4  6]
  ! ==========================================================

  ! Test 7: ITYPE=1, UPLO='U', N=4
  ! B upper packed (column-major): [9, 3, 5, 1, 2, 4, 0, 1, 1, 3]
  bp = 0.0d0
  bp(1) = 9.0d0; bp(2) = 3.0d0; bp(3) = 5.0d0
  bp(4) = 1.0d0; bp(5) = 2.0d0; bp(6) = 4.0d0
  bp(7) = 0.0d0; bp(8) = 1.0d0; bp(9) = 1.0d0; bp(10) = 3.0d0
  call dpptrf('U', 4, bp, info)

  ! A upper packed: [10, 4, 8, 2, 3, 7, 1, 2, 4, 6]
  ap = 0.0d0
  ap(1) = 10.0d0; ap(2) = 4.0d0; ap(3) = 8.0d0
  ap(4) = 2.0d0; ap(5) = 3.0d0; ap(6) = 7.0d0
  ap(7) = 1.0d0; ap(8) = 2.0d0; ap(9) = 4.0d0; ap(10) = 6.0d0
  call dspgst(1, 'U', 4, ap, bp, info)
  call begin_test('itype1_upper_4')
  call print_int('info', info)
  call print_array('AP', ap, 10)
  call end_test()

  ! Test 8: ITYPE=1, UPLO='L', N=4
  ! B lower packed: [9, 3, 1, 0, 5, 2, 1, 4, 1, 3]
  bp = 0.0d0
  bp(1) = 9.0d0; bp(2) = 3.0d0; bp(3) = 1.0d0; bp(4) = 0.0d0
  bp(5) = 5.0d0; bp(6) = 2.0d0; bp(7) = 1.0d0
  bp(8) = 4.0d0; bp(9) = 1.0d0; bp(10) = 3.0d0
  call dpptrf('L', 4, bp, info)

  ! A lower packed: [10, 4, 2, 1, 8, 3, 2, 7, 4, 6]
  ap = 0.0d0
  ap(1) = 10.0d0; ap(2) = 4.0d0; ap(3) = 2.0d0; ap(4) = 1.0d0
  ap(5) = 8.0d0; ap(6) = 3.0d0; ap(7) = 2.0d0
  ap(8) = 7.0d0; ap(9) = 4.0d0; ap(10) = 6.0d0
  call dspgst(1, 'L', 4, ap, bp, info)
  call begin_test('itype1_lower_4')
  call print_int('info', info)
  call print_array('AP', ap, 10)
  call end_test()

  ! Test 9: ITYPE=2, UPLO='U', N=4
  bp = 0.0d0
  bp(1) = 9.0d0; bp(2) = 3.0d0; bp(3) = 5.0d0
  bp(4) = 1.0d0; bp(5) = 2.0d0; bp(6) = 4.0d0
  bp(7) = 0.0d0; bp(8) = 1.0d0; bp(9) = 1.0d0; bp(10) = 3.0d0
  call dpptrf('U', 4, bp, info)

  ap = 0.0d0
  ap(1) = 10.0d0; ap(2) = 4.0d0; ap(3) = 8.0d0
  ap(4) = 2.0d0; ap(5) = 3.0d0; ap(6) = 7.0d0
  ap(7) = 1.0d0; ap(8) = 2.0d0; ap(9) = 4.0d0; ap(10) = 6.0d0
  call dspgst(2, 'U', 4, ap, bp, info)
  call begin_test('itype2_upper_4')
  call print_int('info', info)
  call print_array('AP', ap, 10)
  call end_test()

  ! Test 10: ITYPE=2, UPLO='L', N=4
  bp = 0.0d0
  bp(1) = 9.0d0; bp(2) = 3.0d0; bp(3) = 1.0d0; bp(4) = 0.0d0
  bp(5) = 5.0d0; bp(6) = 2.0d0; bp(7) = 1.0d0
  bp(8) = 4.0d0; bp(9) = 1.0d0; bp(10) = 3.0d0
  call dpptrf('L', 4, bp, info)

  ap = 0.0d0
  ap(1) = 10.0d0; ap(2) = 4.0d0; ap(3) = 2.0d0; ap(4) = 1.0d0
  ap(5) = 8.0d0; ap(6) = 3.0d0; ap(7) = 2.0d0
  ap(8) = 7.0d0; ap(9) = 4.0d0; ap(10) = 6.0d0
  call dspgst(2, 'L', 4, ap, bp, info)
  call begin_test('itype2_lower_4')
  call print_int('info', info)
  call print_array('AP', ap, 10)
  call end_test()

  ! Test 11: ITYPE=3, UPLO='U', N=4
  bp = 0.0d0
  bp(1) = 9.0d0; bp(2) = 3.0d0; bp(3) = 5.0d0
  bp(4) = 1.0d0; bp(5) = 2.0d0; bp(6) = 4.0d0
  bp(7) = 0.0d0; bp(8) = 1.0d0; bp(9) = 1.0d0; bp(10) = 3.0d0
  call dpptrf('U', 4, bp, info)

  ap = 0.0d0
  ap(1) = 10.0d0; ap(2) = 4.0d0; ap(3) = 8.0d0
  ap(4) = 2.0d0; ap(5) = 3.0d0; ap(6) = 7.0d0
  ap(7) = 1.0d0; ap(8) = 2.0d0; ap(9) = 4.0d0; ap(10) = 6.0d0
  call dspgst(3, 'U', 4, ap, bp, info)
  call begin_test('itype3_upper_4')
  call print_int('info', info)
  call print_array('AP', ap, 10)
  call end_test()

  ! Test 12: ITYPE=3, UPLO='L', N=4
  bp = 0.0d0
  bp(1) = 9.0d0; bp(2) = 3.0d0; bp(3) = 1.0d0; bp(4) = 0.0d0
  bp(5) = 5.0d0; bp(6) = 2.0d0; bp(7) = 1.0d0
  bp(8) = 4.0d0; bp(9) = 1.0d0; bp(10) = 3.0d0
  call dpptrf('L', 4, bp, info)

  ap = 0.0d0
  ap(1) = 10.0d0; ap(2) = 4.0d0; ap(3) = 2.0d0; ap(4) = 1.0d0
  ap(5) = 8.0d0; ap(6) = 3.0d0; ap(7) = 2.0d0
  ap(8) = 7.0d0; ap(9) = 4.0d0; ap(10) = 6.0d0
  call dspgst(3, 'L', 4, ap, bp, info)
  call begin_test('itype3_lower_4')
  call print_int('info', info)
  call print_array('AP', ap, 10)
  call end_test()

  ! Test 13: N=0 (quick return)
  call dspgst(1, 'U', 0, ap, bp, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 14: N=1, ITYPE=1, UPLO='U'
  ap(1) = 9.0d0
  bp(1) = 3.0d0
  call dspgst(1, 'U', 1, ap, bp, info)
  call begin_test('n_one_itype1_upper')
  call print_int('info', info)
  call print_array('AP', ap, 1)
  call end_test()

  ! Test 15: N=1, ITYPE=1, UPLO='L'
  ap(1) = 16.0d0
  bp(1) = 4.0d0
  call dspgst(1, 'L', 1, ap, bp, info)
  call begin_test('n_one_itype1_lower')
  call print_int('info', info)
  call print_array('AP', ap, 1)
  call end_test()

  ! Test 16: N=1, ITYPE=2
  ap(1) = 5.0d0
  bp(1) = 2.0d0
  call dspgst(2, 'U', 1, ap, bp, info)
  call begin_test('n_one_itype2')
  call print_int('info', info)
  call print_array('AP', ap, 1)
  call end_test()

  ! Test 17: N=1, ITYPE=3
  ap(1) = 5.0d0
  bp(1) = 2.0d0
  call dspgst(3, 'L', 1, ap, bp, info)
  call begin_test('n_one_itype3')
  call print_int('info', info)
  call print_array('AP', ap, 1)
  call end_test()

end program
