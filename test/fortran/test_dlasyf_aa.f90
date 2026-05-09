program test_dlasyf_aa
  use test_utils
  implicit none
  double precision :: a(200), h(200), work(50)
  integer :: ipiv(20)

  ! ============================================================
  ! Test 1: Lower, first panel (J1=1, K1=2), M=6, NB=3
  ! Symmetric indefinite 6x6, sub-diagonal panel
  ! ============================================================
  a = 0.0d0
  ! Lower triangle of 6x6 symmetric matrix stored column-major
  ! col 1
  a(1) = 4.0d0; a(2) = 1.0d0; a(3) = 2.0d0; a(4) = 0.5d0; a(5) = -1.0d0; a(6) = 0.5d0
  ! col 2
  a(8) = 5.0d0; a(9) = 1.5d0; a(10) = -0.5d0; a(11) = 1.0d0; a(12) = 0.5d0
  ! col 3
  a(15) = 6.0d0; a(16) = 2.0d0; a(17) = 0.5d0; a(18) = -1.0d0
  ! col 4
  a(22) = 7.0d0; a(23) = 1.0d0; a(24) = 1.5d0
  ! col 5
  a(29) = 8.0d0; a(30) = 2.0d0
  ! col 6
  a(36) = 9.0d0
  h = 0.0d0
  work = 0.0d0
  ipiv = 0
  call dlasyf_aa('L', 1, 6, 3, a, 6, ipiv, h, 6, work)
  call begin_test('lower_j1_first_m6_nb3')
  call print_array('a', a, 36)
  call print_int_array('ipiv', ipiv, 6)
  call end_test()

  ! ============================================================
  ! Test 2: Upper, first panel (J1=1, K1=2), M=6, NB=3
  ! ============================================================
  a = 0.0d0
  ! Upper triangle
  ! col 1
  a(1) = 4.0d0
  ! col 2
  a(7) = 1.0d0; a(8) = 5.0d0
  ! col 3
  a(13) = 2.0d0; a(14) = 1.5d0; a(15) = 6.0d0
  ! col 4
  a(19) = 0.5d0; a(20) = -0.5d0; a(21) = 2.0d0; a(22) = 7.0d0
  ! col 5
  a(25) = -1.0d0; a(26) = 1.0d0; a(27) = 0.5d0; a(28) = 1.0d0; a(29) = 8.0d0
  ! col 6
  a(31) = 0.5d0; a(32) = 0.5d0; a(33) = -1.0d0; a(34) = 1.5d0; a(35) = 2.0d0; a(36) = 9.0d0
  h = 0.0d0
  work = 0.0d0
  ipiv = 0
  call dlasyf_aa('U', 1, 6, 3, a, 6, ipiv, h, 6, work)
  call begin_test('upper_j1_first_m6_nb3')
  call print_array('a', a, 36)
  call print_int_array('ipiv', ipiv, 6)
  call end_test()

  ! ============================================================
  ! Test 3: Lower, subsequent panel (J1=2, K1=1), M=5, NB=3
  ! Pre-populate the previous panel's last column in column 0 of the input
  ! ============================================================
  a = 0.0d0
  ! For J1=2, the panel layout is A(LDA, M+1) where col 0 is previous
  ! panel's last column data. We use LDA=6.
  ! col 0 (previous panel last col) - use small values
  a(1) = 0.1d0; a(2) = 0.2d0; a(3) = 0.3d0; a(4) = 0.1d0; a(5) = 0.2d0
  ! col 1 (first panel column to factor)
  a(7) = 0.0d0;  a(8) = 4.0d0; a(9) = 1.0d0; a(10) = 2.0d0; a(11) = 0.5d0; a(12) = -1.0d0
  ! col 2
  a(13) = 0.0d0; a(14) = 0.0d0; a(15) = 5.0d0; a(16) = 1.5d0; a(17) = -0.5d0; a(18) = 1.0d0
  ! col 3
  a(19) = 0.0d0; a(20) = 0.0d0; a(21) = 0.0d0; a(22) = 6.0d0; a(23) = 2.0d0; a(24) = 0.5d0
  ! col 4
  a(25) = 0.0d0; a(26) = 0.0d0; a(27) = 0.0d0; a(28) = 0.0d0; a(29) = 7.0d0; a(30) = 1.0d0
  ! col 5
  a(31) = 0.0d0; a(32) = 0.0d0; a(33) = 0.0d0; a(34) = 0.0d0; a(35) = 0.0d0; a(36) = 8.0d0
  h = 0.0d0
  work = 0.0d0
  ipiv = 0
  call dlasyf_aa('L', 2, 5, 3, a, 6, ipiv, h, 6, work)
  call begin_test('lower_j1_2_m5_nb3')
  call print_array('a', a, 36)
  call print_int_array('ipiv', ipiv, 5)
  call end_test()

  ! ============================================================
  ! Test 4: Upper, subsequent panel (J1=2, K1=1), M=5, NB=3
  ! ============================================================
  a = 0.0d0
  ! col 0 (previous panel last row data, transposed convention for upper)
  a(1) = 0.1d0
  ! col 1
  a(7) = 0.2d0; a(8) = 4.0d0
  ! col 2
  a(13) = 0.3d0; a(14) = 1.0d0; a(15) = 5.0d0
  ! col 3
  a(19) = 0.1d0; a(20) = 2.0d0; a(21) = 1.5d0; a(22) = 6.0d0
  ! col 4
  a(25) = 0.2d0; a(26) = 0.5d0; a(27) = -0.5d0; a(28) = 2.0d0; a(29) = 7.0d0
  ! col 5
  a(31) = 0.0d0; a(32) = -1.0d0; a(33) = 1.0d0; a(34) = 0.5d0; a(35) = 1.0d0; a(36) = 8.0d0
  h = 0.0d0
  work = 0.0d0
  ipiv = 0
  call dlasyf_aa('U', 2, 5, 3, a, 6, ipiv, h, 6, work)
  call begin_test('upper_j1_2_m5_nb3')
  call print_array('a', a, 36)
  call print_int_array('ipiv', ipiv, 5)
  call end_test()

  ! ============================================================
  ! Test 5: Lower, M=4, NB=4 (factor full panel), J1=1
  ! ============================================================
  a = 0.0d0
  ! col 1
  a(1) = 3.0d0; a(2) = 1.0d0; a(3) = 2.0d0; a(4) = 0.5d0
  ! col 2
  a(6) = 5.0d0; a(7) = 1.5d0; a(8) = -1.0d0
  ! col 3
  a(11) = 4.0d0; a(12) = 1.0d0
  ! col 4
  a(16) = 6.0d0
  h = 0.0d0
  work = 0.0d0
  ipiv = 0
  call dlasyf_aa('L', 1, 4, 4, a, 4, ipiv, h, 4, work)
  call begin_test('lower_m4_nb4_full')
  call print_array('a', a, 16)
  call print_int_array('ipiv', ipiv, 4)
  call end_test()

  ! ============================================================
  ! Test 6: Upper, M=4, NB=4 (factor full panel), J1=1
  ! ============================================================
  a = 0.0d0
  ! col 1
  a(1) = 3.0d0
  ! col 2
  a(5) = 1.0d0; a(6) = 5.0d0
  ! col 3
  a(9) = 2.0d0; a(10) = 1.5d0; a(11) = 4.0d0
  ! col 4
  a(13) = 0.5d0; a(14) = -1.0d0; a(15) = 1.0d0; a(16) = 6.0d0
  h = 0.0d0
  work = 0.0d0
  ipiv = 0
  call dlasyf_aa('U', 1, 4, 4, a, 4, ipiv, h, 4, work)
  call begin_test('upper_m4_nb4_full')
  call print_array('a', a, 16)
  call print_int_array('ipiv', ipiv, 4)
  call end_test()

  ! ============================================================
  ! Test 7: Lower, M=1 quick path, J1=1
  ! ============================================================
  a = 0.0d0
  a(1) = 7.0d0
  h = 0.0d0
  work = 0.0d0
  ipiv = 0
  call dlasyf_aa('L', 1, 1, 1, a, 1, ipiv, h, 1, work)
  call begin_test('lower_m1_nb1')
  call print_array('a', a, 1)
  call print_int_array('ipiv', ipiv, 1)
  call end_test()

  ! ============================================================
  ! Test 8: Upper, M=1 quick path, J1=1
  ! ============================================================
  a = 0.0d0
  a(1) = 7.0d0
  h = 0.0d0
  work = 0.0d0
  ipiv = 0
  call dlasyf_aa('U', 1, 1, 1, a, 1, ipiv, h, 1, work)
  call begin_test('upper_m1_nb1')
  call print_array('a', a, 1)
  call print_int_array('ipiv', ipiv, 1)
  call end_test()

  ! ============================================================
  ! Test 9: Lower, NB=1 with M>1 (single column factorization)
  ! ============================================================
  a = 0.0d0
  ! col 1
  a(1) = 4.0d0; a(2) = 1.0d0; a(3) = 2.0d0; a(4) = 0.5d0
  ! col 2
  a(6) = 5.0d0; a(7) = 1.5d0; a(8) = -1.0d0
  ! col 3
  a(11) = 4.0d0; a(12) = 1.0d0
  ! col 4
  a(16) = 6.0d0
  h = 0.0d0
  work = 0.0d0
  ipiv = 0
  call dlasyf_aa('L', 1, 4, 1, a, 4, ipiv, h, 4, work)
  call begin_test('lower_m4_nb1')
  call print_array('a', a, 16)
  call print_int_array('ipiv', ipiv, 4)
  call end_test()

  ! ============================================================
  ! Test 10: Upper, NB=1 with M>1
  ! ============================================================
  a = 0.0d0
  ! col 1
  a(1) = 4.0d0
  ! col 2
  a(5) = 1.0d0; a(6) = 5.0d0
  ! col 3
  a(9) = 2.0d0; a(10) = 1.5d0; a(11) = 4.0d0
  ! col 4
  a(13) = 0.5d0; a(14) = -1.0d0; a(15) = 1.0d0; a(16) = 6.0d0
  h = 0.0d0
  work = 0.0d0
  ipiv = 0
  call dlasyf_aa('U', 1, 4, 1, a, 4, ipiv, h, 4, work)
  call begin_test('upper_m4_nb1')
  call print_array('a', a, 16)
  call print_int_array('ipiv', ipiv, 4)
  call end_test()

  ! ============================================================
  ! Test 11: Lower, M=6, NB=3, J1=1 designed to force pivot swaps
  ! Use a column where the large off-diagonal is far from the diagonal
  ! ============================================================
  a = 0.0d0
  ! col 1: small at (2,1), LARGE at (5,1) so first column triggers swap
  a(1) = 1.0d0; a(2) = 0.5d0; a(3) = 0.3d0; a(4) = 0.2d0; a(5) = 9.0d0; a(6) = 0.4d0
  ! col 2: smallish at top, LARGE at bottom
  a(8) = 2.0d0; a(9) = 0.6d0; a(10) = 0.4d0; a(11) = 0.3d0; a(12) = 8.0d0
  ! col 3: large further down
  a(15) = 3.0d0; a(16) = 0.7d0; a(17) = 0.5d0; a(18) = 7.0d0
  ! col 4
  a(22) = 4.0d0; a(23) = 0.8d0; a(24) = 0.6d0
  ! col 5
  a(29) = 5.0d0; a(30) = 0.9d0
  ! col 6
  a(36) = 6.0d0
  h = 0.0d0
  work = 0.0d0
  ipiv = 0
  call dlasyf_aa('L', 1, 6, 3, a, 6, ipiv, h, 6, work)
  call begin_test('lower_pivot_m6_nb3')
  call print_array('a', a, 36)
  call print_int_array('ipiv', ipiv, 6)
  call end_test()

  ! ============================================================
  ! Test 12: Upper, M=6, NB=3, J1=1 designed to force pivot swaps
  ! Mirror of test 11 in the upper triangle.
  ! ============================================================
  a = 0.0d0
  ! Symmetric with test 11 (transpose lower → upper):
  a(1) = 1.0d0
  a(7) = 0.5d0; a(8) = 2.0d0
  a(13) = 0.3d0; a(14) = 0.6d0; a(15) = 3.0d0
  a(19) = 0.2d0; a(20) = 0.4d0; a(21) = 0.7d0; a(22) = 4.0d0
  a(25) = 9.0d0; a(26) = 0.3d0; a(27) = 0.5d0; a(28) = 0.8d0; a(29) = 5.0d0
  a(31) = 0.4d0; a(32) = 8.0d0; a(33) = 7.0d0; a(34) = 0.6d0; a(35) = 0.9d0; a(36) = 6.0d0
  h = 0.0d0
  work = 0.0d0
  ipiv = 0
  call dlasyf_aa('U', 1, 6, 3, a, 6, ipiv, h, 6, work)
  call begin_test('upper_pivot_m6_nb3')
  call print_array('a', a, 36)
  call print_int_array('ipiv', ipiv, 6)
  call end_test()

  ! ============================================================
  ! Test 13: Lower, M=5, NB=3, J1=2 (subsequent panel) with pivot swaps
  ! ============================================================
  a = 0.0d0
  ! col 0 (previous panel last col)
  a(1) = 0.1d0; a(2) = 0.2d0; a(3) = 0.3d0; a(4) = 0.1d0; a(5) = 0.2d0
  ! col 1 (large at i=4)
  a(7) = 0.0d0;  a(8) = 1.0d0; a(9) = 0.5d0; a(10) = 0.3d0; a(11) = 9.0d0; a(12) = 0.2d0
  ! col 2 large at i=4
  a(14) = 2.0d0; a(15) = 0.6d0; a(16) = 0.4d0; a(17) = 8.0d0
  ! col 3
  a(21) = 3.0d0; a(22) = 0.7d0; a(23) = 0.5d0
  ! col 4
  a(28) = 4.0d0; a(29) = 0.8d0
  ! col 5
  a(35) = 5.0d0
  h = 0.0d0
  work = 0.0d0
  ipiv = 0
  call dlasyf_aa('L', 2, 5, 3, a, 6, ipiv, h, 6, work)
  call begin_test('lower_pivot_j1_2_m5_nb3')
  call print_array('a', a, 36)
  call print_int_array('ipiv', ipiv, 5)
  call end_test()

  ! ============================================================
  ! Test 14: Upper, M=5, NB=3, J1=2 with pivot swaps
  ! ============================================================
  a = 0.0d0
  a(1) = 0.1d0
  a(7) = 0.2d0; a(8) = 1.0d0
  a(13) = 0.3d0; a(14) = 0.5d0; a(15) = 2.0d0
  a(19) = 0.1d0; a(20) = 0.3d0; a(21) = 0.6d0; a(22) = 3.0d0
  a(25) = 0.2d0; a(26) = 9.0d0; a(27) = 8.0d0; a(28) = 0.7d0; a(29) = 4.0d0
  a(31) = 0.0d0; a(32) = 0.2d0; a(33) = 0.4d0; a(34) = 0.5d0; a(35) = 0.8d0; a(36) = 5.0d0
  h = 0.0d0
  work = 0.0d0
  ipiv = 0
  call dlasyf_aa('U', 2, 5, 3, a, 6, ipiv, h, 6, work)
  call begin_test('upper_pivot_j1_2_m5_nb3')
  call print_array('a', a, 36)
  call print_int_array('ipiv', ipiv, 5)
  call end_test()

  ! ============================================================
  ! Test 15: Lower, M=8, NB=4, pivot at i2=M (boundary case)
  ! Large element at the very end of the column
  ! ============================================================
  a = 0.0d0
  ! Make 8x8 lower, NB=4. col 1: large at i=8 -> i2=M
  a(1) = 1.0d0; a(2) = 0.5d0; a(3) = 0.3d0; a(4) = 0.2d0
  a(5) = 0.1d0; a(6) = 0.15d0; a(7) = 0.25d0; a(8) = 9.0d0
  ! col 2
  a(10) = 2.0d0; a(11) = 0.6d0; a(12) = 0.4d0
  a(13) = 0.2d0; a(14) = 0.18d0; a(15) = 0.28d0; a(16) = 5.0d0
  ! col 3
  a(19) = 3.0d0; a(20) = 0.7d0
  a(21) = 0.3d0; a(22) = 0.22d0; a(23) = 0.32d0; a(24) = 4.0d0
  ! col 4
  a(28) = 4.0d0
  a(29) = 0.4d0; a(30) = 0.26d0; a(31) = 0.36d0; a(32) = 3.0d0
  ! cols 5-8 (only diagonal needed for trailing)
  a(37) = 5.0d0; a(46) = 6.0d0; a(55) = 7.0d0; a(64) = 8.0d0
  h = 0.0d0
  work = 0.0d0
  ipiv = 0
  call dlasyf_aa('L', 1, 8, 4, a, 8, ipiv, h, 8, work)
  call begin_test('lower_pivot_at_m_m8_nb4')
  call print_array('a', a, 64)
  call print_int_array('ipiv', ipiv, 8)
  call end_test()

  ! ============================================================
  ! Test 16b: Lower, M=7, NB=3, pivot at i2 STRICTLY less than M
  ! Initialize H(1:M, 1) to a column with largest |.| at an interior row
  ! to force i2 != 2 AND i2 < M on the first iteration.
  ! ============================================================
  a = 0.0d0
  ! col 1
  a(1) = 1.0d0; a(2) = 0.5d0; a(3) = 0.3d0; a(4) = 9.0d0
  a(5) = 0.2d0; a(6) = 0.1d0; a(7) = 0.05d0
  ! col 2
  a(9) = 2.0d0; a(10) = 0.6d0; a(11) = 0.4d0; a(12) = 0.3d0
  a(13) = 0.25d0; a(14) = 0.18d0
  ! col 3
  a(17) = 3.0d0; a(18) = 0.7d0; a(19) = 0.5d0
  a(20) = 0.4d0; a(21) = 0.32d0
  ! col 4
  a(25) = 4.0d0; a(26) = 0.8d0; a(27) = 0.6d0; a(28) = 0.45d0
  ! col 5
  a(33) = 5.0d0; a(34) = 0.9d0; a(35) = 0.55d0
  ! col 6
  a(41) = 6.0d0; a(42) = 0.65d0
  ! col 7
  a(49) = 7.0d0
  h = 0.0d0
  ! Initialize H(1:7, 1) = A(1:7, 1):
  h(1) = 1.0d0; h(2) = 0.5d0; h(3) = 0.3d0; h(4) = 9.0d0
  h(5) = 0.2d0; h(6) = 0.1d0; h(7) = 0.05d0
  work = 0.0d0
  ipiv = 0
  call dlasyf_aa('L', 1, 7, 3, a, 7, ipiv, h, 7, work)
  call begin_test('lower_pivot_mid_m7_nb3')
  call print_array('a', a, 49)
  call print_int_array('ipiv', ipiv, 7)
  call end_test()

  ! ============================================================
  ! Test 16c: Upper mirror of 16b
  ! ============================================================
  a = 0.0d0
  a(1) = 1.0d0
  a(8) = 0.5d0; a(9) = 2.0d0
  a(15) = 0.3d0; a(16) = 0.6d0; a(17) = 3.0d0
  a(22) = 9.0d0; a(23) = 0.4d0; a(24) = 0.7d0; a(25) = 4.0d0
  a(29) = 0.2d0; a(30) = 0.3d0; a(31) = 0.5d0; a(32) = 0.8d0; a(33) = 5.0d0
  a(36) = 0.1d0; a(37) = 0.25d0; a(38) = 0.4d0; a(39) = 0.6d0; a(40) = 0.9d0; a(41) = 6.0d0
  a(43) = 0.05d0; a(44) = 0.18d0; a(45) = 0.32d0; a(46) = 0.45d0; a(47) = 0.55d0; a(48) = 0.65d0; a(49) = 7.0d0
  h = 0.0d0
  ! Initialize H(1:7, 1) = A(1, 1:7) (a row of A):
  h(1) = 1.0d0; h(2) = 0.5d0; h(3) = 0.3d0; h(4) = 9.0d0
  h(5) = 0.2d0; h(6) = 0.1d0; h(7) = 0.05d0
  work = 0.0d0
  ipiv = 0
  call dlasyf_aa('U', 1, 7, 3, a, 7, ipiv, h, 7, work)
  call begin_test('upper_pivot_mid_m7_nb3')
  call print_array('a', a, 49)
  call print_int_array('ipiv', ipiv, 7)
  call end_test()

  ! ============================================================
  ! Test 16: Upper, M=8, NB=4, pivot at i2=M (boundary case)
  ! ============================================================
  a = 0.0d0
  ! Mirror of test 15.
  a(1) = 1.0d0
  a(9) = 0.5d0; a(10) = 2.0d0
  a(17) = 0.3d0; a(18) = 0.6d0; a(19) = 3.0d0
  a(25) = 0.2d0; a(26) = 0.4d0; a(27) = 0.7d0; a(28) = 4.0d0
  a(33) = 0.1d0; a(34) = 0.2d0; a(35) = 0.3d0; a(36) = 0.4d0; a(37) = 5.0d0
  a(41) = 0.15d0; a(42) = 0.18d0; a(43) = 0.22d0; a(44) = 0.26d0
  a(45) = 0.0d0; a(46) = 6.0d0
  a(49) = 0.25d0; a(50) = 0.28d0; a(51) = 0.32d0; a(52) = 0.36d0
  a(53) = 0.0d0; a(54) = 0.0d0; a(55) = 7.0d0
  a(57) = 9.0d0; a(58) = 5.0d0; a(59) = 4.0d0; a(60) = 3.0d0
  a(61) = 0.0d0; a(62) = 0.0d0; a(63) = 0.0d0; a(64) = 8.0d0
  h = 0.0d0
  work = 0.0d0
  ipiv = 0
  call dlasyf_aa('U', 1, 8, 4, a, 8, ipiv, h, 8, work)
  call begin_test('upper_pivot_at_m_m8_nb4')
  call print_array('a', a, 64)
  call print_int_array('ipiv', ipiv, 8)
  call end_test()

end program
