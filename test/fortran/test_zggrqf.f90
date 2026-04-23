program test_zggrqf
  use test_utils
  implicit none

  ! Test 1: 3x3 / 3x3 (square, M=3, P=3, N=3)
  complex*16 :: A33(3,3), B33(3,3), TAUA33(3), TAUB33(3), WORK(10000)
  double precision :: A33_r(18), B33_r(18), TAUA33_r(6), TAUB33_r(6)
  equivalence (A33, A33_r)
  equivalence (B33, B33_r)
  equivalence (TAUA33, TAUA33_r)
  equivalence (TAUB33, TAUB33_r)

  ! Test 2: M < N: M=2, P=3, N=4 (A is 2x4, B is 3x4)
  complex*16 :: A24(2,4), TAUA24(2)
  double precision :: A24_r(16), TAUA24_r(4)
  equivalence (A24, A24_r)
  equivalence (TAUA24, TAUA24_r)

  complex*16 :: B34(3,4), TAUB34(3)
  double precision :: B34_r(24), TAUB34_r(6)
  equivalence (B34, B34_r)
  equivalence (TAUB34, TAUB34_r)

  ! Test 3: M > N: M=4, P=3, N=3 (A is 4x3, B is 3x3)
  complex*16 :: A43(4,3), TAUA43(3)
  double precision :: A43_r(24), TAUA43_r(6)
  equivalence (A43, A43_r)
  equivalence (TAUA43, TAUA43_r)

  complex*16 :: B33b(3,3), TAUB33b(3)
  double precision :: B33b_r(18), TAUB33b_r(6)
  equivalence (B33b, B33b_r)
  equivalence (TAUB33b, TAUB33b_r)

  ! Test 5: 1x1 / 1x1 (minimal, M=1, P=1, N=1)
  complex*16 :: A11(1,1), B11(1,1), TAUA11(1), TAUB11(1)
  double precision :: A11_r(2), B11_r(2), TAUA11_r(2), TAUB11_r(2)
  equivalence (A11, A11_r)
  equivalence (B11, B11_r)
  equivalence (TAUA11, TAUA11_r)
  equivalence (TAUB11, TAUB11_r)

  ! Test 6: Wide-short: M=2, P=5, N=5 (A is 2x5, B is 5x5)
  complex*16 :: A25(2,5), TAUA25(2)
  double precision :: A25_r(20), TAUA25_r(4)
  equivalence (A25, A25_r)
  equivalence (TAUA25, TAUA25_r)

  complex*16 :: B55(5,5), TAUB55(5)
  double precision :: B55_r(50), TAUB55_r(10)
  equivalence (B55, B55_r)
  equivalence (TAUB55, TAUB55_r)

  integer :: info

  ! ---------------------------------------------------------------
  ! Test 1: Basic 3x3, A(3x3) and B(3x3), M=3, P=3, N=3
  ! ---------------------------------------------------------------
  A33 = (0.0d0, 0.0d0); B33 = (0.0d0, 0.0d0)
  TAUA33 = (0.0d0, 0.0d0); TAUB33 = (0.0d0, 0.0d0)
  A33(1,1) = (2.0d0, 1.0d0); A33(1,2) = (1.0d0, 2.0d0); A33(1,3) = (3.0d0, 0.0d0)
  A33(2,1) = (1.0d0, 0.0d0); A33(2,2) = (4.0d0, 1.0d0); A33(2,3) = (2.0d0, -1.0d0)
  A33(3,1) = (3.0d0, -1.0d0); A33(3,2) = (2.0d0, 0.0d0); A33(3,3) = (5.0d0, 2.0d0)
  B33(1,1) = (1.0d0, 0.5d0); B33(1,2) = (2.0d0, 1.0d0); B33(1,3) = (1.0d0, -1.0d0)
  B33(2,1) = (3.0d0, 0.0d0); B33(2,2) = (1.0d0, -1.0d0); B33(2,3) = (2.0d0, 0.5d0)
  B33(3,1) = (2.0d0, 1.0d0); B33(3,2) = (3.0d0, 0.0d0); B33(3,3) = (1.0d0, 1.0d0)
  call ZGGRQF(3, 3, 3, A33, 3, TAUA33, B33, 3, TAUB33, WORK, 10000, info)
  call begin_test('basic_3x3')
  call print_int('info', info)
  call print_array('A', A33_r, 2*3*3)
  call print_array('TAUA', TAUA33_r, 2*3)
  call print_array('B', B33_r, 2*3*3)
  call print_array('TAUB', TAUB33_r, 2*3)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 2: M < N: M=2, P=3, N=4 (A is 2x4, B is 3x4)
  ! ---------------------------------------------------------------
  A24 = (0.0d0, 0.0d0); B34 = (0.0d0, 0.0d0)
  TAUA24 = (0.0d0, 0.0d0); TAUB34 = (0.0d0, 0.0d0)
  A24(1,1) = (2.0d0, 1.0d0); A24(1,2) = (1.0d0, 2.0d0); A24(1,3) = (3.0d0, 0.0d0); A24(1,4) = (1.0d0, 1.0d0)
  A24(2,1) = (1.0d0, 0.0d0); A24(2,2) = (4.0d0, 1.0d0); A24(2,3) = (2.0d0, -1.0d0); A24(2,4) = (3.0d0, 0.0d0)
  B34(1,1) = (1.0d0, 0.5d0); B34(1,2) = (2.0d0, 1.0d0); B34(1,3) = (1.0d0, -1.0d0); B34(1,4) = (3.0d0, 0.0d0)
  B34(2,1) = (3.0d0, 0.0d0); B34(2,2) = (1.0d0, -1.0d0); B34(2,3) = (2.0d0, 0.5d0); B34(2,4) = (1.0d0, 1.0d0)
  B34(3,1) = (2.0d0, 1.0d0); B34(3,2) = (3.0d0, 0.0d0); B34(3,3) = (1.0d0, 1.0d0); B34(3,4) = (2.0d0, -1.0d0)
  call ZGGRQF(2, 3, 4, A24, 2, TAUA24, B34, 3, TAUB34, WORK, 10000, info)
  call begin_test('m_lt_n')
  call print_int('info', info)
  call print_array('A', A24_r, 2*2*4)
  call print_array('TAUA', TAUA24_r, 2*2)
  call print_array('B', B34_r, 2*3*4)
  call print_array('TAUB', TAUB34_r, 2*3)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 3: M > N: M=4, P=3, N=3 (A is 4x3, B is 3x3)
  ! ---------------------------------------------------------------
  A43 = (0.0d0, 0.0d0); B33b = (0.0d0, 0.0d0)
  TAUA43 = (0.0d0, 0.0d0); TAUB33b = (0.0d0, 0.0d0)
  A43(1,1) = (2.0d0, 1.0d0); A43(1,2) = (1.0d0, 2.0d0); A43(1,3) = (3.0d0, 0.0d0)
  A43(2,1) = (1.0d0, 0.0d0); A43(2,2) = (4.0d0, 1.0d0); A43(2,3) = (2.0d0, -1.0d0)
  A43(3,1) = (3.0d0, -1.0d0); A43(3,2) = (2.0d0, 0.0d0); A43(3,3) = (5.0d0, 2.0d0)
  A43(4,1) = (1.0d0, 1.0d0); A43(4,2) = (3.0d0, -1.0d0); A43(4,3) = (1.0d0, 0.5d0)
  B33b(1,1) = (1.0d0, 0.5d0); B33b(1,2) = (2.0d0, 1.0d0); B33b(1,3) = (1.0d0, -1.0d0)
  B33b(2,1) = (3.0d0, 0.0d0); B33b(2,2) = (1.0d0, -1.0d0); B33b(2,3) = (2.0d0, 0.5d0)
  B33b(3,1) = (2.0d0, 1.0d0); B33b(3,2) = (3.0d0, 0.0d0); B33b(3,3) = (1.0d0, 1.0d0)
  call ZGGRQF(4, 3, 3, A43, 4, TAUA43, B33b, 3, TAUB33b, WORK, 10000, info)
  call begin_test('m_gt_n')
  call print_int('info', info)
  call print_array('A', A43_r, 2*4*3)
  call print_array('TAUA', TAUA43_r, 2*3)
  call print_array('B', B33b_r, 2*3*3)
  call print_array('TAUB', TAUB33b_r, 2*3)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 4: M=0 (quick return)
  ! ---------------------------------------------------------------
  call ZGGRQF(0, 3, 3, A33, 3, TAUA33, B33, 3, TAUB33, WORK, 10000, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 5: M=1, P=1, N=1 (minimal)
  ! ---------------------------------------------------------------
  A11 = (0.0d0, 0.0d0); B11 = (0.0d0, 0.0d0)
  TAUA11 = (0.0d0, 0.0d0); TAUB11 = (0.0d0, 0.0d0)
  A11(1,1) = (5.0d0, 2.0d0)
  B11(1,1) = (3.0d0, -1.0d0)
  call ZGGRQF(1, 1, 1, A11, 1, TAUA11, B11, 1, TAUB11, WORK, 10000, info)
  call begin_test('m_one')
  call print_int('info', info)
  call print_array('A', A11_r, 2*1*1)
  call print_array('TAUA', TAUA11_r, 2*1)
  call print_array('B', B11_r, 2*1*1)
  call print_array('TAUB', TAUB11_r, 2*1)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 6: Wide-short: M=2, P=5, N=5 (A is 2x5, B is 5x5)
  ! ---------------------------------------------------------------
  A25 = (0.0d0, 0.0d0); B55 = (0.0d0, 0.0d0)
  TAUA25 = (0.0d0, 0.0d0); TAUB55 = (0.0d0, 0.0d0)
  A25(1,1) = (1.0d0, 0.5d0); A25(1,2) = (2.0d0, 1.0d0); A25(1,3) = (3.0d0, 0.0d0); A25(1,4) = (1.0d0, -1.0d0); A25(1,5) = (2.0d0, 0.0d0)
  A25(2,1) = (3.0d0, 0.0d0); A25(2,2) = (1.0d0, -1.0d0); A25(2,3) = (2.0d0, 0.5d0); A25(2,4) = (1.0d0, 1.0d0); A25(2,5) = (0.5d0, 0.0d0)
  B55(1,1) = (1.0d0, 0.5d0); B55(1,2) = (0.5d0, 1.0d0); B55(1,3) = (2.0d0, 0.0d0); B55(1,4) = (1.0d0, 0.0d0); B55(1,5) = (0.5d0, -0.5d0)
  B55(2,1) = (0.5d0, 0.0d0); B55(2,2) = (3.0d0, -1.0d0); B55(2,3) = (1.0d0, 0.5d0); B55(2,4) = (2.0d0, 0.0d0); B55(2,5) = (1.0d0, 1.0d0)
  B55(3,1) = (2.0d0, 1.0d0); B55(3,2) = (1.0d0, 0.0d0); B55(3,3) = (1.0d0, -1.0d0); B55(3,4) = (0.5d0, 0.5d0); B55(3,5) = (3.0d0, 0.0d0)
  B55(4,1) = (1.0d0, -1.0d0); B55(4,2) = (2.0d0, 0.5d0); B55(4,3) = (0.5d0, 0.0d0); B55(4,4) = (1.0d0, 1.0d0); B55(4,5) = (2.0d0, -0.5d0)
  B55(5,1) = (3.0d0, 0.0d0); B55(5,2) = (1.0d0, 1.0d0); B55(5,3) = (2.0d0, -0.5d0); B55(5,4) = (0.5d0, 0.0d0); B55(5,5) = (1.0d0, 0.5d0)
  call ZGGRQF(2, 5, 5, A25, 2, TAUA25, B55, 5, TAUB55, WORK, 10000, info)
  call begin_test('wide_short')
  call print_int('info', info)
  call print_array('A', A25_r, 2*2*5)
  call print_array('TAUA', TAUA25_r, 2*2)
  call print_array('B', B55_r, 2*5*5)
  call print_array('TAUB', TAUB55_r, 2*5)
  call end_test()

end program
