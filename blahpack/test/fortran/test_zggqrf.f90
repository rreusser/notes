program test_zggqrf
  use test_utils
  implicit none

  ! Test 1: 3x3 / 3x3 (square)
  complex*16 :: A33(3,3), B33(3,3), TAUA33(3), TAUB33(3), WORK(10000)
  double precision :: A33_r(18), B33_r(18), TAUA33_r(6), TAUB33_r(6)
  equivalence (A33, A33_r)
  equivalence (B33, B33_r)
  equivalence (TAUA33, TAUA33_r)
  equivalence (TAUB33, TAUB33_r)

  ! Test 2: 3x4 / 3x3 (M > N)
  complex*16 :: A34(3,4), TAUA34(3)
  double precision :: A34_r(24), TAUA34_r(6)
  equivalence (A34, A34_r)
  equivalence (TAUA34, TAUA34_r)

  complex*16 :: B33b(3,3), TAUB33b(3)
  double precision :: B33b_r(18), TAUB33b_r(6)
  equivalence (B33b, B33b_r)
  equivalence (TAUB33b, TAUB33b_r)

  ! Test 3: 4x3 / 4x4 (M < N)
  complex*16 :: A43(4,3), TAUA43(3)
  double precision :: A43_r(24), TAUA43_r(6)
  equivalence (A43, A43_r)
  equivalence (TAUA43, TAUA43_r)

  complex*16 :: B44(4,4), TAUB44(4)
  double precision :: B44_r(32), TAUB44_r(8)
  equivalence (B44, B44_r)
  equivalence (TAUB44, TAUB44_r)

  ! Test 5: 1x1 / 1x1 (minimal)
  complex*16 :: A11(1,1), B11(1,1), TAUA11(1), TAUB11(1)
  double precision :: A11_r(2), B11_r(2), TAUA11_r(2), TAUB11_r(2)
  equivalence (A11, A11_r)
  equivalence (B11, B11_r)
  equivalence (TAUA11, TAUA11_r)
  equivalence (TAUB11, TAUB11_r)

  ! Test 6: 5x2 / 5x3 (tall-skinny)
  complex*16 :: A52(5,2), TAUA52(2)
  double precision :: A52_r(20), TAUA52_r(4)
  equivalence (A52, A52_r)
  equivalence (TAUA52, TAUA52_r)

  complex*16 :: B53(5,3), TAUB53(3)
  double precision :: B53_r(30), TAUB53_r(6)
  equivalence (B53, B53_r)
  equivalence (TAUB53, TAUB53_r)

  integer :: info

  ! ---------------------------------------------------------------
  ! Test 1: Basic 3x3, A and B both 3x3
  ! ---------------------------------------------------------------
  A33 = (0.0d0, 0.0d0); B33 = (0.0d0, 0.0d0)
  TAUA33 = (0.0d0, 0.0d0); TAUB33 = (0.0d0, 0.0d0)
  A33(1,1) = (2.0d0, 1.0d0); A33(1,2) = (1.0d0, 2.0d0); A33(1,3) = (3.0d0, 0.0d0)
  A33(2,1) = (1.0d0, 0.0d0); A33(2,2) = (4.0d0, 1.0d0); A33(2,3) = (2.0d0, -1.0d0)
  A33(3,1) = (3.0d0, -1.0d0); A33(3,2) = (2.0d0, 0.0d0); A33(3,3) = (5.0d0, 2.0d0)
  B33(1,1) = (1.0d0, 0.5d0); B33(1,2) = (2.0d0, 1.0d0); B33(1,3) = (1.0d0, -1.0d0)
  B33(2,1) = (3.0d0, 0.0d0); B33(2,2) = (1.0d0, -1.0d0); B33(2,3) = (2.0d0, 0.5d0)
  B33(3,1) = (2.0d0, 1.0d0); B33(3,2) = (3.0d0, 0.0d0); B33(3,3) = (1.0d0, 1.0d0)
  call ZGGQRF(3, 3, 3, A33, 3, TAUA33, B33, 3, TAUB33, WORK, 10000, info)
  call begin_test('basic_3x3')
  call print_int('info', info)
  call print_array('A', A33_r, 2*3*3)
  call print_array('TAUA', TAUA33_r, 2*3)
  call print_array('B', B33_r, 2*3*3)
  call print_array('TAUB', TAUB33_r, 2*3)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 2: M > N: N=3, M=4, P=3
  ! ---------------------------------------------------------------
  A34 = (0.0d0, 0.0d0); B33b = (0.0d0, 0.0d0)
  TAUA34 = (0.0d0, 0.0d0); TAUB33b = (0.0d0, 0.0d0)
  A34(1,1) = (2.0d0, 1.0d0); A34(1,2) = (1.0d0, 2.0d0); A34(1,3) = (3.0d0, 0.0d0); A34(1,4) = (1.0d0, 1.0d0)
  A34(2,1) = (1.0d0, 0.0d0); A34(2,2) = (4.0d0, 1.0d0); A34(2,3) = (2.0d0, -1.0d0); A34(2,4) = (3.0d0, 0.0d0)
  A34(3,1) = (3.0d0, -1.0d0); A34(3,2) = (2.0d0, 0.0d0); A34(3,3) = (5.0d0, 2.0d0); A34(3,4) = (2.0d0, -2.0d0)
  B33b(1,1) = (1.0d0, 0.5d0); B33b(1,2) = (2.0d0, 1.0d0); B33b(1,3) = (1.0d0, -1.0d0)
  B33b(2,1) = (3.0d0, 0.0d0); B33b(2,2) = (1.0d0, -1.0d0); B33b(2,3) = (2.0d0, 0.5d0)
  B33b(3,1) = (2.0d0, 1.0d0); B33b(3,2) = (3.0d0, 0.0d0); B33b(3,3) = (1.0d0, 1.0d0)
  call ZGGQRF(3, 4, 3, A34, 3, TAUA34, B33b, 3, TAUB33b, WORK, 10000, info)
  call begin_test('m_gt_n')
  call print_int('info', info)
  call print_array('A', A34_r, 2*3*4)
  call print_array('TAUA', TAUA34_r, 2*3)
  call print_array('B', B33b_r, 2*3*3)
  call print_array('TAUB', TAUB33b_r, 2*3)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 3: M < N: N=4, M=3, P=4
  ! ---------------------------------------------------------------
  A43 = (0.0d0, 0.0d0); B44 = (0.0d0, 0.0d0)
  TAUA43 = (0.0d0, 0.0d0); TAUB44 = (0.0d0, 0.0d0)
  A43(1,1) = (2.0d0, 1.0d0); A43(1,2) = (1.0d0, 2.0d0); A43(1,3) = (3.0d0, 0.0d0)
  A43(2,1) = (1.0d0, 0.0d0); A43(2,2) = (4.0d0, 1.0d0); A43(2,3) = (2.0d0, -1.0d0)
  A43(3,1) = (3.0d0, -1.0d0); A43(3,2) = (2.0d0, 0.0d0); A43(3,3) = (5.0d0, 2.0d0)
  A43(4,1) = (1.0d0, 1.0d0); A43(4,2) = (3.0d0, -1.0d0); A43(4,3) = (1.0d0, 0.5d0)
  B44(1,1) = (1.0d0, 0.5d0); B44(1,2) = (2.0d0, 1.0d0); B44(1,3) = (1.0d0, -1.0d0); B44(1,4) = (3.0d0, 0.0d0)
  B44(2,1) = (3.0d0, 0.0d0); B44(2,2) = (1.0d0, -1.0d0); B44(2,3) = (2.0d0, 0.5d0); B44(2,4) = (1.0d0, 1.0d0)
  B44(3,1) = (2.0d0, 1.0d0); B44(3,2) = (3.0d0, 0.0d0); B44(3,3) = (1.0d0, 1.0d0); B44(3,4) = (2.0d0, -1.0d0)
  B44(4,1) = (1.0d0, -0.5d0); B44(4,2) = (2.0d0, 1.0d0); B44(4,3) = (3.0d0, 0.0d0); B44(4,4) = (1.0d0, 2.0d0)
  call ZGGQRF(4, 3, 4, A43, 4, TAUA43, B44, 4, TAUB44, WORK, 10000, info)
  call begin_test('m_lt_n')
  call print_int('info', info)
  call print_array('A', A43_r, 2*4*3)
  call print_array('TAUA', TAUA43_r, 2*3)
  call print_array('B', B44_r, 2*4*4)
  call print_array('TAUB', TAUB44_r, 2*4)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 4: N=0 (quick return)
  ! ---------------------------------------------------------------
  call ZGGQRF(0, 3, 3, A33, 3, TAUA33, B33, 3, TAUB33, WORK, 10000, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 5: N=1, M=1, P=1 (minimal)
  ! ---------------------------------------------------------------
  A11 = (0.0d0, 0.0d0); B11 = (0.0d0, 0.0d0)
  TAUA11 = (0.0d0, 0.0d0); TAUB11 = (0.0d0, 0.0d0)
  A11(1,1) = (5.0d0, 2.0d0)
  B11(1,1) = (3.0d0, -1.0d0)
  call ZGGQRF(1, 1, 1, A11, 1, TAUA11, B11, 1, TAUB11, WORK, 10000, info)
  call begin_test('n_one')
  call print_int('info', info)
  call print_array('A', A11_r, 2*1*1)
  call print_array('TAUA', TAUA11_r, 2*1)
  call print_array('B', B11_r, 2*1*1)
  call print_array('TAUB', TAUB11_r, 2*1)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 6: Tall-skinny: N=5, M=2, P=3
  ! ---------------------------------------------------------------
  A52 = (0.0d0, 0.0d0); B53 = (0.0d0, 0.0d0)
  TAUA52 = (0.0d0, 0.0d0); TAUB53 = (0.0d0, 0.0d0)
  A52(1,1) = (1.0d0, 0.5d0); A52(1,2) = (2.0d0, 1.0d0)
  A52(2,1) = (3.0d0, 0.0d0); A52(2,2) = (1.0d0, -1.0d0)
  A52(3,1) = (2.0d0, 1.0d0); A52(3,2) = (3.0d0, 0.0d0)
  A52(4,1) = (1.0d0, -0.5d0); A52(4,2) = (1.0d0, 1.0d0)
  A52(5,1) = (2.0d0, 0.0d0); A52(5,2) = (2.0d0, -1.0d0)
  B53(1,1) = (1.0d0, 0.5d0); B53(1,2) = (0.5d0, 1.0d0); B53(1,3) = (2.0d0, 0.0d0)
  B53(2,1) = (0.5d0, 0.0d0); B53(2,2) = (3.0d0, -1.0d0); B53(2,3) = (1.0d0, 0.5d0)
  B53(3,1) = (2.0d0, 1.0d0); B53(3,2) = (1.0d0, 0.0d0); B53(3,3) = (1.0d0, -1.0d0)
  B53(4,1) = (1.0d0, -1.0d0); B53(4,2) = (2.0d0, 0.5d0); B53(4,3) = (0.5d0, 0.0d0)
  B53(5,1) = (3.0d0, 0.0d0); B53(5,2) = (1.0d0, 1.0d0); B53(5,3) = (2.0d0, -0.5d0)
  call ZGGQRF(5, 2, 3, A52, 5, TAUA52, B53, 5, TAUB53, WORK, 10000, info)
  call begin_test('tall_skinny')
  call print_int('info', info)
  call print_array('A', A52_r, 2*5*2)
  call print_array('TAUA', TAUA52_r, 2*2)
  call print_array('B', B53_r, 2*5*3)
  call print_array('TAUB', TAUB53_r, 2*3)
  call end_test()

end program
