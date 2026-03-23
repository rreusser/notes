program test_dgbtrf
  use test_utils
  implicit none
  double precision :: AB4(4, 10), AB7(7, 10), AB5(5, 10), AB1(1, 10)
  integer :: ipiv(10), info

  ! ============================================================
  ! Test 1: 4x4 tridiagonal (KL=1, KU=1), LDAB=4
  ! Same as dgbtf2 test - should get same result (NB > KL so falls through)
  AB4 = 0.0d0
  AB4(2,1) = 0.0d0;  AB4(3,1) = 4.0d0;  AB4(4,1) = -1.0d0
  AB4(2,2) = -1.0d0; AB4(3,2) = 4.0d0;  AB4(4,2) = -1.0d0
  AB4(2,3) = -1.0d0; AB4(3,3) = 4.0d0;  AB4(4,3) = -1.0d0
  AB4(2,4) = -1.0d0; AB4(3,4) = 4.0d0;  AB4(4,4) = 0.0d0
  ipiv = 0
  call DGBTRF(4, 4, 1, 1, AB4, 4, ipiv, info)
  call begin_test('tridiag_4x4')
  call print_matrix('AB', AB4, 4, 4, 4)
  call print_int_array('ipiv', ipiv, 4)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 2: 5x5 pentadiagonal (KL=2, KU=2), LDAB=7
  AB7 = 0.0d0
  AB7(5,1) = 6.0d0; AB7(6,1) = -2.0d0; AB7(7,1) = 1.0d0
  AB7(4,2) = -2.0d0; AB7(5,2) = 6.0d0; AB7(6,2) = -2.0d0; AB7(7,2) = 1.0d0
  AB7(3,3) = 1.0d0; AB7(4,3) = -2.0d0; AB7(5,3) = 6.0d0; AB7(6,3) = -2.0d0; AB7(7,3) = 1.0d0
  AB7(3,4) = 1.0d0; AB7(4,4) = -2.0d0; AB7(5,4) = 6.0d0; AB7(6,4) = -2.0d0
  AB7(3,5) = 1.0d0; AB7(4,5) = -2.0d0; AB7(5,5) = 6.0d0
  ipiv = 0
  call DGBTRF(5, 5, 2, 2, AB7, 7, ipiv, info)
  call begin_test('pentadiag_5x5')
  call print_matrix('AB', AB7, 7, 7, 5)
  call print_int_array('ipiv', ipiv, 5)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 3: N=0 quick return
  call DGBTRF(3, 0, 1, 1, AB4, 4, ipiv, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 4: M=0 quick return
  call DGBTRF(0, 3, 1, 1, AB4, 4, ipiv, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 5: 1x1
  AB1 = 0.0d0
  AB1(1,1) = 7.0d0
  ipiv = 0
  call DGBTRF(1, 1, 0, 0, AB1, 1, ipiv, info)
  call begin_test('one_by_one')
  call print_matrix('AB', AB1, 1, 1, 1)
  call print_int_array('ipiv', ipiv, 1)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 6: Pivot test 2x2
  AB4 = 0.0d0
  AB4(3,1) = 1.0d0; AB4(4,1) = 3.0d0
  AB4(2,2) = 2.0d0; AB4(3,2) = 4.0d0
  ipiv = 0
  call DGBTRF(2, 2, 1, 1, AB4, 4, ipiv, info)
  call begin_test('pivot_2x2')
  call print_matrix('AB', AB4, 4, 4, 2)
  call print_int_array('ipiv', ipiv, 2)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 7: 3x3 with KL=1, KU=2 (non-square bandwidth), LDAB=5
  AB5 = 0.0d0
  AB5(4,1) = 5.0d0; AB5(5,1) = 2.0d0
  AB5(3,2) = 3.0d0; AB5(4,2) = 6.0d0; AB5(5,2) = 1.0d0
  AB5(2,3) = 1.0d0; AB5(3,3) = 4.0d0; AB5(4,3) = 7.0d0
  ipiv = 0
  call DGBTRF(3, 3, 1, 2, AB5, 5, ipiv, info)
  call begin_test('kl1_ku2_3x3')
  call print_matrix('AB', AB5, 5, 5, 3)
  call print_int_array('ipiv', ipiv, 3)
  call print_int('info', info)
  call end_test()

end program
