program test_dgbtf2
  use test_utils
  implicit none
  double precision :: AB4(4, 10), AB7(7, 10), AB5(5, 10), AB1(1, 10), AB2(2, 10)
  integer :: ipiv(10), info

  ! ============================================================
  ! Test 1: 4x4 tridiagonal (KL=1, KU=1), LDAB=2*1+1+1=4
  ! Full A = [4 -1  0  0]
  !          [-1  4 -1  0]
  !          [ 0 -1  4 -1]
  !          [ 0  0 -1  4]
  ! Band storage (LDAB=4, KL=1, KU=1, KV=2):
  !   Row 1: fill-in
  !   Row 2: superdiag [*  -1  -1  -1]
  !   Row 3: diagonal  [4   4   4   4]
  !   Row 4: subdiag   [-1  -1  -1  *]
  AB4 = 0.0d0
  AB4(2,1) = 0.0d0;  AB4(3,1) = 4.0d0;  AB4(4,1) = -1.0d0
  AB4(2,2) = -1.0d0; AB4(3,2) = 4.0d0;  AB4(4,2) = -1.0d0
  AB4(2,3) = -1.0d0; AB4(3,3) = 4.0d0;  AB4(4,3) = -1.0d0
  AB4(2,4) = -1.0d0; AB4(3,4) = 4.0d0;  AB4(4,4) = 0.0d0
  ipiv = 0
  call DGBTF2(4, 4, 1, 1, AB4, 4, ipiv, info)
  call begin_test('tridiag_4x4')
  call print_matrix('AB', AB4, 4, 4, 4)
  call print_int_array('ipiv', ipiv, 4)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 2: 5x5 pentadiagonal (KL=2, KU=2), LDAB=2*2+2+1=7
  ! Well-conditioned pentadiagonal
  ! Full A:
  !   [6  -2   1   0   0]
  !   [-2   6  -2   1   0]
  !   [1   -2   6  -2   1]
  !   [0    1  -2   6  -2]
  !   [0    0   1  -2   6]
  ! KV = KU + KL = 4. Diagonal at row KV+1 = 5
  AB7 = 0.0d0
  ! Col 1
  AB7(5,1) = 6.0d0; AB7(6,1) = -2.0d0; AB7(7,1) = 1.0d0
  ! Col 2
  AB7(4,2) = -2.0d0; AB7(5,2) = 6.0d0; AB7(6,2) = -2.0d0; AB7(7,2) = 1.0d0
  ! Col 3
  AB7(3,3) = 1.0d0; AB7(4,3) = -2.0d0; AB7(5,3) = 6.0d0; AB7(6,3) = -2.0d0; AB7(7,3) = 1.0d0
  ! Col 4
  AB7(3,4) = 1.0d0; AB7(4,4) = -2.0d0; AB7(5,4) = 6.0d0; AB7(6,4) = -2.0d0
  ! Col 5
  AB7(3,5) = 1.0d0; AB7(4,5) = -2.0d0; AB7(5,5) = 6.0d0
  ipiv = 0
  call DGBTF2(5, 5, 2, 2, AB7, 7, ipiv, info)
  call begin_test('pentadiag_5x5')
  call print_matrix('AB', AB7, 7, 7, 5)
  call print_int_array('ipiv', ipiv, 5)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 3: 3x3 with KL=1, KU=2 (non-square bandwidth)
  ! Full A = [5 3 1; 2 6 4; 0 1 7]
  ! KV = KU+KL = 3, LDAB = KL+KV+1 = 5
  ! Diagonal at row KV+1 = 4
  AB5 = 0.0d0
  ! Col 1
  AB5(4,1) = 5.0d0; AB5(5,1) = 2.0d0
  ! Col 2
  AB5(3,2) = 3.0d0; AB5(4,2) = 6.0d0; AB5(5,2) = 1.0d0
  ! Col 3
  AB5(2,3) = 1.0d0; AB5(3,3) = 4.0d0; AB5(4,3) = 7.0d0
  ipiv = 0
  call DGBTF2(3, 3, 1, 2, AB5, 5, ipiv, info)
  call begin_test('kl1_ku2_3x3')
  call print_matrix('AB', AB5, 5, 5, 3)
  call print_int_array('ipiv', ipiv, 3)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 4: N=0 quick return
  call DGBTF2(3, 0, 1, 1, AB4, 4, ipiv, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 5: M=0 quick return
  call DGBTF2(0, 3, 1, 1, AB4, 4, ipiv, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 6: 1x1, KL=0, KU=0, LDAB=1
  AB1 = 0.0d0
  AB1(1,1) = 7.0d0
  ipiv = 0
  call DGBTF2(1, 1, 0, 0, AB1, 1, ipiv, info)
  call begin_test('one_by_one')
  call print_matrix('AB', AB1, 1, 1, 1)
  call print_int_array('ipiv', ipiv, 1)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 7: Singular matrix, KL=0, KU=1, LDAB=2
  ! Full A = [0 1; 0 0]
  AB2 = 0.0d0
  AB2(1,1) = 0.0d0; AB2(1,2) = 1.0d0  ! superdiag
  AB2(2,1) = 0.0d0; AB2(2,2) = 0.0d0  ! diagonal
  ipiv = 0
  call DGBTF2(2, 2, 0, 1, AB2, 2, ipiv, info)
  call begin_test('singular')
  call print_int_array('ipiv', ipiv, 2)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 8: 5x3 tall matrix (M > N), KL=1, KU=1, LDAB=4
  ! Full A (5x3):
  !   [4  -1   0]
  !   [-1  4  -1]
  !   [0  -1   4]
  !   [0   0  -1]
  !   [0   0   0]
  AB4 = 0.0d0
  AB4(3,1) = 4.0d0; AB4(4,1) = -1.0d0
  AB4(2,2) = -1.0d0; AB4(3,2) = 4.0d0; AB4(4,2) = -1.0d0
  AB4(2,3) = -1.0d0; AB4(3,3) = 4.0d0; AB4(4,3) = -1.0d0
  ipiv = 0
  call DGBTF2(5, 3, 1, 1, AB4, 4, ipiv, info)
  call begin_test('tall_5x3')
  call print_matrix('AB', AB4, 4, 4, 3)
  call print_int_array('ipiv', ipiv, 3)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 9: Pivoting test - force a pivot swap
  ! Full A = [1  2; 3  4], KL=1, KU=1, LDAB=4
  ! Row 0 should pivot to row 1 (since |3| > |1|)
  AB4 = 0.0d0
  AB4(3,1) = 1.0d0; AB4(4,1) = 3.0d0
  AB4(2,2) = 2.0d0; AB4(3,2) = 4.0d0
  ipiv = 0
  call DGBTF2(2, 2, 1, 1, AB4, 4, ipiv, info)
  call begin_test('pivot_2x2')
  call print_matrix('AB', AB4, 4, 4, 2)
  call print_int_array('ipiv', ipiv, 2)
  call print_int('info', info)
  call end_test()

end program
