program test_zgbtf2
  use test_utils
  implicit none
  ! Using EQUIVALENCE to print interleaved re/im pairs
  double precision :: AB4_r(80), AB7_r(140), AB5_r(100), AB1_r(10), AB2_r(40)
  complex*16 :: AB4(4, 10), AB7(7, 10), AB5(5, 10), AB1(1, 10), AB2(2, 10)
  equivalence (AB4, AB4_r)
  equivalence (AB7, AB7_r)
  equivalence (AB5, AB5_r)
  equivalence (AB1, AB1_r)
  equivalence (AB2, AB2_r)
  integer :: ipiv(10), info

  ! ============================================================
  ! Test 1: 4x4 tridiagonal (KL=1, KU=1), LDAB=4
  ! Full A = [4+i  -1     0     0   ]
  !          [-1    4+i   -1     0   ]
  !          [ 0   -1      4+i  -1   ]
  !          [ 0    0     -1     4+i ]
  ! KV = KU+KL = 2, diagonal at row KV+1 = 3 (1-based)
  AB4 = (0.0d0, 0.0d0)
  AB4(2,1) = (0.0d0,0.0d0);  AB4(3,1) = (4.0d0,1.0d0);  AB4(4,1) = (-1.0d0,0.0d0)
  AB4(2,2) = (-1.0d0,0.0d0); AB4(3,2) = (4.0d0,1.0d0);  AB4(4,2) = (-1.0d0,0.0d0)
  AB4(2,3) = (-1.0d0,0.0d0); AB4(3,3) = (4.0d0,1.0d0);  AB4(4,3) = (-1.0d0,0.0d0)
  AB4(2,4) = (-1.0d0,0.0d0); AB4(3,4) = (4.0d0,1.0d0);  AB4(4,4) = (0.0d0,0.0d0)
  ipiv = 0
  call ZGBTF2(4, 4, 1, 1, AB4, 4, ipiv, info)
  call begin_test('tridiag_4x4')
  call print_array('AB', AB4_r, 4*4*2)
  call print_int_array('ipiv', ipiv, 4)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 2: 3x3 with KL=1, KU=2 (non-square bandwidth), LDAB=5
  ! Full A = [5+i   3     1+i ]
  !          [2     6+i   4   ]
  !          [0     1     7+i ]
  ! KV = KU+KL = 3, diagonal at row KV+1 = 4 (1-based)
  AB5 = (0.0d0, 0.0d0)
  ! Col 1
  AB5(4,1) = (5.0d0,1.0d0); AB5(5,1) = (2.0d0,0.0d0)
  ! Col 2
  AB5(3,2) = (3.0d0,0.0d0); AB5(4,2) = (6.0d0,1.0d0); AB5(5,2) = (1.0d0,0.0d0)
  ! Col 3
  AB5(2,3) = (1.0d0,1.0d0); AB5(3,3) = (4.0d0,0.0d0); AB5(4,3) = (7.0d0,1.0d0)
  ipiv = 0
  call ZGBTF2(3, 3, 1, 2, AB5, 5, ipiv, info)
  call begin_test('kl1_ku2_3x3')
  call print_array('AB', AB5_r, 5*3*2)
  call print_int_array('ipiv', ipiv, 3)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 3: N=0 quick return
  call ZGBTF2(3, 0, 1, 1, AB4, 4, ipiv, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 4: M=0 quick return
  call ZGBTF2(0, 3, 1, 1, AB4, 4, ipiv, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 5: 1x1, KL=0, KU=0, LDAB=1
  AB1 = (0.0d0, 0.0d0)
  AB1(1,1) = (7.0d0, 2.0d0)
  ipiv = 0
  call ZGBTF2(1, 1, 0, 0, AB1, 1, ipiv, info)
  call begin_test('one_by_one')
  call print_array('AB', AB1_r, 2)
  call print_int_array('ipiv', ipiv, 1)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 6: Singular matrix, KL=0, KU=1, LDAB=2
  ! Full A = [0  1+i; 0  0]
  AB2 = (0.0d0, 0.0d0)
  AB2(1,1) = (0.0d0,0.0d0); AB2(1,2) = (1.0d0,1.0d0)
  AB2(2,1) = (0.0d0,0.0d0); AB2(2,2) = (0.0d0,0.0d0)
  ipiv = 0
  call ZGBTF2(2, 2, 0, 1, AB2, 2, ipiv, info)
  call begin_test('singular')
  call print_int_array('ipiv', ipiv, 2)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 7: 5x3 tall matrix (M > N), KL=1, KU=1, LDAB=4
  AB4 = (0.0d0, 0.0d0)
  AB4(3,1) = (4.0d0,1.0d0); AB4(4,1) = (-1.0d0,0.0d0)
  AB4(2,2) = (-1.0d0,0.0d0); AB4(3,2) = (4.0d0,1.0d0); AB4(4,2) = (-1.0d0,0.0d0)
  AB4(2,3) = (-1.0d0,0.0d0); AB4(3,3) = (4.0d0,1.0d0); AB4(4,3) = (-1.0d0,0.0d0)
  ipiv = 0
  call ZGBTF2(5, 3, 1, 1, AB4, 4, ipiv, info)
  call begin_test('tall_5x3')
  call print_array('AB', AB4_r, 4*3*2)
  call print_int_array('ipiv', ipiv, 3)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 8: Pivoting test - force a pivot swap
  ! Full A = [1+0i  2+0i; 3+0i  4+0i], KL=1, KU=1, LDAB=4
  AB4 = (0.0d0, 0.0d0)
  AB4(3,1) = (1.0d0,0.0d0); AB4(4,1) = (3.0d0,0.0d0)
  AB4(2,2) = (2.0d0,0.0d0); AB4(3,2) = (4.0d0,0.0d0)
  ipiv = 0
  call ZGBTF2(2, 2, 1, 1, AB4, 4, ipiv, info)
  call begin_test('pivot_2x2')
  call print_array('AB', AB4_r, 4*2*2)
  call print_int_array('ipiv', ipiv, 2)
  call print_int('info', info)
  call end_test()

end program
