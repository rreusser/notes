program test_ztbtrs
  use test_utils
  implicit none

  ! EQUIVALENCE to print interleaved re/im pairs
  double precision :: AB2_r(40), AB3_r(60), B_r(200)
  complex*16 :: AB2(2, 10), AB3(3, 10), B(10, 10)
  equivalence (AB2, AB2_r)
  equivalence (AB3, AB3_r)
  equivalence (B, B_r)
  integer :: info

  ! ============================================================
  ! Test 1: Upper, No transpose, Non-unit, N=3, KD=1, NRHS=1
  ! Band storage LDA=2:
  !   Row 1 (superdiag): [*        (1+i)     (2-i)]
  !   Row 2 (diag):      [(3+0i)   (4+i)     (5-i)]
  ! Full A: [3   1+i   0  ]
  !         [0   4+i   2-i]
  !         [0   0     5-i]
  AB2 = (0.0d0, 0.0d0)
  AB2(1,1) = (0.0d0,0.0d0); AB2(2,1) = (3.0d0,0.0d0)
  AB2(1,2) = (1.0d0,1.0d0); AB2(2,2) = (4.0d0,1.0d0)
  AB2(1,3) = (2.0d0,-1.0d0); AB2(2,3) = (5.0d0,-1.0d0)
  B = (0.0d0, 0.0d0)
  B(1,1) = (10.0d0,2.0d0); B(2,1) = (20.0d0,5.0d0); B(3,1) = (15.0d0,-3.0d0)
  call ZTBTRS('U', 'N', 'N', 3, 1, 1, AB2, 2, B, 10, info)
  call begin_test('upper_no_trans')
  call print_array('x', B_r, 6)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 2: Lower, No transpose, Non-unit, N=3, KD=1, NRHS=1
  ! Band storage LDA=2:
  !   Row 1 (diag):    [(2+i)    (3+0i)   (4-i)]
  !   Row 2 (subdiag): [(1+i)    (2-i)    *    ]
  AB2 = (0.0d0, 0.0d0)
  AB2(1,1) = (2.0d0,1.0d0); AB2(2,1) = (1.0d0,1.0d0)
  AB2(1,2) = (3.0d0,0.0d0); AB2(2,2) = (2.0d0,-1.0d0)
  AB2(1,3) = (4.0d0,-1.0d0); AB2(2,3) = (0.0d0,0.0d0)
  B = (0.0d0, 0.0d0)
  B(1,1) = (5.0d0,3.0d0); B(2,1) = (10.0d0,1.0d0); B(3,1) = (8.0d0,-2.0d0)
  call ZTBTRS('L', 'N', 'N', 3, 1, 1, AB2, 2, B, 10, info)
  call begin_test('lower_no_trans')
  call print_array('x', B_r, 6)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 3: Upper, Conjugate-transpose, Non-unit, N=3, KD=1, NRHS=1
  ! Same AB as Test 1
  AB2 = (0.0d0, 0.0d0)
  AB2(1,1) = (0.0d0,0.0d0); AB2(2,1) = (3.0d0,0.0d0)
  AB2(1,2) = (1.0d0,1.0d0); AB2(2,2) = (4.0d0,1.0d0)
  AB2(1,3) = (2.0d0,-1.0d0); AB2(2,3) = (5.0d0,-1.0d0)
  B = (0.0d0, 0.0d0)
  B(1,1) = (10.0d0,2.0d0); B(2,1) = (20.0d0,5.0d0); B(3,1) = (15.0d0,-3.0d0)
  call ZTBTRS('U', 'C', 'N', 3, 1, 1, AB2, 2, B, 10, info)
  call begin_test('upper_conj_trans')
  call print_array('x', B_r, 6)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 4: Lower, Conjugate-transpose, Non-unit, N=3, KD=1, NRHS=1
  ! Same AB as Test 2
  AB2 = (0.0d0, 0.0d0)
  AB2(1,1) = (2.0d0,1.0d0); AB2(2,1) = (1.0d0,1.0d0)
  AB2(1,2) = (3.0d0,0.0d0); AB2(2,2) = (2.0d0,-1.0d0)
  AB2(1,3) = (4.0d0,-1.0d0); AB2(2,3) = (0.0d0,0.0d0)
  B = (0.0d0, 0.0d0)
  B(1,1) = (5.0d0,3.0d0); B(2,1) = (10.0d0,1.0d0); B(3,1) = (8.0d0,-2.0d0)
  call ZTBTRS('L', 'C', 'N', 3, 1, 1, AB2, 2, B, 10, info)
  call begin_test('lower_conj_trans')
  call print_array('x', B_r, 6)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 5: Upper, Unit diagonal, No transpose, N=3, KD=1, NRHS=1
  AB2 = (0.0d0, 0.0d0)
  AB2(1,1) = (0.0d0,0.0d0); AB2(2,1) = (99.0d0,99.0d0)
  AB2(1,2) = (2.0d0,1.0d0); AB2(2,2) = (99.0d0,99.0d0)
  AB2(1,3) = (3.0d0,-1.0d0); AB2(2,3) = (99.0d0,99.0d0)
  B = (0.0d0, 0.0d0)
  B(1,1) = (7.0d0,1.0d0); B(2,1) = (5.0d0,2.0d0); B(3,1) = (1.0d0,0.0d0)
  call ZTBTRS('U', 'N', 'U', 3, 1, 1, AB2, 2, B, 10, info)
  call begin_test('upper_unit_diag')
  call print_array('x', B_r, 6)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 6: Lower, Unit diagonal, No transpose, N=3, KD=1, NRHS=1
  AB2 = (0.0d0, 0.0d0)
  AB2(1,1) = (99.0d0,99.0d0); AB2(2,1) = (1.0d0,1.0d0)
  AB2(1,2) = (99.0d0,99.0d0); AB2(2,2) = (2.0d0,-1.0d0)
  AB2(1,3) = (99.0d0,99.0d0); AB2(2,3) = (0.0d0,0.0d0)
  B = (0.0d0, 0.0d0)
  B(1,1) = (3.0d0,1.0d0); B(2,1) = (5.0d0,2.0d0); B(3,1) = (4.0d0,0.0d0)
  call ZTBTRS('L', 'N', 'U', 3, 1, 1, AB2, 2, B, 10, info)
  call begin_test('lower_unit_diag')
  call print_array('x', B_r, 6)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 7: N=0 quick return
  info = 999
  call ZTBTRS('U', 'N', 'N', 0, 0, 1, AB2, 1, B, 1, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 8: Singular matrix (zero on diagonal), upper, KD=1
  ! Diagonal at row 2: [2+0i, 0+0i, 3+0i] -> singular at j=2
  AB2 = (0.0d0, 0.0d0)
  AB2(1,1) = (0.0d0,0.0d0); AB2(2,1) = (2.0d0,0.0d0)
  AB2(1,2) = (1.0d0,0.0d0); AB2(2,2) = (0.0d0,0.0d0)
  AB2(1,3) = (1.0d0,0.0d0); AB2(2,3) = (3.0d0,0.0d0)
  B = (0.0d0, 0.0d0)
  B(1,1) = (1.0d0,0.0d0); B(2,1) = (2.0d0,0.0d0); B(3,1) = (3.0d0,0.0d0)
  call ZTBTRS('U', 'N', 'N', 3, 1, 1, AB2, 2, B, 10, info)
  call begin_test('singular_upper')
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 9: Singular matrix, lower, KD=1
  ! Diagonal at row 1: [0+0i, 3+0i, 4+0i] -> singular at j=1
  AB2 = (0.0d0, 0.0d0)
  AB2(1,1) = (0.0d0,0.0d0); AB2(2,1) = (1.0d0,0.0d0)
  AB2(1,2) = (3.0d0,0.0d0); AB2(2,2) = (2.0d0,0.0d0)
  AB2(1,3) = (4.0d0,0.0d0); AB2(2,3) = (0.0d0,0.0d0)
  B = (0.0d0, 0.0d0)
  B(1,1) = (1.0d0,0.0d0); B(2,1) = (2.0d0,0.0d0); B(3,1) = (3.0d0,0.0d0)
  call ZTBTRS('L', 'N', 'N', 3, 1, 1, AB2, 2, B, 10, info)
  call begin_test('singular_lower')
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 10: Multiple RHS (NRHS=2), upper, KD=1, N=3
  AB2 = (0.0d0, 0.0d0)
  AB2(1,1) = (0.0d0,0.0d0); AB2(2,1) = (3.0d0,0.0d0)
  AB2(1,2) = (1.0d0,1.0d0); AB2(2,2) = (4.0d0,0.0d0)
  AB2(1,3) = (2.0d0,0.0d0); AB2(2,3) = (5.0d0,0.0d0)
  B = (0.0d0, 0.0d0)
  ! RHS 1
  B(1,1) = (6.0d0,1.0d0); B(2,1) = (12.0d0,3.0d0); B(3,1) = (10.0d0,0.0d0)
  ! RHS 2
  B(1,2) = (3.0d0,0.0d0); B(2,2) = (8.0d0,2.0d0); B(3,2) = (5.0d0,-1.0d0)
  call ZTBTRS('U', 'N', 'N', 3, 1, 2, AB2, 2, B, 10, info)
  call begin_test('multi_rhs')
  ! Print first 3 entries of each column (B is 10xN, so col2 starts at index 11)
  call print_array('x1', B_r, 6)
  call print_array('x2', B_r(21:26), 6)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 11: N=1 edge case
  AB2 = (0.0d0, 0.0d0)
  AB2(1,1) = (5.0d0,2.0d0)
  B = (0.0d0, 0.0d0)
  B(1,1) = (15.0d0,6.0d0)
  call ZTBTRS('U', 'N', 'N', 1, 0, 1, AB2, 1, B, 10, info)
  call begin_test('n_one')
  call print_array('x', B_r, 2)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 12: Upper, KD=2, N=4, NRHS=1, No transpose
  ! Band storage LDA=3:
  !   Row 1 (2nd superdiag): [*      *       1+i    2+0i ]
  !   Row 2 (1st superdiag): [*      1+0i    2+i    3-i  ]
  !   Row 3 (diagonal):      [3+0i   4+i     5-i    6+0i ]
  AB3 = (0.0d0, 0.0d0)
  AB3(1,1) = (0.0d0,0.0d0); AB3(2,1) = (0.0d0,0.0d0); AB3(3,1) = (3.0d0,0.0d0)
  AB3(1,2) = (0.0d0,0.0d0); AB3(2,2) = (1.0d0,0.0d0); AB3(3,2) = (4.0d0,1.0d0)
  AB3(1,3) = (1.0d0,1.0d0); AB3(2,3) = (2.0d0,1.0d0); AB3(3,3) = (5.0d0,-1.0d0)
  AB3(1,4) = (2.0d0,0.0d0); AB3(2,4) = (3.0d0,-1.0d0); AB3(3,4) = (6.0d0,0.0d0)
  B = (0.0d0, 0.0d0)
  B(1,1) = (10.0d0,3.0d0); B(2,1) = (20.0d0,5.0d0)
  B(3,1) = (15.0d0,-2.0d0); B(4,1) = (12.0d0,1.0d0)
  call ZTBTRS('U', 'N', 'N', 4, 2, 1, AB3, 3, B, 10, info)
  call begin_test('upper_kd2_no_trans')
  call print_array('x', B_r, 8)
  call print_int('info', info)
  call end_test()

end program
