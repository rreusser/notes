program test_zgbtrs
  use test_utils
  implicit none
  double precision :: AB4_r(80), B_r(40), AB7_r(140), AB2_r(20)
  complex*16 :: AB4(4, 10), B(10, 4), AB7(7, 10), AB2(2, 5)
  equivalence (AB4, AB4_r)
  equivalence (B, B_r)
  equivalence (AB7, AB7_r)
  equivalence (AB2, AB2_r)
  integer :: ipiv(10), info

  ! ============================================================
  ! Test 1: Solve Ax = b, 4x4 tridiagonal (KL=1, KU=1)
  ! A = [4+i  -1     0     0   ]
  !     [-1    4+i   -1     0   ]
  !     [ 0   -1      4+i  -1   ]
  !     [ 0    0     -1     4+i ]
  ! First factorize:
  AB4 = (0.0d0, 0.0d0)
  AB4(2,1) = (0.0d0,0.0d0);  AB4(3,1) = (4.0d0,1.0d0);  AB4(4,1) = (-1.0d0,0.0d0)
  AB4(2,2) = (-1.0d0,0.0d0); AB4(3,2) = (4.0d0,1.0d0);  AB4(4,2) = (-1.0d0,0.0d0)
  AB4(2,3) = (-1.0d0,0.0d0); AB4(3,3) = (4.0d0,1.0d0);  AB4(4,3) = (-1.0d0,0.0d0)
  AB4(2,4) = (-1.0d0,0.0d0); AB4(3,4) = (4.0d0,1.0d0);  AB4(4,4) = (0.0d0,0.0d0)
  ipiv = 0
  call ZGBTRF(4, 4, 1, 1, AB4, 4, ipiv, info)

  ! Now solve with b = [1, 2, 3, 4]
  B(1,1) = (1.0d0, 0.0d0)
  B(2,1) = (2.0d0, 0.0d0)
  B(3,1) = (3.0d0, 0.0d0)
  B(4,1) = (4.0d0, 0.0d0)
  call ZGBTRS('N', 4, 1, 1, 1, AB4, 4, ipiv, B, 10, info)
  call begin_test('notrans_tridiag')
  call print_array('B', B_r, 8)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 2: Solve A^T x = b (same factored matrix)
  B(1,1) = (1.0d0, 0.0d0)
  B(2,1) = (2.0d0, 0.0d0)
  B(3,1) = (3.0d0, 0.0d0)
  B(4,1) = (4.0d0, 0.0d0)
  call ZGBTRS('T', 4, 1, 1, 1, AB4, 4, ipiv, B, 10, info)
  call begin_test('trans_tridiag')
  call print_array('B', B_r, 8)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 3: Solve A^H x = b (conjugate transpose, same factored matrix)
  B(1,1) = (1.0d0, 0.0d0)
  B(2,1) = (2.0d0, 0.0d0)
  B(3,1) = (3.0d0, 0.0d0)
  B(4,1) = (4.0d0, 0.0d0)
  call ZGBTRS('C', 4, 1, 1, 1, AB4, 4, ipiv, B, 10, info)
  call begin_test('conjtrans_tridiag')
  call print_array('B', B_r, 8)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 4: Multiple RHS, NRHS=2
  AB4 = (0.0d0, 0.0d0)
  AB4(2,1) = (0.0d0,0.0d0);  AB4(3,1) = (4.0d0,1.0d0);  AB4(4,1) = (-1.0d0,0.0d0)
  AB4(2,2) = (-1.0d0,0.0d0); AB4(3,2) = (4.0d0,1.0d0);  AB4(4,2) = (-1.0d0,0.0d0)
  AB4(2,3) = (-1.0d0,0.0d0); AB4(3,3) = (4.0d0,1.0d0);  AB4(4,3) = (-1.0d0,0.0d0)
  AB4(2,4) = (-1.0d0,0.0d0); AB4(3,4) = (4.0d0,1.0d0);  AB4(4,4) = (0.0d0,0.0d0)
  ipiv = 0
  call ZGBTRF(4, 4, 1, 1, AB4, 4, ipiv, info)
  B(1,1) = (1.0d0, 0.0d0); B(1,2) = (5.0d0, 1.0d0)
  B(2,1) = (2.0d0, 0.0d0); B(2,2) = (6.0d0, 2.0d0)
  B(3,1) = (3.0d0, 0.0d0); B(3,2) = (7.0d0, 3.0d0)
  B(4,1) = (4.0d0, 0.0d0); B(4,2) = (8.0d0, 4.0d0)
  call ZGBTRS('N', 4, 1, 1, 2, AB4, 4, ipiv, B, 10, info)
  call begin_test('nrhs_2')
  call print_matrix('B', B_r, 20, 8, 2)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 5: N=0 quick return
  call ZGBTRS('N', 0, 1, 1, 1, AB4, 4, ipiv, B, 10, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 6: NRHS=0 quick return
  call ZGBTRS('N', 4, 1, 1, 0, AB4, 4, ipiv, B, 10, info)
  call begin_test('nrhs_zero')
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 7: 5x5 pentadiagonal solve (KL=2, KU=2)
  AB7 = (0.0d0, 0.0d0)
  AB7(5,1) = (6.0d0,1.0d0); AB7(6,1) = (-2.0d0,0.0d0); AB7(7,1) = (1.0d0,0.0d0)
  AB7(4,2) = (-2.0d0,0.0d0); AB7(5,2) = (6.0d0,1.0d0); AB7(6,2) = (-2.0d0,0.0d0); AB7(7,2) = (1.0d0,0.0d0)
  AB7(3,3) = (1.0d0,0.0d0); AB7(4,3) = (-2.0d0,0.0d0); AB7(5,3) = (6.0d0,1.0d0); AB7(6,3) = (-2.0d0,0.0d0); AB7(7,3) = (1.0d0,0.0d0)
  AB7(3,4) = (1.0d0,0.0d0); AB7(4,4) = (-2.0d0,0.0d0); AB7(5,4) = (6.0d0,1.0d0); AB7(6,4) = (-2.0d0,0.0d0)
  AB7(3,5) = (1.0d0,0.0d0); AB7(4,5) = (-2.0d0,0.0d0); AB7(5,5) = (6.0d0,1.0d0)
  ipiv = 0
  call ZGBTRF(5, 5, 2, 2, AB7, 7, ipiv, info)
  B(1,1) = (1.0d0, 1.0d0)
  B(2,1) = (2.0d0, 2.0d0)
  B(3,1) = (3.0d0, 3.0d0)
  B(4,1) = (4.0d0, 4.0d0)
  B(5,1) = (5.0d0, 5.0d0)
  call ZGBTRS('N', 5, 2, 2, 1, AB7, 7, ipiv, B, 10, info)
  call begin_test('pentadiag_5x5')
  call print_array('B', B_r, 10)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 8: KL=0 (no subdiagonals, upper triangular)
  ! A = [2+i  1; 0  3+i], KL=0, KU=1, LDAB=2
  ! Actually LDAB = 2*KL+KU+1 = 0+1+1 = 2
  ! But for zgbtrf, LDAB >= KL+KV+1 = 0+1+1 = 2. So KV=KU+KL=1.
  ! Diagonal at row KV+1=2 (1-based) or row 1 (0-based)
  ! Band: Row 0 (KU): [*,  1], Row 1 (diag): [2+i, 3+i]
  AB2 = (0.0d0, 0.0d0)
  AB2(1,1) = (0.0d0,0.0d0); AB2(2,1) = (2.0d0,1.0d0)
  AB2(1,2) = (1.0d0,0.0d0); AB2(2,2) = (3.0d0,1.0d0)
  ipiv = 0
  call ZGBTRF(2, 2, 0, 1, AB2, 2, ipiv, info)
  B(1,1) = (5.0d0, 3.0d0)
  B(2,1) = (6.0d0, 4.0d0)
  call ZGBTRS('N', 2, 0, 1, 1, AB2, 2, ipiv, B, 10, info)
  call begin_test('kl0_upper')
  call print_array('B', B_r, 4)
  call print_int('info', info)
  call end_test()

end program
