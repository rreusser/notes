program test_dgbsv
  use test_utils
  implicit none
  double precision :: AB4(4, 10), AB7(7, 10)
  double precision :: B(10, 5)
  integer :: ipiv(10), info
  integer :: i, j

  ! ============================================================
  ! Test 1: 4x4 tridiagonal, single RHS
  ! A = [4 -1  0  0; -1  4 -1  0; 0 -1  4 -1; 0  0 -1  4]
  ! b = [1, 2, 3, 4]
  ! LDAB = 2*KL+KU+1 = 2*1+1+1 = 4
  AB4 = 0.0d0
  ! KL=1, KU=1 => row layout: [0:KL-1]=unused super, [KL]=KU superdiag, [KL+KU]=diag, [KL+KU+1..]=subdiags
  ! Actually for dgbsv, band storage has extra KL rows on top for fill-in
  ! Row 0 (fill): unused
  ! Row 1 (KU superdiag)
  ! Row 2 (diagonal = KL+KU)
  ! Row 3 (subdiag)
  AB4(2,1) = 0.0d0;  AB4(3,1) = 4.0d0;  AB4(4,1) = -1.0d0
  AB4(2,2) = -1.0d0; AB4(3,2) = 4.0d0;  AB4(4,2) = -1.0d0
  AB4(2,3) = -1.0d0; AB4(3,3) = 4.0d0;  AB4(4,3) = -1.0d0
  AB4(2,4) = -1.0d0; AB4(3,4) = 4.0d0;  AB4(4,4) = 0.0d0
  B = 0.0d0
  B(1,1) = 1.0d0; B(2,1) = 2.0d0; B(3,1) = 3.0d0; B(4,1) = 4.0d0
  ipiv = 0
  call DGBSV(4, 1, 1, 1, AB4, 4, ipiv, B, 10, info)
  call begin_test('tridiag_4x4_1rhs')
  call print_array('x', B(1,1), 4)
  call print_int('info', info)
  call print_int_array('ipiv', ipiv, 4)
  call end_test()

  ! ============================================================
  ! Test 2: 4x4 tridiagonal, 2 RHS
  AB4 = 0.0d0
  AB4(2,1) = 0.0d0;  AB4(3,1) = 4.0d0;  AB4(4,1) = -1.0d0
  AB4(2,2) = -1.0d0; AB4(3,2) = 4.0d0;  AB4(4,2) = -1.0d0
  AB4(2,3) = -1.0d0; AB4(3,3) = 4.0d0;  AB4(4,3) = -1.0d0
  AB4(2,4) = -1.0d0; AB4(3,4) = 4.0d0;  AB4(4,4) = 0.0d0
  B = 0.0d0
  B(1,1) = 1.0d0; B(2,1) = 2.0d0; B(3,1) = 3.0d0; B(4,1) = 4.0d0
  B(1,2) = 4.0d0; B(2,2) = 3.0d0; B(3,2) = 2.0d0; B(4,2) = 1.0d0
  ipiv = 0
  call DGBSV(4, 1, 1, 2, AB4, 4, ipiv, B, 10, info)
  call begin_test('tridiag_4x4_2rhs')
  call print_matrix('x', B, 10, 4, 2)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 3: 5x5 pentadiagonal (KL=2, KU=2), single RHS
  ! LDAB = 2*2+2+1 = 7
  AB7 = 0.0d0
  AB7(5,1) = 6.0d0; AB7(6,1) = -2.0d0; AB7(7,1) = 1.0d0
  AB7(4,2) = -2.0d0; AB7(5,2) = 6.0d0; AB7(6,2) = -2.0d0; AB7(7,2) = 1.0d0
  AB7(3,3) = 1.0d0; AB7(4,3) = -2.0d0; AB7(5,3) = 6.0d0; AB7(6,3) = -2.0d0; AB7(7,3) = 1.0d0
  AB7(3,4) = 1.0d0; AB7(4,4) = -2.0d0; AB7(5,4) = 6.0d0; AB7(6,4) = -2.0d0
  AB7(3,5) = 1.0d0; AB7(4,5) = -2.0d0; AB7(5,5) = 6.0d0
  B = 0.0d0
  B(1,1) = 1.0d0; B(2,1) = 2.0d0; B(3,1) = 3.0d0; B(4,1) = 4.0d0; B(5,1) = 5.0d0
  ipiv = 0
  call DGBSV(5, 2, 2, 1, AB7, 7, ipiv, B, 10, info)
  call begin_test('pentadiag_5x5_1rhs')
  call print_array('x', B(1,1), 5)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 4: N=0 quick return
  ipiv = 0
  B = 0.0d0
  call DGBSV(0, 1, 1, 1, AB4, 4, ipiv, B, 10, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 5: NRHS=0 quick return
  AB4 = 0.0d0
  AB4(2,1) = 0.0d0;  AB4(3,1) = 4.0d0;  AB4(4,1) = -1.0d0
  AB4(2,2) = -1.0d0; AB4(3,2) = 4.0d0;  AB4(4,2) = -1.0d0
  AB4(2,3) = -1.0d0; AB4(3,3) = 4.0d0;  AB4(4,3) = -1.0d0
  AB4(2,4) = -1.0d0; AB4(3,4) = 4.0d0;  AB4(4,4) = 0.0d0
  ipiv = 0
  call DGBSV(4, 1, 1, 0, AB4, 4, ipiv, B, 10, info)
  call begin_test('nrhs_zero')
  call print_int('info', info)
  call print_int_array('ipiv', ipiv, 4)
  call end_test()

  ! ============================================================
  ! Test 6: Singular matrix — second diagonal element is zero
  ! A = [0 1; 1 0], KL=1, KU=1, LDAB=4
  ! After pivoting: A = [1 0; 0 1], but actually with KL=1 KU=1:
  ! Let's use: A = [1 0; 0 0] which is clearly singular
  ! Band storage: row1=super, row2=diag, row3=sub
  AB4 = 0.0d0
  AB4(3,1) = 1.0d0  ! diag(1) = 1
  AB4(3,2) = 0.0d0  ! diag(2) = 0 => singular
  B = 0.0d0
  B(1,1) = 1.0d0; B(2,1) = 2.0d0
  ipiv = 0
  call DGBSV(2, 1, 1, 1, AB4, 4, ipiv, B, 10, info)
  call begin_test('singular')
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 7: Pivoting test: A = [1 2; 3 4], KL=1, KU=1
  ! b = [5, 11] => x = [1, 2]
  AB4 = 0.0d0
  AB4(3,1) = 1.0d0; AB4(4,1) = 3.0d0
  AB4(2,2) = 2.0d0; AB4(3,2) = 4.0d0
  B = 0.0d0
  B(1,1) = 5.0d0; B(2,1) = 11.0d0
  ipiv = 0
  call DGBSV(2, 1, 1, 1, AB4, 4, ipiv, B, 10, info)
  call begin_test('pivot_2x2')
  call print_array('x', B(1,1), 2)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 8: 5x5 pentadiagonal, 3 RHS
  AB7 = 0.0d0
  AB7(5,1) = 6.0d0; AB7(6,1) = -2.0d0; AB7(7,1) = 1.0d0
  AB7(4,2) = -2.0d0; AB7(5,2) = 6.0d0; AB7(6,2) = -2.0d0; AB7(7,2) = 1.0d0
  AB7(3,3) = 1.0d0; AB7(4,3) = -2.0d0; AB7(5,3) = 6.0d0; AB7(6,3) = -2.0d0; AB7(7,3) = 1.0d0
  AB7(3,4) = 1.0d0; AB7(4,4) = -2.0d0; AB7(5,4) = 6.0d0; AB7(6,4) = -2.0d0
  AB7(3,5) = 1.0d0; AB7(4,5) = -2.0d0; AB7(5,5) = 6.0d0
  B = 0.0d0
  B(1,1) = 1.0d0; B(2,1) = 0.0d0; B(3,1) = 0.0d0; B(4,1) = 0.0d0; B(5,1) = 0.0d0
  B(1,2) = 0.0d0; B(2,2) = 0.0d0; B(3,2) = 1.0d0; B(4,2) = 0.0d0; B(5,2) = 0.0d0
  B(1,3) = 1.0d0; B(2,3) = 2.0d0; B(3,3) = 3.0d0; B(4,3) = 4.0d0; B(5,3) = 5.0d0
  ipiv = 0
  call DGBSV(5, 2, 2, 3, AB7, 7, ipiv, B, 10, info)
  call begin_test('pentadiag_5x5_3rhs')
  call print_matrix('x', B, 10, 5, 3)
  call print_int('info', info)
  call end_test()

end program
