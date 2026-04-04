program test_ztbrfs
  use test_utils
  implicit none

  ! Arrays with EQUIVALENCE for printing interleaved re/im pairs
  double precision :: AB_r(60), B_r(200), X_r(200), WORK_r(400)
  complex*16 :: AB(3,10), B(10,10), X(10,10), WORK(200)
  equivalence (AB, AB_r)
  equivalence (B, B_r)
  equivalence (X, X_r)
  equivalence (WORK, WORK_r)
  double precision :: FERR(10), BERR(10), RWORK(200)
  integer :: info, n, kd, nrhs, ldab, ldb, ldx

  ldab = 3
  ldb = 10
  ldx = 10

  ! ============================================================
  ! Test 1: Upper, No transpose, Non-unit, N=3, KD=1, NRHS=1
  ! Band storage LDAB=2:
  !   Row 1 (superdiag): [*        (1+i)     (2-i)]
  !   Row 2 (diag):      [(3+0i)   (4+i)     (5-i)]
  n = 3; kd = 1; nrhs = 1
  AB = (0.0d0, 0.0d0)
  AB(1,1) = (0.0d0,0.0d0); AB(2,1) = (3.0d0,0.0d0)
  AB(1,2) = (1.0d0,1.0d0); AB(2,2) = (4.0d0,1.0d0)
  AB(1,3) = (2.0d0,-1.0d0); AB(2,3) = (5.0d0,-1.0d0)
  ! b = A * [1+0i; 1+0i; 1+0i]
  ! row 1: 3 + (1+i) = 4+i
  ! row 2: (4+i) + (2-i) = 6
  ! row 3: (5-i) = 5-i
  B = (0.0d0, 0.0d0)
  B(1,1) = (4.0d0,1.0d0); B(2,1) = (6.0d0,0.0d0); B(3,1) = (5.0d0,-1.0d0)
  ! Solve for X
  X = (0.0d0, 0.0d0)
  X(1,1) = B(1,1); X(2,1) = B(2,1); X(3,1) = B(3,1)
  call ZTBTRS('U', 'N', 'N', n, kd, nrhs, AB, ldab, X, ldx, info)
  ! Now call ztbrfs
  call ZTBRFS('U', 'N', 'N', n, kd, nrhs, AB, ldab, B, ldb, X, ldx, &
              FERR, BERR, WORK, RWORK, info)
  call begin_test('upper_no_trans')
  call print_array('x', X_r, 2*n)
  call print_array('ferr', FERR, nrhs)
  call print_array('berr', BERR, nrhs)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 2: Lower, No transpose, Non-unit, N=3, KD=1, NRHS=1
  ! Band storage LDAB=2:
  !   Row 1 (diag):    [(2+i)    (3+0i)   (4-i)]
  !   Row 2 (subdiag): [(1+i)    (2-i)    *    ]
  n = 3; kd = 1; nrhs = 1
  AB = (0.0d0, 0.0d0)
  AB(1,1) = (2.0d0,1.0d0); AB(2,1) = (1.0d0,1.0d0)
  AB(1,2) = (3.0d0,0.0d0); AB(2,2) = (2.0d0,-1.0d0)
  AB(1,3) = (4.0d0,-1.0d0); AB(2,3) = (0.0d0,0.0d0)
  ! b = A * [1;1;1] (lower tri)
  ! row 1: (2+i) = 2+i
  ! row 2: (1+i) + (3+0i) = 4+i
  ! row 3: (2-i) + (4-i) = 6-2i
  B = (0.0d0, 0.0d0)
  B(1,1) = (2.0d0,1.0d0); B(2,1) = (4.0d0,1.0d0); B(3,1) = (6.0d0,-2.0d0)
  X = (0.0d0, 0.0d0)
  X(1,1) = B(1,1); X(2,1) = B(2,1); X(3,1) = B(3,1)
  call ZTBTRS('L', 'N', 'N', n, kd, nrhs, AB, ldab, X, ldx, info)
  call ZTBRFS('L', 'N', 'N', n, kd, nrhs, AB, ldab, B, ldb, X, ldx, &
              FERR, BERR, WORK, RWORK, info)
  call begin_test('lower_no_trans')
  call print_array('x', X_r, 2*n)
  call print_array('ferr', FERR, nrhs)
  call print_array('berr', BERR, nrhs)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 3: Upper, Conjugate-transpose, Non-unit, N=3, KD=1, NRHS=1
  ! Same band matrix as Test 1
  n = 3; kd = 1; nrhs = 1
  AB = (0.0d0, 0.0d0)
  AB(1,1) = (0.0d0,0.0d0); AB(2,1) = (3.0d0,0.0d0)
  AB(1,2) = (1.0d0,1.0d0); AB(2,2) = (4.0d0,1.0d0)
  AB(1,3) = (2.0d0,-1.0d0); AB(2,3) = (5.0d0,-1.0d0)
  ! b = A^H * [1;1;1]
  ! A^H col 1 (conj of row 1 of A): (3,0) + (1,-1) = (4,-1) -- but actually:
  ! A^H(i,j) = conj(A(j,i))
  ! row 1 of A^H: conj(A(1,1))=3, conj(A(2,1))=0, conj(A(3,1))=0 -> b1 = 3
  ! row 2 of A^H: conj(A(1,2))=(1,-1), conj(A(2,2))=(4,-1), conj(A(3,2))=0 -> b2 = (5,-2)
  ! row 3 of A^H: conj(A(1,3))=0, conj(A(2,3))=(2,1), conj(A(3,3))=(5,1) -> b3 = (7,2)
  B = (0.0d0, 0.0d0)
  B(1,1) = (3.0d0,0.0d0); B(2,1) = (5.0d0,-2.0d0); B(3,1) = (7.0d0,2.0d0)
  X = (0.0d0, 0.0d0)
  X(1,1) = B(1,1); X(2,1) = B(2,1); X(3,1) = B(3,1)
  call ZTBTRS('U', 'C', 'N', n, kd, nrhs, AB, ldab, X, ldx, info)
  call ZTBRFS('U', 'C', 'N', n, kd, nrhs, AB, ldab, B, ldb, X, ldx, &
              FERR, BERR, WORK, RWORK, info)
  call begin_test('upper_conj_trans')
  call print_array('x', X_r, 2*n)
  call print_array('ferr', FERR, nrhs)
  call print_array('berr', BERR, nrhs)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 4: Lower, Conjugate-transpose, Non-unit, N=3, KD=1, NRHS=1
  n = 3; kd = 1; nrhs = 1
  AB = (0.0d0, 0.0d0)
  AB(1,1) = (2.0d0,1.0d0); AB(2,1) = (1.0d0,1.0d0)
  AB(1,2) = (3.0d0,0.0d0); AB(2,2) = (2.0d0,-1.0d0)
  AB(1,3) = (4.0d0,-1.0d0); AB(2,3) = (0.0d0,0.0d0)
  ! b = A^H * [1;1;1]
  ! Lower A: A(1,1)=(2+i), A(2,1)=(1+i), A(2,2)=(3), A(3,2)=(2-i), A(3,3)=(4-i)
  ! A^H row 1: conj(A(1,1))=(2-i), conj(A(2,1))=(1-i) -> b1 = (3-2i)
  ! A^H row 2: conj(A(2,2))=(3), conj(A(3,2))=(2+i) -> b2 = (5+i)
  ! A^H row 3: conj(A(3,3))=(4+i) -> b3 = (4+i)
  B = (0.0d0, 0.0d0)
  B(1,1) = (3.0d0,-2.0d0); B(2,1) = (5.0d0,1.0d0); B(3,1) = (4.0d0,1.0d0)
  X = (0.0d0, 0.0d0)
  X(1,1) = B(1,1); X(2,1) = B(2,1); X(3,1) = B(3,1)
  call ZTBTRS('L', 'C', 'N', n, kd, nrhs, AB, ldab, X, ldx, info)
  call ZTBRFS('L', 'C', 'N', n, kd, nrhs, AB, ldab, B, ldb, X, ldx, &
              FERR, BERR, WORK, RWORK, info)
  call begin_test('lower_conj_trans')
  call print_array('x', X_r, 2*n)
  call print_array('ferr', FERR, nrhs)
  call print_array('berr', BERR, nrhs)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 5: Upper, Unit diagonal, No transpose, N=3, KD=1, NRHS=1
  n = 3; kd = 1; nrhs = 1
  AB = (0.0d0, 0.0d0)
  AB(1,1) = (0.0d0,0.0d0); AB(2,1) = (99.0d0,99.0d0)
  AB(1,2) = (2.0d0,1.0d0); AB(2,2) = (99.0d0,99.0d0)
  AB(1,3) = (3.0d0,-1.0d0); AB(2,3) = (99.0d0,99.0d0)
  ! Unit upper A: diag=1, superdiag: A(1,2)=(2+i), A(2,3)=(3-i)
  ! b = A * [1;1;1]: row1 = 1 + (2+i) = 3+i, row2 = 1 + (3-i) = 4-i, row3 = 1
  B = (0.0d0, 0.0d0)
  B(1,1) = (3.0d0,1.0d0); B(2,1) = (4.0d0,-1.0d0); B(3,1) = (1.0d0,0.0d0)
  X = (0.0d0, 0.0d0)
  X(1,1) = B(1,1); X(2,1) = B(2,1); X(3,1) = B(3,1)
  call ZTBTRS('U', 'N', 'U', n, kd, nrhs, AB, ldab, X, ldx, info)
  call ZTBRFS('U', 'N', 'U', n, kd, nrhs, AB, ldab, B, ldb, X, ldx, &
              FERR, BERR, WORK, RWORK, info)
  call begin_test('upper_unit_diag')
  call print_array('x', X_r, 2*n)
  call print_array('ferr', FERR, nrhs)
  call print_array('berr', BERR, nrhs)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 6: Lower, Unit diagonal, No transpose, N=3, KD=1, NRHS=1
  n = 3; kd = 1; nrhs = 1
  AB = (0.0d0, 0.0d0)
  AB(1,1) = (99.0d0,99.0d0); AB(2,1) = (1.0d0,1.0d0)
  AB(1,2) = (99.0d0,99.0d0); AB(2,2) = (2.0d0,-1.0d0)
  AB(1,3) = (99.0d0,99.0d0); AB(2,3) = (0.0d0,0.0d0)
  ! Unit lower A: diag=1, subdiag: A(2,1)=(1+i), A(3,2)=(2-i)
  ! b = A * [1;1;1]: row1=1, row2=(1+i)+1=2+i, row3=(2-i)+1=3-i
  B = (0.0d0, 0.0d0)
  B(1,1) = (1.0d0,0.0d0); B(2,1) = (2.0d0,1.0d0); B(3,1) = (3.0d0,-1.0d0)
  X = (0.0d0, 0.0d0)
  X(1,1) = B(1,1); X(2,1) = B(2,1); X(3,1) = B(3,1)
  call ZTBTRS('L', 'N', 'U', n, kd, nrhs, AB, ldab, X, ldx, info)
  call ZTBRFS('L', 'N', 'U', n, kd, nrhs, AB, ldab, B, ldb, X, ldx, &
              FERR, BERR, WORK, RWORK, info)
  call begin_test('lower_unit_diag')
  call print_array('x', X_r, 2*n)
  call print_array('ferr', FERR, nrhs)
  call print_array('berr', BERR, nrhs)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 7: N=0 quick return
  info = 999
  call ZTBRFS('U', 'N', 'N', 0, 0, 1, AB, ldab, B, ldb, X, ldx, &
              FERR, BERR, WORK, RWORK, info)
  call begin_test('n_zero')
  call print_array('ferr', FERR, 1)
  call print_array('berr', BERR, 1)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 8: NRHS=0 quick return
  info = 999
  call ZTBRFS('U', 'N', 'N', 3, 1, 0, AB, ldab, B, ldb, X, ldx, &
              FERR, BERR, WORK, RWORK, info)
  call begin_test('nrhs_zero')
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 9: Multiple RHS (NRHS=2), upper, KD=1, N=3
  n = 3; kd = 1; nrhs = 2
  AB = (0.0d0, 0.0d0)
  AB(1,1) = (0.0d0,0.0d0); AB(2,1) = (3.0d0,0.0d0)
  AB(1,2) = (1.0d0,1.0d0); AB(2,2) = (4.0d0,1.0d0)
  AB(1,3) = (2.0d0,-1.0d0); AB(2,3) = (5.0d0,-1.0d0)
  ! RHS 1: b = A * [1;1;1]
  B = (0.0d0, 0.0d0)
  B(1,1) = (4.0d0,1.0d0); B(2,1) = (6.0d0,0.0d0); B(3,1) = (5.0d0,-1.0d0)
  ! RHS 2: b = A * [2+i; 1-i; 0.5+0.5i]
  ! row 1: 3*(2+i) + (1+i)*(1-i) = (6+3i) + (1-i+i-i^2) = (6+3i)+(2) = (8+3i)
  ! row 2: (4+i)*(1-i) + (2-i)*(0.5+0.5i) = (4-4i+i-i^2)+(1+i-0.5i-0.5i^2) = (5-3i)+(1.5+0.5i) = (6.5-2.5i)
  ! row 3: (5-i)*(0.5+0.5i) = (2.5+2.5i-0.5i-0.5i^2) = (3+2i)
  B(1,2) = (8.0d0,3.0d0); B(2,2) = (6.5d0,-2.5d0); B(3,2) = (3.0d0,2.0d0)
  X = (0.0d0, 0.0d0)
  X(1,1) = B(1,1); X(2,1) = B(2,1); X(3,1) = B(3,1)
  X(1,2) = B(1,2); X(2,2) = B(2,2); X(3,2) = B(3,2)
  call ZTBTRS('U', 'N', 'N', n, kd, nrhs, AB, ldab, X, ldx, info)
  call ZTBRFS('U', 'N', 'N', n, kd, nrhs, AB, ldab, B, ldb, X, ldx, &
              FERR, BERR, WORK, RWORK, info)
  call begin_test('multi_rhs')
  call print_array('x1', X_r, 2*n)
  call print_array('x2', X_r(2*ldx+1:2*ldx+2*n), 2*n)
  call print_array('ferr', FERR, nrhs)
  call print_array('berr', BERR, nrhs)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 10: Upper, KD=2, N=4, NRHS=1, No transpose
  ! Band storage LDAB=3:
  !   Row 1 (2nd superdiag): [*      *       1+i    2+0i ]
  !   Row 2 (1st superdiag): [*      1+0i    2+i    3-i  ]
  !   Row 3 (diagonal):      [3+0i   4+i     5-i    6+0i ]
  n = 4; kd = 2; nrhs = 1
  AB = (0.0d0, 0.0d0)
  AB(1,1) = (0.0d0,0.0d0); AB(2,1) = (0.0d0,0.0d0); AB(3,1) = (3.0d0,0.0d0)
  AB(1,2) = (0.0d0,0.0d0); AB(2,2) = (1.0d0,0.0d0); AB(3,2) = (4.0d0,1.0d0)
  AB(1,3) = (1.0d0,1.0d0); AB(2,3) = (2.0d0,1.0d0); AB(3,3) = (5.0d0,-1.0d0)
  AB(1,4) = (2.0d0,0.0d0); AB(2,4) = (3.0d0,-1.0d0); AB(3,4) = (6.0d0,0.0d0)
  ! Full upper A:
  !   [3     1     1+i   0  ]
  !   [0     4+i   2+i   2  ]
  !   [0     0     5-i   3-i]
  !   [0     0     0     6  ]
  ! b = A * [1;1;1;1]
  ! row 1: 3 + 1 + (1+i) = 5+i
  ! row 2: (4+i) + (2+i) + 2 = 8+2i
  ! row 3: (5-i) + (3-i) = 8-2i
  ! row 4: 6
  B = (0.0d0, 0.0d0)
  B(1,1) = (5.0d0,1.0d0); B(2,1) = (8.0d0,2.0d0)
  B(3,1) = (8.0d0,-2.0d0); B(4,1) = (6.0d0,0.0d0)
  X = (0.0d0, 0.0d0)
  X(1,1) = B(1,1); X(2,1) = B(2,1); X(3,1) = B(3,1); X(4,1) = B(4,1)
  call ZTBTRS('U', 'N', 'N', n, kd, nrhs, AB, ldab, X, ldx, info)
  call ZTBRFS('U', 'N', 'N', n, kd, nrhs, AB, ldab, B, ldb, X, ldx, &
              FERR, BERR, WORK, RWORK, info)
  call begin_test('upper_kd2')
  call print_array('x', X_r, 2*n)
  call print_array('ferr', FERR, nrhs)
  call print_array('berr', BERR, nrhs)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 11: Lower, KD=2, N=4, Conjugate-transpose, Non-unit
  n = 4; kd = 2; nrhs = 1
  AB = (0.0d0, 0.0d0)
  AB(1,1) = (3.0d0,1.0d0); AB(2,1) = (1.0d0,0.5d0); AB(3,1) = (0.5d0,0.2d0)
  AB(1,2) = (4.0d0,0.0d0); AB(2,2) = (2.0d0,-1.0d0); AB(3,2) = (1.0d0,0.3d0)
  AB(1,3) = (5.0d0,-1.0d0); AB(2,3) = (3.0d0,0.5d0); AB(3,3) = (0.0d0,0.0d0)
  AB(1,4) = (6.0d0,2.0d0); AB(2,4) = (0.0d0,0.0d0); AB(3,4) = (0.0d0,0.0d0)
  ! Full lower A:
  !   [3+i    0       0       0   ]
  !   [1+0.5i 4       0       0   ]
  !   [0.5+0.2i 2-i   5-i     0   ]
  !   [0      1+0.3i  3+0.5i  6+2i]
  ! b = A^H * [1;1;1;1]
  ! A^H(i,j) = conj(A(j,i))
  ! row 1: conj(3+i)+conj(1+0.5i)+conj(0.5+0.2i) = (3-i)+(1-0.5i)+(0.5-0.2i) = (4.5-1.7i)
  ! row 2: conj(4)+conj(2-i)+conj(1+0.3i) = 4+(2+i)+(1-0.3i) = (7+0.7i)
  ! row 3: conj(5-i)+conj(3+0.5i) = (5+i)+(3-0.5i) = (8+0.5i)
  ! row 4: conj(6+2i) = (6-2i)
  B = (0.0d0, 0.0d0)
  B(1,1) = (4.5d0,-1.7d0); B(2,1) = (7.0d0,0.7d0)
  B(3,1) = (8.0d0,0.5d0); B(4,1) = (6.0d0,-2.0d0)
  X = (0.0d0, 0.0d0)
  X(1,1) = B(1,1); X(2,1) = B(2,1); X(3,1) = B(3,1); X(4,1) = B(4,1)
  call ZTBTRS('L', 'C', 'N', n, kd, nrhs, AB, ldab, X, ldx, info)
  call ZTBRFS('L', 'C', 'N', n, kd, nrhs, AB, ldab, B, ldb, X, ldx, &
              FERR, BERR, WORK, RWORK, info)
  call begin_test('lower_kd2_conj_trans')
  call print_array('x', X_r, 2*n)
  call print_array('ferr', FERR, nrhs)
  call print_array('berr', BERR, nrhs)
  call print_int('info', info)
  call end_test()

end program
