program test_zhbevx
  use test_utils
  implicit none

  integer, parameter :: NMAX = 10
  integer :: INFO, N, KD, LDAB, LDQ, LDZ, IL, IU, M, i, j
  double precision :: VL, VU, ABSTOL

  ! Complex arrays with EQUIVALENCE for printing
  complex*16 :: AB(3*NMAX), Q(NMAX*NMAX), Z(NMAX*NMAX), WORK(NMAX)
  double precision :: AB_r(6*NMAX), Q_r(2*NMAX*NMAX), Z_r(2*NMAX*NMAX), WORK_r(2*NMAX)
  equivalence (AB, AB_r)
  equivalence (Q, Q_r)
  equivalence (Z, Z_r)
  equivalence (WORK, WORK_r)

  double precision :: W(NMAX), RWORK(7*NMAX)
  integer :: IWORK(5*NMAX), IFAIL(NMAX)

  ! Hermitian 5x5 matrix used throughout:
  !   4      (1+i)   (2-i)   0       0
  !  (1-i)    5      (3+i)  (1-i)    0
  !  (2+i)   (3-i)    6     (2+i)   (1-i)
  !   0      (1+i)   (2-i)   7      (3+i)
  !   0       0      (1+i)  (3-i)    8

  ! =====================================================
  ! Test 1: JOBZ='V', RANGE='A', UPLO='U', KD=2, N=5
  ! Upper band storage (LDAB=3=KD+1)
  ! =====================================================
  N = 5; KD = 2; LDAB = 3; LDQ = NMAX; LDZ = NMAX
  ABSTOL = 0.0d0
  AB = (0.0d0, 0.0d0); Q = (0.0d0, 0.0d0); Z = (0.0d0, 0.0d0)
  W = 0.0d0; WORK = (0.0d0, 0.0d0); RWORK = 0.0d0
  IWORK = 0; IFAIL = 0

  ! Column 1
  AB(3) = (4.0d0, 0.0d0)
  ! Column 2
  AB(5) = (1.0d0, 1.0d0); AB(6) = (5.0d0, 0.0d0)
  ! Column 3
  AB(7) = (2.0d0, -1.0d0); AB(8) = (3.0d0, 1.0d0); AB(9) = (6.0d0, 0.0d0)
  ! Column 4
  AB(10) = (1.0d0, -1.0d0); AB(11) = (2.0d0, 1.0d0); AB(12) = (7.0d0, 0.0d0)
  ! Column 5
  AB(13) = (1.0d0, -1.0d0); AB(14) = (3.0d0, 1.0d0); AB(15) = (8.0d0, 0.0d0)

  call ZHBEVX('V', 'A', 'U', N, KD, AB, LDAB, Q, LDQ, VL, VU, &
              IL, IU, ABSTOL, M, W, Z, LDZ, WORK, RWORK, IWORK, IFAIL, INFO)
  call begin_test('V_A_U_kd2_n5')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('w', W, M)
  call print_array('Z', Z_r, N*M*2)
  call end_test()

  ! =====================================================
  ! Test 2: JOBZ='V', RANGE='A', UPLO='L', KD=2, N=5
  ! Lower band storage
  ! =====================================================
  N = 5; KD = 2; LDAB = 3; LDQ = NMAX; LDZ = NMAX
  ABSTOL = 0.0d0
  AB = (0.0d0, 0.0d0); Q = (0.0d0, 0.0d0); Z = (0.0d0, 0.0d0)
  W = 0.0d0; WORK = (0.0d0, 0.0d0); RWORK = 0.0d0
  IWORK = 0; IFAIL = 0

  ! Column 1
  AB(1) = (4.0d0, 0.0d0); AB(2) = (1.0d0, -1.0d0); AB(3) = (2.0d0, 1.0d0)
  ! Column 2
  AB(4) = (5.0d0, 0.0d0); AB(5) = (3.0d0, -1.0d0); AB(6) = (1.0d0, 1.0d0)
  ! Column 3
  AB(7) = (6.0d0, 0.0d0); AB(8) = (2.0d0, -1.0d0); AB(9) = (1.0d0, 1.0d0)
  ! Column 4
  AB(10) = (7.0d0, 0.0d0); AB(11) = (3.0d0, -1.0d0)
  ! Column 5
  AB(13) = (8.0d0, 0.0d0)

  call ZHBEVX('V', 'A', 'L', N, KD, AB, LDAB, Q, LDQ, VL, VU, &
              IL, IU, ABSTOL, M, W, Z, LDZ, WORK, RWORK, IWORK, IFAIL, INFO)
  call begin_test('V_A_L_kd2_n5')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('w', W, M)
  call print_array('Z', Z_r, N*M*2)
  call end_test()

  ! =====================================================
  ! Test 3: JOBZ='N', RANGE='A', UPLO='U', KD=2, N=5
  ! =====================================================
  N = 5; KD = 2; LDAB = 3; LDQ = 1; LDZ = 1
  ABSTOL = 0.0d0
  AB = (0.0d0, 0.0d0); W = 0.0d0; RWORK = 0.0d0
  IWORK = 0; IFAIL = 0

  AB(3) = (4.0d0, 0.0d0)
  AB(5) = (1.0d0, 1.0d0); AB(6) = (5.0d0, 0.0d0)
  AB(7) = (2.0d0, -1.0d0); AB(8) = (3.0d0, 1.0d0); AB(9) = (6.0d0, 0.0d0)
  AB(10) = (1.0d0, -1.0d0); AB(11) = (2.0d0, 1.0d0); AB(12) = (7.0d0, 0.0d0)
  AB(13) = (1.0d0, -1.0d0); AB(14) = (3.0d0, 1.0d0); AB(15) = (8.0d0, 0.0d0)

  call ZHBEVX('N', 'A', 'U', N, KD, AB, LDAB, Q, LDQ, VL, VU, &
              IL, IU, ABSTOL, M, W, Z, LDZ, WORK, RWORK, IWORK, IFAIL, INFO)
  call begin_test('N_A_U_kd2_n5')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('w', W, M)
  call end_test()

  ! =====================================================
  ! Test 4: JOBZ='V', RANGE='V', UPLO='U', KD=2, N=5
  ! Select eigenvalues in [3.0, 8.0]
  ! =====================================================
  N = 5; KD = 2; LDAB = 3; LDQ = NMAX; LDZ = NMAX
  VL = 3.0d0; VU = 8.0d0; ABSTOL = 0.0d0
  AB = (0.0d0, 0.0d0); Q = (0.0d0, 0.0d0); Z = (0.0d0, 0.0d0)
  W = 0.0d0; WORK = (0.0d0, 0.0d0); RWORK = 0.0d0
  IWORK = 0; IFAIL = 0

  AB(3) = (4.0d0, 0.0d0)
  AB(5) = (1.0d0, 1.0d0); AB(6) = (5.0d0, 0.0d0)
  AB(7) = (2.0d0, -1.0d0); AB(8) = (3.0d0, 1.0d0); AB(9) = (6.0d0, 0.0d0)
  AB(10) = (1.0d0, -1.0d0); AB(11) = (2.0d0, 1.0d0); AB(12) = (7.0d0, 0.0d0)
  AB(13) = (1.0d0, -1.0d0); AB(14) = (3.0d0, 1.0d0); AB(15) = (8.0d0, 0.0d0)

  call ZHBEVX('V', 'V', 'U', N, KD, AB, LDAB, Q, LDQ, VL, VU, &
              IL, IU, ABSTOL, M, W, Z, LDZ, WORK, RWORK, IWORK, IFAIL, INFO)
  call begin_test('V_V_U_kd2_n5')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('w', W, M)
  call print_array('Z', Z_r, N*M*2)
  call end_test()

  ! =====================================================
  ! Test 5: JOBZ='V', RANGE='I', UPLO='U', KD=2, N=5
  ! Select eigenvalues 2 through 4
  ! =====================================================
  N = 5; KD = 2; LDAB = 3; LDQ = NMAX; LDZ = NMAX
  IL = 2; IU = 4; ABSTOL = 0.0d0
  AB = (0.0d0, 0.0d0); Q = (0.0d0, 0.0d0); Z = (0.0d0, 0.0d0)
  W = 0.0d0; WORK = (0.0d0, 0.0d0); RWORK = 0.0d0
  IWORK = 0; IFAIL = 0

  AB(3) = (4.0d0, 0.0d0)
  AB(5) = (1.0d0, 1.0d0); AB(6) = (5.0d0, 0.0d0)
  AB(7) = (2.0d0, -1.0d0); AB(8) = (3.0d0, 1.0d0); AB(9) = (6.0d0, 0.0d0)
  AB(10) = (1.0d0, -1.0d0); AB(11) = (2.0d0, 1.0d0); AB(12) = (7.0d0, 0.0d0)
  AB(13) = (1.0d0, -1.0d0); AB(14) = (3.0d0, 1.0d0); AB(15) = (8.0d0, 0.0d0)

  call ZHBEVX('V', 'I', 'U', N, KD, AB, LDAB, Q, LDQ, VL, VU, &
              IL, IU, ABSTOL, M, W, Z, LDZ, WORK, RWORK, IWORK, IFAIL, INFO)
  call begin_test('V_I_U_kd2_n5')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('w', W, M)
  call print_array('Z', Z_r, N*M*2)
  call end_test()

  ! =====================================================
  ! Test 6: JOBZ='N', RANGE='V', UPLO='L', KD=2, N=5
  ! =====================================================
  N = 5; KD = 2; LDAB = 3; LDQ = 1; LDZ = 1
  VL = 1.0d0; VU = 5.0d0; ABSTOL = 0.0d0
  AB = (0.0d0, 0.0d0); W = 0.0d0; RWORK = 0.0d0
  IWORK = 0; IFAIL = 0

  AB(1) = (4.0d0, 0.0d0); AB(2) = (1.0d0, -1.0d0); AB(3) = (2.0d0, 1.0d0)
  AB(4) = (5.0d0, 0.0d0); AB(5) = (3.0d0, -1.0d0); AB(6) = (1.0d0, 1.0d0)
  AB(7) = (6.0d0, 0.0d0); AB(8) = (2.0d0, -1.0d0); AB(9) = (1.0d0, 1.0d0)
  AB(10) = (7.0d0, 0.0d0); AB(11) = (3.0d0, -1.0d0)
  AB(13) = (8.0d0, 0.0d0)

  call ZHBEVX('N', 'V', 'L', N, KD, AB, LDAB, Q, LDQ, VL, VU, &
              IL, IU, ABSTOL, M, W, Z, LDZ, WORK, RWORK, IWORK, IFAIL, INFO)
  call begin_test('N_V_L_kd2_n5')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('w', W, M)
  call end_test()

  ! =====================================================
  ! Test 7: JOBZ='N', RANGE='I', UPLO='L', KD=2, N=5
  ! Select eigenvalue 3 only
  ! =====================================================
  N = 5; KD = 2; LDAB = 3; LDQ = 1; LDZ = 1
  IL = 3; IU = 3; ABSTOL = 0.0d0
  AB = (0.0d0, 0.0d0); W = 0.0d0; RWORK = 0.0d0
  IWORK = 0; IFAIL = 0

  AB(1) = (4.0d0, 0.0d0); AB(2) = (1.0d0, -1.0d0); AB(3) = (2.0d0, 1.0d0)
  AB(4) = (5.0d0, 0.0d0); AB(5) = (3.0d0, -1.0d0); AB(6) = (1.0d0, 1.0d0)
  AB(7) = (6.0d0, 0.0d0); AB(8) = (2.0d0, -1.0d0); AB(9) = (1.0d0, 1.0d0)
  AB(10) = (7.0d0, 0.0d0); AB(11) = (3.0d0, -1.0d0)
  AB(13) = (8.0d0, 0.0d0)

  call ZHBEVX('N', 'I', 'L', N, KD, AB, LDAB, Q, LDQ, VL, VU, &
              IL, IU, ABSTOL, M, W, Z, LDZ, WORK, RWORK, IWORK, IFAIL, INFO)
  call begin_test('N_I_L_kd2_n5')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('w', W, M)
  call end_test()

  ! =====================================================
  ! Test 8: N=1, JOBZ='V', RANGE='A', UPLO='L'
  ! =====================================================
  N = 1; KD = 0; LDAB = 1; LDQ = NMAX; LDZ = NMAX
  ABSTOL = 0.0d0
  AB = (0.0d0, 0.0d0); Q = (0.0d0, 0.0d0); Z = (0.0d0, 0.0d0)
  W = 0.0d0; WORK = (0.0d0, 0.0d0); RWORK = 0.0d0
  IWORK = 0; IFAIL = 0
  AB(1) = (3.5d0, 0.0d0)

  call ZHBEVX('V', 'A', 'L', N, KD, AB, LDAB, Q, LDQ, VL, VU, &
              IL, IU, ABSTOL, M, W, Z, LDZ, WORK, RWORK, IWORK, IFAIL, INFO)
  call begin_test('n1_V_A_L')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_scalar('w1', W(1))
  call print_array('z11', Z_r, 2)
  call end_test()

  ! =====================================================
  ! Test 9: N=1, JOBZ='V', RANGE='V' (value excludes eigenvalue)
  ! =====================================================
  N = 1; KD = 0; LDAB = 1; LDQ = NMAX; LDZ = NMAX
  VL = 0.0d0; VU = 3.0d0; ABSTOL = 0.0d0
  AB = (0.0d0, 0.0d0); Q = (0.0d0, 0.0d0); Z = (0.0d0, 0.0d0)
  W = 0.0d0; WORK = (0.0d0, 0.0d0); RWORK = 0.0d0
  IWORK = 0; IFAIL = 0
  AB(1) = (3.5d0, 0.0d0)

  call ZHBEVX('V', 'V', 'L', N, KD, AB, LDAB, Q, LDQ, VL, VU, &
              IL, IU, ABSTOL, M, W, Z, LDZ, WORK, RWORK, IWORK, IFAIL, INFO)
  call begin_test('n1_V_V_excluded')
  call print_int('info', INFO)
  call print_int('M', M)
  call end_test()

  ! =====================================================
  ! Test 10: N=1, JOBZ='V', RANGE='V' (value includes eigenvalue)
  ! =====================================================
  N = 1; KD = 0; LDAB = 1; LDQ = NMAX; LDZ = NMAX
  VL = 3.0d0; VU = 4.0d0; ABSTOL = 0.0d0
  AB = (0.0d0, 0.0d0); Q = (0.0d0, 0.0d0); Z = (0.0d0, 0.0d0)
  W = 0.0d0; WORK = (0.0d0, 0.0d0); RWORK = 0.0d0
  IWORK = 0; IFAIL = 0
  AB(1) = (3.5d0, 0.0d0)

  call ZHBEVX('V', 'V', 'L', N, KD, AB, LDAB, Q, LDQ, VL, VU, &
              IL, IU, ABSTOL, M, W, Z, LDZ, WORK, RWORK, IWORK, IFAIL, INFO)
  call begin_test('n1_V_V_included')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_scalar('w1', W(1))
  call print_array('z11', Z_r, 2)
  call end_test()

  ! =====================================================
  ! Test 11: N=0 (quick return)
  ! =====================================================
  N = 0; KD = 0; LDAB = 1; LDQ = 1; LDZ = 1
  ABSTOL = 0.0d0

  call ZHBEVX('V', 'A', 'U', N, KD, AB, LDAB, Q, LDQ, VL, VU, &
              IL, IU, ABSTOL, M, W, Z, LDZ, WORK, RWORK, IWORK, IFAIL, INFO)
  call begin_test('n0')
  call print_int('info', INFO)
  call print_int('M', M)
  call end_test()

  ! =====================================================
  ! Test 12: JOBZ='V', RANGE='I', UPLO='L', KD=1, N=4
  ! IL=1, IU=N (fast path with vectors)
  ! =====================================================
  N = 4; KD = 1; LDAB = 2; LDQ = NMAX; LDZ = NMAX
  IL = 1; IU = 4; ABSTOL = 0.0d0
  AB = (0.0d0, 0.0d0); Q = (0.0d0, 0.0d0); Z = (0.0d0, 0.0d0)
  W = 0.0d0; WORK = (0.0d0, 0.0d0); RWORK = 0.0d0
  IWORK = 0; IFAIL = 0

  ! Lower band (LDAB=2): diag=row1, subdiag=row2
  AB(1) = (4.0d0, 0.0d0); AB(2) = (1.0d0, -1.0d0)
  AB(3) = (5.0d0, 0.0d0); AB(4) = (2.0d0, 1.0d0)
  AB(5) = (6.0d0, 0.0d0); AB(6) = (3.0d0, -1.0d0)
  AB(7) = (7.0d0, 0.0d0)

  call ZHBEVX('V', 'I', 'L', N, KD, AB, LDAB, Q, LDQ, VL, VU, &
              IL, IU, ABSTOL, M, W, Z, LDZ, WORK, RWORK, IWORK, IFAIL, INFO)
  call begin_test('V_I_L_kd1_n4_fast')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('w', W, M)
  call print_array('Z', Z_r, N*M*2)
  call end_test()

  ! =====================================================
  ! Test 13: JOBZ='V', RANGE='I', UPLO='U', KD=2, N=5
  ! Select single eigenvalue IL=IU=1 (smallest)
  ! =====================================================
  N = 5; KD = 2; LDAB = 3; LDQ = NMAX; LDZ = NMAX
  IL = 1; IU = 1; ABSTOL = 0.0d0
  AB = (0.0d0, 0.0d0); Q = (0.0d0, 0.0d0); Z = (0.0d0, 0.0d0)
  W = 0.0d0; WORK = (0.0d0, 0.0d0); RWORK = 0.0d0
  IWORK = 0; IFAIL = 0

  AB(3) = (4.0d0, 0.0d0)
  AB(5) = (1.0d0, 1.0d0); AB(6) = (5.0d0, 0.0d0)
  AB(7) = (2.0d0, -1.0d0); AB(8) = (3.0d0, 1.0d0); AB(9) = (6.0d0, 0.0d0)
  AB(10) = (1.0d0, -1.0d0); AB(11) = (2.0d0, 1.0d0); AB(12) = (7.0d0, 0.0d0)
  AB(13) = (1.0d0, -1.0d0); AB(14) = (3.0d0, 1.0d0); AB(15) = (8.0d0, 0.0d0)

  call ZHBEVX('V', 'I', 'U', N, KD, AB, LDAB, Q, LDQ, VL, VU, &
              IL, IU, ABSTOL, M, W, Z, LDZ, WORK, RWORK, IWORK, IFAIL, INFO)
  call begin_test('V_I_U_kd2_n5_single')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('w', W, M)
  call print_array('Z', Z_r, N*M*2)
  call end_test()

  ! =====================================================
  ! Test 14: N=1, JOBZ='V', RANGE='I', IL=1, IU=1, UPLO='U', KD=2
  ! =====================================================
  N = 1; KD = 2; LDAB = 3; LDQ = NMAX; LDZ = NMAX
  IL = 1; IU = 1; ABSTOL = 0.0d0
  AB = (0.0d0, 0.0d0); Q = (0.0d0, 0.0d0); Z = (0.0d0, 0.0d0)
  W = 0.0d0; WORK = (0.0d0, 0.0d0); RWORK = 0.0d0
  IWORK = 0; IFAIL = 0
  AB(3) = (7.25d0, 0.0d0)

  call ZHBEVX('V', 'I', 'U', N, KD, AB, LDAB, Q, LDQ, VL, VU, &
              IL, IU, ABSTOL, M, W, Z, LDZ, WORK, RWORK, IWORK, IFAIL, INFO)
  call begin_test('n1_V_I_U_kd2')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_scalar('w1', W(1))
  call print_array('z11', Z_r, 2)
  call end_test()

end program test_zhbevx
