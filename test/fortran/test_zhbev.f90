program test_zhbev
  use test_utils
  implicit none

  integer :: INFO, N, KD, LDAB, LDZ

  ! Complex arrays with EQUIVALENCE for printing
  complex*16 :: AB(100), Z(100), WORK(100)
  double precision :: AB_r(200), Z_r(200), WORK_r(200)
  equivalence (AB, AB_r)
  equivalence (Z, Z_r)
  equivalence (WORK, WORK_r)

  double precision :: W(10), RWORK(100)

  ! =====================================================================
  ! Test 1: JOBZ='V', UPLO='U', KD=2, N=5
  !
  ! Hermitian 5x5 matrix:
  !   4      (1+i)   (2-i)   0       0
  !  (1-i)    5      (3+i)  (1-i)    0
  !  (2+i)   (3-i)    6     (2+i)   (1-i)
  !   0      (1+i)   (2-i)   7      (3+i)
  !   0       0      (1+i)  (3-i)    8
  !
  ! Upper band storage (LDAB=3=KD+1):
  !   Row 1 (2nd superdiag): *       *      (2-i)  (1-i)  (1-i)
  !   Row 2 (1st superdiag): *      (1+i)  (3+i)  (2+i)  (3+i)
  !   Row 3 (diagonal):      4       5      6      7      8
  ! =====================================================================
  N = 5; KD = 2; LDAB = 3; LDZ = 5
  AB = (0.0d0, 0.0d0); W = 0.0d0; Z = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0); RWORK = 0.0d0

  ! Column 1: AB(3,1)=4
  AB(3) = (4.0d0, 0.0d0)
  ! Column 2: AB(2,2)=(1+i), AB(3,2)=5
  AB(5) = (1.0d0, 1.0d0)
  AB(6) = (5.0d0, 0.0d0)
  ! Column 3: AB(1,3)=(2-i), AB(2,3)=(3+i), AB(3,3)=6
  AB(7) = (2.0d0, -1.0d0)
  AB(8) = (3.0d0, 1.0d0)
  AB(9) = (6.0d0, 0.0d0)
  ! Column 4: AB(1,4)=(1-i), AB(2,4)=(2+i), AB(3,4)=7
  AB(10) = (1.0d0, -1.0d0)
  AB(11) = (2.0d0, 1.0d0)
  AB(12) = (7.0d0, 0.0d0)
  ! Column 5: AB(1,5)=(1-i), AB(2,5)=(3+i), AB(3,5)=8
  AB(13) = (1.0d0, -1.0d0)
  AB(14) = (3.0d0, 1.0d0)
  AB(15) = (8.0d0, 0.0d0)

  call ZHBEV('V', 'U', N, KD, AB, LDAB, W, Z, LDZ, WORK, RWORK, INFO)
  call begin_test('jobz_v_uplo_u_kd2_n5')
  call print_int('info', INFO)
  call print_array('w', W, 5)
  call print_array('Z', Z_r, 5*5*2)
  call end_test()

  ! =====================================================================
  ! Test 2: JOBZ='V', UPLO='L', KD=2, N=5 (same matrix)
  !
  ! Lower band storage (LDAB=3=KD+1):
  !   Row 1 (diagonal):      4       5      6      7      8
  !   Row 2 (1st subdiag):  (1-i)   (3-i)  (2-i)  (3-i)   *
  !   Row 3 (2nd subdiag):  (2+i)   (1+i)  (1+i)   *      *
  ! =====================================================================
  N = 5; KD = 2; LDAB = 3; LDZ = 5
  AB = (0.0d0, 0.0d0); W = 0.0d0; Z = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0); RWORK = 0.0d0

  ! Column 1: AB(1,1)=4, AB(2,1)=(1-i), AB(3,1)=(2+i)
  AB(1) = (4.0d0, 0.0d0)
  AB(2) = (1.0d0, -1.0d0)
  AB(3) = (2.0d0, 1.0d0)
  ! Column 2: AB(1,2)=5, AB(2,2)=(3-i), AB(3,2)=(1+i)
  AB(4) = (5.0d0, 0.0d0)
  AB(5) = (3.0d0, -1.0d0)
  AB(6) = (1.0d0, 1.0d0)
  ! Column 3: AB(1,3)=6, AB(2,3)=(2-i), AB(3,3)=(1+i)
  AB(7) = (6.0d0, 0.0d0)
  AB(8) = (2.0d0, -1.0d0)
  AB(9) = (1.0d0, 1.0d0)
  ! Column 4: AB(1,4)=7, AB(2,4)=(3-i)
  AB(10) = (7.0d0, 0.0d0)
  AB(11) = (3.0d0, -1.0d0)
  ! Column 5: AB(1,5)=8
  AB(13) = (8.0d0, 0.0d0)

  call ZHBEV('V', 'L', N, KD, AB, LDAB, W, Z, LDZ, WORK, RWORK, INFO)
  call begin_test('jobz_v_uplo_l_kd2_n5')
  call print_int('info', INFO)
  call print_array('w', W, 5)
  call print_array('Z', Z_r, 5*5*2)
  call end_test()

  ! =====================================================================
  ! Test 3: JOBZ='N', UPLO='U', KD=2, N=5 (eigenvalues only)
  ! =====================================================================
  N = 5; KD = 2; LDAB = 3; LDZ = 1
  AB = (0.0d0, 0.0d0); W = 0.0d0; Z = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0); RWORK = 0.0d0

  AB(3) = (4.0d0, 0.0d0)
  AB(5) = (1.0d0, 1.0d0); AB(6) = (5.0d0, 0.0d0)
  AB(7) = (2.0d0, -1.0d0); AB(8) = (3.0d0, 1.0d0); AB(9) = (6.0d0, 0.0d0)
  AB(10) = (1.0d0, -1.0d0); AB(11) = (2.0d0, 1.0d0); AB(12) = (7.0d0, 0.0d0)
  AB(13) = (1.0d0, -1.0d0); AB(14) = (3.0d0, 1.0d0); AB(15) = (8.0d0, 0.0d0)

  call ZHBEV('N', 'U', N, KD, AB, LDAB, W, Z, LDZ, WORK, RWORK, INFO)
  call begin_test('jobz_n_uplo_u_kd2_n5')
  call print_int('info', INFO)
  call print_array('w', W, 5)
  call end_test()

  ! =====================================================================
  ! Test 4: JOBZ='N', UPLO='L', KD=2, N=5 (eigenvalues only)
  ! =====================================================================
  N = 5; KD = 2; LDAB = 3; LDZ = 1
  AB = (0.0d0, 0.0d0); W = 0.0d0; Z = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0); RWORK = 0.0d0

  AB(1) = (4.0d0, 0.0d0); AB(2) = (1.0d0, -1.0d0); AB(3) = (2.0d0, 1.0d0)
  AB(4) = (5.0d0, 0.0d0); AB(5) = (3.0d0, -1.0d0); AB(6) = (1.0d0, 1.0d0)
  AB(7) = (6.0d0, 0.0d0); AB(8) = (2.0d0, -1.0d0); AB(9) = (1.0d0, 1.0d0)
  AB(10) = (7.0d0, 0.0d0); AB(11) = (3.0d0, -1.0d0)
  AB(13) = (8.0d0, 0.0d0)

  call ZHBEV('N', 'L', N, KD, AB, LDAB, W, Z, LDZ, WORK, RWORK, INFO)
  call begin_test('jobz_n_uplo_l_kd2_n5')
  call print_int('info', INFO)
  call print_array('w', W, 5)
  call end_test()

  ! =====================================================================
  ! Test 5: JOBZ='V', UPLO='U', KD=1, N=4 (tridiagonal)
  !
  ! Hermitian 4x4 tridiagonal:
  !   4     (1+i)   0      0
  !  (1-i)   5     (2-i)   0
  !   0     (2+i)   6     (3+i)
  !   0      0     (3-i)   7
  !
  ! Upper band (LDAB=2=KD+1):
  !   Row 1 (1st superdiag): *      (1+i)  (2-i)  (3+i)
  !   Row 2 (diagonal):      4       5      6      7
  ! =====================================================================
  N = 4; KD = 1; LDAB = 2; LDZ = 4
  AB = (0.0d0, 0.0d0); W = 0.0d0; Z = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0); RWORK = 0.0d0

  AB(2) = (4.0d0, 0.0d0)
  AB(3) = (1.0d0, 1.0d0); AB(4) = (5.0d0, 0.0d0)
  AB(5) = (2.0d0, -1.0d0); AB(6) = (6.0d0, 0.0d0)
  AB(7) = (3.0d0, 1.0d0); AB(8) = (7.0d0, 0.0d0)

  call ZHBEV('V', 'U', N, KD, AB, LDAB, W, Z, LDZ, WORK, RWORK, INFO)
  call begin_test('jobz_v_uplo_u_kd1_n4')
  call print_int('info', INFO)
  call print_array('w', W, 4)
  call print_array('Z', Z_r, 4*4*2)
  call end_test()

  ! =====================================================================
  ! Test 6: JOBZ='V', UPLO='L', KD=1, N=4 (tridiagonal)
  !
  ! Lower band (LDAB=2=KD+1):
  !   Row 1 (diagonal):      4       5      6      7
  !   Row 2 (1st subdiag):  (1-i)   (2+i)  (3-i)   *
  ! =====================================================================
  N = 4; KD = 1; LDAB = 2; LDZ = 4
  AB = (0.0d0, 0.0d0); W = 0.0d0; Z = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0); RWORK = 0.0d0

  AB(1) = (4.0d0, 0.0d0); AB(2) = (1.0d0, -1.0d0)
  AB(3) = (5.0d0, 0.0d0); AB(4) = (2.0d0, 1.0d0)
  AB(5) = (6.0d0, 0.0d0); AB(6) = (3.0d0, -1.0d0)
  AB(7) = (7.0d0, 0.0d0)

  call ZHBEV('V', 'L', N, KD, AB, LDAB, W, Z, LDZ, WORK, RWORK, INFO)
  call begin_test('jobz_v_uplo_l_kd1_n4')
  call print_int('info', INFO)
  call print_array('w', W, 4)
  call print_array('Z', Z_r, 4*4*2)
  call end_test()

  ! =====================================================================
  ! Test 7: JOBZ='N', UPLO='U', KD=1, N=4 (eigenvalues only)
  ! =====================================================================
  N = 4; KD = 1; LDAB = 2; LDZ = 1
  AB = (0.0d0, 0.0d0); W = 0.0d0; Z = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0); RWORK = 0.0d0

  AB(2) = (4.0d0, 0.0d0)
  AB(3) = (1.0d0, 1.0d0); AB(4) = (5.0d0, 0.0d0)
  AB(5) = (2.0d0, -1.0d0); AB(6) = (6.0d0, 0.0d0)
  AB(7) = (3.0d0, 1.0d0); AB(8) = (7.0d0, 0.0d0)

  call ZHBEV('N', 'U', N, KD, AB, LDAB, W, Z, LDZ, WORK, RWORK, INFO)
  call begin_test('jobz_n_uplo_u_kd1_n4')
  call print_int('info', INFO)
  call print_array('w', W, 4)
  call end_test()

  ! =====================================================================
  ! Test 8: N=1, JOBZ='V', UPLO='L'
  ! =====================================================================
  N = 1; KD = 0; LDAB = 1; LDZ = 1
  AB = (0.0d0, 0.0d0); W = 0.0d0; Z = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0); RWORK = 0.0d0
  AB(1) = (3.5d0, 0.0d0)

  call ZHBEV('V', 'L', N, KD, AB, LDAB, W, Z, LDZ, WORK, RWORK, INFO)
  call begin_test('n1_jobz_v_lower')
  call print_int('info', INFO)
  call print_scalar('w1', W(1))
  call print_array('z11', Z_r, 2)
  call end_test()

  ! =====================================================================
  ! Test 9: N=1, JOBZ='V', UPLO='U', KD=2
  ! The element is at AB(KD+1, 1) = AB(3, 1)
  ! =====================================================================
  N = 1; KD = 2; LDAB = 3; LDZ = 1
  AB = (0.0d0, 0.0d0); W = 0.0d0; Z = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0); RWORK = 0.0d0
  AB(3) = (7.25d0, 0.0d0)

  call ZHBEV('V', 'U', N, KD, AB, LDAB, W, Z, LDZ, WORK, RWORK, INFO)
  call begin_test('n1_jobz_v_upper_kd2')
  call print_int('info', INFO)
  call print_scalar('w1', W(1))
  call print_array('z11', Z_r, 2)
  call end_test()

  ! =====================================================================
  ! Test 10: N=1, JOBZ='N', UPLO='U'
  ! =====================================================================
  N = 1; KD = 0; LDAB = 1; LDZ = 1
  AB = (0.0d0, 0.0d0); W = 0.0d0
  WORK = (0.0d0, 0.0d0); RWORK = 0.0d0
  AB(1) = (9.0d0, 0.0d0)

  call ZHBEV('N', 'U', N, KD, AB, LDAB, W, Z, LDZ, WORK, RWORK, INFO)
  call begin_test('n1_jobz_n')
  call print_int('info', INFO)
  call print_scalar('w1', W(1))
  call end_test()

  ! =====================================================================
  ! Test 11: N=0, JOBZ='V' (quick return)
  ! =====================================================================
  N = 0; KD = 0; LDAB = 1; LDZ = 1
  call ZHBEV('V', 'U', N, KD, AB, LDAB, W, Z, LDZ, WORK, RWORK, INFO)
  call begin_test('n0')
  call print_int('info', INFO)
  call end_test()

  ! =====================================================================
  ! Test 12: KD=0 (diagonal matrix), JOBZ='V', UPLO='U'
  !
  ! A = diag(3, 1, 4, 2) (complex diagonal, all real)
  ! Eigenvalues should be sorted: 1, 2, 3, 4
  ! =====================================================================
  N = 4; KD = 0; LDAB = 1; LDZ = 4
  AB = (0.0d0, 0.0d0); W = 0.0d0; Z = (0.0d0, 0.0d0)
  WORK = (0.0d0, 0.0d0); RWORK = 0.0d0
  AB(1) = (3.0d0, 0.0d0)
  AB(2) = (1.0d0, 0.0d0)
  AB(3) = (4.0d0, 0.0d0)
  AB(4) = (2.0d0, 0.0d0)

  call ZHBEV('V', 'U', N, KD, AB, LDAB, W, Z, LDZ, WORK, RWORK, INFO)
  call begin_test('diagonal_jobz_v')
  call print_int('info', INFO)
  call print_array('w', W, 4)
  call print_array('Z', Z_r, 4*4*2)
  call end_test()

  ! =====================================================================
  ! Test 13: KD=0 (diagonal matrix), JOBZ='N', UPLO='L'
  ! =====================================================================
  N = 4; KD = 0; LDAB = 1; LDZ = 1
  AB = (0.0d0, 0.0d0); W = 0.0d0
  WORK = (0.0d0, 0.0d0); RWORK = 0.0d0
  AB(1) = (3.0d0, 0.0d0)
  AB(2) = (1.0d0, 0.0d0)
  AB(3) = (4.0d0, 0.0d0)
  AB(4) = (2.0d0, 0.0d0)

  call ZHBEV('N', 'L', N, KD, AB, LDAB, W, Z, LDZ, WORK, RWORK, INFO)
  call begin_test('diagonal_jobz_n')
  call print_int('info', INFO)
  call print_array('w', W, 4)
  call end_test()

end program
