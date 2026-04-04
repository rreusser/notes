program test_dsbev
  use test_utils
  implicit none

  integer :: INFO, N, KD, LDAB, LDZ
  double precision :: W(10), Z(10, 10), WORK(1000)

  double precision :: AB3(3,10), AB2(2,10), AB4(4,10), AB1(1,10)

  ! =====================================================================
  ! Test 1: JOBZ='V', UPLO='U', KD=2, N=5
  !
  ! Symmetric 5x5 matrix:
  !   4  1  2  0  0
  !   1  5  3  1  0
  !   2  3  6  2  1
  !   0  1  2  7  3
  !   0  0  1  3  8
  !
  ! Upper band storage (LDAB=3=KD+1):
  !   Row 1 (2nd superdiag): *, *, 2, 1, 1
  !   Row 2 (1st superdiag): *, 1, 3, 2, 3
  !   Row 3 (diagonal):      4, 5, 6, 7, 8
  ! =====================================================================
  N = 5; KD = 2; LDAB = 3; LDZ = 10
  AB3 = 0.0d0; W = 0.0d0; Z = 0.0d0; WORK = 0.0d0

  AB3(3,1) = 4.0d0
  AB3(2,2) = 1.0d0; AB3(3,2) = 5.0d0
  AB3(1,3) = 2.0d0; AB3(2,3) = 3.0d0; AB3(3,3) = 6.0d0
  AB3(1,4) = 1.0d0; AB3(2,4) = 2.0d0; AB3(3,4) = 7.0d0
  AB3(1,5) = 1.0d0; AB3(2,5) = 3.0d0; AB3(3,5) = 8.0d0

  call DSBEV('V', 'U', N, KD, AB3, LDAB, W, Z, LDZ, WORK, INFO)
  call begin_test('jobz_v_uplo_u_kd2_n5')
  call print_int('info', INFO)
  call print_array('w', W, 5)
  call print_matrix('Z', Z, LDZ, N, N)
  call end_test()

  ! =====================================================================
  ! Test 2: JOBZ='V', UPLO='L', KD=2, N=5 (same matrix)
  !
  ! Lower band storage (LDAB=3=KD+1):
  !   Row 1 (diagonal):      4, 5, 6, 7, 8
  !   Row 2 (1st subdiag):   1, 3, 2, 3, *
  !   Row 3 (2nd subdiag):   2, 1, 1, *, *
  ! =====================================================================
  N = 5; KD = 2; LDAB = 3; LDZ = 10
  AB3 = 0.0d0; W = 0.0d0; Z = 0.0d0; WORK = 0.0d0

  AB3(1,1) = 4.0d0; AB3(2,1) = 1.0d0; AB3(3,1) = 2.0d0
  AB3(1,2) = 5.0d0; AB3(2,2) = 3.0d0; AB3(3,2) = 1.0d0
  AB3(1,3) = 6.0d0; AB3(2,3) = 2.0d0; AB3(3,3) = 1.0d0
  AB3(1,4) = 7.0d0; AB3(2,4) = 3.0d0
  AB3(1,5) = 8.0d0

  call DSBEV('V', 'L', N, KD, AB3, LDAB, W, Z, LDZ, WORK, INFO)
  call begin_test('jobz_v_uplo_l_kd2_n5')
  call print_int('info', INFO)
  call print_array('w', W, 5)
  call print_matrix('Z', Z, LDZ, N, N)
  call end_test()

  ! =====================================================================
  ! Test 3: JOBZ='N', UPLO='U', KD=2, N=5 (eigenvalues only)
  ! =====================================================================
  N = 5; KD = 2; LDAB = 3; LDZ = 1
  AB3 = 0.0d0; W = 0.0d0; Z = 0.0d0; WORK = 0.0d0

  AB3(3,1) = 4.0d0
  AB3(2,2) = 1.0d0; AB3(3,2) = 5.0d0
  AB3(1,3) = 2.0d0; AB3(2,3) = 3.0d0; AB3(3,3) = 6.0d0
  AB3(1,4) = 1.0d0; AB3(2,4) = 2.0d0; AB3(3,4) = 7.0d0
  AB3(1,5) = 1.0d0; AB3(2,5) = 3.0d0; AB3(3,5) = 8.0d0

  call DSBEV('N', 'U', N, KD, AB3, LDAB, W, Z, LDZ, WORK, INFO)
  call begin_test('jobz_n_uplo_u_kd2_n5')
  call print_int('info', INFO)
  call print_array('w', W, 5)
  call end_test()

  ! =====================================================================
  ! Test 4: JOBZ='N', UPLO='L', KD=2, N=5 (eigenvalues only)
  ! =====================================================================
  N = 5; KD = 2; LDAB = 3; LDZ = 1
  AB3 = 0.0d0; W = 0.0d0; Z = 0.0d0; WORK = 0.0d0

  AB3(1,1) = 4.0d0; AB3(2,1) = 1.0d0; AB3(3,1) = 2.0d0
  AB3(1,2) = 5.0d0; AB3(2,2) = 3.0d0; AB3(3,2) = 1.0d0
  AB3(1,3) = 6.0d0; AB3(2,3) = 2.0d0; AB3(3,3) = 1.0d0
  AB3(1,4) = 7.0d0; AB3(2,4) = 3.0d0
  AB3(1,5) = 8.0d0

  call DSBEV('N', 'L', N, KD, AB3, LDAB, W, Z, LDZ, WORK, INFO)
  call begin_test('jobz_n_uplo_l_kd2_n5')
  call print_int('info', INFO)
  call print_array('w', W, 5)
  call end_test()

  ! =====================================================================
  ! Test 5: JOBZ='V', UPLO='U', KD=1, N=4 (tridiagonal)
  !
  ! Symmetric 4x4 tridiagonal:
  !   4  1  0  0
  !   1  5  2  0
  !   0  2  6  3
  !   0  0  3  7
  !
  ! Upper band (LDAB=2=KD+1):
  !   Row 1 (1st superdiag): *, 1, 2, 3
  !   Row 2 (diagonal):      4, 5, 6, 7
  ! =====================================================================
  N = 4; KD = 1; LDAB = 2; LDZ = 10
  AB2 = 0.0d0; W = 0.0d0; Z = 0.0d0; WORK = 0.0d0

  AB2(2,1) = 4.0d0
  AB2(1,2) = 1.0d0; AB2(2,2) = 5.0d0
  AB2(1,3) = 2.0d0; AB2(2,3) = 6.0d0
  AB2(1,4) = 3.0d0; AB2(2,4) = 7.0d0

  call DSBEV('V', 'U', N, KD, AB2, LDAB, W, Z, LDZ, WORK, INFO)
  call begin_test('jobz_v_uplo_u_kd1_n4')
  call print_int('info', INFO)
  call print_array('w', W, 4)
  call print_matrix('Z', Z, LDZ, N, N)
  call end_test()

  ! =====================================================================
  ! Test 6: JOBZ='V', UPLO='L', KD=1, N=4 (tridiagonal)
  !
  ! Lower band (LDAB=2=KD+1):
  !   Row 1 (diagonal):      4, 5, 6, 7
  !   Row 2 (1st subdiag):   1, 2, 3, *
  ! =====================================================================
  N = 4; KD = 1; LDAB = 2; LDZ = 10
  AB2 = 0.0d0; W = 0.0d0; Z = 0.0d0; WORK = 0.0d0

  AB2(1,1) = 4.0d0; AB2(2,1) = 1.0d0
  AB2(1,2) = 5.0d0; AB2(2,2) = 2.0d0
  AB2(1,3) = 6.0d0; AB2(2,3) = 3.0d0
  AB2(1,4) = 7.0d0

  call DSBEV('V', 'L', N, KD, AB2, LDAB, W, Z, LDZ, WORK, INFO)
  call begin_test('jobz_v_uplo_l_kd1_n4')
  call print_int('info', INFO)
  call print_array('w', W, 4)
  call print_matrix('Z', Z, LDZ, N, N)
  call end_test()

  ! =====================================================================
  ! Test 7: JOBZ='N', UPLO='U', KD=1, N=4 (eigenvalues only)
  ! =====================================================================
  N = 4; KD = 1; LDAB = 2; LDZ = 1
  AB2 = 0.0d0; W = 0.0d0; Z = 0.0d0; WORK = 0.0d0

  AB2(2,1) = 4.0d0
  AB2(1,2) = 1.0d0; AB2(2,2) = 5.0d0
  AB2(1,3) = 2.0d0; AB2(2,3) = 6.0d0
  AB2(1,4) = 3.0d0; AB2(2,4) = 7.0d0

  call DSBEV('N', 'U', N, KD, AB2, LDAB, W, Z, LDZ, WORK, INFO)
  call begin_test('jobz_n_uplo_u_kd1_n4')
  call print_int('info', INFO)
  call print_array('w', W, 4)
  call end_test()

  ! =====================================================================
  ! Test 8: JOBZ='V', UPLO='U', KD=3, N=6
  !
  ! Symmetric 6x6 with bandwidth 3:
  !   5  2  1  3  0  0
  !   2  6  3  1  2  0
  !   1  3  7  2  3  1
  !   3  1  2  8  1  2
  !   0  2  3  1  9  3
  !   0  0  1  2  3 10
  !
  ! Upper band (LDAB=4=KD+1):
  !   Row 1 (3rd superdiag): *, *, *, 3, 2, 1
  !   Row 2 (2nd superdiag): *, *, 1, 1, 3, 2
  !   Row 3 (1st superdiag): *, 2, 3, 2, 1, 3
  !   Row 4 (diagonal):      5, 6, 7, 8, 9,10
  ! =====================================================================
  N = 6; KD = 3; LDAB = 4; LDZ = 10
  AB4 = 0.0d0; W = 0.0d0; Z = 0.0d0; WORK = 0.0d0

  AB4(4,1) = 5.0d0
  AB4(3,2) = 2.0d0; AB4(4,2) = 6.0d0
  AB4(2,3) = 1.0d0; AB4(3,3) = 3.0d0; AB4(4,3) = 7.0d0
  AB4(1,4) = 3.0d0; AB4(2,4) = 1.0d0; AB4(3,4) = 2.0d0; AB4(4,4) = 8.0d0
  AB4(1,5) = 2.0d0; AB4(2,5) = 3.0d0; AB4(3,5) = 1.0d0; AB4(4,5) = 9.0d0
  AB4(1,6) = 1.0d0; AB4(2,6) = 2.0d0; AB4(3,6) = 3.0d0; AB4(4,6) = 10.0d0

  call DSBEV('V', 'U', N, KD, AB4, LDAB, W, Z, LDZ, WORK, INFO)
  call begin_test('jobz_v_uplo_u_kd3_n6')
  call print_int('info', INFO)
  call print_array('w', W, 6)
  call print_matrix('Z', Z, LDZ, N, N)
  call end_test()

  ! =====================================================================
  ! Test 9: JOBZ='V', UPLO='L', KD=3, N=6 (same matrix)
  !
  ! Lower band (LDAB=4=KD+1):
  !   Row 1 (diagonal):      5, 6, 7, 8, 9,10
  !   Row 2 (1st subdiag):   2, 3, 2, 1, 3, *
  !   Row 3 (2nd subdiag):   1, 1, 3, 2, *, *
  !   Row 4 (3rd subdiag):   3, 2, 1, *, *, *
  ! =====================================================================
  N = 6; KD = 3; LDAB = 4; LDZ = 10
  AB4 = 0.0d0; W = 0.0d0; Z = 0.0d0; WORK = 0.0d0

  AB4(1,1) = 5.0d0; AB4(2,1) = 2.0d0; AB4(3,1) = 1.0d0; AB4(4,1) = 3.0d0
  AB4(1,2) = 6.0d0; AB4(2,2) = 3.0d0; AB4(3,2) = 1.0d0; AB4(4,2) = 2.0d0
  AB4(1,3) = 7.0d0; AB4(2,3) = 2.0d0; AB4(3,3) = 3.0d0; AB4(4,3) = 1.0d0
  AB4(1,4) = 8.0d0; AB4(2,4) = 1.0d0; AB4(3,4) = 2.0d0
  AB4(1,5) = 9.0d0; AB4(2,5) = 3.0d0
  AB4(1,6) = 10.0d0

  call DSBEV('V', 'L', N, KD, AB4, LDAB, W, Z, LDZ, WORK, INFO)
  call begin_test('jobz_v_uplo_l_kd3_n6')
  call print_int('info', INFO)
  call print_array('w', W, 6)
  call print_matrix('Z', Z, LDZ, N, N)
  call end_test()

  ! =====================================================================
  ! Test 10: N=1, JOBZ='V', UPLO='L'
  ! =====================================================================
  N = 1; KD = 0; LDAB = 1; LDZ = 10
  AB1 = 0.0d0; W = 0.0d0; Z = 0.0d0; WORK = 0.0d0
  AB1(1,1) = 3.5d0

  call DSBEV('V', 'L', N, KD, AB1, LDAB, W, Z, LDZ, WORK, INFO)
  call begin_test('n1_jobz_v_lower')
  call print_int('info', INFO)
  call print_scalar('w1', W(1))
  call print_scalar('z11', Z(1,1))
  call end_test()

  ! =====================================================================
  ! Test 11: N=1, JOBZ='V', UPLO='U', KD=2
  ! The element is at AB(KD+1, 1) = AB(3, 1)
  ! =====================================================================
  N = 1; KD = 2; LDAB = 3; LDZ = 10
  AB3 = 0.0d0; W = 0.0d0; Z = 0.0d0; WORK = 0.0d0
  AB3(3,1) = 7.25d0

  call DSBEV('V', 'U', N, KD, AB3, LDAB, W, Z, LDZ, WORK, INFO)
  call begin_test('n1_jobz_v_upper_kd2')
  call print_int('info', INFO)
  call print_scalar('w1', W(1))
  call print_scalar('z11', Z(1,1))
  call end_test()

  ! =====================================================================
  ! Test 12: N=1, JOBZ='N', UPLO='U'
  ! =====================================================================
  N = 1; KD = 0; LDAB = 1; LDZ = 1
  AB1 = 0.0d0; W = 0.0d0; WORK = 0.0d0
  AB1(1,1) = 9.0d0

  call DSBEV('N', 'U', N, KD, AB1, LDAB, W, Z, LDZ, WORK, INFO)
  call begin_test('n1_jobz_n')
  call print_int('info', INFO)
  call print_scalar('w1', W(1))
  call end_test()

  ! =====================================================================
  ! Test 13: N=0, JOBZ='V' (quick return)
  ! =====================================================================
  N = 0; KD = 0; LDAB = 1; LDZ = 1
  call DSBEV('V', 'U', N, KD, AB1, LDAB, W, Z, LDZ, WORK, INFO)
  call begin_test('n0')
  call print_int('info', INFO)
  call end_test()

  ! =====================================================================
  ! Test 14: KD=0 (diagonal matrix), JOBZ='V', UPLO='U'
  !
  ! A = diag(3, 1, 4, 2)
  ! Band storage (LDAB=1): Row 1 (diagonal): 3, 1, 4, 2
  ! Eigenvalues should be sorted: 1, 2, 3, 4
  ! =====================================================================
  N = 4; KD = 0; LDAB = 1; LDZ = 10
  AB1 = 0.0d0; W = 0.0d0; Z = 0.0d0; WORK = 0.0d0
  AB1(1,1) = 3.0d0; AB1(1,2) = 1.0d0; AB1(1,3) = 4.0d0; AB1(1,4) = 2.0d0

  call DSBEV('V', 'U', N, KD, AB1, LDAB, W, Z, LDZ, WORK, INFO)
  call begin_test('diagonal_jobz_v')
  call print_int('info', INFO)
  call print_array('w', W, 4)
  call print_matrix('Z', Z, LDZ, N, N)
  call end_test()

  ! =====================================================================
  ! Test 15: KD=0 (diagonal matrix), JOBZ='N', UPLO='L'
  ! =====================================================================
  N = 4; KD = 0; LDAB = 1; LDZ = 1
  AB1 = 0.0d0; W = 0.0d0; WORK = 0.0d0
  AB1(1,1) = 3.0d0; AB1(1,2) = 1.0d0; AB1(1,3) = 4.0d0; AB1(1,4) = 2.0d0

  call DSBEV('N', 'L', N, KD, AB1, LDAB, W, Z, LDZ, WORK, INFO)
  call begin_test('diagonal_jobz_n')
  call print_int('info', INFO)
  call print_array('w', W, 4)
  call end_test()

  ! =====================================================================
  ! Test 16: JOBZ='N', UPLO='L', KD=3, N=6 (eigenvalues only)
  ! =====================================================================
  N = 6; KD = 3; LDAB = 4; LDZ = 1
  AB4 = 0.0d0; W = 0.0d0; WORK = 0.0d0

  AB4(1,1) = 5.0d0; AB4(2,1) = 2.0d0; AB4(3,1) = 1.0d0; AB4(4,1) = 3.0d0
  AB4(1,2) = 6.0d0; AB4(2,2) = 3.0d0; AB4(3,2) = 1.0d0; AB4(4,2) = 2.0d0
  AB4(1,3) = 7.0d0; AB4(2,3) = 2.0d0; AB4(3,3) = 3.0d0; AB4(4,3) = 1.0d0
  AB4(1,4) = 8.0d0; AB4(2,4) = 1.0d0; AB4(3,4) = 2.0d0
  AB4(1,5) = 9.0d0; AB4(2,5) = 3.0d0
  AB4(1,6) = 10.0d0

  call DSBEV('N', 'L', N, KD, AB4, LDAB, W, Z, LDZ, WORK, INFO)
  call begin_test('jobz_n_uplo_l_kd3_n6')
  call print_int('info', INFO)
  call print_array('w', W, 6)
  call end_test()

end program
