program test_dsbtrd
  use test_utils
  implicit none

  integer :: INFO, N, KD, LDAB, LDQ, I
  ! Use separate smaller arrays for tests with specific LDAB
  double precision :: D(10), E(10), Q(10, 10), WORK(10)

  ! We need separate AB arrays with correct LDAB (leading dim = KD+1)
  double precision :: AB3(3,10), AB2(2,10), AB4(4,10), AB1(1,10)

  ! =====================================================================
  ! Test 1: UPLO='U', KD=2, N=5, VECT='N' (no Q)
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
  N = 5; KD = 2; LDAB = 3; LDQ = 1
  AB3 = 0.0d0; D = 0.0d0; E = 0.0d0; Q = 0.0d0; WORK = 0.0d0

  AB3(3,1) = 4.0d0
  AB3(2,2) = 1.0d0; AB3(3,2) = 5.0d0
  AB3(1,3) = 2.0d0; AB3(2,3) = 3.0d0; AB3(3,3) = 6.0d0
  AB3(1,4) = 1.0d0; AB3(2,4) = 2.0d0; AB3(3,4) = 7.0d0
  AB3(1,5) = 1.0d0; AB3(2,5) = 3.0d0; AB3(3,5) = 8.0d0

  call DSBTRD('N', 'U', N, KD, AB3, LDAB, D, E, Q, LDQ, WORK, INFO)
  call begin_test('upper_kd2_n5_none')
  call print_int('info', INFO)
  call print_array('d', D, 5)
  call print_array('e', E, 4)
  call end_test()

  ! =====================================================================
  ! Test 2: Same matrix, UPLO='L', KD=2, N=5, VECT='N'
  !
  ! Lower band storage (LDAB=3=KD+1):
  !   Row 1 (diagonal):      4, 5, 6, 7, 8
  !   Row 2 (1st subdiag):   1, 3, 2, 3, *
  !   Row 3 (2nd subdiag):   2, 1, 1, *, *
  ! =====================================================================
  N = 5; KD = 2; LDAB = 3; LDQ = 1
  AB3 = 0.0d0; D = 0.0d0; E = 0.0d0; Q = 0.0d0; WORK = 0.0d0

  AB3(1,1) = 4.0d0; AB3(2,1) = 1.0d0; AB3(3,1) = 2.0d0
  AB3(1,2) = 5.0d0; AB3(2,2) = 3.0d0; AB3(3,2) = 1.0d0
  AB3(1,3) = 6.0d0; AB3(2,3) = 2.0d0; AB3(3,3) = 1.0d0
  AB3(1,4) = 7.0d0; AB3(2,4) = 3.0d0
  AB3(1,5) = 8.0d0

  call DSBTRD('N', 'L', N, KD, AB3, LDAB, D, E, Q, LDQ, WORK, INFO)
  call begin_test('lower_kd2_n5_none')
  call print_int('info', INFO)
  call print_array('d', D, 5)
  call print_array('e', E, 4)
  call end_test()

  ! =====================================================================
  ! Test 3: UPLO='U', KD=1, N=4, VECT='N' (tridiagonal input)
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
  N = 4; KD = 1; LDAB = 2; LDQ = 1
  AB2 = 0.0d0; D = 0.0d0; E = 0.0d0; Q = 0.0d0; WORK = 0.0d0

  AB2(2,1) = 4.0d0
  AB2(1,2) = 1.0d0; AB2(2,2) = 5.0d0
  AB2(1,3) = 2.0d0; AB2(2,3) = 6.0d0
  AB2(1,4) = 3.0d0; AB2(2,4) = 7.0d0

  call DSBTRD('N', 'U', N, KD, AB2, LDAB, D, E, Q, LDQ, WORK, INFO)
  call begin_test('upper_kd1_n4_none')
  call print_int('info', INFO)
  call print_array('d', D, 4)
  call print_array('e', E, 3)
  call end_test()

  ! =====================================================================
  ! Test 4: UPLO='L', KD=1, N=4, VECT='N' (tridiagonal input)
  !
  ! Lower band (LDAB=2=KD+1):
  !   Row 1 (diagonal):      4, 5, 6, 7
  !   Row 2 (1st subdiag):   1, 2, 3, *
  ! =====================================================================
  N = 4; KD = 1; LDAB = 2; LDQ = 1
  AB2 = 0.0d0; D = 0.0d0; E = 0.0d0; Q = 0.0d0; WORK = 0.0d0

  AB2(1,1) = 4.0d0; AB2(2,1) = 1.0d0
  AB2(1,2) = 5.0d0; AB2(2,2) = 2.0d0
  AB2(1,3) = 6.0d0; AB2(2,3) = 3.0d0
  AB2(1,4) = 7.0d0

  call DSBTRD('N', 'L', N, KD, AB2, LDAB, D, E, Q, LDQ, WORK, INFO)
  call begin_test('lower_kd1_n4_none')
  call print_int('info', INFO)
  call print_array('d', D, 4)
  call print_array('e', E, 3)
  call end_test()

  ! =====================================================================
  ! Test 5: UPLO='U', KD=3, N=6, VECT='V' (initialize Q)
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
  N = 6; KD = 3; LDAB = 4; LDQ = 10
  AB4 = 0.0d0; D = 0.0d0; E = 0.0d0; Q = 0.0d0; WORK = 0.0d0

  AB4(4,1) = 5.0d0
  AB4(3,2) = 2.0d0; AB4(4,2) = 6.0d0
  AB4(2,3) = 1.0d0; AB4(3,3) = 3.0d0; AB4(4,3) = 7.0d0
  AB4(1,4) = 3.0d0; AB4(2,4) = 1.0d0; AB4(3,4) = 2.0d0; AB4(4,4) = 8.0d0
  AB4(1,5) = 2.0d0; AB4(2,5) = 3.0d0; AB4(3,5) = 1.0d0; AB4(4,5) = 9.0d0
  AB4(1,6) = 1.0d0; AB4(2,6) = 2.0d0; AB4(3,6) = 3.0d0; AB4(4,6) = 10.0d0

  call DSBTRD('V', 'U', N, KD, AB4, LDAB, D, E, Q, LDQ, WORK, INFO)
  call begin_test('upper_kd3_n6_init')
  call print_int('info', INFO)
  call print_array('d', D, 6)
  call print_array('e', E, 5)
  call print_matrix('Q', Q, LDQ, N, N)
  call end_test()

  ! =====================================================================
  ! Test 6: UPLO='L', KD=3, N=6, VECT='V' (initialize Q), same matrix
  !
  ! Lower band (LDAB=4=KD+1):
  !   Row 1 (diagonal):      5, 6, 7, 8, 9,10
  !   Row 2 (1st subdiag):   2, 3, 2, 1, 3, *
  !   Row 3 (2nd subdiag):   1, 1, 3, 2, *, *
  !   Row 4 (3rd subdiag):   3, 2, 1, *, *, *
  ! =====================================================================
  N = 6; KD = 3; LDAB = 4; LDQ = 10
  AB4 = 0.0d0; D = 0.0d0; E = 0.0d0; Q = 0.0d0; WORK = 0.0d0

  AB4(1,1) = 5.0d0; AB4(2,1) = 2.0d0; AB4(3,1) = 1.0d0; AB4(4,1) = 3.0d0
  AB4(1,2) = 6.0d0; AB4(2,2) = 3.0d0; AB4(3,2) = 1.0d0; AB4(4,2) = 2.0d0
  AB4(1,3) = 7.0d0; AB4(2,3) = 2.0d0; AB4(3,3) = 3.0d0; AB4(4,3) = 1.0d0
  AB4(1,4) = 8.0d0; AB4(2,4) = 1.0d0; AB4(3,4) = 2.0d0
  AB4(1,5) = 9.0d0; AB4(2,5) = 3.0d0
  AB4(1,6) = 10.0d0

  call DSBTRD('V', 'L', N, KD, AB4, LDAB, D, E, Q, LDQ, WORK, INFO)
  call begin_test('lower_kd3_n6_init')
  call print_int('info', INFO)
  call print_array('d', D, 6)
  call print_array('e', E, 5)
  call print_matrix('Q', Q, LDQ, N, N)
  call end_test()

  ! =====================================================================
  ! Test 7: UPLO='U', KD=2, N=4, VECT='U' (update Q)
  !
  ! Symmetric 4x4:
  !   3  1  2  0
  !   1  4  1  2
  !   2  1  5  1
  !   0  2  1  6
  !
  ! Upper band (LDAB=3=KD+1):
  !   Row 1 (2nd superdiag): *, *, 2, 2
  !   Row 2 (1st superdiag): *, 1, 1, 1
  !   Row 3 (diagonal):      3, 4, 5, 6
  !
  ! Initialize Q to identity before calling with 'U'
  ! =====================================================================
  N = 4; KD = 2; LDAB = 3; LDQ = 10
  AB3 = 0.0d0; D = 0.0d0; E = 0.0d0; Q = 0.0d0; WORK = 0.0d0

  AB3(3,1) = 3.0d0
  AB3(2,2) = 1.0d0; AB3(3,2) = 4.0d0
  AB3(1,3) = 2.0d0; AB3(2,3) = 1.0d0; AB3(3,3) = 5.0d0
  AB3(1,4) = 2.0d0; AB3(2,4) = 1.0d0; AB3(3,4) = 6.0d0

  ! Set Q to identity (use LDQ=10 since Q is 10x10)
  do I = 1, N
    Q(I, I) = 1.0d0
  end do

  call DSBTRD('U', 'U', N, KD, AB3, LDAB, D, E, Q, LDQ, WORK, INFO)
  call begin_test('upper_kd2_n4_update')
  call print_int('info', INFO)
  call print_array('d', D, 4)
  call print_array('e', E, 3)
  call print_matrix('Q', Q, LDQ, N, N)
  call end_test()

  ! =====================================================================
  ! Test 8: N=0, quick return
  ! =====================================================================
  N = 0; KD = 0; LDAB = 1; LDQ = 1
  AB1 = 0.0d0; D = 0.0d0; E = 0.0d0; Q = 0.0d0; WORK = 0.0d0
  call DSBTRD('N', 'U', N, KD, AB1, LDAB, D, E, Q, LDQ, WORK, INFO)
  call begin_test('n_zero')
  call print_int('info', INFO)
  call end_test()

  ! =====================================================================
  ! Test 9: N=1, UPLO='U', VECT='V'
  ! =====================================================================
  N = 1; KD = 0; LDAB = 1; LDQ = 10
  AB1 = 0.0d0; D = 0.0d0; E = 0.0d0; Q = 0.0d0; WORK = 0.0d0
  AB1(1,1) = 7.0d0
  call DSBTRD('V', 'U', N, KD, AB1, LDAB, D, E, Q, LDQ, WORK, INFO)
  call begin_test('n_one_upper_init')
  call print_int('info', INFO)
  call print_array('d', D, 1)
  call print_matrix('Q', Q, LDQ, 1, 1)
  call end_test()

  ! =====================================================================
  ! Test 10: N=1, UPLO='L', VECT='N'
  ! =====================================================================
  N = 1; KD = 0; LDAB = 1; LDQ = 1
  AB1 = 0.0d0; D = 0.0d0; E = 0.0d0; Q = 0.0d0; WORK = 0.0d0
  AB1(1,1) = 7.0d0
  call DSBTRD('N', 'L', N, KD, AB1, LDAB, D, E, Q, LDQ, WORK, INFO)
  call begin_test('n_one_lower_none')
  call print_int('info', INFO)
  call print_array('d', D, 1)
  call end_test()

  ! =====================================================================
  ! Test 11: KD=0 (diagonal matrix), UPLO='U', VECT='N'
  !
  ! A = diag(2, 5, 8) in band storage (LDAB=1):
  !   Row 1 (diagonal): 2, 5, 8
  ! =====================================================================
  N = 3; KD = 0; LDAB = 1; LDQ = 1
  AB1 = 0.0d0; D = 0.0d0; E = 0.0d0; Q = 0.0d0; WORK = 0.0d0
  AB1(1,1) = 2.0d0; AB1(1,2) = 5.0d0; AB1(1,3) = 8.0d0

  call DSBTRD('N', 'U', N, KD, AB1, LDAB, D, E, Q, LDQ, WORK, INFO)
  call begin_test('kd0_upper_none')
  call print_int('info', INFO)
  call print_array('d', D, 3)
  call print_array('e', E, 2)
  call end_test()

  ! =====================================================================
  ! Test 12: UPLO='L', KD=2, N=4, VECT='V'
  !
  ! Symmetric 4x4:
  !   3  1  2  0
  !   1  4  1  2
  !   2  1  5  1
  !   0  2  1  6
  !
  ! Lower band (LDAB=3=KD+1):
  !   Row 1 (diagonal):      3, 4, 5, 6
  !   Row 2 (1st subdiag):   1, 1, 1, *
  !   Row 3 (2nd subdiag):   2, 2, *, *
  ! =====================================================================
  N = 4; KD = 2; LDAB = 3; LDQ = 10
  AB3 = 0.0d0; D = 0.0d0; E = 0.0d0; Q = 0.0d0; WORK = 0.0d0

  AB3(1,1) = 3.0d0; AB3(2,1) = 1.0d0; AB3(3,1) = 2.0d0
  AB3(1,2) = 4.0d0; AB3(2,2) = 1.0d0; AB3(3,2) = 2.0d0
  AB3(1,3) = 5.0d0; AB3(2,3) = 1.0d0
  AB3(1,4) = 6.0d0

  call DSBTRD('V', 'L', N, KD, AB3, LDAB, D, E, Q, LDQ, WORK, INFO)
  call begin_test('lower_kd2_n4_init')
  call print_int('info', INFO)
  call print_array('d', D, 4)
  call print_array('e', E, 3)
  call print_matrix('Q', Q, LDQ, N, N)
  call end_test()

  ! =====================================================================
  ! Test 13: UPLO='U', KD=1, N=4, VECT='V' (tridiagonal, initialize Q)
  ! Same tridiagonal matrix as test 3
  ! =====================================================================
  N = 4; KD = 1; LDAB = 2; LDQ = 10
  AB2 = 0.0d0; D = 0.0d0; E = 0.0d0; Q = 0.0d0; WORK = 0.0d0

  AB2(2,1) = 4.0d0
  AB2(1,2) = 1.0d0; AB2(2,2) = 5.0d0
  AB2(1,3) = 2.0d0; AB2(2,3) = 6.0d0
  AB2(1,4) = 3.0d0; AB2(2,4) = 7.0d0

  call DSBTRD('V', 'U', N, KD, AB2, LDAB, D, E, Q, LDQ, WORK, INFO)
  call begin_test('upper_kd1_n4_init')
  call print_int('info', INFO)
  call print_array('d', D, 4)
  call print_array('e', E, 3)
  call print_matrix('Q', Q, LDQ, N, N)
  call end_test()

  ! =====================================================================
  ! Test 14: UPLO='L', KD=1, N=4, VECT='V' (tridiagonal, initialize Q)
  ! Same tridiagonal matrix as test 4
  ! =====================================================================
  N = 4; KD = 1; LDAB = 2; LDQ = 10
  AB2 = 0.0d0; D = 0.0d0; E = 0.0d0; Q = 0.0d0; WORK = 0.0d0

  AB2(1,1) = 4.0d0; AB2(2,1) = 1.0d0
  AB2(1,2) = 5.0d0; AB2(2,2) = 2.0d0
  AB2(1,3) = 6.0d0; AB2(2,3) = 3.0d0
  AB2(1,4) = 7.0d0

  call DSBTRD('V', 'L', N, KD, AB2, LDAB, D, E, Q, LDQ, WORK, INFO)
  call begin_test('lower_kd1_n4_init')
  call print_int('info', INFO)
  call print_array('d', D, 4)
  call print_array('e', E, 3)
  call print_matrix('Q', Q, LDQ, N, N)
  call end_test()

end program
