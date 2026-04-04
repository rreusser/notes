program test_dsbevx
  use test_utils
  implicit none

  integer, parameter :: NMAX = 10
  integer :: INFO, N, KD, LDAB, LDQ, LDZ, IL, IU, M, i, j
  double precision :: VL, VU, ABSTOL
  double precision :: AB1(1, NMAX), AB2(2, NMAX), AB3(3, NMAX)
  double precision :: Q(NMAX, NMAX), Z(NMAX, NMAX)
  double precision :: W(NMAX), WORK(7*NMAX)
  integer :: IWORK(5*NMAX), IFAIL(NMAX)
  double precision :: Zflat(NMAX*NMAX)

  ! =====================================================
  ! Test 1: JOBZ='V', RANGE='A', UPLO='U', KD=2, N=5
  ! =====================================================
  N = 5; KD = 2; LDAB = 3; LDQ = NMAX; LDZ = NMAX
  ABSTOL = 0.0d0
  AB3 = 0.0d0; Q = 0.0d0; Z = 0.0d0; W = 0.0d0; WORK = 0.0d0
  IWORK = 0; IFAIL = 0

  AB3(3,1) = 4.0d0
  AB3(2,2) = 1.0d0; AB3(3,2) = 5.0d0
  AB3(1,3) = 2.0d0; AB3(2,3) = 3.0d0; AB3(3,3) = 6.0d0
  AB3(1,4) = 1.0d0; AB3(2,4) = 2.0d0; AB3(3,4) = 7.0d0
  AB3(1,5) = 1.0d0; AB3(2,5) = 3.0d0; AB3(3,5) = 8.0d0

  call DSBEVX('V', 'A', 'U', N, KD, AB3, LDAB, Q, LDQ, VL, VU, &
              IL, IU, ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('V_A_U_kd2_n5')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('w', W, M)
  do j = 1, M
    do i = 1, N
      Zflat((j-1)*N + i) = Z(i,j)
    end do
  end do
  call print_array('Z', Zflat, N*M)
  call end_test()

  ! =====================================================
  ! Test 2: JOBZ='V', RANGE='A', UPLO='L', KD=2, N=5
  ! =====================================================
  N = 5; KD = 2; LDAB = 3; LDQ = NMAX; LDZ = NMAX
  ABSTOL = 0.0d0
  AB3 = 0.0d0; Q = 0.0d0; Z = 0.0d0; W = 0.0d0; WORK = 0.0d0
  IWORK = 0; IFAIL = 0

  AB3(1,1) = 4.0d0; AB3(2,1) = 1.0d0; AB3(3,1) = 2.0d0
  AB3(1,2) = 5.0d0; AB3(2,2) = 3.0d0; AB3(3,2) = 1.0d0
  AB3(1,3) = 6.0d0; AB3(2,3) = 2.0d0; AB3(3,3) = 1.0d0
  AB3(1,4) = 7.0d0; AB3(2,4) = 3.0d0
  AB3(1,5) = 8.0d0

  call DSBEVX('V', 'A', 'L', N, KD, AB3, LDAB, Q, LDQ, VL, VU, &
              IL, IU, ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('V_A_L_kd2_n5')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('w', W, M)
  do j = 1, M
    do i = 1, N
      Zflat((j-1)*N + i) = Z(i,j)
    end do
  end do
  call print_array('Z', Zflat, N*M)
  call end_test()

  ! =====================================================
  ! Test 3: JOBZ='N', RANGE='A', UPLO='U', KD=2, N=5
  ! =====================================================
  N = 5; KD = 2; LDAB = 3; LDQ = 1; LDZ = 1
  ABSTOL = 0.0d0
  AB3 = 0.0d0; W = 0.0d0; WORK = 0.0d0
  IWORK = 0; IFAIL = 0

  AB3(3,1) = 4.0d0
  AB3(2,2) = 1.0d0; AB3(3,2) = 5.0d0
  AB3(1,3) = 2.0d0; AB3(2,3) = 3.0d0; AB3(3,3) = 6.0d0
  AB3(1,4) = 1.0d0; AB3(2,4) = 2.0d0; AB3(3,4) = 7.0d0
  AB3(1,5) = 1.0d0; AB3(2,5) = 3.0d0; AB3(3,5) = 8.0d0

  call DSBEVX('N', 'A', 'U', N, KD, AB3, LDAB, Q, LDQ, VL, VU, &
              IL, IU, ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('N_A_U_kd2_n5')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('w', W, M)
  call end_test()

  ! =====================================================
  ! Test 4: JOBZ='V', RANGE='V' (value range), UPLO='U'
  ! Select eigenvalues in [3.0, 8.0]
  ! =====================================================
  N = 5; KD = 2; LDAB = 3; LDQ = NMAX; LDZ = NMAX
  VL = 3.0d0; VU = 8.0d0; ABSTOL = 0.0d0
  AB3 = 0.0d0; Q = 0.0d0; Z = 0.0d0; W = 0.0d0; WORK = 0.0d0
  IWORK = 0; IFAIL = 0

  AB3(3,1) = 4.0d0
  AB3(2,2) = 1.0d0; AB3(3,2) = 5.0d0
  AB3(1,3) = 2.0d0; AB3(2,3) = 3.0d0; AB3(3,3) = 6.0d0
  AB3(1,4) = 1.0d0; AB3(2,4) = 2.0d0; AB3(3,4) = 7.0d0
  AB3(1,5) = 1.0d0; AB3(2,5) = 3.0d0; AB3(3,5) = 8.0d0

  call DSBEVX('V', 'V', 'U', N, KD, AB3, LDAB, Q, LDQ, VL, VU, &
              IL, IU, ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('V_V_U_kd2_n5')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('w', W, M)
  do j = 1, M
    do i = 1, N
      Zflat((j-1)*N + i) = Z(i,j)
    end do
  end do
  call print_array('Z', Zflat, N*M)
  call end_test()

  ! =====================================================
  ! Test 5: JOBZ='V', RANGE='I' (index range), UPLO='U'
  ! Select eigenvalues 2 through 4
  ! =====================================================
  N = 5; KD = 2; LDAB = 3; LDQ = NMAX; LDZ = NMAX
  IL = 2; IU = 4; ABSTOL = 0.0d0
  AB3 = 0.0d0; Q = 0.0d0; Z = 0.0d0; W = 0.0d0; WORK = 0.0d0
  IWORK = 0; IFAIL = 0

  AB3(3,1) = 4.0d0
  AB3(2,2) = 1.0d0; AB3(3,2) = 5.0d0
  AB3(1,3) = 2.0d0; AB3(2,3) = 3.0d0; AB3(3,3) = 6.0d0
  AB3(1,4) = 1.0d0; AB3(2,4) = 2.0d0; AB3(3,4) = 7.0d0
  AB3(1,5) = 1.0d0; AB3(2,5) = 3.0d0; AB3(3,5) = 8.0d0

  call DSBEVX('V', 'I', 'U', N, KD, AB3, LDAB, Q, LDQ, VL, VU, &
              IL, IU, ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('V_I_U_kd2_n5')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('w', W, M)
  do j = 1, M
    do i = 1, N
      Zflat((j-1)*N + i) = Z(i,j)
    end do
  end do
  call print_array('Z', Zflat, N*M)
  call end_test()

  ! =====================================================
  ! Test 6: JOBZ='N', RANGE='V', UPLO='L'
  ! =====================================================
  N = 5; KD = 2; LDAB = 3; LDQ = 1; LDZ = 1
  VL = 1.0d0; VU = 5.0d0; ABSTOL = 0.0d0
  AB3 = 0.0d0; W = 0.0d0; WORK = 0.0d0
  IWORK = 0; IFAIL = 0

  AB3(1,1) = 4.0d0; AB3(2,1) = 1.0d0; AB3(3,1) = 2.0d0
  AB3(1,2) = 5.0d0; AB3(2,2) = 3.0d0; AB3(3,2) = 1.0d0
  AB3(1,3) = 6.0d0; AB3(2,3) = 2.0d0; AB3(3,3) = 1.0d0
  AB3(1,4) = 7.0d0; AB3(2,4) = 3.0d0
  AB3(1,5) = 8.0d0

  call DSBEVX('N', 'V', 'L', N, KD, AB3, LDAB, Q, LDQ, VL, VU, &
              IL, IU, ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('N_V_L_kd2_n5')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('w', W, M)
  call end_test()

  ! =====================================================
  ! Test 7: JOBZ='N', RANGE='I', UPLO='L'
  ! Select eigenvalue 3 only
  ! =====================================================
  N = 5; KD = 2; LDAB = 3; LDQ = 1; LDZ = 1
  IL = 3; IU = 3; ABSTOL = 0.0d0
  AB3 = 0.0d0; W = 0.0d0; WORK = 0.0d0
  IWORK = 0; IFAIL = 0

  AB3(1,1) = 4.0d0; AB3(2,1) = 1.0d0; AB3(3,1) = 2.0d0
  AB3(1,2) = 5.0d0; AB3(2,2) = 3.0d0; AB3(3,2) = 1.0d0
  AB3(1,3) = 6.0d0; AB3(2,3) = 2.0d0; AB3(3,3) = 1.0d0
  AB3(1,4) = 7.0d0; AB3(2,4) = 3.0d0
  AB3(1,5) = 8.0d0

  call DSBEVX('N', 'I', 'L', N, KD, AB3, LDAB, Q, LDQ, VL, VU, &
              IL, IU, ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
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
  AB1 = 0.0d0; W = 0.0d0; Z = 0.0d0; WORK = 0.0d0
  IWORK = 0; IFAIL = 0
  AB1(1,1) = 3.5d0

  call DSBEVX('V', 'A', 'L', N, KD, AB1, LDAB, Q, LDQ, VL, VU, &
              IL, IU, ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('n1_V_A_L')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_scalar('w1', W(1))
  call print_scalar('z11', Z(1,1))
  call end_test()

  ! =====================================================
  ! Test 9: N=1, JOBZ='V', RANGE='V' (value excludes eigenvalue)
  ! =====================================================
  N = 1; KD = 0; LDAB = 1; LDQ = NMAX; LDZ = NMAX
  VL = 0.0d0; VU = 3.0d0; ABSTOL = 0.0d0
  AB1 = 0.0d0; W = 0.0d0; Z = 0.0d0; WORK = 0.0d0
  IWORK = 0; IFAIL = 0
  AB1(1,1) = 3.5d0

  call DSBEVX('V', 'V', 'L', N, KD, AB1, LDAB, Q, LDQ, VL, VU, &
              IL, IU, ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('n1_V_V_excluded')
  call print_int('info', INFO)
  call print_int('M', M)
  call end_test()

  ! =====================================================
  ! Test 10: N=1, JOBZ='V', RANGE='V' (value includes eigenvalue)
  ! =====================================================
  N = 1; KD = 0; LDAB = 1; LDQ = NMAX; LDZ = NMAX
  VL = 3.0d0; VU = 4.0d0; ABSTOL = 0.0d0
  AB1 = 0.0d0; W = 0.0d0; Z = 0.0d0; WORK = 0.0d0
  IWORK = 0; IFAIL = 0
  AB1(1,1) = 3.5d0

  call DSBEVX('V', 'V', 'L', N, KD, AB1, LDAB, Q, LDQ, VL, VU, &
              IL, IU, ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('n1_V_V_included')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_scalar('w1', W(1))
  call print_scalar('z11', Z(1,1))
  call end_test()

  ! =====================================================
  ! Test 11: N=0 (quick return)
  ! =====================================================
  N = 0; KD = 0; LDAB = 1; LDQ = 1; LDZ = 1
  ABSTOL = 0.0d0

  call DSBEVX('V', 'A', 'U', N, KD, AB1, LDAB, Q, LDQ, VL, VU, &
              IL, IU, ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
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
  AB2 = 0.0d0; Q = 0.0d0; Z = 0.0d0; W = 0.0d0; WORK = 0.0d0
  IWORK = 0; IFAIL = 0

  AB2(1,1) = 4.0d0; AB2(2,1) = 1.0d0
  AB2(1,2) = 5.0d0; AB2(2,2) = 2.0d0
  AB2(1,3) = 6.0d0; AB2(2,3) = 3.0d0
  AB2(1,4) = 7.0d0

  call DSBEVX('V', 'I', 'L', N, KD, AB2, LDAB, Q, LDQ, VL, VU, &
              IL, IU, ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('V_I_L_kd1_n4_fast')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('w', W, M)
  do j = 1, M
    do i = 1, N
      Zflat((j-1)*N + i) = Z(i,j)
    end do
  end do
  call print_array('Z', Zflat, N*M)
  call end_test()

  ! =====================================================
  ! Test 13: JOBZ='V', RANGE='V', UPLO='L', KD=1, N=4
  ! VL=4.5, VU=7.5 (selective with vectors)
  ! =====================================================
  N = 4; KD = 1; LDAB = 2; LDQ = NMAX; LDZ = NMAX
  VL = 4.5d0; VU = 7.5d0; ABSTOL = 0.0d0
  AB2 = 0.0d0; Q = 0.0d0; Z = 0.0d0; W = 0.0d0; WORK = 0.0d0
  IWORK = 0; IFAIL = 0

  AB2(1,1) = 4.0d0; AB2(2,1) = 1.0d0
  AB2(1,2) = 5.0d0; AB2(2,2) = 2.0d0
  AB2(1,3) = 6.0d0; AB2(2,3) = 3.0d0
  AB2(1,4) = 7.0d0

  call DSBEVX('V', 'V', 'L', N, KD, AB2, LDAB, Q, LDQ, VL, VU, &
              IL, IU, ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('V_V_L_kd1_n4')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('w', W, M)
  do j = 1, M
    do i = 1, N
      Zflat((j-1)*N + i) = Z(i,j)
    end do
  end do
  call print_array('Z', Zflat, N*M)
  call end_test()

  ! =====================================================
  ! Test 14: JOBZ='N', RANGE='I', UPLO='U', KD=1, N=4
  ! IL=1, IU=N (fast path no vectors)
  ! =====================================================
  N = 4; KD = 1; LDAB = 2; LDQ = 1; LDZ = 1
  IL = 1; IU = 4; ABSTOL = 0.0d0
  AB2 = 0.0d0; W = 0.0d0; WORK = 0.0d0
  IWORK = 0; IFAIL = 0

  AB2(2,1) = 4.0d0
  AB2(1,2) = 1.0d0; AB2(2,2) = 5.0d0
  AB2(1,3) = 2.0d0; AB2(2,3) = 6.0d0
  AB2(1,4) = 3.0d0; AB2(2,4) = 7.0d0

  call DSBEVX('N', 'I', 'U', N, KD, AB2, LDAB, Q, LDQ, VL, VU, &
              IL, IU, ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('N_I_U_kd1_n4_fast')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('w', W, M)
  call end_test()

  ! =====================================================
  ! Test 15: KD=0 (diagonal), JOBZ='V', RANGE='A'
  ! A = diag(3, 1, 4, 2)
  ! =====================================================
  N = 4; KD = 0; LDAB = 1; LDQ = NMAX; LDZ = NMAX
  ABSTOL = 0.0d0
  AB1 = 0.0d0; Q = 0.0d0; Z = 0.0d0; W = 0.0d0; WORK = 0.0d0
  IWORK = 0; IFAIL = 0
  AB1(1,1) = 3.0d0; AB1(1,2) = 1.0d0; AB1(1,3) = 4.0d0; AB1(1,4) = 2.0d0

  call DSBEVX('V', 'A', 'U', N, KD, AB1, LDAB, Q, LDQ, VL, VU, &
              IL, IU, ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('diagonal_V_A')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('w', W, M)
  do j = 1, M
    do i = 1, N
      Zflat((j-1)*N + i) = Z(i,j)
    end do
  end do
  call print_array('Z', Zflat, N*M)
  call end_test()

  ! =====================================================
  ! Test 16: N=1, JOBZ='V', RANGE='I', IL=1, IU=1
  ! =====================================================
  N = 1; KD = 0; LDAB = 1; LDQ = NMAX; LDZ = NMAX
  IL = 1; IU = 1; ABSTOL = 0.0d0
  AB1 = 0.0d0; W = 0.0d0; Z = 0.0d0; WORK = 0.0d0
  IWORK = 0; IFAIL = 0
  AB1(1,1) = 5.0d0

  call DSBEVX('V', 'I', 'U', N, KD, AB1, LDAB, Q, LDQ, VL, VU, &
              IL, IU, ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('n1_V_I')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_scalar('w1', W(1))
  call print_scalar('z11', Z(1,1))
  call end_test()

  ! =====================================================
  ! Test 17: JOBZ='V', RANGE='I', UPLO='U', KD=2, N=5
  ! Select single eigenvalue IL=IU=1 (smallest)
  ! =====================================================
  N = 5; KD = 2; LDAB = 3; LDQ = NMAX; LDZ = NMAX
  IL = 1; IU = 1; ABSTOL = 0.0d0
  AB3 = 0.0d0; Q = 0.0d0; Z = 0.0d0; W = 0.0d0; WORK = 0.0d0
  IWORK = 0; IFAIL = 0

  AB3(3,1) = 4.0d0
  AB3(2,2) = 1.0d0; AB3(3,2) = 5.0d0
  AB3(1,3) = 2.0d0; AB3(2,3) = 3.0d0; AB3(3,3) = 6.0d0
  AB3(1,4) = 1.0d0; AB3(2,4) = 2.0d0; AB3(3,4) = 7.0d0
  AB3(1,5) = 1.0d0; AB3(2,5) = 3.0d0; AB3(3,5) = 8.0d0

  call DSBEVX('V', 'I', 'U', N, KD, AB3, LDAB, Q, LDQ, VL, VU, &
              IL, IU, ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('V_I_U_kd2_n5_single')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('w', W, M)
  do j = 1, M
    do i = 1, N
      Zflat((j-1)*N + i) = Z(i,j)
    end do
  end do
  call print_array('Z', Zflat, N*M)
  call end_test()

end program test_dsbevx
