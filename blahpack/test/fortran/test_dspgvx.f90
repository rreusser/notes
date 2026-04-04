program test_dspgvx
  use test_utils
  implicit none

  integer, parameter :: NMAX = 4
  integer :: N, LDZ, IL, IU, M, INFO, i, j
  double precision :: AP(NMAX*(NMAX+1)/2), BP(NMAX*(NMAX+1)/2)
  double precision :: W(NMAX), Z(NMAX, NMAX), WORK(8*NMAX)
  integer :: IWORK(5*NMAX), IFAIL(NMAX)
  double precision :: VL, VU, ABSTOL
  double precision :: Zflat(NMAX*NMAX)

  ! Matrix A = [4 2 1; 2 5 3; 1 3 6], B = [4 2 0; 2 5 1; 0 1 3]

  ! =====================================================
  ! Test 1: ITYPE=1, JOBZ='V', RANGE='A', UPLO='U', N=3
  ! =====================================================
  N = 3
  LDZ = NMAX
  ABSTOL = 0.0d0
  AP = 0.0d0; BP = 0.0d0
  AP(1) = 4.0d0; AP(2) = 2.0d0; AP(3) = 5.0d0
  AP(4) = 1.0d0; AP(5) = 3.0d0; AP(6) = 6.0d0
  BP(1) = 4.0d0; BP(2) = 2.0d0; BP(3) = 5.0d0
  BP(4) = 0.0d0; BP(5) = 1.0d0; BP(6) = 3.0d0
  W = 0.0d0; Z = 0.0d0; IFAIL = 0
  call DSPGVX(1, 'V', 'A', 'U', N, AP, BP, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('itype1_V_A_U')
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
  ! Test 2: ITYPE=1, JOBZ='V', RANGE='A', UPLO='L', N=3
  ! =====================================================
  AP = 0.0d0; BP = 0.0d0
  AP(1) = 4.0d0; AP(2) = 2.0d0; AP(3) = 1.0d0
  AP(4) = 5.0d0; AP(5) = 3.0d0; AP(6) = 6.0d0
  BP(1) = 4.0d0; BP(2) = 2.0d0; BP(3) = 0.0d0
  BP(4) = 5.0d0; BP(5) = 1.0d0; BP(6) = 3.0d0
  W = 0.0d0; Z = 0.0d0; IFAIL = 0
  call DSPGVX(1, 'V', 'A', 'L', N, AP, BP, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('itype1_V_A_L')
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
  ! Test 3: ITYPE=1, JOBZ='N', RANGE='A', UPLO='L', N=3
  ! =====================================================
  AP = 0.0d0; BP = 0.0d0
  AP(1) = 4.0d0; AP(2) = 2.0d0; AP(3) = 1.0d0
  AP(4) = 5.0d0; AP(5) = 3.0d0; AP(6) = 6.0d0
  BP(1) = 4.0d0; BP(2) = 2.0d0; BP(3) = 0.0d0
  BP(4) = 5.0d0; BP(5) = 1.0d0; BP(6) = 3.0d0
  W = 0.0d0; Z = 0.0d0; IFAIL = 0
  call DSPGVX(1, 'N', 'A', 'L', N, AP, BP, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, 1, WORK, IWORK, IFAIL, INFO)
  call begin_test('itype1_N_A_L')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('w', W, M)
  call end_test()

  ! =====================================================
  ! Test 4: ITYPE=1, JOBZ='V', RANGE='V', UPLO='U', N=3
  ! Select eigenvalues in (0.5, 1.5]
  ! =====================================================
  AP = 0.0d0; BP = 0.0d0
  AP(1) = 4.0d0; AP(2) = 2.0d0; AP(3) = 5.0d0
  AP(4) = 1.0d0; AP(5) = 3.0d0; AP(6) = 6.0d0
  BP(1) = 4.0d0; BP(2) = 2.0d0; BP(3) = 5.0d0
  BP(4) = 0.0d0; BP(5) = 1.0d0; BP(6) = 3.0d0
  W = 0.0d0; Z = 0.0d0; IFAIL = 0
  VL = 0.5d0; VU = 1.5d0
  call DSPGVX(1, 'V', 'V', 'U', N, AP, BP, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('itype1_V_V_U')
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
  ! Test 5: ITYPE=1, JOBZ='V', RANGE='I', UPLO='L', N=3
  ! Select eigenvalues 1 through 2
  ! =====================================================
  AP = 0.0d0; BP = 0.0d0
  AP(1) = 4.0d0; AP(2) = 2.0d0; AP(3) = 1.0d0
  AP(4) = 5.0d0; AP(5) = 3.0d0; AP(6) = 6.0d0
  BP(1) = 4.0d0; BP(2) = 2.0d0; BP(3) = 0.0d0
  BP(4) = 5.0d0; BP(5) = 1.0d0; BP(6) = 3.0d0
  W = 0.0d0; Z = 0.0d0; IFAIL = 0
  IL = 1; IU = 2
  call DSPGVX(1, 'V', 'I', 'L', N, AP, BP, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('itype1_V_I_L')
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
  ! Test 6: ITYPE=2, JOBZ='V', RANGE='A', UPLO='U', N=3
  ! =====================================================
  AP = 0.0d0; BP = 0.0d0
  AP(1) = 4.0d0; AP(2) = 2.0d0; AP(3) = 5.0d0
  AP(4) = 1.0d0; AP(5) = 3.0d0; AP(6) = 6.0d0
  BP(1) = 4.0d0; BP(2) = 2.0d0; BP(3) = 5.0d0
  BP(4) = 0.0d0; BP(5) = 1.0d0; BP(6) = 3.0d0
  W = 0.0d0; Z = 0.0d0; IFAIL = 0
  call DSPGVX(2, 'V', 'A', 'U', N, AP, BP, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('itype2_V_A_U')
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
  ! Test 7: ITYPE=3, JOBZ='V', RANGE='A', UPLO='L', N=3
  ! =====================================================
  AP = 0.0d0; BP = 0.0d0
  AP(1) = 4.0d0; AP(2) = 2.0d0; AP(3) = 1.0d0
  AP(4) = 5.0d0; AP(5) = 3.0d0; AP(6) = 6.0d0
  BP(1) = 4.0d0; BP(2) = 2.0d0; BP(3) = 0.0d0
  BP(4) = 5.0d0; BP(5) = 1.0d0; BP(6) = 3.0d0
  W = 0.0d0; Z = 0.0d0; IFAIL = 0
  call DSPGVX(3, 'V', 'A', 'L', N, AP, BP, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('itype3_V_A_L')
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
  ! Test 8: ITYPE=3, JOBZ='V', RANGE='I', UPLO='U', N=3
  ! Select eigenvalue 2 through 3
  ! =====================================================
  AP = 0.0d0; BP = 0.0d0
  AP(1) = 4.0d0; AP(2) = 2.0d0; AP(3) = 5.0d0
  AP(4) = 1.0d0; AP(5) = 3.0d0; AP(6) = 6.0d0
  BP(1) = 4.0d0; BP(2) = 2.0d0; BP(3) = 5.0d0
  BP(4) = 0.0d0; BP(5) = 1.0d0; BP(6) = 3.0d0
  W = 0.0d0; Z = 0.0d0; IFAIL = 0
  IL = 2; IU = 3
  call DSPGVX(3, 'V', 'I', 'U', N, AP, BP, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('itype3_V_I_U')
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
  ! Test 9: ITYPE=1, JOBZ='N', RANGE='V', UPLO='U', N=3
  ! =====================================================
  AP = 0.0d0; BP = 0.0d0
  AP(1) = 4.0d0; AP(2) = 2.0d0; AP(3) = 5.0d0
  AP(4) = 1.0d0; AP(5) = 3.0d0; AP(6) = 6.0d0
  BP(1) = 4.0d0; BP(2) = 2.0d0; BP(3) = 5.0d0
  BP(4) = 0.0d0; BP(5) = 1.0d0; BP(6) = 3.0d0
  W = 0.0d0; Z = 0.0d0; IFAIL = 0
  VL = 0.5d0; VU = 1.5d0
  call DSPGVX(1, 'N', 'V', 'U', N, AP, BP, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, 1, WORK, IWORK, IFAIL, INFO)
  call begin_test('itype1_N_V_U')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('w', W, M)
  call end_test()

  ! =====================================================
  ! Test 10: N=0 quick return
  ! =====================================================
  call DSPGVX(1, 'V', 'A', 'U', 0, AP, BP, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, 1, WORK, IWORK, IFAIL, INFO)
  call begin_test('n_zero')
  call print_int('info', INFO)
  call print_int('M', M)
  call end_test()

  ! =====================================================
  ! Test 11: N=1
  ! =====================================================
  AP(1) = 6.0d0; BP(1) = 2.0d0
  W = 0.0d0; Z = 0.0d0; IFAIL = 0
  call DSPGVX(1, 'V', 'A', 'U', 1, AP, BP, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('n_one')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('w', W, M)
  call print_array('Z', Z(1,1), 1)
  call end_test()

  ! =====================================================
  ! Test 12: Non-positive definite B
  ! =====================================================
  AP(1) = 1.0d0; AP(2) = 0.0d0; AP(3) = 1.0d0
  BP(1) = -1.0d0; BP(2) = 0.0d0; BP(3) = 1.0d0
  W = 0.0d0; Z = 0.0d0; IFAIL = 0
  call DSPGVX(1, 'V', 'A', 'L', 2, AP, BP, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('not_posdef')
  call print_int('info', INFO)
  call end_test()

  ! =====================================================
  ! Test 13: ITYPE=2, JOBZ='V', RANGE='I', UPLO='L', N=3
  ! =====================================================
  AP = 0.0d0; BP = 0.0d0
  AP(1) = 4.0d0; AP(2) = 2.0d0; AP(3) = 1.0d0
  AP(4) = 5.0d0; AP(5) = 3.0d0; AP(6) = 6.0d0
  BP(1) = 4.0d0; BP(2) = 2.0d0; BP(3) = 0.0d0
  BP(4) = 5.0d0; BP(5) = 1.0d0; BP(6) = 3.0d0
  W = 0.0d0; Z = 0.0d0; IFAIL = 0
  IL = 2; IU = 3
  call DSPGVX(2, 'V', 'I', 'L', N, AP, BP, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('itype2_V_I_L')
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
  ! Test 14: 4x4 ITYPE=1, JOBZ='V', RANGE='A', UPLO='L'
  ! A = [4 1 -2 0; 1 3 0 1; -2 0 5 -1; 0 1 -1 6]
  ! B = [4 1 0 0; 1 5 1 0; 0 1 6 1; 0 0 1 3]
  ! =====================================================
  N = 4
  LDZ = NMAX
  AP = 0.0d0; BP = 0.0d0
  ! Lower packed col-major
  AP(1) = 4.0d0; AP(2) = 1.0d0; AP(3) = -2.0d0; AP(4) = 0.0d0
  AP(5) = 3.0d0; AP(6) = 0.0d0; AP(7) = 1.0d0
  AP(8) = 5.0d0; AP(9) = -1.0d0
  AP(10) = 6.0d0
  BP(1) = 4.0d0; BP(2) = 1.0d0; BP(3) = 0.0d0; BP(4) = 0.0d0
  BP(5) = 5.0d0; BP(6) = 1.0d0; BP(7) = 0.0d0
  BP(8) = 6.0d0; BP(9) = 1.0d0
  BP(10) = 3.0d0
  W = 0.0d0; Z = 0.0d0; IFAIL = 0
  call DSPGVX(1, 'V', 'A', 'L', N, AP, BP, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('itype1_V_A_L_4x4')
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
  ! Test 15: 4x4 ITYPE=1, JOBZ='V', RANGE='I', UPLO='L'
  ! Same matrices, select eigenvalues 2 through 3
  ! =====================================================
  AP = 0.0d0; BP = 0.0d0
  AP(1) = 4.0d0; AP(2) = 1.0d0; AP(3) = -2.0d0; AP(4) = 0.0d0
  AP(5) = 3.0d0; AP(6) = 0.0d0; AP(7) = 1.0d0
  AP(8) = 5.0d0; AP(9) = -1.0d0
  AP(10) = 6.0d0
  BP(1) = 4.0d0; BP(2) = 1.0d0; BP(3) = 0.0d0; BP(4) = 0.0d0
  BP(5) = 5.0d0; BP(6) = 1.0d0; BP(7) = 0.0d0
  BP(8) = 6.0d0; BP(9) = 1.0d0
  BP(10) = 3.0d0
  W = 0.0d0; Z = 0.0d0; IFAIL = 0
  IL = 2; IU = 3
  call DSPGVX(1, 'V', 'I', 'L', N, AP, BP, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO)
  call begin_test('itype1_V_I_L_4x4')
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

end program test_dspgvx
