program test_zhpgvx
  use test_utils
  implicit none

  integer, parameter :: NMAX = 4
  integer :: N, LDZ, IL, IU, M, INFO, i, j
  complex*16 :: AP(NMAX*(NMAX+1)/2), BP(NMAX*(NMAX+1)/2)
  complex*16 :: Z(NMAX, NMAX), WORK(2*NMAX)
  double precision :: W(NMAX), RWORK(7*NMAX)
  integer :: IWORK(5*NMAX), IFAIL(NMAX)
  double precision :: VL, VU, ABSTOL

  ! EQUIVALENCE for printing interleaved re/im
  double precision :: Z_r(2*NMAX*NMAX)
  equivalence (Z, Z_r)
  double precision :: Zflat_r(2*NMAX*NMAX)
  complex*16 :: Zflat(NMAX*NMAX)
  equivalence (Zflat, Zflat_r)

  ! Hermitian 3x3:
  ! A = [4     1-i    2   ]
  !     [1+i   5     3-i  ]
  !     [2     3+i    6   ]
  !
  ! B = [4     1-i    0   ]
  !     [1+i   5      i   ]
  !     [0     -i     3   ]

  ! =====================================================
  ! Test 1: ITYPE=1, JOBZ='V', RANGE='A', UPLO='U', N=3
  ! Upper packed (col-major):
  ! A: col1=[4], col2=[1-i,5], col3=[2,3-i,6]
  ! B: col1=[4], col2=[1-i,5], col3=[0,i,3]
  ! =====================================================
  N = 3
  LDZ = NMAX
  ABSTOL = 0.0d0

  AP(1) = (4.0d0, 0.0d0)
  AP(2) = (1.0d0, -1.0d0)
  AP(3) = (5.0d0, 0.0d0)
  AP(4) = (2.0d0, 0.0d0)
  AP(5) = (3.0d0, -1.0d0)
  AP(6) = (6.0d0, 0.0d0)

  BP(1) = (4.0d0, 0.0d0)
  BP(2) = (1.0d0, -1.0d0)
  BP(3) = (5.0d0, 0.0d0)
  BP(4) = (0.0d0, 0.0d0)
  BP(5) = (0.0d0, 1.0d0)
  BP(6) = (3.0d0, 0.0d0)

  W = 0.0d0; Z = (0.0d0, 0.0d0); IFAIL = 0
  call ZHPGVX(1, 'V', 'A', 'U', N, AP, BP, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, RWORK, IWORK, IFAIL, INFO)
  call begin_test('itype1_V_A_U')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('w', W, M)
  do j = 1, M
    do i = 1, N
      Zflat((j-1)*N + i) = Z(i,j)
    end do
  end do
  call print_array('Z', Zflat_r, 2*N*M)
  call end_test()

  ! =====================================================
  ! Test 2: ITYPE=1, JOBZ='V', RANGE='A', UPLO='L', N=3
  ! Lower packed (col-major):
  ! A: col1=[4,1+i,2], col2=[5,3+i], col3=[6]
  ! B: col1=[4,1+i,0], col2=[5,-i], col3=[3]
  ! =====================================================
  AP(1) = (4.0d0, 0.0d0)
  AP(2) = (1.0d0, 1.0d0)
  AP(3) = (2.0d0, 0.0d0)
  AP(4) = (5.0d0, 0.0d0)
  AP(5) = (3.0d0, 1.0d0)
  AP(6) = (6.0d0, 0.0d0)

  BP(1) = (4.0d0, 0.0d0)
  BP(2) = (1.0d0, 1.0d0)
  BP(3) = (0.0d0, 0.0d0)
  BP(4) = (5.0d0, 0.0d0)
  BP(5) = (0.0d0, -1.0d0)
  BP(6) = (3.0d0, 0.0d0)

  W = 0.0d0; Z = (0.0d0, 0.0d0); IFAIL = 0
  call ZHPGVX(1, 'V', 'A', 'L', N, AP, BP, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, RWORK, IWORK, IFAIL, INFO)
  call begin_test('itype1_V_A_L')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('w', W, M)
  do j = 1, M
    do i = 1, N
      Zflat((j-1)*N + i) = Z(i,j)
    end do
  end do
  call print_array('Z', Zflat_r, 2*N*M)
  call end_test()

  ! =====================================================
  ! Test 3: ITYPE=1, JOBZ='N', RANGE='A', UPLO='L', N=3
  ! =====================================================
  AP(1) = (4.0d0, 0.0d0)
  AP(2) = (1.0d0, 1.0d0)
  AP(3) = (2.0d0, 0.0d0)
  AP(4) = (5.0d0, 0.0d0)
  AP(5) = (3.0d0, 1.0d0)
  AP(6) = (6.0d0, 0.0d0)

  BP(1) = (4.0d0, 0.0d0)
  BP(2) = (1.0d0, 1.0d0)
  BP(3) = (0.0d0, 0.0d0)
  BP(4) = (5.0d0, 0.0d0)
  BP(5) = (0.0d0, -1.0d0)
  BP(6) = (3.0d0, 0.0d0)

  W = 0.0d0; Z = (0.0d0, 0.0d0); IFAIL = 0
  call ZHPGVX(1, 'N', 'A', 'L', N, AP, BP, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, 1, WORK, RWORK, IWORK, IFAIL, INFO)
  call begin_test('itype1_N_A_L')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('w', W, M)
  call end_test()

  ! =====================================================
  ! Test 4: ITYPE=1, JOBZ='V', RANGE='V', UPLO='U', N=3
  ! Select eigenvalues in (0.5, 1.5]
  ! =====================================================
  AP(1) = (4.0d0, 0.0d0)
  AP(2) = (1.0d0, -1.0d0)
  AP(3) = (5.0d0, 0.0d0)
  AP(4) = (2.0d0, 0.0d0)
  AP(5) = (3.0d0, -1.0d0)
  AP(6) = (6.0d0, 0.0d0)

  BP(1) = (4.0d0, 0.0d0)
  BP(2) = (1.0d0, -1.0d0)
  BP(3) = (5.0d0, 0.0d0)
  BP(4) = (0.0d0, 0.0d0)
  BP(5) = (0.0d0, 1.0d0)
  BP(6) = (3.0d0, 0.0d0)

  W = 0.0d0; Z = (0.0d0, 0.0d0); IFAIL = 0
  VL = 0.5d0; VU = 1.5d0
  call ZHPGVX(1, 'V', 'V', 'U', N, AP, BP, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, RWORK, IWORK, IFAIL, INFO)
  call begin_test('itype1_V_V_U')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('w', W, M)
  if (M > 0) then
    do j = 1, M
      do i = 1, N
        Zflat((j-1)*N + i) = Z(i,j)
      end do
    end do
    call print_array('Z', Zflat_r, 2*N*M)
  end if
  call end_test()

  ! =====================================================
  ! Test 5: ITYPE=1, JOBZ='V', RANGE='I', UPLO='L', N=3
  ! Select eigenvalues 1 through 2
  ! =====================================================
  AP(1) = (4.0d0, 0.0d0)
  AP(2) = (1.0d0, 1.0d0)
  AP(3) = (2.0d0, 0.0d0)
  AP(4) = (5.0d0, 0.0d0)
  AP(5) = (3.0d0, 1.0d0)
  AP(6) = (6.0d0, 0.0d0)

  BP(1) = (4.0d0, 0.0d0)
  BP(2) = (1.0d0, 1.0d0)
  BP(3) = (0.0d0, 0.0d0)
  BP(4) = (5.0d0, 0.0d0)
  BP(5) = (0.0d0, -1.0d0)
  BP(6) = (3.0d0, 0.0d0)

  W = 0.0d0; Z = (0.0d0, 0.0d0); IFAIL = 0
  IL = 1; IU = 2
  call ZHPGVX(1, 'V', 'I', 'L', N, AP, BP, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, RWORK, IWORK, IFAIL, INFO)
  call begin_test('itype1_V_I_L')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('w', W, M)
  do j = 1, M
    do i = 1, N
      Zflat((j-1)*N + i) = Z(i,j)
    end do
  end do
  call print_array('Z', Zflat_r, 2*N*M)
  call end_test()

  ! =====================================================
  ! Test 6: ITYPE=2, JOBZ='V', RANGE='A', UPLO='U', N=3
  ! =====================================================
  AP(1) = (4.0d0, 0.0d0)
  AP(2) = (1.0d0, -1.0d0)
  AP(3) = (5.0d0, 0.0d0)
  AP(4) = (2.0d0, 0.0d0)
  AP(5) = (3.0d0, -1.0d0)
  AP(6) = (6.0d0, 0.0d0)

  BP(1) = (4.0d0, 0.0d0)
  BP(2) = (1.0d0, -1.0d0)
  BP(3) = (5.0d0, 0.0d0)
  BP(4) = (0.0d0, 0.0d0)
  BP(5) = (0.0d0, 1.0d0)
  BP(6) = (3.0d0, 0.0d0)

  W = 0.0d0; Z = (0.0d0, 0.0d0); IFAIL = 0
  call ZHPGVX(2, 'V', 'A', 'U', N, AP, BP, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, RWORK, IWORK, IFAIL, INFO)
  call begin_test('itype2_V_A_U')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('w', W, M)
  do j = 1, M
    do i = 1, N
      Zflat((j-1)*N + i) = Z(i,j)
    end do
  end do
  call print_array('Z', Zflat_r, 2*N*M)
  call end_test()

  ! =====================================================
  ! Test 7: ITYPE=3, JOBZ='V', RANGE='A', UPLO='L', N=3
  ! =====================================================
  AP(1) = (4.0d0, 0.0d0)
  AP(2) = (1.0d0, 1.0d0)
  AP(3) = (2.0d0, 0.0d0)
  AP(4) = (5.0d0, 0.0d0)
  AP(5) = (3.0d0, 1.0d0)
  AP(6) = (6.0d0, 0.0d0)

  BP(1) = (4.0d0, 0.0d0)
  BP(2) = (1.0d0, 1.0d0)
  BP(3) = (0.0d0, 0.0d0)
  BP(4) = (5.0d0, 0.0d0)
  BP(5) = (0.0d0, -1.0d0)
  BP(6) = (3.0d0, 0.0d0)

  W = 0.0d0; Z = (0.0d0, 0.0d0); IFAIL = 0
  call ZHPGVX(3, 'V', 'A', 'L', N, AP, BP, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, RWORK, IWORK, IFAIL, INFO)
  call begin_test('itype3_V_A_L')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('w', W, M)
  do j = 1, M
    do i = 1, N
      Zflat((j-1)*N + i) = Z(i,j)
    end do
  end do
  call print_array('Z', Zflat_r, 2*N*M)
  call end_test()

  ! =====================================================
  ! Test 8: ITYPE=3, JOBZ='V', RANGE='I', UPLO='U', N=3
  ! Select eigenvalue 2 through 3
  ! =====================================================
  AP(1) = (4.0d0, 0.0d0)
  AP(2) = (1.0d0, -1.0d0)
  AP(3) = (5.0d0, 0.0d0)
  AP(4) = (2.0d0, 0.0d0)
  AP(5) = (3.0d0, -1.0d0)
  AP(6) = (6.0d0, 0.0d0)

  BP(1) = (4.0d0, 0.0d0)
  BP(2) = (1.0d0, -1.0d0)
  BP(3) = (5.0d0, 0.0d0)
  BP(4) = (0.0d0, 0.0d0)
  BP(5) = (0.0d0, 1.0d0)
  BP(6) = (3.0d0, 0.0d0)

  W = 0.0d0; Z = (0.0d0, 0.0d0); IFAIL = 0
  IL = 2; IU = 3
  call ZHPGVX(3, 'V', 'I', 'U', N, AP, BP, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, RWORK, IWORK, IFAIL, INFO)
  call begin_test('itype3_V_I_U')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('w', W, M)
  do j = 1, M
    do i = 1, N
      Zflat((j-1)*N + i) = Z(i,j)
    end do
  end do
  call print_array('Z', Zflat_r, 2*N*M)
  call end_test()

  ! =====================================================
  ! Test 9: ITYPE=1, JOBZ='N', RANGE='V', UPLO='U', N=3
  ! =====================================================
  AP(1) = (4.0d0, 0.0d0)
  AP(2) = (1.0d0, -1.0d0)
  AP(3) = (5.0d0, 0.0d0)
  AP(4) = (2.0d0, 0.0d0)
  AP(5) = (3.0d0, -1.0d0)
  AP(6) = (6.0d0, 0.0d0)

  BP(1) = (4.0d0, 0.0d0)
  BP(2) = (1.0d0, -1.0d0)
  BP(3) = (5.0d0, 0.0d0)
  BP(4) = (0.0d0, 0.0d0)
  BP(5) = (0.0d0, 1.0d0)
  BP(6) = (3.0d0, 0.0d0)

  W = 0.0d0; Z = (0.0d0, 0.0d0); IFAIL = 0
  VL = 0.5d0; VU = 1.5d0
  call ZHPGVX(1, 'N', 'V', 'U', N, AP, BP, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, 1, WORK, RWORK, IWORK, IFAIL, INFO)
  call begin_test('itype1_N_V_U')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('w', W, M)
  call end_test()

  ! =====================================================
  ! Test 10: N=1
  ! =====================================================
  AP(1) = (6.0d0, 0.0d0)
  BP(1) = (2.0d0, 0.0d0)
  W = 0.0d0; Z = (0.0d0, 0.0d0); IFAIL = 0
  call ZHPGVX(1, 'V', 'A', 'U', 1, AP, BP, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, RWORK, IWORK, IFAIL, INFO)
  call begin_test('n_one')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('w', W, M)
  Zflat(1) = Z(1,1)
  call print_array('Z', Zflat_r, 2)
  call end_test()

  ! =====================================================
  ! Test 11: Non-positive definite B
  ! =====================================================
  AP(1) = (1.0d0, 0.0d0)
  AP(2) = (0.0d0, 0.0d0)
  AP(3) = (1.0d0, 0.0d0)
  BP(1) = (-1.0d0, 0.0d0)
  BP(2) = (0.0d0, 0.0d0)
  BP(3) = (1.0d0, 0.0d0)
  W = 0.0d0; Z = (0.0d0, 0.0d0); IFAIL = 0
  call ZHPGVX(1, 'V', 'A', 'L', 2, AP, BP, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, RWORK, IWORK, IFAIL, INFO)
  call begin_test('not_posdef')
  call print_int('info', INFO)
  call end_test()

  ! =====================================================
  ! Test 12: ITYPE=2, JOBZ='V', RANGE='I', UPLO='L', N=3
  ! =====================================================
  AP(1) = (4.0d0, 0.0d0)
  AP(2) = (1.0d0, 1.0d0)
  AP(3) = (2.0d0, 0.0d0)
  AP(4) = (5.0d0, 0.0d0)
  AP(5) = (3.0d0, 1.0d0)
  AP(6) = (6.0d0, 0.0d0)

  BP(1) = (4.0d0, 0.0d0)
  BP(2) = (1.0d0, 1.0d0)
  BP(3) = (0.0d0, 0.0d0)
  BP(4) = (5.0d0, 0.0d0)
  BP(5) = (0.0d0, -1.0d0)
  BP(6) = (3.0d0, 0.0d0)

  W = 0.0d0; Z = (0.0d0, 0.0d0); IFAIL = 0
  IL = 2; IU = 3
  call ZHPGVX(2, 'V', 'I', 'L', N, AP, BP, VL, VU, IL, IU, &
              ABSTOL, M, W, Z, LDZ, WORK, RWORK, IWORK, IFAIL, INFO)
  call begin_test('itype2_V_I_L')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('w', W, M)
  do j = 1, M
    do i = 1, N
      Zflat((j-1)*N + i) = Z(i,j)
    end do
  end do
  call print_array('Z', Zflat_r, 2*N*M)
  call end_test()

end program test_zhpgvx
