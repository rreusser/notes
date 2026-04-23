program test_zhpgv
  use test_utils
  implicit none

  integer, parameter :: NMAX = 3
  integer :: N, INFO, I, J
  complex*16 :: AP(NMAX*(NMAX+1)/2), BP(NMAX*(NMAX+1)/2)
  complex*16 :: WORK(2*NMAX), Z(NMAX, NMAX)
  double precision :: W(NMAX), RWORK(3*NMAX)

  ! EQUIVALENCE for printing interleaved re/im
  double precision :: Zflat_r(2*NMAX*NMAX)
  complex*16 :: Zflat(NMAX*NMAX)
  equivalence (Zflat, Zflat_r)

  ! =====================================================
  ! Hermitian 3x3 matrices:
  ! A = [4     1-i    2+i ]
  !     [1+i   5      3   ]
  !     [2-i   3      6   ]
  !
  ! B = [2     0.5-0.5i   0       ]
  !     [0.5+0.5i  3      0.5     ]
  !     [0         0.5    2       ]
  ! =====================================================

  ! =====================================================
  ! Test 1: ITYPE=1, JOBZ='V', UPLO='L', N=3
  ! Lower packed for A: col1=[4, 1+i, 2-i], col2=[5, 3], col3=[6]
  ! Lower packed for B: col1=[2, 0.5+0.5i, 0], col2=[3, 0.5], col3=[2]
  ! =====================================================
  N = 3
  AP = (0.0d0, 0.0d0); BP = (0.0d0, 0.0d0); W = 0.0d0; Z = (0.0d0, 0.0d0)
  AP(1) = (4.0d0, 0.0d0)
  AP(2) = (1.0d0, 1.0d0)
  AP(3) = (2.0d0, -1.0d0)
  AP(4) = (5.0d0, 0.0d0)
  AP(5) = (3.0d0, 0.0d0)
  AP(6) = (6.0d0, 0.0d0)

  BP(1) = (2.0d0, 0.0d0)
  BP(2) = (0.5d0, 0.5d0)
  BP(3) = (0.0d0, 0.0d0)
  BP(4) = (3.0d0, 0.0d0)
  BP(5) = (0.5d0, 0.0d0)
  BP(6) = (2.0d0, 0.0d0)

  call ZHPGV(1, 'V', 'L', N, AP, BP, W, Z, N, WORK, RWORK, INFO)
  do J = 1, N
    do I = 1, N
      Zflat((J-1)*N + I) = Z(I,J)
    end do
  end do
  call begin_test('itype1_v_lower')
  call print_int('info', INFO)
  call print_array('w', W, N)
  call print_array('Z', Zflat_r, 2*N*N)
  call end_test()

  ! =====================================================
  ! Test 2: ITYPE=1, JOBZ='V', UPLO='U', N=3
  ! Upper packed for A: col1=[4], col2=[1-i, 5], col3=[2+i, 3, 6]
  ! Upper packed for B: col1=[2], col2=[0.5-0.5i, 3], col3=[0, 0.5, 2]
  ! =====================================================
  N = 3
  AP = (0.0d0, 0.0d0); BP = (0.0d0, 0.0d0); W = 0.0d0; Z = (0.0d0, 0.0d0)
  AP(1) = (4.0d0, 0.0d0)
  AP(2) = (1.0d0, -1.0d0)
  AP(3) = (5.0d0, 0.0d0)
  AP(4) = (2.0d0, 1.0d0)
  AP(5) = (3.0d0, 0.0d0)
  AP(6) = (6.0d0, 0.0d0)

  BP(1) = (2.0d0, 0.0d0)
  BP(2) = (0.5d0, -0.5d0)
  BP(3) = (3.0d0, 0.0d0)
  BP(4) = (0.0d0, 0.0d0)
  BP(5) = (0.5d0, 0.0d0)
  BP(6) = (2.0d0, 0.0d0)

  call ZHPGV(1, 'V', 'U', N, AP, BP, W, Z, N, WORK, RWORK, INFO)
  do J = 1, N
    do I = 1, N
      Zflat((J-1)*N + I) = Z(I,J)
    end do
  end do
  call begin_test('itype1_v_upper')
  call print_int('info', INFO)
  call print_array('w', W, N)
  call print_array('Z', Zflat_r, 2*N*N)
  call end_test()

  ! =====================================================
  ! Test 3: ITYPE=1, JOBZ='N', UPLO='L', N=3 (eigenvalues only)
  ! =====================================================
  N = 3
  AP = (0.0d0, 0.0d0); BP = (0.0d0, 0.0d0); W = 0.0d0; Z = (0.0d0, 0.0d0)
  AP(1) = (4.0d0, 0.0d0)
  AP(2) = (1.0d0, 1.0d0)
  AP(3) = (2.0d0, -1.0d0)
  AP(4) = (5.0d0, 0.0d0)
  AP(5) = (3.0d0, 0.0d0)
  AP(6) = (6.0d0, 0.0d0)

  BP(1) = (2.0d0, 0.0d0)
  BP(2) = (0.5d0, 0.5d0)
  BP(3) = (0.0d0, 0.0d0)
  BP(4) = (3.0d0, 0.0d0)
  BP(5) = (0.5d0, 0.0d0)
  BP(6) = (2.0d0, 0.0d0)

  call ZHPGV(1, 'N', 'L', N, AP, BP, W, Z, 1, WORK, RWORK, INFO)
  call begin_test('itype1_n_lower')
  call print_int('info', INFO)
  call print_array('w', W, N)
  call end_test()

  ! =====================================================
  ! Test 4: ITYPE=1, JOBZ='N', UPLO='U', N=3 (eigenvalues only)
  ! =====================================================
  N = 3
  AP = (0.0d0, 0.0d0); BP = (0.0d0, 0.0d0); W = 0.0d0; Z = (0.0d0, 0.0d0)
  AP(1) = (4.0d0, 0.0d0)
  AP(2) = (1.0d0, -1.0d0)
  AP(3) = (5.0d0, 0.0d0)
  AP(4) = (2.0d0, 1.0d0)
  AP(5) = (3.0d0, 0.0d0)
  AP(6) = (6.0d0, 0.0d0)

  BP(1) = (2.0d0, 0.0d0)
  BP(2) = (0.5d0, -0.5d0)
  BP(3) = (3.0d0, 0.0d0)
  BP(4) = (0.0d0, 0.0d0)
  BP(5) = (0.5d0, 0.0d0)
  BP(6) = (2.0d0, 0.0d0)

  call ZHPGV(1, 'N', 'U', N, AP, BP, W, Z, 1, WORK, RWORK, INFO)
  call begin_test('itype1_n_upper')
  call print_int('info', INFO)
  call print_array('w', W, N)
  call end_test()

  ! =====================================================
  ! Test 5: ITYPE=2, JOBZ='V', UPLO='L', N=3
  ! =====================================================
  N = 3
  AP = (0.0d0, 0.0d0); BP = (0.0d0, 0.0d0); W = 0.0d0; Z = (0.0d0, 0.0d0)
  AP(1) = (4.0d0, 0.0d0)
  AP(2) = (1.0d0, 1.0d0)
  AP(3) = (2.0d0, -1.0d0)
  AP(4) = (5.0d0, 0.0d0)
  AP(5) = (3.0d0, 0.0d0)
  AP(6) = (6.0d0, 0.0d0)

  BP(1) = (2.0d0, 0.0d0)
  BP(2) = (0.5d0, 0.5d0)
  BP(3) = (0.0d0, 0.0d0)
  BP(4) = (3.0d0, 0.0d0)
  BP(5) = (0.5d0, 0.0d0)
  BP(6) = (2.0d0, 0.0d0)

  call ZHPGV(2, 'V', 'L', N, AP, BP, W, Z, N, WORK, RWORK, INFO)
  do J = 1, N
    do I = 1, N
      Zflat((J-1)*N + I) = Z(I,J)
    end do
  end do
  call begin_test('itype2_v_lower')
  call print_int('info', INFO)
  call print_array('w', W, N)
  call print_array('Z', Zflat_r, 2*N*N)
  call end_test()

  ! =====================================================
  ! Test 6: ITYPE=2, JOBZ='V', UPLO='U', N=3
  ! =====================================================
  N = 3
  AP = (0.0d0, 0.0d0); BP = (0.0d0, 0.0d0); W = 0.0d0; Z = (0.0d0, 0.0d0)
  AP(1) = (4.0d0, 0.0d0)
  AP(2) = (1.0d0, -1.0d0)
  AP(3) = (5.0d0, 0.0d0)
  AP(4) = (2.0d0, 1.0d0)
  AP(5) = (3.0d0, 0.0d0)
  AP(6) = (6.0d0, 0.0d0)

  BP(1) = (2.0d0, 0.0d0)
  BP(2) = (0.5d0, -0.5d0)
  BP(3) = (3.0d0, 0.0d0)
  BP(4) = (0.0d0, 0.0d0)
  BP(5) = (0.5d0, 0.0d0)
  BP(6) = (2.0d0, 0.0d0)

  call ZHPGV(2, 'V', 'U', N, AP, BP, W, Z, N, WORK, RWORK, INFO)
  do J = 1, N
    do I = 1, N
      Zflat((J-1)*N + I) = Z(I,J)
    end do
  end do
  call begin_test('itype2_v_upper')
  call print_int('info', INFO)
  call print_array('w', W, N)
  call print_array('Z', Zflat_r, 2*N*N)
  call end_test()

  ! =====================================================
  ! Test 7: ITYPE=3, JOBZ='V', UPLO='L', N=3
  ! =====================================================
  N = 3
  AP = (0.0d0, 0.0d0); BP = (0.0d0, 0.0d0); W = 0.0d0; Z = (0.0d0, 0.0d0)
  AP(1) = (4.0d0, 0.0d0)
  AP(2) = (1.0d0, 1.0d0)
  AP(3) = (2.0d0, -1.0d0)
  AP(4) = (5.0d0, 0.0d0)
  AP(5) = (3.0d0, 0.0d0)
  AP(6) = (6.0d0, 0.0d0)

  BP(1) = (2.0d0, 0.0d0)
  BP(2) = (0.5d0, 0.5d0)
  BP(3) = (0.0d0, 0.0d0)
  BP(4) = (3.0d0, 0.0d0)
  BP(5) = (0.5d0, 0.0d0)
  BP(6) = (2.0d0, 0.0d0)

  call ZHPGV(3, 'V', 'L', N, AP, BP, W, Z, N, WORK, RWORK, INFO)
  do J = 1, N
    do I = 1, N
      Zflat((J-1)*N + I) = Z(I,J)
    end do
  end do
  call begin_test('itype3_v_lower')
  call print_int('info', INFO)
  call print_array('w', W, N)
  call print_array('Z', Zflat_r, 2*N*N)
  call end_test()

  ! =====================================================
  ! Test 8: ITYPE=3, JOBZ='V', UPLO='U', N=3
  ! =====================================================
  N = 3
  AP = (0.0d0, 0.0d0); BP = (0.0d0, 0.0d0); W = 0.0d0; Z = (0.0d0, 0.0d0)
  AP(1) = (4.0d0, 0.0d0)
  AP(2) = (1.0d0, -1.0d0)
  AP(3) = (5.0d0, 0.0d0)
  AP(4) = (2.0d0, 1.0d0)
  AP(5) = (3.0d0, 0.0d0)
  AP(6) = (6.0d0, 0.0d0)

  BP(1) = (2.0d0, 0.0d0)
  BP(2) = (0.5d0, -0.5d0)
  BP(3) = (3.0d0, 0.0d0)
  BP(4) = (0.0d0, 0.0d0)
  BP(5) = (0.5d0, 0.0d0)
  BP(6) = (2.0d0, 0.0d0)

  call ZHPGV(3, 'V', 'U', N, AP, BP, W, Z, N, WORK, RWORK, INFO)
  do J = 1, N
    do I = 1, N
      Zflat((J-1)*N + I) = Z(I,J)
    end do
  end do
  call begin_test('itype3_v_upper')
  call print_int('info', INFO)
  call print_array('w', W, N)
  call print_array('Z', Zflat_r, 2*N*N)
  call end_test()

  ! =====================================================
  ! Test 9: N=0 quick return
  ! =====================================================
  call ZHPGV(1, 'V', 'U', 0, AP, BP, W, Z, 1, WORK, RWORK, INFO)
  call begin_test('n_zero')
  call print_int('info', INFO)
  call end_test()

  ! =====================================================
  ! Test 10: N=1
  ! =====================================================
  AP(1) = (6.0d0, 0.0d0)
  BP(1) = (2.0d0, 0.0d0)
  W = 0.0d0; Z = (0.0d0, 0.0d0)
  call ZHPGV(1, 'V', 'U', 1, AP, BP, W, Z, 1, WORK, RWORK, INFO)
  Zflat(1) = Z(1,1)
  call begin_test('n_one')
  call print_int('info', INFO)
  call print_scalar('w1', W(1))
  call print_array('Z', Zflat_r, 2)
  call end_test()

  ! =====================================================
  ! Test 11: Non-positive definite B (should return info = N + k)
  ! =====================================================
  N = 2
  AP(1) = (1.0d0, 0.0d0)
  AP(2) = (0.0d0, 0.0d0)
  AP(3) = (1.0d0, 0.0d0)
  BP(1) = (-1.0d0, 0.0d0)
  BP(2) = (0.0d0, 0.0d0)
  BP(3) = (1.0d0, 0.0d0)
  call ZHPGV(1, 'V', 'L', N, AP, BP, W, Z, N, WORK, RWORK, INFO)
  call begin_test('not_posdef')
  call print_int('info', INFO)
  call end_test()

end program
