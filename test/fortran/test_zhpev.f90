program test_zhpev
  use test_utils
  implicit none

  integer, parameter :: NMAX = 4
  integer :: N, INFO, I, J
  complex*16 :: AP(NMAX*(NMAX+1)/2), WORK(2*NMAX)
  complex*16 :: Z(NMAX, NMAX)
  double precision :: W(NMAX), RWORK(3*NMAX)

  ! EQUIVALENCE for printing interleaved re/im
  double precision :: AP_r(2*NMAX*(NMAX+1)/2)
  double precision :: Z_r(2*NMAX*NMAX)
  equivalence (AP, AP_r)
  equivalence (Z, Z_r)
  double precision :: Zflat_r(2*NMAX*NMAX)
  complex*16 :: Zflat(NMAX*NMAX)
  equivalence (Zflat, Zflat_r)

  ! =====================================================
  ! Hermitian 4x4 matrix:
  ! A = [4     1-i   -2+i   2   ]
  !     [1+i   2      0     1-i ]
  !     [-2-i  0      3    -2+i ]
  !     [2     1+i   -2-i  -1   ]
  ! =====================================================

  ! =====================================================
  ! Test 1: JOBZ='V', UPLO='U', 4x4
  ! Upper packed (column-major):
  ! col1=[4], col2=[1-i, 2], col3=[-2+i, 0, 3], col4=[2, 1-i, -2-i, -1]
  ! =====================================================
  N = 4
  AP = (0.0d0, 0.0d0); W = 0.0d0; Z = (0.0d0, 0.0d0)
  AP(1) = (4.0d0, 0.0d0)
  AP(2) = (1.0d0, -1.0d0)
  AP(3) = (2.0d0, 0.0d0)
  AP(4) = (-2.0d0, 1.0d0)
  AP(5) = (0.0d0, 0.0d0)
  AP(6) = (3.0d0, 0.0d0)
  AP(7) = (2.0d0, 0.0d0)
  AP(8) = (1.0d0, -1.0d0)
  AP(9) = (-2.0d0, -1.0d0)
  AP(10) = (-1.0d0, 0.0d0)

  call ZHPEV('V', 'U', N, AP, W, Z, N, WORK, RWORK, INFO)
  do J = 1, N
    do I = 1, N
      Zflat((J-1)*N + I) = Z(I,J)
    end do
  end do
  call begin_test('jobz_v_uplo_u_4x4')
  call print_int('info', INFO)
  call print_array('w', W, N)
  call print_array('Z', Zflat_r, 2*N*N)
  call end_test()

  ! =====================================================
  ! Test 2: JOBZ='V', UPLO='L', 4x4
  ! Lower packed (column-major):
  ! col1=[4,1+i,-2-i,2], col2=[2,0,1-i], col3=[3,-2+i], col4=[-1]
  ! =====================================================
  N = 4
  AP = (0.0d0, 0.0d0); W = 0.0d0; Z = (0.0d0, 0.0d0)
  AP(1) = (4.0d0, 0.0d0)
  AP(2) = (1.0d0, 1.0d0)
  AP(3) = (-2.0d0, -1.0d0)
  AP(4) = (2.0d0, 0.0d0)
  AP(5) = (2.0d0, 0.0d0)
  AP(6) = (0.0d0, 0.0d0)
  AP(7) = (1.0d0, -1.0d0)
  AP(8) = (3.0d0, 0.0d0)
  AP(9) = (-2.0d0, 1.0d0)
  AP(10) = (-1.0d0, 0.0d0)

  call ZHPEV('V', 'L', N, AP, W, Z, N, WORK, RWORK, INFO)
  do J = 1, N
    do I = 1, N
      Zflat((J-1)*N + I) = Z(I,J)
    end do
  end do
  call begin_test('jobz_v_uplo_l_4x4')
  call print_int('info', INFO)
  call print_array('w', W, N)
  call print_array('Z', Zflat_r, 2*N*N)
  call end_test()

  ! =====================================================
  ! Test 3: JOBZ='N', UPLO='U', 4x4 -- eigenvalues only
  ! =====================================================
  N = 4
  AP = (0.0d0, 0.0d0); W = 0.0d0; Z = (0.0d0, 0.0d0)
  AP(1) = (4.0d0, 0.0d0)
  AP(2) = (1.0d0, -1.0d0)
  AP(3) = (2.0d0, 0.0d0)
  AP(4) = (-2.0d0, 1.0d0)
  AP(5) = (0.0d0, 0.0d0)
  AP(6) = (3.0d0, 0.0d0)
  AP(7) = (2.0d0, 0.0d0)
  AP(8) = (1.0d0, -1.0d0)
  AP(9) = (-2.0d0, -1.0d0)
  AP(10) = (-1.0d0, 0.0d0)

  call ZHPEV('N', 'U', N, AP, W, Z, 1, WORK, RWORK, INFO)
  call begin_test('jobz_n_uplo_u_4x4')
  call print_int('info', INFO)
  call print_array('w', W, N)
  call end_test()

  ! =====================================================
  ! Test 4: JOBZ='N', UPLO='L', 4x4 -- eigenvalues only
  ! =====================================================
  N = 4
  AP = (0.0d0, 0.0d0); W = 0.0d0; Z = (0.0d0, 0.0d0)
  AP(1) = (4.0d0, 0.0d0)
  AP(2) = (1.0d0, 1.0d0)
  AP(3) = (-2.0d0, -1.0d0)
  AP(4) = (2.0d0, 0.0d0)
  AP(5) = (2.0d0, 0.0d0)
  AP(6) = (0.0d0, 0.0d0)
  AP(7) = (1.0d0, -1.0d0)
  AP(8) = (3.0d0, 0.0d0)
  AP(9) = (-2.0d0, 1.0d0)
  AP(10) = (-1.0d0, 0.0d0)

  call ZHPEV('N', 'L', N, AP, W, Z, 1, WORK, RWORK, INFO)
  call begin_test('jobz_n_uplo_l_4x4')
  call print_int('info', INFO)
  call print_array('w', W, N)
  call end_test()

  ! =====================================================
  ! Test 5: JOBZ='V', UPLO='L', 3x3
  ! A = [5     1-i    2+i ]
  !     [1+i   4      1   ]
  !     [2-i   1      6   ]
  ! Lower packed: col1=[5,1+i,2-i], col2=[4,1], col3=[6]
  ! =====================================================
  N = 3
  AP = (0.0d0, 0.0d0); W = 0.0d0; Z = (0.0d0, 0.0d0)
  AP(1) = (5.0d0, 0.0d0)
  AP(2) = (1.0d0, 1.0d0)
  AP(3) = (2.0d0, -1.0d0)
  AP(4) = (4.0d0, 0.0d0)
  AP(5) = (1.0d0, 0.0d0)
  AP(6) = (6.0d0, 0.0d0)

  call ZHPEV('V', 'L', N, AP, W, Z, N, WORK, RWORK, INFO)
  do J = 1, N
    do I = 1, N
      Zflat((J-1)*N + I) = Z(I,J)
    end do
  end do
  call begin_test('jobz_v_uplo_l_3x3')
  call print_int('info', INFO)
  call print_array('w', W, N)
  call print_array('Z', Zflat_r, 2*N*N)
  call end_test()

  ! =====================================================
  ! Test 6: JOBZ='V', UPLO='U', 3x3
  ! Upper packed: col1=[5], col2=[1-i,4], col3=[2+i,1,6]
  ! =====================================================
  N = 3
  AP = (0.0d0, 0.0d0); W = 0.0d0; Z = (0.0d0, 0.0d0)
  AP(1) = (5.0d0, 0.0d0)
  AP(2) = (1.0d0, -1.0d0)
  AP(3) = (4.0d0, 0.0d0)
  AP(4) = (2.0d0, 1.0d0)
  AP(5) = (1.0d0, 0.0d0)
  AP(6) = (6.0d0, 0.0d0)

  call ZHPEV('V', 'U', N, AP, W, Z, N, WORK, RWORK, INFO)
  do J = 1, N
    do I = 1, N
      Zflat((J-1)*N + I) = Z(I,J)
    end do
  end do
  call begin_test('jobz_v_uplo_u_3x3')
  call print_int('info', INFO)
  call print_array('w', W, N)
  call print_array('Z', Zflat_r, 2*N*N)
  call end_test()

  ! =====================================================
  ! Test 7: N=1, JOBZ='V'
  ! =====================================================
  N = 1
  AP = (0.0d0, 0.0d0); W = 0.0d0; Z = (0.0d0, 0.0d0)
  AP(1) = (3.5d0, 0.0d0)

  call ZHPEV('V', 'L', N, AP, W, Z, N, WORK, RWORK, INFO)
  Zflat(1) = Z(1,1)
  call begin_test('n1_jobz_v')
  call print_int('info', INFO)
  call print_scalar('w1', W(1))
  call print_array('Z', Zflat_r, 2)
  call end_test()

  ! =====================================================
  ! Test 8: N=1, JOBZ='N'
  ! =====================================================
  N = 1
  AP = (0.0d0, 0.0d0); W = 0.0d0; Z = (0.0d0, 0.0d0)
  AP(1) = (7.25d0, 0.0d0)

  call ZHPEV('N', 'U', N, AP, W, Z, 1, WORK, RWORK, INFO)
  call begin_test('n1_jobz_n')
  call print_int('info', INFO)
  call print_scalar('w1', W(1))
  call end_test()

  ! =====================================================
  ! Test 9: N=0, JOBZ='V'
  ! =====================================================
  call ZHPEV('V', 'L', 0, AP, W, Z, 1, WORK, RWORK, INFO)
  call begin_test('n0')
  call print_int('info', INFO)
  call end_test()

  ! =====================================================
  ! Test 10: JOBZ='N', UPLO='L', 3x3 -- eigenvalues only
  ! =====================================================
  N = 3
  AP = (0.0d0, 0.0d0); W = 0.0d0; Z = (0.0d0, 0.0d0)
  AP(1) = (5.0d0, 0.0d0)
  AP(2) = (1.0d0, 1.0d0)
  AP(3) = (2.0d0, -1.0d0)
  AP(4) = (4.0d0, 0.0d0)
  AP(5) = (1.0d0, 0.0d0)
  AP(6) = (6.0d0, 0.0d0)

  call ZHPEV('N', 'L', N, AP, W, Z, 1, WORK, RWORK, INFO)
  call begin_test('jobz_n_uplo_l_3x3')
  call print_int('info', INFO)
  call print_array('w', W, N)
  call end_test()

  ! =====================================================
  ! Test 11: diagonal matrix 4x4, JOBZ='V', UPLO='L'
  ! diag(3, 1, 4, 2)
  ! =====================================================
  N = 4
  AP = (0.0d0, 0.0d0); W = 0.0d0; Z = (0.0d0, 0.0d0)
  AP(1) = (3.0d0, 0.0d0)
  AP(2) = (0.0d0, 0.0d0)
  AP(3) = (0.0d0, 0.0d0)
  AP(4) = (0.0d0, 0.0d0)
  AP(5) = (1.0d0, 0.0d0)
  AP(6) = (0.0d0, 0.0d0)
  AP(7) = (0.0d0, 0.0d0)
  AP(8) = (4.0d0, 0.0d0)
  AP(9) = (0.0d0, 0.0d0)
  AP(10) = (2.0d0, 0.0d0)

  call ZHPEV('V', 'L', N, AP, W, Z, N, WORK, RWORK, INFO)
  do J = 1, N
    do I = 1, N
      Zflat((J-1)*N + I) = Z(I,J)
    end do
  end do
  call begin_test('diagonal_4x4')
  call print_int('info', INFO)
  call print_array('w', W, N)
  call print_array('Z', Zflat_r, 2*N*N)
  call end_test()

end program
