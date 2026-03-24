program test_dlahqr
  use test_utils
  implicit none

  integer, parameter :: MAXN = 6
  integer, parameter :: LDH = MAXN
  integer, parameter :: LDZ = MAXN
  double precision :: H(LDH, MAXN), Z(LDZ, MAXN), WR(MAXN), WI(MAXN)
  integer :: INFO, i, j, N

  ! ---------------------------------------------------------------
  ! Test 1: 4x4 upper Hessenberg with real eigenvalues
  ! Matrix (upper Hessenberg):
  !   [ 4   3   2   1 ]
  !   [ 1   4   3   2 ]
  !   [ 0   1   4   3 ]
  !   [ 0   0   1   4 ]
  ! WANTT=true, WANTZ=true, ILO=1, IHI=4
  ! ---------------------------------------------------------------
  N = 4
  H = 0.0d0
  H(1,1) = 4.0d0; H(1,2) = 3.0d0; H(1,3) = 2.0d0; H(1,4) = 1.0d0
  H(2,1) = 1.0d0; H(2,2) = 4.0d0; H(2,3) = 3.0d0; H(2,4) = 2.0d0
  H(3,2) = 1.0d0; H(3,3) = 4.0d0; H(3,4) = 3.0d0
  H(4,3) = 1.0d0; H(4,4) = 4.0d0
  ! Initialize Z to identity
  Z = 0.0d0
  do i = 1, N
    Z(i,i) = 1.0d0
  end do
  call DLAHQR(.TRUE., .TRUE., N, 1, N, H, LDH, WR, WI, 1, N, Z, LDZ, INFO)
  call begin_test('real_eigenvalues_4x4')
  call print_int('info', INFO)
  call print_array('wr', WR, N)
  call print_array('wi', WI, N)
  call print_matrix('h', H, LDH, N, N)
  call print_matrix('z', Z, LDZ, N, N)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 2: 4x4 with complex conjugate eigenvalue pairs
  ! Matrix:
  !   [ 0   -1   2   1 ]
  !   [ 1    0   1   2 ]
  !   [ 0    1   0  -1 ]
  !   [ 0    0   1   0 ]
  ! ---------------------------------------------------------------
  N = 4
  H = 0.0d0
  H(1,1) = 0.0d0; H(1,2) = -1.0d0; H(1,3) = 2.0d0; H(1,4) = 1.0d0
  H(2,1) = 1.0d0; H(2,2) =  0.0d0; H(2,3) = 1.0d0; H(2,4) = 2.0d0
  H(3,2) = 1.0d0; H(3,3) = 0.0d0; H(3,4) = -1.0d0
  H(4,3) = 1.0d0; H(4,4) = 0.0d0
  Z = 0.0d0
  do i = 1, N
    Z(i,i) = 1.0d0
  end do
  call DLAHQR(.TRUE., .TRUE., N, 1, N, H, LDH, WR, WI, 1, N, Z, LDZ, INFO)
  call begin_test('complex_eigenvalues_4x4')
  call print_int('info', INFO)
  call print_array('wr', WR, N)
  call print_array('wi', WI, N)
  call print_matrix('h', H, LDH, N, N)
  call print_matrix('z', Z, LDZ, N, N)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 3: Already triangular (quick convergence)
  ! Matrix:
  !   [ 1   2   3 ]
  !   [ 0   4   5 ]
  !   [ 0   0   6 ]
  ! Eigenvalues should be 1, 4, 6
  ! ---------------------------------------------------------------
  N = 3
  H = 0.0d0
  H(1,1) = 1.0d0; H(1,2) = 2.0d0; H(1,3) = 3.0d0
  H(2,2) = 4.0d0; H(2,3) = 5.0d0
  H(3,3) = 6.0d0
  Z = 0.0d0
  do i = 1, N
    Z(i,i) = 1.0d0
  end do
  call DLAHQR(.TRUE., .TRUE., N, 1, N, H, LDH, WR, WI, 1, N, Z, LDZ, INFO)
  call begin_test('triangular_3x3')
  call print_int('info', INFO)
  call print_array('wr', WR, N)
  call print_array('wi', WI, N)
  call print_matrix('h', H, LDH, N, N)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 4: WANTT=false, WANTZ=false (eigenvalues only)
  ! Same 4x4 upper Hessenberg as test 1
  ! ---------------------------------------------------------------
  N = 4
  H = 0.0d0
  H(1,1) = 4.0d0; H(1,2) = 3.0d0; H(1,3) = 2.0d0; H(1,4) = 1.0d0
  H(2,1) = 1.0d0; H(2,2) = 4.0d0; H(2,3) = 3.0d0; H(2,4) = 2.0d0
  H(3,2) = 1.0d0; H(3,3) = 4.0d0; H(3,4) = 3.0d0
  H(4,3) = 1.0d0; H(4,4) = 4.0d0
  call DLAHQR(.FALSE., .FALSE., N, 1, N, H, LDH, WR, WI, 1, N, Z, LDZ, INFO)
  call begin_test('eigenvalues_only_4x4')
  call print_int('info', INFO)
  call print_array('wr', WR, N)
  call print_array('wi', WI, N)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 5: ILO=IHI (1x1 submatrix)
  ! ---------------------------------------------------------------
  N = 4
  H = 0.0d0
  H(1,1) = 5.0d0; H(1,2) = 3.0d0; H(1,3) = 2.0d0; H(1,4) = 1.0d0
  H(2,2) = 4.0d0; H(2,3) = 3.0d0; H(2,4) = 2.0d0
  H(3,3) = 3.0d0; H(3,4) = 1.0d0
  H(4,4) = 7.0d0
  ! ILO=2, IHI=2 means just extract eigenvalue at position 2
  call DLAHQR(.TRUE., .FALSE., N, 2, 2, H, LDH, WR, WI, 1, N, Z, LDZ, INFO)
  call begin_test('ilo_eq_ihi')
  call print_int('info', INFO)
  call print_scalar('wr2', WR(2))
  call print_scalar('wi2', WI(2))
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 6: N=0 edge case
  ! ---------------------------------------------------------------
  N = 0
  call DLAHQR(.TRUE., .TRUE., N, 1, 0, H, LDH, WR, WI, 1, 0, Z, LDZ, INFO)
  call begin_test('n0')
  call print_int('info', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 7: 2x2 submatrix with complex eigenvalues
  ! Matrix:
  !   [ 0  -2 ]
  !   [ 1   0 ]
  ! Eigenvalues: +/- i*sqrt(2)
  ! ---------------------------------------------------------------
  N = 2
  H = 0.0d0
  H(1,1) = 0.0d0; H(1,2) = -2.0d0
  H(2,1) = 1.0d0; H(2,2) = 0.0d0
  Z = 0.0d0
  Z(1,1) = 1.0d0; Z(2,2) = 1.0d0
  call DLAHQR(.TRUE., .TRUE., N, 1, N, H, LDH, WR, WI, 1, N, Z, LDZ, INFO)
  call begin_test('2x2_complex')
  call print_int('info', INFO)
  call print_array('wr', WR, N)
  call print_array('wi', WI, N)
  call print_matrix('h', H, LDH, N, N)
  call print_matrix('z', Z, LDZ, N, N)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 8: Partial range ILO/IHI (deflated block)
  ! 6x6 matrix, only process rows/cols 2-5
  ! ---------------------------------------------------------------
  N = 6
  H = 0.0d0
  H(1,1) = 10.0d0; H(1,2) = 1.0d0; H(1,3) = 2.0d0
  H(1,4) = 3.0d0; H(1,5) = 4.0d0; H(1,6) = 5.0d0
  H(2,2) = 4.0d0; H(2,3) = 3.0d0; H(2,4) = 1.0d0; H(2,5) = 0.5d0; H(2,6) = 0.1d0
  H(3,2) = 1.0d0; H(3,3) = 3.0d0; H(3,4) = 2.0d0; H(3,5) = 1.0d0; H(3,6) = 0.2d0
  H(4,3) = 0.5d0; H(4,4) = 2.0d0; H(4,5) = 1.5d0; H(4,6) = 0.3d0
  H(5,4) = 0.25d0; H(5,5) = 1.0d0; H(5,6) = 0.4d0
  H(6,6) = 20.0d0
  Z = 0.0d0
  do i = 1, N
    Z(i,i) = 1.0d0
  end do
  call DLAHQR(.TRUE., .TRUE., N, 2, 5, H, LDH, WR, WI, 1, N, Z, LDZ, INFO)
  call begin_test('partial_range_6x6')
  call print_int('info', INFO)
  call print_array('wr', WR, N)
  call print_array('wi', WI, N)
  call print_matrix('h', H, LDH, N, N)
  call print_matrix('z', Z, LDZ, N, N)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 9: 5x5 with mixed real and complex eigenvalues
  ! ---------------------------------------------------------------
  N = 5
  H = 0.0d0
  H(1,1) = 5.0d0; H(1,2) = 4.0d0; H(1,3) = 1.0d0; H(1,4) = 0.5d0; H(1,5) = 0.1d0
  H(2,1) = 1.0d0; H(2,2) = 3.0d0; H(2,3) = 2.0d0; H(2,4) = 1.0d0; H(2,5) = 0.5d0
  H(3,2) = 2.0d0; H(3,3) = 1.0d0; H(3,4) = 3.0d0; H(3,5) = 1.0d0
  H(4,3) = 1.5d0; H(4,4) = 2.0d0; H(4,5) = 2.0d0
  H(5,4) = 0.5d0; H(5,5) = 4.0d0
  Z = 0.0d0
  do i = 1, N
    Z(i,i) = 1.0d0
  end do
  call DLAHQR(.TRUE., .TRUE., N, 1, N, H, LDH, WR, WI, 1, N, Z, LDZ, INFO)
  call begin_test('mixed_eigenvalues_5x5')
  call print_int('info', INFO)
  call print_array('wr', WR, N)
  call print_array('wi', WI, N)
  call print_matrix('h', H, LDH, N, N)
  call print_matrix('z', Z, LDZ, N, N)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 10: WANTT=true, WANTZ=false (Schur form but no Z)
  ! ---------------------------------------------------------------
  N = 3
  H = 0.0d0
  H(1,1) = 2.0d0; H(1,2) = 1.0d0; H(1,3) = 0.5d0
  H(2,1) = 3.0d0; H(2,2) = 1.0d0; H(2,3) = 2.0d0
  H(3,2) = 1.0d0; H(3,3) = 3.0d0
  call DLAHQR(.TRUE., .FALSE., N, 1, N, H, LDH, WR, WI, 1, N, Z, LDZ, INFO)
  call begin_test('wantt_no_wantz_3x3')
  call print_int('info', INFO)
  call print_array('wr', WR, N)
  call print_array('wi', WI, N)
  call print_matrix('h', H, LDH, N, N)
  call end_test()

end program
