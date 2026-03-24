program test_dhseqr
  use test_utils
  implicit none

  integer, parameter :: MAXN = 6
  integer, parameter :: LDH = MAXN
  integer, parameter :: LDZ = MAXN
  double precision :: H(LDH, MAXN), Z(LDZ, MAXN), WR(MAXN), WI(MAXN)
  double precision :: WORK(256)
  integer :: INFO, LWORK, i, N

  LWORK = 256

  ! ---------------------------------------------------------------
  ! Test 1: JOB='E' (eigenvalues only), COMPZ='N', 6x6 Hessenberg
  ! ---------------------------------------------------------------
  N = 6
  H = 0.0d0
  H(1,1) = 4.0d0; H(1,2) = 3.0d0; H(1,3) = 2.0d0; H(1,4) = 1.0d0; H(1,5) = 0.5d0; H(1,6) = 0.1d0
  H(2,1) = 1.0d0; H(2,2) = 4.0d0; H(2,3) = 3.0d0; H(2,4) = 2.0d0; H(2,5) = 1.0d0; H(2,6) = 0.5d0
  H(3,2) = 1.0d0; H(3,3) = 4.0d0; H(3,4) = 3.0d0; H(3,5) = 2.0d0; H(3,6) = 1.0d0
  H(4,3) = 1.0d0; H(4,4) = 4.0d0; H(4,5) = 3.0d0; H(4,6) = 2.0d0
  H(5,4) = 1.0d0; H(5,5) = 4.0d0; H(5,6) = 3.0d0
  H(6,5) = 1.0d0; H(6,6) = 4.0d0
  WR = 0.0d0
  WI = 0.0d0
  call DHSEQR('E', 'N', N, 1, N, H, LDH, WR, WI, Z, LDZ, WORK, LWORK, INFO)
  call begin_test('eigenvalues_only_6x6')
  call print_int('info', INFO)
  call print_array('wr', WR, N)
  call print_array('wi', WI, N)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 2: JOB='S', COMPZ='I' (Schur form + compute Z from scratch)
  ! 4x4 Hessenberg
  ! ---------------------------------------------------------------
  N = 4
  H = 0.0d0
  H(1,1) = 4.0d0; H(1,2) = 3.0d0; H(1,3) = 2.0d0; H(1,4) = 1.0d0
  H(2,1) = 1.0d0; H(2,2) = 4.0d0; H(2,3) = 3.0d0; H(2,4) = 2.0d0
  H(3,2) = 1.0d0; H(3,3) = 4.0d0; H(3,4) = 3.0d0
  H(4,3) = 1.0d0; H(4,4) = 4.0d0
  WR = 0.0d0
  WI = 0.0d0
  call DHSEQR('S', 'I', N, 1, N, H, LDH, WR, WI, Z, LDZ, WORK, LWORK, INFO)
  call begin_test('schur_with_z_init_4x4')
  call print_int('info', INFO)
  call print_array('wr', WR, N)
  call print_array('wi', WI, N)
  call print_matrix('h', H, LDH, N, N)
  call print_matrix('z', Z, LDZ, N, N)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 3: JOB='S', COMPZ='V' (Schur form + update existing Z)
  ! 4x4 with Z initialized to identity
  ! ---------------------------------------------------------------
  N = 4
  H = 0.0d0
  H(1,1) = 2.0d0; H(1,2) = 1.0d0; H(1,3) = 0.5d0; H(1,4) = 0.1d0
  H(2,1) = 3.0d0; H(2,2) = 1.0d0; H(2,3) = 2.0d0; H(2,4) = 0.5d0
  H(3,2) = 1.0d0; H(3,3) = 3.0d0; H(3,4) = 1.0d0
  H(4,3) = 0.5d0; H(4,4) = 4.0d0
  Z = 0.0d0
  do i = 1, N
    Z(i,i) = 1.0d0
  end do
  WR = 0.0d0
  WI = 0.0d0
  call DHSEQR('S', 'V', N, 1, N, H, LDH, WR, WI, Z, LDZ, WORK, LWORK, INFO)
  call begin_test('schur_with_z_update_4x4')
  call print_int('info', INFO)
  call print_array('wr', WR, N)
  call print_array('wi', WI, N)
  call print_matrix('h', H, LDH, N, N)
  call print_matrix('z', Z, LDZ, N, N)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 4: COMPZ='N' (no Z), JOB='S'
  ! ---------------------------------------------------------------
  N = 4
  H = 0.0d0
  H(1,1) = 4.0d0; H(1,2) = 3.0d0; H(1,3) = 2.0d0; H(1,4) = 1.0d0
  H(2,1) = 1.0d0; H(2,2) = 4.0d0; H(2,3) = 3.0d0; H(2,4) = 2.0d0
  H(3,2) = 1.0d0; H(3,3) = 4.0d0; H(3,4) = 3.0d0
  H(4,3) = 1.0d0; H(4,4) = 4.0d0
  WR = 0.0d0
  WI = 0.0d0
  call DHSEQR('S', 'N', N, 1, N, H, LDH, WR, WI, Z, LDZ, WORK, LWORK, INFO)
  call begin_test('schur_no_z_4x4')
  call print_int('info', INFO)
  call print_array('wr', WR, N)
  call print_array('wi', WI, N)
  call print_matrix('h', H, LDH, N, N)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 5: N=0 edge case
  ! ---------------------------------------------------------------
  N = 0
  call DHSEQR('S', 'I', N, 1, 0, H, LDH, WR, WI, Z, LDZ, WORK, LWORK, INFO)
  call begin_test('n0')
  call print_int('info', INFO)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 6: N=1 edge case
  ! ---------------------------------------------------------------
  N = 1
  H = 0.0d0
  H(1,1) = 7.0d0
  WR = 0.0d0
  WI = 0.0d0
  call DHSEQR('S', 'I', N, 1, N, H, LDH, WR, WI, Z, LDZ, WORK, LWORK, INFO)
  call begin_test('n1')
  call print_int('info', INFO)
  call print_scalar('wr1', WR(1))
  call print_scalar('wi1', WI(1))
  call print_scalar('h11', H(1,1))
  call print_scalar('z11', Z(1,1))
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 7: N=2, complex conjugate eigenvalue pair
  ! ---------------------------------------------------------------
  N = 2
  H = 0.0d0
  H(1,1) = 0.0d0; H(1,2) = -2.0d0
  H(2,1) = 1.0d0; H(2,2) = 0.0d0
  WR = 0.0d0
  WI = 0.0d0
  call DHSEQR('S', 'I', N, 1, N, H, LDH, WR, WI, Z, LDZ, WORK, LWORK, INFO)
  call begin_test('n2_complex')
  call print_int('info', INFO)
  call print_array('wr', WR, N)
  call print_array('wi', WI, N)
  call print_matrix('h', H, LDH, N, N)
  call print_matrix('z', Z, LDZ, N, N)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 8: ILO=IHI (single element in active block)
  ! ---------------------------------------------------------------
  N = 4
  H = 0.0d0
  H(1,1) = 5.0d0; H(1,2) = 3.0d0; H(1,3) = 2.0d0; H(1,4) = 1.0d0
  H(2,2) = 4.0d0; H(2,3) = 3.0d0; H(2,4) = 2.0d0
  H(3,3) = 3.0d0; H(3,4) = 1.0d0
  H(4,4) = 7.0d0
  WR = 0.0d0
  WI = 0.0d0
  call DHSEQR('S', 'N', N, 2, 2, H, LDH, WR, WI, Z, LDZ, WORK, LWORK, INFO)
  call begin_test('ilo_eq_ihi')
  call print_int('info', INFO)
  call print_scalar('wr2', WR(2))
  call print_scalar('wi2', WI(2))
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 9: 6x6 with complex eigenvalue pairs, JOB='S', COMPZ='I'
  ! ---------------------------------------------------------------
  N = 6
  H = 0.0d0
  H(1,1) = 1.0d0; H(1,2) = -2.0d0; H(1,3) = 1.0d0; H(1,4) = 0.5d0; H(1,5) = 0.1d0; H(1,6) = 0.2d0
  H(2,1) = 2.0d0; H(2,2) =  1.0d0; H(2,3) = -1.0d0; H(2,4) = 0.3d0; H(2,5) = 0.4d0; H(2,6) = 0.1d0
  H(3,2) = 1.5d0; H(3,3) = 2.0d0; H(3,4) = -1.0d0; H(3,5) = 0.5d0; H(3,6) = 0.3d0
  H(4,3) = 1.0d0; H(4,4) = 3.0d0; H(4,5) = -2.0d0; H(4,6) = 0.4d0
  H(5,4) = 2.0d0; H(5,5) = 1.0d0; H(5,6) = -1.0d0
  H(6,5) = 1.0d0; H(6,6) = 2.0d0
  WR = 0.0d0
  WI = 0.0d0
  call DHSEQR('S', 'I', N, 1, N, H, LDH, WR, WI, Z, LDZ, WORK, LWORK, INFO)
  call begin_test('complex_pairs_6x6')
  call print_int('info', INFO)
  call print_array('wr', WR, N)
  call print_array('wi', WI, N)
  call print_matrix('h', H, LDH, N, N)
  call print_matrix('z', Z, LDZ, N, N)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 10: Partial range ILO=2, IHI=5, JOB='S', COMPZ='I'
  ! 6x6 matrix
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
  WR = 0.0d0
  WI = 0.0d0
  call DHSEQR('S', 'I', N, 2, 5, H, LDH, WR, WI, Z, LDZ, WORK, LWORK, INFO)
  call begin_test('partial_range_6x6')
  call print_int('info', INFO)
  call print_array('wr', WR, N)
  call print_array('wi', WI, N)
  call print_matrix('h', H, LDH, N, N)
  call print_matrix('z', Z, LDZ, N, N)
  call end_test()

end program
