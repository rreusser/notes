program test_dtgsen
  use test_utils
  implicit none

  integer, parameter :: MAXN = 6
  double precision :: A(MAXN,MAXN), B(MAXN,MAXN)
  double precision :: Q(MAXN,MAXN), Z(MAXN,MAXN)
  double precision :: ALPHAR(MAXN), ALPHAI(MAXN), BETA(MAXN)
  double precision :: DIF(2), PL, PR
  double precision :: WORK(500)
  integer :: IWORK(500), M, INFO, N, i
  logical :: SELCT(MAXN)

  ! ==========================================================================
  ! Test 1: IJOB=0, 4x4, select eigenvalues 1 and 3 (1x1 blocks only)
  ! ==========================================================================
  N = 4
  A = 0.0d0; B = 0.0d0; Q = 0.0d0; Z = 0.0d0

  ! Upper triangular A (all 1x1 blocks in Schur form)
  A(1,1) = 1.0d0; A(1,2) = 0.5d0; A(1,3) = 0.3d0; A(1,4) = 0.2d0
  A(2,2) = 2.0d0; A(2,3) = 0.4d0; A(2,4) = 0.1d0
  A(3,3) = 3.0d0; A(3,4) = 0.6d0
  A(4,4) = 4.0d0

  ! Upper triangular B
  B(1,1) = 1.0d0; B(1,2) = 0.2d0; B(1,3) = 0.1d0; B(1,4) = 0.05d0
  B(2,2) = 1.5d0; B(2,3) = 0.3d0; B(2,4) = 0.15d0
  B(3,3) = 2.0d0; B(3,4) = 0.4d0
  B(4,4) = 2.5d0

  do i = 1, N
    Q(i,i) = 1.0d0
    Z(i,i) = 1.0d0
  end do

  SELCT = .false.
  SELCT(1) = .true.
  SELCT(3) = .true.

  call DTGSEN(0, .TRUE., .TRUE., SELCT, N, A, MAXN, B, MAXN, &
              ALPHAR, ALPHAI, BETA, Q, MAXN, Z, MAXN, M, PL, PR, DIF, &
              WORK, 500, IWORK, 500, INFO)

  call begin_test('ijob0_select13')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_matrix('A', A, MAXN, N, N)
  call print_matrix('B', B, MAXN, N, N)
  call print_matrix('Q', Q, MAXN, N, N)
  call print_matrix('Z', Z, MAXN, N, N)
  call print_array('ALPHAR', ALPHAR, N)
  call print_array('ALPHAI', ALPHAI, N)
  call print_array('BETA', BETA, N)
  call end_test()

  ! ==========================================================================
  ! Test 2: IJOB=0, 4x4 with 2x2 block (complex pair), select complex pair
  ! ==========================================================================
  N = 4
  A = 0.0d0; B = 0.0d0; Q = 0.0d0; Z = 0.0d0

  ! Upper quasi-triangular A: 1x1, 1x1, then 2x2 block
  A(1,1) = 1.0d0; A(1,2) = 0.5d0; A(1,3) = 0.3d0; A(1,4) = 0.2d0
  A(2,2) = 2.0d0; A(2,3) = 0.4d0; A(2,4) = 0.1d0
  A(3,3) = 4.0d0; A(3,4) = 1.5d0
  A(4,3) = -1.5d0; A(4,4) = 4.0d0

  B(1,1) = 1.0d0; B(1,2) = 0.2d0; B(1,3) = 0.1d0; B(1,4) = 0.05d0
  B(2,2) = 1.5d0; B(2,3) = 0.3d0; B(2,4) = 0.15d0
  B(3,3) = 2.0d0; B(3,4) = 0.4d0
  B(4,4) = 2.5d0

  do i = 1, N
    Q(i,i) = 1.0d0
    Z(i,i) = 1.0d0
  end do

  SELCT = .false.
  SELCT(3) = .true.
  SELCT(4) = .true.

  call DTGSEN(0, .TRUE., .TRUE., SELCT, N, A, MAXN, B, MAXN, &
              ALPHAR, ALPHAI, BETA, Q, MAXN, Z, MAXN, M, PL, PR, DIF, &
              WORK, 500, IWORK, 500, INFO)

  call begin_test('ijob0_complex_pair')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_matrix('A', A, MAXN, N, N)
  call print_matrix('B', B, MAXN, N, N)
  call print_matrix('Q', Q, MAXN, N, N)
  call print_matrix('Z', Z, MAXN, N, N)
  call print_array('ALPHAR', ALPHAR, N)
  call print_array('ALPHAI', ALPHAI, N)
  call print_array('BETA', BETA, N)
  call end_test()

  ! ==========================================================================
  ! Test 3: IJOB=1, compute PL and PR
  ! ==========================================================================
  N = 4
  A = 0.0d0; B = 0.0d0; Q = 0.0d0; Z = 0.0d0

  A(1,1) = 1.0d0; A(1,2) = 0.5d0; A(1,3) = 0.3d0; A(1,4) = 0.2d0
  A(2,2) = 2.0d0; A(2,3) = 0.4d0; A(2,4) = 0.1d0
  A(3,3) = 3.0d0; A(3,4) = 0.6d0
  A(4,4) = 4.0d0

  B(1,1) = 1.0d0; B(1,2) = 0.2d0; B(1,3) = 0.1d0; B(1,4) = 0.05d0
  B(2,2) = 1.5d0; B(2,3) = 0.3d0; B(2,4) = 0.15d0
  B(3,3) = 2.0d0; B(3,4) = 0.4d0
  B(4,4) = 2.5d0

  do i = 1, N
    Q(i,i) = 1.0d0
    Z(i,i) = 1.0d0
  end do

  SELCT = .false.
  SELCT(1) = .true.
  SELCT(2) = .true.

  PL = 0.0d0; PR = 0.0d0
  call DTGSEN(1, .TRUE., .TRUE., SELCT, N, A, MAXN, B, MAXN, &
              ALPHAR, ALPHAI, BETA, Q, MAXN, Z, MAXN, M, PL, PR, DIF, &
              WORK, 500, IWORK, 500, INFO)

  call begin_test('ijob1_pl_pr')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_scalar('PL', PL)
  call print_scalar('PR', PR)
  call print_matrix('A', A, MAXN, N, N)
  call print_matrix('B', B, MAXN, N, N)
  call print_array('ALPHAR', ALPHAR, N)
  call print_array('ALPHAI', ALPHAI, N)
  call print_array('BETA', BETA, N)
  call end_test()

  ! ==========================================================================
  ! Test 4: IJOB=4, compute PL, PR, DIF (Frobenius)
  ! ==========================================================================
  N = 4
  A = 0.0d0; B = 0.0d0; Q = 0.0d0; Z = 0.0d0

  A(1,1) = 1.0d0; A(1,2) = 0.5d0; A(1,3) = 0.3d0; A(1,4) = 0.2d0
  A(2,2) = 2.0d0; A(2,3) = 0.4d0; A(2,4) = 0.1d0
  A(3,3) = 3.0d0; A(3,4) = 0.6d0
  A(4,4) = 4.0d0

  B(1,1) = 1.0d0; B(1,2) = 0.2d0; B(1,3) = 0.1d0; B(1,4) = 0.05d0
  B(2,2) = 1.5d0; B(2,3) = 0.3d0; B(2,4) = 0.15d0
  B(3,3) = 2.0d0; B(3,4) = 0.4d0
  B(4,4) = 2.5d0

  do i = 1, N
    Q(i,i) = 1.0d0
    Z(i,i) = 1.0d0
  end do

  SELCT = .false.
  SELCT(1) = .true.
  SELCT(2) = .true.

  PL = 0.0d0; PR = 0.0d0; DIF = 0.0d0
  call DTGSEN(4, .TRUE., .TRUE., SELCT, N, A, MAXN, B, MAXN, &
              ALPHAR, ALPHAI, BETA, Q, MAXN, Z, MAXN, M, PL, PR, DIF, &
              WORK, 500, IWORK, 500, INFO)

  call begin_test('ijob4_dif_frobenius')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_scalar('PL', PL)
  call print_scalar('PR', PR)
  call print_array('DIF', DIF, 2)
  call print_matrix('A', A, MAXN, N, N)
  call print_matrix('B', B, MAXN, N, N)
  call print_array('ALPHAR', ALPHAR, N)
  call print_array('ALPHAI', ALPHAI, N)
  call print_array('BETA', BETA, N)
  call end_test()

  ! ==========================================================================
  ! Test 5: IJOB=5, compute PL, PR, DIF (one-norm)
  ! ==========================================================================
  N = 4
  A = 0.0d0; B = 0.0d0; Q = 0.0d0; Z = 0.0d0

  A(1,1) = 1.0d0; A(1,2) = 0.5d0; A(1,3) = 0.3d0; A(1,4) = 0.2d0
  A(2,2) = 2.0d0; A(2,3) = 0.4d0; A(2,4) = 0.1d0
  A(3,3) = 3.0d0; A(3,4) = 0.6d0
  A(4,4) = 4.0d0

  B(1,1) = 1.0d0; B(1,2) = 0.2d0; B(1,3) = 0.1d0; B(1,4) = 0.05d0
  B(2,2) = 1.5d0; B(2,3) = 0.3d0; B(2,4) = 0.15d0
  B(3,3) = 2.0d0; B(3,4) = 0.4d0
  B(4,4) = 2.5d0

  do i = 1, N
    Q(i,i) = 1.0d0
    Z(i,i) = 1.0d0
  end do

  SELCT = .false.
  SELCT(1) = .true.
  SELCT(2) = .true.

  PL = 0.0d0; PR = 0.0d0; DIF = 0.0d0
  call DTGSEN(5, .TRUE., .TRUE., SELCT, N, A, MAXN, B, MAXN, &
              ALPHAR, ALPHAI, BETA, Q, MAXN, Z, MAXN, M, PL, PR, DIF, &
              WORK, 500, IWORK, 500, INFO)

  call begin_test('ijob5_dif_onenorm')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_scalar('PL', PL)
  call print_scalar('PR', PR)
  call print_array('DIF', DIF, 2)
  call print_matrix('A', A, MAXN, N, N)
  call print_matrix('B', B, MAXN, N, N)
  call print_array('ALPHAR', ALPHAR, N)
  call print_array('ALPHAI', ALPHAI, N)
  call print_array('BETA', BETA, N)
  call end_test()

  ! ==========================================================================
  ! Test 6: All selected (M=N quick return)
  ! ==========================================================================
  N = 3
  A = 0.0d0; B = 0.0d0; Q = 0.0d0; Z = 0.0d0

  A(1,1) = 1.0d0; A(1,2) = 0.5d0; A(1,3) = 0.3d0
  A(2,2) = 2.0d0; A(2,3) = 0.4d0
  A(3,3) = 3.0d0

  B(1,1) = 1.0d0; B(1,2) = 0.2d0; B(1,3) = 0.1d0
  B(2,2) = 1.5d0; B(2,3) = 0.3d0
  B(3,3) = 2.0d0

  do i = 1, N
    Q(i,i) = 1.0d0
    Z(i,i) = 1.0d0
  end do

  SELCT = .false.
  SELCT(1) = .true.
  SELCT(2) = .true.
  SELCT(3) = .true.

  PL = 0.0d0; PR = 0.0d0; DIF = 0.0d0
  call DTGSEN(4, .TRUE., .TRUE., SELCT, N, A, MAXN, B, MAXN, &
              ALPHAR, ALPHAI, BETA, Q, MAXN, Z, MAXN, M, PL, PR, DIF, &
              WORK, 500, IWORK, 500, INFO)

  call begin_test('all_selected')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_scalar('PL', PL)
  call print_scalar('PR', PR)
  call print_array('DIF', DIF, 2)
  call print_array('ALPHAR', ALPHAR, N)
  call print_array('ALPHAI', ALPHAI, N)
  call print_array('BETA', BETA, N)
  call end_test()

  ! ==========================================================================
  ! Test 7: None selected (M=0 quick return)
  ! ==========================================================================
  N = 3
  A = 0.0d0; B = 0.0d0; Q = 0.0d0; Z = 0.0d0

  A(1,1) = 1.0d0; A(1,2) = 0.5d0; A(1,3) = 0.3d0
  A(2,2) = 2.0d0; A(2,3) = 0.4d0
  A(3,3) = 3.0d0

  B(1,1) = 1.0d0; B(1,2) = 0.2d0; B(1,3) = 0.1d0
  B(2,2) = 1.5d0; B(2,3) = 0.3d0
  B(3,3) = 2.0d0

  do i = 1, N
    Q(i,i) = 1.0d0
    Z(i,i) = 1.0d0
  end do

  SELCT = .false.

  PL = 0.0d0; PR = 0.0d0; DIF = 0.0d0
  call DTGSEN(1, .TRUE., .TRUE., SELCT, N, A, MAXN, B, MAXN, &
              ALPHAR, ALPHAI, BETA, Q, MAXN, Z, MAXN, M, PL, PR, DIF, &
              WORK, 500, IWORK, 500, INFO)

  call begin_test('none_selected')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_scalar('PL', PL)
  call print_scalar('PR', PR)
  call print_array('ALPHAR', ALPHAR, N)
  call print_array('ALPHAI', ALPHAI, N)
  call print_array('BETA', BETA, N)
  call end_test()

  ! ==========================================================================
  ! Test 8: N=1 trivial
  ! ==========================================================================
  N = 1
  A = 0.0d0; B = 0.0d0; Q = 0.0d0; Z = 0.0d0
  A(1,1) = 5.0d0
  B(1,1) = 2.0d0
  Q(1,1) = 1.0d0
  Z(1,1) = 1.0d0
  SELCT = .false.
  SELCT(1) = .true.

  call DTGSEN(0, .TRUE., .TRUE., SELCT, N, A, MAXN, B, MAXN, &
              ALPHAR, ALPHAI, BETA, Q, MAXN, Z, MAXN, M, PL, PR, DIF, &
              WORK, 500, IWORK, 500, INFO)

  call begin_test('n1_trivial')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('ALPHAR', ALPHAR, N)
  call print_array('ALPHAI', ALPHAI, N)
  call print_array('BETA', BETA, N)
  call end_test()

  ! ==========================================================================
  ! Test 9: Select one eigenvalue out of complex pair (both get selected)
  ! ==========================================================================
  N = 4
  A = 0.0d0; B = 0.0d0; Q = 0.0d0; Z = 0.0d0

  A(1,1) = 4.0d0; A(1,2) = 1.5d0; A(1,3) = 0.3d0; A(1,4) = 0.2d0
  A(2,1) = -1.5d0; A(2,2) = 4.0d0; A(2,3) = 0.4d0; A(2,4) = 0.1d0
  A(3,3) = 1.0d0; A(3,4) = 0.6d0
  A(4,4) = 2.0d0

  B(1,1) = 2.0d0; B(1,2) = 0.4d0; B(1,3) = 0.1d0; B(1,4) = 0.05d0
  B(2,2) = 2.5d0; B(2,3) = 0.3d0; B(2,4) = 0.15d0
  B(3,3) = 1.0d0; B(3,4) = 0.2d0
  B(4,4) = 1.5d0

  do i = 1, N
    Q(i,i) = 1.0d0
    Z(i,i) = 1.0d0
  end do

  ! Select only one of the complex pair - both should be selected
  SELCT = .false.
  SELCT(3) = .true.
  SELCT(4) = .true.

  call DTGSEN(0, .TRUE., .TRUE., SELCT, N, A, MAXN, B, MAXN, &
              ALPHAR, ALPHAI, BETA, Q, MAXN, Z, MAXN, M, PL, PR, DIF, &
              WORK, 500, IWORK, 500, INFO)

  call begin_test('select_behind_complex')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_matrix('A', A, MAXN, N, N)
  call print_matrix('B', B, MAXN, N, N)
  call print_array('ALPHAR', ALPHAR, N)
  call print_array('ALPHAI', ALPHAI, N)
  call print_array('BETA', BETA, N)
  call end_test()

  ! ==========================================================================
  ! Test 10: IJOB=2, DIF via Frobenius norm (IDIFJB path)
  ! ==========================================================================
  N = 4
  A = 0.0d0; B = 0.0d0; Q = 0.0d0; Z = 0.0d0

  A(1,1) = 1.0d0; A(1,2) = 0.5d0; A(1,3) = 0.3d0; A(1,4) = 0.2d0
  A(2,2) = 2.0d0; A(2,3) = 0.4d0; A(2,4) = 0.1d0
  A(3,3) = 3.0d0; A(3,4) = 0.6d0
  A(4,4) = 4.0d0

  B(1,1) = 1.0d0; B(1,2) = 0.2d0; B(1,3) = 0.1d0; B(1,4) = 0.05d0
  B(2,2) = 1.5d0; B(2,3) = 0.3d0; B(2,4) = 0.15d0
  B(3,3) = 2.0d0; B(3,4) = 0.4d0
  B(4,4) = 2.5d0

  do i = 1, N
    Q(i,i) = 1.0d0
    Z(i,i) = 1.0d0
  end do

  SELCT = .false.
  SELCT(1) = .true.

  DIF = 0.0d0
  call DTGSEN(2, .TRUE., .TRUE., SELCT, N, A, MAXN, B, MAXN, &
              ALPHAR, ALPHAI, BETA, Q, MAXN, Z, MAXN, M, PL, PR, DIF, &
              WORK, 500, IWORK, 500, INFO)

  call begin_test('ijob2_dif_frobenius')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_array('DIF', DIF, 2)
  call print_array('ALPHAR', ALPHAR, N)
  call print_array('ALPHAI', ALPHAI, N)
  call print_array('BETA', BETA, N)
  call end_test()

  ! ==========================================================================
  ! Test 11: IJOB=3, DIF via one-norm + PL/PR
  ! ==========================================================================
  N = 4
  A = 0.0d0; B = 0.0d0; Q = 0.0d0; Z = 0.0d0

  A(1,1) = 1.0d0; A(1,2) = 0.5d0; A(1,3) = 0.3d0; A(1,4) = 0.2d0
  A(2,2) = 2.0d0; A(2,3) = 0.4d0; A(2,4) = 0.1d0
  A(3,3) = 3.0d0; A(3,4) = 0.6d0
  A(4,4) = 4.0d0

  B(1,1) = 1.0d0; B(1,2) = 0.2d0; B(1,3) = 0.1d0; B(1,4) = 0.05d0
  B(2,2) = 1.5d0; B(2,3) = 0.3d0; B(2,4) = 0.15d0
  B(3,3) = 2.0d0; B(3,4) = 0.4d0
  B(4,4) = 2.5d0

  do i = 1, N
    Q(i,i) = 1.0d0
    Z(i,i) = 1.0d0
  end do

  SELCT = .false.
  SELCT(1) = .true.
  SELCT(2) = .true.

  PL = 0.0d0; PR = 0.0d0; DIF = 0.0d0
  call DTGSEN(3, .TRUE., .TRUE., SELCT, N, A, MAXN, B, MAXN, &
              ALPHAR, ALPHAI, BETA, Q, MAXN, Z, MAXN, M, PL, PR, DIF, &
              WORK, 500, IWORK, 500, INFO)

  call begin_test('ijob3_onenorm_plpr')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_scalar('PL', PL)
  call print_scalar('PR', PR)
  call print_array('DIF', DIF, 2)
  call print_array('ALPHAR', ALPHAR, N)
  call print_array('ALPHAI', ALPHAI, N)
  call print_array('BETA', BETA, N)
  call end_test()

  ! ==========================================================================
  ! Test 12: Negative B diagonal (sign flip path)
  ! ==========================================================================
  N = 3
  A = 0.0d0; B = 0.0d0; Q = 0.0d0; Z = 0.0d0

  A(1,1) = 2.0d0; A(1,2) = 0.5d0; A(1,3) = 0.3d0
  A(2,2) = 3.0d0; A(2,3) = 0.4d0
  A(3,3) = 1.0d0

  B(1,1) = -1.0d0; B(1,2) = 0.2d0; B(1,3) = 0.1d0
  B(2,2) = 1.5d0; B(2,3) = 0.3d0
  B(3,3) = -2.0d0

  do i = 1, N
    Q(i,i) = 1.0d0
    Z(i,i) = 1.0d0
  end do

  SELCT = .false.
  SELCT(1) = .true.

  call DTGSEN(0, .TRUE., .TRUE., SELCT, N, A, MAXN, B, MAXN, &
              ALPHAR, ALPHAI, BETA, Q, MAXN, Z, MAXN, M, PL, PR, DIF, &
              WORK, 500, IWORK, 500, INFO)

  call begin_test('negative_b_diag')
  call print_int('info', INFO)
  call print_int('M', M)
  call print_matrix('A', A, MAXN, N, N)
  call print_matrix('B', B, MAXN, N, N)
  call print_matrix('Q', Q, MAXN, N, N)
  call print_array('ALPHAR', ALPHAR, N)
  call print_array('ALPHAI', ALPHAI, N)
  call print_array('BETA', BETA, N)
  call end_test()

end program
