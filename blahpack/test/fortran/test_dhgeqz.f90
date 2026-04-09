program test_dhgeqz
  use test_utils
  implicit none

  integer, parameter :: MAXN = 8
  double precision :: H(MAXN, MAXN), TT(MAXN, MAXN)
  double precision :: Q(MAXN, MAXN), Z(MAXN, MAXN)
  double precision :: ALPHAR(MAXN), ALPHAI(MAXN), BETA(MAXN)
  double precision :: WORK(MAXN * 10)
  integer :: INFO, N, ILO, IHI, I, LWORK

  LWORK = MAXN * 10

  ! ============================================================
  ! Test 1: JOB='E', COMPQ='N', COMPZ='N', 4x4 full range
  ! Hessenberg-triangular pair with known eigenvalues
  ! ============================================================
  N = 4; ILO = 1; IHI = 4
  H = 0.0d0; TT = 0.0d0; Q = 0.0d0; Z = 0.0d0
  ALPHAR = 0.0d0; ALPHAI = 0.0d0; BETA = 0.0d0

  ! Upper Hessenberg H
  H(1,1) = 2.0d0; H(1,2) = 3.0d0; H(1,3) = 1.0d0; H(1,4) = 0.5d0
  H(2,1) = 1.5d0; H(2,2) = 4.0d0; H(2,3) = 2.0d0; H(2,4) = 1.0d0
                   H(3,2) = 1.0d0; H(3,3) = 3.0d0; H(3,4) = 1.5d0
                                    H(4,3) = 0.8d0; H(4,4) = 1.0d0

  ! Upper triangular T
  TT(1,1) = 1.0d0; TT(1,2) = 0.5d0; TT(1,3) = 0.2d0; TT(1,4) = 0.1d0
                    TT(2,2) = 2.0d0; TT(2,3) = 0.3d0; TT(2,4) = 0.15d0
                                      TT(3,3) = 1.5d0; TT(3,4) = 0.4d0
                                                        TT(4,4) = 1.0d0

  call DHGEQZ('E', 'N', 'N', N, ILO, IHI, H, MAXN, TT, MAXN, &
              ALPHAR, ALPHAI, BETA, Q, MAXN, Z, MAXN, WORK, LWORK, INFO)

  call begin_test('eigenvalues only 4x4')
  call print_array('ALPHAR', ALPHAR, N)
  call print_array('ALPHAI', ALPHAI, N)
  call print_array('BETA', BETA, N)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 2: JOB='S', COMPQ='I', COMPZ='I', 4x4 full range
  ! Compute full Schur form with orthogonal matrices
  ! ============================================================
  N = 4; ILO = 1; IHI = 4
  H = 0.0d0; TT = 0.0d0; Q = 0.0d0; Z = 0.0d0
  ALPHAR = 0.0d0; ALPHAI = 0.0d0; BETA = 0.0d0

  H(1,1) = 2.0d0; H(1,2) = 3.0d0; H(1,3) = 1.0d0; H(1,4) = 0.5d0
  H(2,1) = 1.5d0; H(2,2) = 4.0d0; H(2,3) = 2.0d0; H(2,4) = 1.0d0
                   H(3,2) = 1.0d0; H(3,3) = 3.0d0; H(3,4) = 1.5d0
                                    H(4,3) = 0.8d0; H(4,4) = 1.0d0

  TT(1,1) = 1.0d0; TT(1,2) = 0.5d0; TT(1,3) = 0.2d0; TT(1,4) = 0.1d0
                    TT(2,2) = 2.0d0; TT(2,3) = 0.3d0; TT(2,4) = 0.15d0
                                      TT(3,3) = 1.5d0; TT(3,4) = 0.4d0
                                                        TT(4,4) = 1.0d0

  call DHGEQZ('S', 'I', 'I', N, ILO, IHI, H, MAXN, TT, MAXN, &
              ALPHAR, ALPHAI, BETA, Q, MAXN, Z, MAXN, WORK, LWORK, INFO)

  call begin_test('schur form 4x4 init')
  call print_matrix('H', H, MAXN, N, N)
  call print_matrix('TT', TT, MAXN, N, N)
  call print_matrix('Q', Q, MAXN, N, N)
  call print_matrix('Z', Z, MAXN, N, N)
  call print_array('ALPHAR', ALPHAR, N)
  call print_array('ALPHAI', ALPHAI, N)
  call print_array('BETA', BETA, N)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 3: JOB='E', COMPQ='N', COMPZ='N', 3x3, subrange ILO=2, IHI=3
  ! ============================================================
  N = 3; ILO = 2; IHI = 3
  H = 0.0d0; TT = 0.0d0; Q = 0.0d0; Z = 0.0d0
  ALPHAR = 0.0d0; ALPHAI = 0.0d0; BETA = 0.0d0

  H(1,1) = 5.0d0; H(1,2) = 1.0d0; H(1,3) = 0.5d0
                   H(2,2) = 3.0d0; H(2,3) = 2.0d0
                                    H(3,3) = 1.0d0
  ! H(2,1) must be zero since ILO=2 (already balanced)
  H(3,2) = 1.5d0

  TT(1,1) = 2.0d0; TT(1,2) = 0.3d0; TT(1,3) = 0.1d0
                    TT(2,2) = 1.0d0; TT(2,3) = 0.4d0
                                      TT(3,3) = 3.0d0

  call DHGEQZ('E', 'N', 'N', N, ILO, IHI, H, MAXN, TT, MAXN, &
              ALPHAR, ALPHAI, BETA, Q, MAXN, Z, MAXN, WORK, LWORK, INFO)

  call begin_test('eigenvalues subrange 3x3')
  call print_array('ALPHAR', ALPHAR, N)
  call print_array('ALPHAI', ALPHAI, N)
  call print_array('BETA', BETA, N)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 4: JOB='S', COMPQ='I', COMPZ='I', 3x3, subrange ILO=2, IHI=3
  ! ============================================================
  N = 3; ILO = 2; IHI = 3
  H = 0.0d0; TT = 0.0d0; Q = 0.0d0; Z = 0.0d0
  ALPHAR = 0.0d0; ALPHAI = 0.0d0; BETA = 0.0d0

  H(1,1) = 5.0d0; H(1,2) = 1.0d0; H(1,3) = 0.5d0
                   H(2,2) = 3.0d0; H(2,3) = 2.0d0
                                    H(3,3) = 1.0d0
  H(3,2) = 1.5d0

  TT(1,1) = 2.0d0; TT(1,2) = 0.3d0; TT(1,3) = 0.1d0
                    TT(2,2) = 1.0d0; TT(2,3) = 0.4d0
                                      TT(3,3) = 3.0d0

  call DHGEQZ('S', 'I', 'I', N, ILO, IHI, H, MAXN, TT, MAXN, &
              ALPHAR, ALPHAI, BETA, Q, MAXN, Z, MAXN, WORK, LWORK, INFO)

  call begin_test('schur subrange 3x3')
  call print_matrix('H', H, MAXN, N, N)
  call print_matrix('TT', TT, MAXN, N, N)
  call print_matrix('Q', Q, MAXN, N, N)
  call print_matrix('Z', Z, MAXN, N, N)
  call print_array('ALPHAR', ALPHAR, N)
  call print_array('ALPHAI', ALPHAI, N)
  call print_array('BETA', BETA, N)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 5: JOB='S', COMPQ='I', COMPZ='I', 5x5 with complex eigenvalues
  ! Use a matrix pair whose eigenvalues include complex conjugate pairs
  ! ============================================================
  N = 5; ILO = 1; IHI = 5
  H = 0.0d0; TT = 0.0d0; Q = 0.0d0; Z = 0.0d0
  ALPHAR = 0.0d0; ALPHAI = 0.0d0; BETA = 0.0d0

  ! Upper Hessenberg H designed to have complex eigenvalues
  H(1,1) = 0.5d0; H(1,2) = 1.0d0; H(1,3) = 0.3d0; H(1,4) = 0.1d0; H(1,5) = 0.2d0
  H(2,1) = 2.0d0; H(2,2) = 0.5d0; H(2,3) = 0.5d0; H(2,4) = 0.2d0; H(2,5) = 0.1d0
                   H(3,2) = 3.0d0; H(3,3) = 0.5d0; H(3,4) = 0.4d0; H(3,5) = 0.3d0
                                    H(4,3) = 2.5d0; H(4,4) = 0.5d0; H(4,5) = 0.5d0
                                                     H(5,4) = 2.0d0; H(5,5) = 0.5d0

  ! Upper triangular T (identity-like for clean eigenvalues)
  TT(1,1) = 1.0d0; TT(1,2) = 0.1d0; TT(1,3) = 0.05d0; TT(1,4) = 0.02d0; TT(1,5) = 0.01d0
                    TT(2,2) = 1.0d0; TT(2,3) = 0.1d0;  TT(2,4) = 0.05d0; TT(2,5) = 0.02d0
                                      TT(3,3) = 1.0d0;  TT(3,4) = 0.1d0;  TT(3,5) = 0.05d0
                                                         TT(4,4) = 1.0d0;  TT(4,5) = 0.1d0
                                                                            TT(5,5) = 1.0d0

  call DHGEQZ('S', 'I', 'I', N, ILO, IHI, H, MAXN, TT, MAXN, &
              ALPHAR, ALPHAI, BETA, Q, MAXN, Z, MAXN, WORK, LWORK, INFO)

  call begin_test('schur 5x5 complex eigs')
  call print_matrix('H', H, MAXN, N, N)
  call print_matrix('TT', TT, MAXN, N, N)
  call print_matrix('Q', Q, MAXN, N, N)
  call print_matrix('Z', Z, MAXN, N, N)
  call print_array('ALPHAR', ALPHAR, N)
  call print_array('ALPHAI', ALPHAI, N)
  call print_array('BETA', BETA, N)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 6: JOB='E', COMPQ='N', COMPZ='N', 2x2 matrix
  ! ============================================================
  N = 2; ILO = 1; IHI = 2
  H = 0.0d0; TT = 0.0d0; Q = 0.0d0; Z = 0.0d0
  ALPHAR = 0.0d0; ALPHAI = 0.0d0; BETA = 0.0d0

  H(1,1) = 1.0d0; H(1,2) = 2.0d0
  H(2,1) = 3.0d0; H(2,2) = 4.0d0

  TT(1,1) = 1.0d0; TT(1,2) = 0.5d0
                    TT(2,2) = 2.0d0

  call DHGEQZ('E', 'N', 'N', N, ILO, IHI, H, MAXN, TT, MAXN, &
              ALPHAR, ALPHAI, BETA, Q, MAXN, Z, MAXN, WORK, LWORK, INFO)

  call begin_test('eigenvalues 2x2')
  call print_array('ALPHAR', ALPHAR, N)
  call print_array('ALPHAI', ALPHAI, N)
  call print_array('BETA', BETA, N)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 7: JOB='S', COMPQ='V', COMPZ='V', 4x4 with pre-existing Q, Z
  ! ============================================================
  N = 4; ILO = 1; IHI = 4
  H = 0.0d0; TT = 0.0d0; Q = 0.0d0; Z = 0.0d0
  ALPHAR = 0.0d0; ALPHAI = 0.0d0; BETA = 0.0d0

  H(1,1) = 2.0d0; H(1,2) = 3.0d0; H(1,3) = 1.0d0; H(1,4) = 0.5d0
  H(2,1) = 1.5d0; H(2,2) = 4.0d0; H(2,3) = 2.0d0; H(2,4) = 1.0d0
                   H(3,2) = 1.0d0; H(3,3) = 3.0d0; H(3,4) = 1.5d0
                                    H(4,3) = 0.8d0; H(4,4) = 1.0d0

  TT(1,1) = 1.0d0; TT(1,2) = 0.5d0; TT(1,3) = 0.2d0; TT(1,4) = 0.1d0
                    TT(2,2) = 2.0d0; TT(2,3) = 0.3d0; TT(2,4) = 0.15d0
                                      TT(3,3) = 1.5d0; TT(3,4) = 0.4d0
                                                        TT(4,4) = 1.0d0

  ! Start Q and Z as identity (simulating V mode with prior balancing)
  do I = 1, N
    Q(I, I) = 1.0d0
    Z(I, I) = 1.0d0
  end do

  call DHGEQZ('S', 'V', 'V', N, ILO, IHI, H, MAXN, TT, MAXN, &
              ALPHAR, ALPHAI, BETA, Q, MAXN, Z, MAXN, WORK, LWORK, INFO)

  call begin_test('schur 4x4 update')
  call print_matrix('H', H, MAXN, N, N)
  call print_matrix('TT', TT, MAXN, N, N)
  call print_matrix('Q', Q, MAXN, N, N)
  call print_matrix('Z', Z, MAXN, N, N)
  call print_array('ALPHAR', ALPHAR, N)
  call print_array('ALPHAI', ALPHAI, N)
  call print_array('BETA', BETA, N)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 8: N=1 edge case
  ! ============================================================
  N = 1; ILO = 1; IHI = 1
  H = 0.0d0; TT = 0.0d0; Q = 0.0d0; Z = 0.0d0
  ALPHAR = 0.0d0; ALPHAI = 0.0d0; BETA = 0.0d0

  H(1,1) = 7.0d0
  TT(1,1) = 3.0d0

  call DHGEQZ('S', 'I', 'I', N, ILO, IHI, H, MAXN, TT, MAXN, &
              ALPHAR, ALPHAI, BETA, Q, MAXN, Z, MAXN, WORK, LWORK, INFO)

  call begin_test('n=1 edge case')
  call print_array('ALPHAR', ALPHAR, N)
  call print_array('ALPHAI', ALPHAI, N)
  call print_array('BETA', BETA, N)
  call print_matrix('Q', Q, MAXN, N, N)
  call print_matrix('Z', Z, MAXN, N, N)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 9: 3x3 with complex eigenvalue pair (2x2 complex block path)
  ! ============================================================
  N = 3; ILO = 1; IHI = 3
  H = 0.0d0; TT = 0.0d0; Q = 0.0d0; Z = 0.0d0
  ALPHAR = 0.0d0; ALPHAI = 0.0d0; BETA = 0.0d0

  ! H designed so the 2x2 bottom block has complex eigenvalues
  H(1,1) = 1.0d0; H(1,2) = 0.5d0; H(1,3) = 0.3d0
  H(2,1) = 4.0d0; H(2,2) = 1.0d0; H(2,3) = 0.5d0
                   H(3,2) = 3.0d0; H(3,3) = 1.0d0

  TT(1,1) = 1.0d0; TT(1,2) = 0.1d0; TT(1,3) = 0.05d0
                    TT(2,2) = 1.0d0; TT(2,3) = 0.1d0
                                      TT(3,3) = 1.0d0

  call DHGEQZ('S', 'I', 'I', N, ILO, IHI, H, MAXN, TT, MAXN, &
              ALPHAR, ALPHAI, BETA, Q, MAXN, Z, MAXN, WORK, LWORK, INFO)

  call begin_test('complex 2x2 block 3x3')
  call print_array('ALPHAR', ALPHAR, N)
  call print_array('ALPHAI', ALPHAI, N)
  call print_array('BETA', BETA, N)
  call print_matrix('H', H, MAXN, N, N)
  call print_matrix('TT', TT, MAXN, N, N)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 10: 4x4 with negative T diagonal (tests negation path)
  ! ============================================================
  N = 4; ILO = 1; IHI = 4
  H = 0.0d0; TT = 0.0d0; Q = 0.0d0; Z = 0.0d0
  ALPHAR = 0.0d0; ALPHAI = 0.0d0; BETA = 0.0d0

  H(1,1) = 2.0d0; H(1,2) = 1.0d0; H(1,3) = 0.5d0; H(1,4) = 0.2d0
  H(2,1) = 1.0d0; H(2,2) = 3.0d0; H(2,3) = 1.0d0; H(2,4) = 0.3d0
                   H(3,2) = 0.5d0; H(3,3) = 1.0d0; H(3,4) = 0.5d0
                                    H(4,3) = 0.3d0; H(4,4) = 4.0d0

  TT(1,1) = -1.0d0; TT(1,2) = 0.5d0; TT(1,3) = 0.2d0; TT(1,4) = 0.1d0
                     TT(2,2) = 2.0d0; TT(2,3) = 0.3d0; TT(2,4) = 0.15d0
                                       TT(3,3) = -1.5d0; TT(3,4) = 0.4d0
                                                          TT(4,4) = 1.0d0

  call DHGEQZ('S', 'I', 'I', N, ILO, IHI, H, MAXN, TT, MAXN, &
              ALPHAR, ALPHAI, BETA, Q, MAXN, Z, MAXN, WORK, LWORK, INFO)

  call begin_test('negative T diagonal 4x4')
  call print_array('ALPHAR', ALPHAR, N)
  call print_array('ALPHAI', ALPHAI, N)
  call print_array('BETA', BETA, N)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 11: 4x4 with ILO=2, IHI=3 (tests handleLowEigenvalues)
  ! ============================================================
  N = 4; ILO = 2; IHI = 3
  H = 0.0d0; TT = 0.0d0; Q = 0.0d0; Z = 0.0d0
  ALPHAR = 0.0d0; ALPHAI = 0.0d0; BETA = 0.0d0

  H(1,1) = 5.0d0; H(1,2) = 1.0d0; H(1,3) = 0.5d0; H(1,4) = 0.2d0
                   H(2,2) = 3.0d0; H(2,3) = 2.0d0; H(2,4) = 0.5d0
                                    H(3,3) = 1.0d0; H(3,4) = 0.3d0
                                                     H(4,4) = 7.0d0
  H(3,2) = 1.5d0

  TT(1,1) = 2.0d0; TT(1,2) = 0.3d0; TT(1,3) = 0.1d0; TT(1,4) = 0.05d0
                    TT(2,2) = 1.0d0; TT(2,3) = 0.4d0; TT(2,4) = 0.15d0
                                      TT(3,3) = 3.0d0; TT(3,4) = 0.2d0
                                                        TT(4,4) = -1.0d0

  call DHGEQZ('S', 'I', 'I', N, ILO, IHI, H, MAXN, TT, MAXN, &
              ALPHAR, ALPHAI, BETA, Q, MAXN, Z, MAXN, WORK, LWORK, INFO)

  call begin_test('subrange with below and above 4x4')
  call print_array('ALPHAR', ALPHAR, N)
  call print_array('ALPHAI', ALPHAI, N)
  call print_array('BETA', BETA, N)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 12: N=0 quick return
  ! ============================================================
  N = 0; ILO = 1; IHI = 0
  H = 0.0d0; TT = 0.0d0; Q = 0.0d0; Z = 0.0d0
  ALPHAR = 0.0d0; ALPHAI = 0.0d0; BETA = 0.0d0
  WORK = 0.0d0

  call DHGEQZ('S', 'I', 'I', N, ILO, IHI, H, MAXN, TT, MAXN, &
              ALPHAR, ALPHAI, BETA, Q, MAXN, Z, MAXN, WORK, LWORK, INFO)

  call begin_test('n=0 quick return')
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 13: IHI < ILO (skip main iteration)
  ! ============================================================
  N = 3; ILO = 3; IHI = 2
  H = 0.0d0; TT = 0.0d0; Q = 0.0d0; Z = 0.0d0
  ALPHAR = 0.0d0; ALPHAI = 0.0d0; BETA = 0.0d0

  H(1,1) = 5.0d0; H(1,2) = 1.0d0; H(1,3) = 0.5d0
                   H(2,2) = 3.0d0; H(2,3) = 2.0d0
                                    H(3,3) = 1.0d0

  TT(1,1) = 2.0d0; TT(1,2) = 0.3d0; TT(1,3) = 0.1d0
                    TT(2,2) = 1.0d0; TT(2,3) = 0.4d0
                                      TT(3,3) = 3.0d0

  call DHGEQZ('S', 'I', 'I', N, ILO, IHI, H, MAXN, TT, MAXN, &
              ALPHAR, ALPHAI, BETA, Q, MAXN, Z, MAXN, WORK, LWORK, INFO)

  call begin_test('ihi lt ilo skip')
  call print_array('ALPHAR', ALPHAR, N)
  call print_array('ALPHAI', ALPHAI, N)
  call print_array('BETA', BETA, N)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 14: Eigenvalues-only with negative T diag above active block
  ! Tests lines 275-277 (non-Schur path with neg T diagonal)
  ! ============================================================
  N = 4; ILO = 1; IHI = 2
  H = 0.0d0; TT = 0.0d0; Q = 0.0d0; Z = 0.0d0
  ALPHAR = 0.0d0; ALPHAI = 0.0d0; BETA = 0.0d0

  H(1,1) = 2.0d0; H(1,2) = 1.0d0; H(1,3) = 0.0d0; H(1,4) = 0.0d0
  H(2,1) = 0.5d0; H(2,2) = 3.0d0; H(2,3) = 0.0d0; H(2,4) = 0.0d0
                                    H(3,3) = 7.0d0; H(3,4) = 0.5d0
                                                     H(4,4) = 5.0d0

  TT(1,1) = 1.0d0; TT(1,2) = 0.2d0; TT(1,3) = 0.0d0; TT(1,4) = 0.0d0
                    TT(2,2) = 2.0d0;  TT(2,3) = 0.0d0; TT(2,4) = 0.0d0
                                       TT(3,3) = -1.0d0; TT(3,4) = 0.1d0
                                                          TT(4,4) = -3.0d0

  call DHGEQZ('E', 'N', 'N', N, ILO, IHI, H, MAXN, TT, MAXN, &
              ALPHAR, ALPHAI, BETA, Q, MAXN, Z, MAXN, WORK, LWORK, INFO)

  call begin_test('eig only neg T above')
  call print_array('ALPHAR', ALPHAR, N)
  call print_array('ALPHAI', ALPHAI, N)
  call print_array('BETA', BETA, N)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 15: Eigenvalues-only with neg T below (handleLowEigenvalues)
  ! Tests lines 1159-1173
  ! ============================================================
  N = 4; ILO = 3; IHI = 4
  H = 0.0d0; TT = 0.0d0; Q = 0.0d0; Z = 0.0d0
  ALPHAR = 0.0d0; ALPHAI = 0.0d0; BETA = 0.0d0

  H(1,1) = 5.0d0; H(1,2) = 1.0d0; H(1,3) = 0.0d0; H(1,4) = 0.0d0
                   H(2,2) = 3.0d0; H(2,3) = 0.0d0; H(2,4) = 0.0d0
                                    H(3,3) = 2.0d0; H(3,4) = 1.0d0
                                                     H(4,4) = 4.0d0
  H(4,3) = 0.5d0

  TT(1,1) = -2.0d0; TT(1,2) = 0.3d0; TT(1,3) = 0.0d0; TT(1,4) = 0.0d0
                     TT(2,2) = -1.0d0; TT(2,3) = 0.0d0; TT(2,4) = 0.0d0
                                        TT(3,3) = 1.0d0;  TT(3,4) = 0.2d0
                                                           TT(4,4) = 3.0d0

  call DHGEQZ('E', 'N', 'N', N, ILO, IHI, H, MAXN, TT, MAXN, &
              ALPHAR, ALPHAI, BETA, Q, MAXN, Z, MAXN, WORK, LWORK, INFO)

  call begin_test('eig only neg T below')
  call print_array('ALPHAR', ALPHAR, N)
  call print_array('ALPHAI', ALPHAI, N)
  call print_array('BETA', BETA, N)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 16: Eigenvalues-only 5x5 with complex eigs
  ! Tests eigenvalues-only path through doComplexShift/do2x2Block
  ! ============================================================
  N = 5; ILO = 1; IHI = 5
  H = 0.0d0; TT = 0.0d0; Q = 0.0d0; Z = 0.0d0
  ALPHAR = 0.0d0; ALPHAI = 0.0d0; BETA = 0.0d0

  H(1,1) = 0.5d0; H(1,2) = 1.0d0; H(1,3) = 0.3d0; H(1,4) = 0.1d0; H(1,5) = 0.2d0
  H(2,1) = 2.0d0; H(2,2) = 0.5d0; H(2,3) = 0.5d0; H(2,4) = 0.2d0; H(2,5) = 0.1d0
                   H(3,2) = 3.0d0; H(3,3) = 0.5d0; H(3,4) = 0.4d0; H(3,5) = 0.3d0
                                    H(4,3) = 2.5d0; H(4,4) = 0.5d0; H(4,5) = 0.5d0
                                                     H(5,4) = 2.0d0; H(5,5) = 0.5d0

  TT(1,1) = 1.0d0; TT(1,2) = 0.1d0; TT(1,3) = 0.05d0; TT(1,4) = 0.02d0; TT(1,5) = 0.01d0
                    TT(2,2) = 1.0d0; TT(2,3) = 0.1d0;  TT(2,4) = 0.05d0; TT(2,5) = 0.02d0
                                      TT(3,3) = 1.0d0;  TT(3,4) = 0.1d0;  TT(3,5) = 0.05d0
                                                         TT(4,4) = 1.0d0;  TT(4,5) = 0.1d0
                                                                            TT(5,5) = 1.0d0

  call DHGEQZ('E', 'N', 'N', N, ILO, IHI, H, MAXN, TT, MAXN, &
              ALPHAR, ALPHAI, BETA, Q, MAXN, Z, MAXN, WORK, LWORK, INFO)

  call begin_test('eig only 5x5 complex')
  call print_array('ALPHAR', ALPHAR, N)
  call print_array('ALPHAI', ALPHAI, N)
  call print_array('BETA', BETA, N)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 17: 6x6 with complex eigenvalues (tests doDoubleShiftQZStep)
  ! Needs block size >= 3 (ifirst+1 < ilast) for implicit double shift
  ! ============================================================
  N = 6; ILO = 1; IHI = 6
  H = 0.0d0; TT = 0.0d0; Q = 0.0d0; Z = 0.0d0
  ALPHAR = 0.0d0; ALPHAI = 0.0d0; BETA = 0.0d0

  ! Hessenberg with strong complex eigenvalue content
  H(1,1) = 1.0d0; H(1,2) = 2.0d0; H(1,3) = 0.5d0; H(1,4) = 0.1d0; H(1,5) = 0.05d0; H(1,6) = 0.02d0
  H(2,1) = 3.0d0; H(2,2) = 1.0d0; H(2,3) = 1.0d0; H(2,4) = 0.3d0; H(2,5) = 0.1d0;  H(2,6) = 0.05d0
                   H(3,2) = 4.0d0; H(3,3) = 1.0d0; H(3,4) = 0.8d0; H(3,5) = 0.2d0;  H(3,6) = 0.1d0
                                    H(4,3) = 3.0d0; H(4,4) = 1.0d0; H(4,5) = 1.0d0;  H(4,6) = 0.3d0
                                                     H(5,4) = 2.5d0; H(5,5) = 1.0d0;  H(5,6) = 0.5d0
                                                                      H(6,5) = 2.0d0;  H(6,6) = 1.0d0

  TT(1,1) = 1.0d0; TT(1,2) = 0.1d0; TT(1,3) = 0.05d0; TT(1,4) = 0.02d0; TT(1,5) = 0.01d0; TT(1,6) = 0.005d0
                    TT(2,2) = 1.0d0; TT(2,3) = 0.1d0;  TT(2,4) = 0.05d0; TT(2,5) = 0.02d0; TT(2,6) = 0.01d0
                                      TT(3,3) = 1.0d0;  TT(3,4) = 0.1d0;  TT(3,5) = 0.05d0; TT(3,6) = 0.02d0
                                                         TT(4,4) = 1.0d0;  TT(4,5) = 0.1d0;  TT(4,6) = 0.05d0
                                                                            TT(5,5) = 1.0d0;  TT(5,6) = 0.1d0
                                                                                               TT(6,6) = 1.0d0

  call DHGEQZ('S', 'I', 'I', N, ILO, IHI, H, MAXN, TT, MAXN, &
              ALPHAR, ALPHAI, BETA, Q, MAXN, Z, MAXN, WORK, LWORK, INFO)

  call begin_test('schur 6x6 double shift')
  call print_matrix('H', H, MAXN, N, N)
  call print_matrix('TT', TT, MAXN, N, N)
  call print_array('ALPHAR', ALPHAR, N)
  call print_array('ALPHAI', ALPHAI, N)
  call print_array('BETA', BETA, N)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 18: Eigenvalues-only 6x6 double shift path
  ! ============================================================
  N = 6; ILO = 1; IHI = 6
  H = 0.0d0; TT = 0.0d0; Q = 0.0d0; Z = 0.0d0
  ALPHAR = 0.0d0; ALPHAI = 0.0d0; BETA = 0.0d0

  H(1,1) = 1.0d0; H(1,2) = 2.0d0; H(1,3) = 0.5d0; H(1,4) = 0.1d0; H(1,5) = 0.05d0; H(1,6) = 0.02d0
  H(2,1) = 3.0d0; H(2,2) = 1.0d0; H(2,3) = 1.0d0; H(2,4) = 0.3d0; H(2,5) = 0.1d0;  H(2,6) = 0.05d0
                   H(3,2) = 4.0d0; H(3,3) = 1.0d0; H(3,4) = 0.8d0; H(3,5) = 0.2d0;  H(3,6) = 0.1d0
                                    H(4,3) = 3.0d0; H(4,4) = 1.0d0; H(4,5) = 1.0d0;  H(4,6) = 0.3d0
                                                     H(5,4) = 2.5d0; H(5,5) = 1.0d0;  H(5,6) = 0.5d0
                                                                      H(6,5) = 2.0d0;  H(6,6) = 1.0d0

  TT(1,1) = 1.0d0; TT(1,2) = 0.1d0; TT(1,3) = 0.05d0; TT(1,4) = 0.02d0; TT(1,5) = 0.01d0; TT(1,6) = 0.005d0
                    TT(2,2) = 1.0d0; TT(2,3) = 0.1d0;  TT(2,4) = 0.05d0; TT(2,5) = 0.02d0; TT(2,6) = 0.01d0
                                      TT(3,3) = 1.0d0;  TT(3,4) = 0.1d0;  TT(3,5) = 0.05d0; TT(3,6) = 0.02d0
                                                         TT(4,4) = 1.0d0;  TT(4,5) = 0.1d0;  TT(4,6) = 0.05d0
                                                                            TT(5,5) = 1.0d0;  TT(5,6) = 0.1d0
                                                                                               TT(6,6) = 1.0d0

  call DHGEQZ('E', 'N', 'N', N, ILO, IHI, H, MAXN, TT, MAXN, &
              ALPHAR, ALPHAI, BETA, Q, MAXN, Z, MAXN, WORK, LWORK, INFO)

  call begin_test('eig only 6x6 double shift')
  call print_array('ALPHAR', ALPHAR, N)
  call print_array('ALPHAI', ALPHAI, N)
  call print_array('BETA', BETA, N)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 19: 4x4 eigenvalues-only with negative T diagonal at deflation
  ! Tests doLabel80 eigenvalues-only negative T path (line 520-522)
  ! ============================================================
  N = 4; ILO = 1; IHI = 4
  H = 0.0d0; TT = 0.0d0; Q = 0.0d0; Z = 0.0d0
  ALPHAR = 0.0d0; ALPHAI = 0.0d0; BETA = 0.0d0

  H(1,1) = 2.0d0; H(1,2) = 1.0d0; H(1,3) = 0.5d0; H(1,4) = 0.2d0
  H(2,1) = 1.0d0; H(2,2) = 3.0d0; H(2,3) = 1.0d0; H(2,4) = 0.3d0
                   H(3,2) = 0.5d0; H(3,3) = 1.0d0; H(3,4) = 0.5d0
                                    H(4,3) = 0.3d0; H(4,4) = 4.0d0

  TT(1,1) = -1.0d0; TT(1,2) = 0.5d0; TT(1,3) = 0.2d0; TT(1,4) = 0.1d0
                     TT(2,2) = 2.0d0; TT(2,3) = 0.3d0; TT(2,4) = 0.15d0
                                       TT(3,3) = -1.5d0; TT(3,4) = 0.4d0
                                                          TT(4,4) = 1.0d0

  call DHGEQZ('E', 'N', 'N', N, ILO, IHI, H, MAXN, TT, MAXN, &
              ALPHAR, ALPHAI, BETA, Q, MAXN, Z, MAXN, WORK, LWORK, INFO)

  call begin_test('eig only neg T diagonal 4x4')
  call print_array('ALPHAR', ALPHAR, N)
  call print_array('ALPHAI', ALPHAI, N)
  call print_array('BETA', BETA, N)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 20: 3x3 eigenvalues-only with complex pair
  ! Tests eigenvalues-only through complex 2x2 block
  ! ============================================================
  N = 3; ILO = 1; IHI = 3
  H = 0.0d0; TT = 0.0d0; Q = 0.0d0; Z = 0.0d0
  ALPHAR = 0.0d0; ALPHAI = 0.0d0; BETA = 0.0d0

  H(1,1) = 1.0d0; H(1,2) = 0.5d0; H(1,3) = 0.3d0
  H(2,1) = 4.0d0; H(2,2) = 1.0d0; H(2,3) = 0.5d0
                   H(3,2) = 3.0d0; H(3,3) = 1.0d0

  TT(1,1) = 1.0d0; TT(1,2) = 0.1d0; TT(1,3) = 0.05d0
                    TT(2,2) = 1.0d0; TT(2,3) = 0.1d0
                                      TT(3,3) = 1.0d0

  call DHGEQZ('E', 'N', 'N', N, ILO, IHI, H, MAXN, TT, MAXN, &
              ALPHAR, ALPHAI, BETA, Q, MAXN, Z, MAXN, WORK, LWORK, INFO)

  call begin_test('eig only complex 3x3')
  call print_array('ALPHAR', ALPHAR, N)
  call print_array('ALPHAI', ALPHAI, N)
  call print_array('BETA', BETA, N)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 21: Schur with negative T diag below and above active block
  ! Tests handleLowEigenvalues with ilschr=true and neg T
  ! ============================================================
  N = 4; ILO = 2; IHI = 3
  H = 0.0d0; TT = 0.0d0; Q = 0.0d0; Z = 0.0d0
  ALPHAR = 0.0d0; ALPHAI = 0.0d0; BETA = 0.0d0

  H(1,1) = 5.0d0; H(1,2) = 1.0d0; H(1,3) = 0.5d0; H(1,4) = 0.2d0
                   H(2,2) = 3.0d0; H(2,3) = 2.0d0; H(2,4) = 0.5d0
                                    H(3,3) = 1.0d0; H(3,4) = 0.3d0
                                                     H(4,4) = 7.0d0
  H(3,2) = 1.5d0

  TT(1,1) = -2.0d0; TT(1,2) = 0.3d0; TT(1,3) = 0.1d0; TT(1,4) = 0.05d0
                     TT(2,2) = 1.0d0; TT(2,3) = 0.4d0; TT(2,4) = 0.15d0
                                       TT(3,3) = 3.0d0; TT(3,4) = 0.2d0
                                                         TT(4,4) = -1.0d0

  call DHGEQZ('S', 'I', 'I', N, ILO, IHI, H, MAXN, TT, MAXN, &
              ALPHAR, ALPHAI, BETA, Q, MAXN, Z, MAXN, WORK, LWORK, INFO)

  call begin_test('schur neg T below and above')
  call print_array('ALPHAR', ALPHAR, N)
  call print_array('ALPHAI', ALPHAI, N)
  call print_array('BETA', BETA, N)
  call print_matrix('H', H, MAXN, N, N)
  call print_matrix('TT', TT, MAXN, N, N)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 22: 8x8 complex eigenvalues (larger double-shift case)
  ! Tests deeper into doDoubleShiftQZStep loop
  ! ============================================================
  N = 8; ILO = 1; IHI = 8
  H = 0.0d0; TT = 0.0d0; Q = 0.0d0; Z = 0.0d0
  ALPHAR = 0.0d0; ALPHAI = 0.0d0; BETA = 0.0d0

  H(1,1) = 1.0d0; H(1,2) = 2.0d0; H(1,3) = 0.3d0; H(1,4) = 0.1d0
  H(1,5) = 0.05d0; H(1,6) = 0.02d0; H(1,7) = 0.01d0; H(1,8) = 0.005d0
  H(2,1) = 3.0d0; H(2,2) = 1.0d0; H(2,3) = 1.0d0; H(2,4) = 0.2d0
  H(2,5) = 0.1d0;  H(2,6) = 0.05d0; H(2,7) = 0.02d0; H(2,8) = 0.01d0
  H(3,2) = 4.0d0;  H(3,3) = 1.0d0; H(3,4) = 0.8d0
  H(3,5) = 0.2d0;  H(3,6) = 0.1d0;  H(3,7) = 0.05d0; H(3,8) = 0.02d0
  H(4,3) = 3.5d0;  H(4,4) = 1.0d0; H(4,5) = 0.9d0
  H(4,6) = 0.3d0;  H(4,7) = 0.1d0;  H(4,8) = 0.05d0
  H(5,4) = 3.0d0;  H(5,5) = 1.0d0; H(5,6) = 0.7d0
  H(5,7) = 0.2d0;  H(5,8) = 0.1d0
  H(6,5) = 2.5d0;  H(6,6) = 1.0d0; H(6,7) = 0.6d0; H(6,8) = 0.2d0
  H(7,6) = 2.0d0;  H(7,7) = 1.0d0; H(7,8) = 0.5d0
  H(8,7) = 1.5d0;  H(8,8) = 1.0d0

  do I = 1, N
    TT(I, I) = 1.0d0
  end do
  TT(1,2) = 0.1d0; TT(1,3) = 0.05d0
  TT(2,3) = 0.1d0; TT(2,4) = 0.05d0
  TT(3,4) = 0.1d0; TT(3,5) = 0.05d0
  TT(4,5) = 0.1d0; TT(4,6) = 0.05d0
  TT(5,6) = 0.1d0; TT(5,7) = 0.05d0
  TT(6,7) = 0.1d0; TT(6,8) = 0.05d0
  TT(7,8) = 0.1d0

  call DHGEQZ('S', 'I', 'I', N, ILO, IHI, H, MAXN, TT, MAXN, &
              ALPHAR, ALPHAI, BETA, Q, MAXN, Z, MAXN, WORK, LWORK, INFO)

  call begin_test('schur 8x8 complex double shift')
  call print_array('ALPHAR', ALPHAR, N)
  call print_array('ALPHAI', ALPHAI, N)
  call print_array('BETA', BETA, N)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 23: 3x3 with near-zero T diagonal (triggers doLabel70 path)
  ! T(3,3) is negligible, triggering the "bumping" path
  ! ============================================================
  N = 3; ILO = 1; IHI = 3
  H = 0.0d0; TT = 0.0d0; Q = 0.0d0; Z = 0.0d0
  ALPHAR = 0.0d0; ALPHAI = 0.0d0; BETA = 0.0d0

  H(1,1) = 2.0d0; H(1,2) = 1.0d0; H(1,3) = 0.5d0
  H(2,1) = 1.0d0; H(2,2) = 3.0d0; H(2,3) = 0.8d0
                   H(3,2) = 0.5d0; H(3,3) = 1.5d0

  TT(1,1) = 1.0d0; TT(1,2) = 0.2d0; TT(1,3) = 0.1d0
                    TT(2,2) = 1.0d0; TT(2,3) = 0.3d0
                                      TT(3,3) = 1.0d-20

  call DHGEQZ('S', 'I', 'I', N, ILO, IHI, H, MAXN, TT, MAXN, &
              ALPHAR, ALPHAI, BETA, Q, MAXN, Z, MAXN, WORK, LWORK, INFO)

  call begin_test('near zero T diag 3x3')
  call print_array('ALPHAR', ALPHAR, N)
  call print_array('ALPHAI', ALPHAI, N)
  call print_array('BETA', BETA, N)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 24: eigenvalues-only with near-zero T diagonal
  ! Tests doLabel70/doLabel80 in eigenvalues-only mode
  ! ============================================================
  N = 3; ILO = 1; IHI = 3
  H = 0.0d0; TT = 0.0d0; Q = 0.0d0; Z = 0.0d0
  ALPHAR = 0.0d0; ALPHAI = 0.0d0; BETA = 0.0d0

  H(1,1) = 2.0d0; H(1,2) = 1.0d0; H(1,3) = 0.5d0
  H(2,1) = 1.0d0; H(2,2) = 3.0d0; H(2,3) = 0.8d0
                   H(3,2) = 0.5d0; H(3,3) = 1.5d0

  TT(1,1) = 1.0d0; TT(1,2) = 0.2d0; TT(1,3) = 0.1d0
                    TT(2,2) = 1.0d0; TT(2,3) = 0.3d0
                                      TT(3,3) = 1.0d-20

  call DHGEQZ('E', 'N', 'N', N, ILO, IHI, H, MAXN, TT, MAXN, &
              ALPHAR, ALPHAI, BETA, Q, MAXN, Z, MAXN, WORK, LWORK, INFO)

  call begin_test('eig only near zero T diag 3x3')
  call print_array('ALPHAR', ALPHAR, N)
  call print_array('ALPHAI', ALPHAI, N)
  call print_array('BETA', BETA, N)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 25: 4x4 with zero T diagonal in middle (triggers ilazro chase)
  ! T(2,2)=0 triggers the "chase zero T diagonal" path (DO 40 loop)
  ! ============================================================
  N = 4; ILO = 1; IHI = 4
  H = 0.0d0; TT = 0.0d0; Q = 0.0d0; Z = 0.0d0
  ALPHAR = 0.0d0; ALPHAI = 0.0d0; BETA = 0.0d0

  H(1,1) = 2.0d0; H(1,2) = 1.0d0; H(1,3) = 0.5d0; H(1,4) = 0.2d0
  H(2,1) = 1.5d0; H(2,2) = 3.0d0; H(2,3) = 1.0d0; H(2,4) = 0.3d0
                   H(3,2) = 0.5d0; H(3,3) = 4.0d0; H(3,4) = 0.5d0
                                    H(4,3) = 0.3d0; H(4,4) = 1.0d0

  TT(1,1) = 1.0d0; TT(1,2) = 0.2d0; TT(1,3) = 0.1d0; TT(1,4) = 0.05d0
                    TT(2,2) = 1.0d-20; TT(2,3) = 0.3d0; TT(2,4) = 0.1d0
                                        TT(3,3) = 2.0d0;  TT(3,4) = 0.2d0
                                                           TT(4,4) = 1.5d0

  call DHGEQZ('S', 'I', 'I', N, ILO, IHI, H, MAXN, TT, MAXN, &
              ALPHAR, ALPHAI, BETA, Q, MAXN, Z, MAXN, WORK, LWORK, INFO)

  call begin_test('zero T diag middle 4x4')
  call print_array('ALPHAR', ALPHAR, N)
  call print_array('ALPHAI', ALPHAI, N)
  call print_array('BETA', BETA, N)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 26: eigenvalues-only zero T diag middle 4x4
  ! ============================================================
  N = 4; ILO = 1; IHI = 4
  H = 0.0d0; TT = 0.0d0; Q = 0.0d0; Z = 0.0d0
  ALPHAR = 0.0d0; ALPHAI = 0.0d0; BETA = 0.0d0

  H(1,1) = 2.0d0; H(1,2) = 1.0d0; H(1,3) = 0.5d0; H(1,4) = 0.2d0
  H(2,1) = 1.5d0; H(2,2) = 3.0d0; H(2,3) = 1.0d0; H(2,4) = 0.3d0
                   H(3,2) = 0.5d0; H(3,3) = 4.0d0; H(3,4) = 0.5d0
                                    H(4,3) = 0.3d0; H(4,4) = 1.0d0

  TT(1,1) = 1.0d0; TT(1,2) = 0.2d0; TT(1,3) = 0.1d0; TT(1,4) = 0.05d0
                    TT(2,2) = 1.0d-20; TT(2,3) = 0.3d0; TT(2,4) = 0.1d0
                                        TT(3,3) = 2.0d0;  TT(3,4) = 0.2d0
                                                           TT(4,4) = 1.5d0

  call DHGEQZ('E', 'N', 'N', N, ILO, IHI, H, MAXN, TT, MAXN, &
              ALPHAR, ALPHAI, BETA, Q, MAXN, Z, MAXN, WORK, LWORK, INFO)

  call begin_test('eig only zero T diag middle 4x4')
  call print_array('ALPHAR', ALPHAR, N)
  call print_array('ALPHAI', ALPHAI, N)
  call print_array('BETA', BETA, N)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 27: 4x4 with two zero T diagonals (tests DO 50 chase from bottom)
  ! T(2,2) and T(3,3) near zero
  ! ============================================================
  N = 4; ILO = 1; IHI = 4
  H = 0.0d0; TT = 0.0d0; Q = 0.0d0; Z = 0.0d0
  ALPHAR = 0.0d0; ALPHAI = 0.0d0; BETA = 0.0d0

  H(1,1) = 2.0d0; H(1,2) = 1.0d0; H(1,3) = 0.5d0; H(1,4) = 0.2d0
  H(2,1) = 1.5d0; H(2,2) = 3.0d0; H(2,3) = 1.0d0; H(2,4) = 0.3d0
                   H(3,2) = 0.5d0; H(3,3) = 4.0d0; H(3,4) = 0.8d0
                                    H(4,3) = 0.3d0; H(4,4) = 1.0d0

  TT(1,1) = 1.0d0; TT(1,2) = 0.2d0; TT(1,3) = 0.1d0; TT(1,4) = 0.05d0
                    TT(2,2) = 1.0d-20; TT(2,3) = 0.0d0; TT(2,4) = 0.0d0
                                        TT(3,3) = 1.0d-20; TT(3,4) = 0.0d0
                                                            TT(4,4) = 1.5d0

  call DHGEQZ('S', 'I', 'I', N, ILO, IHI, H, MAXN, TT, MAXN, &
              ALPHAR, ALPHAI, BETA, Q, MAXN, Z, MAXN, WORK, LWORK, INFO)

  call begin_test('two zero T diag 4x4')
  call print_array('ALPHAR', ALPHAR, N)
  call print_array('ALPHAI', ALPHAI, N)
  call print_array('BETA', BETA, N)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 28: 4x4 eigenvalues-only complex pair (2x2 block)
  ! This triggers doComplexShift→do2x2Block in eigenvalues-only mode
  ! ============================================================
  N = 4; ILO = 1; IHI = 4
  H = 0.0d0; TT = 0.0d0; Q = 0.0d0; Z = 0.0d0
  ALPHAR = 0.0d0; ALPHAI = 0.0d0; BETA = 0.0d0

  H(1,1) = 1.0d0; H(1,2) = 0.5d0; H(1,3) = 0.3d0; H(1,4) = 0.1d0
  H(2,1) = 4.0d0; H(2,2) = 1.0d0; H(2,3) = 0.5d0; H(2,4) = 0.2d0
                   H(3,2) = 3.0d0; H(3,3) = 1.0d0; H(3,4) = 0.4d0
                                    H(4,3) = 2.5d0; H(4,4) = 1.0d0

  TT(1,1) = 1.0d0; TT(1,2) = 0.1d0; TT(1,3) = 0.05d0; TT(1,4) = 0.02d0
                    TT(2,2) = 1.0d0; TT(2,3) = 0.1d0;  TT(2,4) = 0.05d0
                                      TT(3,3) = 1.0d0;  TT(3,4) = 0.1d0
                                                         TT(4,4) = 1.0d0

  call DHGEQZ('E', 'N', 'N', N, ILO, IHI, H, MAXN, TT, MAXN, &
              ALPHAR, ALPHAI, BETA, Q, MAXN, Z, MAXN, WORK, LWORK, INFO)

  call begin_test('eig only 4x4 complex pairs')
  call print_array('ALPHAR', ALPHAR, N)
  call print_array('ALPHAI', ALPHAI, N)
  call print_array('BETA', BETA, N)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 29: 4x4 schur complex pair (triggers do2x2Block path)
  ! ============================================================
  N = 4; ILO = 1; IHI = 4
  H = 0.0d0; TT = 0.0d0; Q = 0.0d0; Z = 0.0d0
  ALPHAR = 0.0d0; ALPHAI = 0.0d0; BETA = 0.0d0

  H(1,1) = 1.0d0; H(1,2) = 0.5d0; H(1,3) = 0.3d0; H(1,4) = 0.1d0
  H(2,1) = 4.0d0; H(2,2) = 1.0d0; H(2,3) = 0.5d0; H(2,4) = 0.2d0
                   H(3,2) = 3.0d0; H(3,3) = 1.0d0; H(3,4) = 0.4d0
                                    H(4,3) = 2.5d0; H(4,4) = 1.0d0

  TT(1,1) = 1.0d0; TT(1,2) = 0.1d0; TT(1,3) = 0.05d0; TT(1,4) = 0.02d0
                    TT(2,2) = 1.0d0; TT(2,3) = 0.1d0;  TT(2,4) = 0.05d0
                                      TT(3,3) = 1.0d0;  TT(3,4) = 0.1d0
                                                         TT(4,4) = 1.0d0

  call DHGEQZ('S', 'I', 'I', N, ILO, IHI, H, MAXN, TT, MAXN, &
              ALPHAR, ALPHAI, BETA, Q, MAXN, Z, MAXN, WORK, LWORK, INFO)

  call begin_test('schur 4x4 complex pairs')
  call print_array('ALPHAR', ALPHAR, N)
  call print_array('ALPHAI', ALPHAI, N)
  call print_array('BETA', BETA, N)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 30: 2x2 with complex eigenvalues (triggers do2x2Block directly)
  ! The Hessenberg matrix has complex eigenvalues immediately
  ! ============================================================
  N = 2; ILO = 1; IHI = 2
  H = 0.0d0; TT = 0.0d0; Q = 0.0d0; Z = 0.0d0
  ALPHAR = 0.0d0; ALPHAI = 0.0d0; BETA = 0.0d0

  H(1,1) = 0.0d0; H(1,2) = -1.0d0
  H(2,1) = 1.0d0; H(2,2) = 0.0d0

  TT(1,1) = 1.0d0; TT(1,2) = 0.0d0
                    TT(2,2) = 1.0d0

  call DHGEQZ('S', 'I', 'I', N, ILO, IHI, H, MAXN, TT, MAXN, &
              ALPHAR, ALPHAI, BETA, Q, MAXN, Z, MAXN, WORK, LWORK, INFO)

  call begin_test('schur 2x2 complex eigs')
  call print_array('ALPHAR', ALPHAR, N)
  call print_array('ALPHAI', ALPHAI, N)
  call print_array('BETA', BETA, N)
  call print_matrix('H', H, MAXN, N, N)
  call print_matrix('TT', TT, MAXN, N, N)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 31: eigenvalues-only 2x2 complex eigenvalues
  ! ============================================================
  N = 2; ILO = 1; IHI = 2
  H = 0.0d0; TT = 0.0d0; Q = 0.0d0; Z = 0.0d0
  ALPHAR = 0.0d0; ALPHAI = 0.0d0; BETA = 0.0d0

  H(1,1) = 0.0d0; H(1,2) = -1.0d0
  H(2,1) = 1.0d0; H(2,2) = 0.0d0

  TT(1,1) = 1.0d0; TT(1,2) = 0.0d0
                    TT(2,2) = 1.0d0

  call DHGEQZ('E', 'N', 'N', N, ILO, IHI, H, MAXN, TT, MAXN, &
              ALPHAR, ALPHAI, BETA, Q, MAXN, Z, MAXN, WORK, LWORK, INFO)

  call begin_test('eig only 2x2 complex eigs')
  call print_array('ALPHAR', ALPHAR, N)
  call print_array('ALPHAI', ALPHAI, N)
  call print_array('BETA', BETA, N)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 32: 3x3 eigenvalues-only targeting doComplexShift→doDoubleShiftQZStep
  ! 3x3 block with complex eigenvalues (ifirst+1 < ilast → double shift)
  ! ============================================================
  N = 3; ILO = 1; IHI = 3
  H = 0.0d0; TT = 0.0d0; Q = 0.0d0; Z = 0.0d0
  ALPHAR = 0.0d0; ALPHAI = 0.0d0; BETA = 0.0d0

  ! Strong complex eigenvalue structure
  H(1,1) = 0.0d0; H(1,2) = -1.0d0; H(1,3) = 0.5d0
  H(2,1) = 1.0d0; H(2,2) = 0.0d0;  H(2,3) = 0.3d0
                   H(3,2) = 1.0d0;  H(3,3) = 2.0d0

  TT(1,1) = 1.0d0; TT(1,2) = 0.0d0; TT(1,3) = 0.0d0
                    TT(2,2) = 1.0d0; TT(2,3) = 0.0d0
                                      TT(3,3) = 1.0d0

  call DHGEQZ('E', 'N', 'N', N, ILO, IHI, H, MAXN, TT, MAXN, &
              ALPHAR, ALPHAI, BETA, Q, MAXN, Z, MAXN, WORK, LWORK, INFO)

  call begin_test('eig only 3x3 double shift')
  call print_array('ALPHAR', ALPHAR, N)
  call print_array('ALPHAI', ALPHAI, N)
  call print_array('BETA', BETA, N)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 33: 3x3 schur targeting doComplexShift→doDoubleShiftQZStep
  ! ============================================================
  N = 3; ILO = 1; IHI = 3
  H = 0.0d0; TT = 0.0d0; Q = 0.0d0; Z = 0.0d0
  ALPHAR = 0.0d0; ALPHAI = 0.0d0; BETA = 0.0d0

  H(1,1) = 0.0d0; H(1,2) = -1.0d0; H(1,3) = 0.5d0
  H(2,1) = 1.0d0; H(2,2) = 0.0d0;  H(2,3) = 0.3d0
                   H(3,2) = 1.0d0;  H(3,3) = 2.0d0

  TT(1,1) = 1.0d0; TT(1,2) = 0.0d0; TT(1,3) = 0.0d0
                    TT(2,2) = 1.0d0; TT(2,3) = 0.0d0
                                      TT(3,3) = 1.0d0

  call DHGEQZ('S', 'I', 'I', N, ILO, IHI, H, MAXN, TT, MAXN, &
              ALPHAR, ALPHAI, BETA, Q, MAXN, Z, MAXN, WORK, LWORK, INFO)

  call begin_test('schur 3x3 double shift')
  call print_array('ALPHAR', ALPHAR, N)
  call print_array('ALPHAI', ALPHAI, N)
  call print_array('BETA', BETA, N)
  call print_matrix('H', H, MAXN, N, N)
  call print_matrix('TT', TT, MAXN, N, N)
  call print_int('info', INFO)
  call end_test()

end program test_dhgeqz
