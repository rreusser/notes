program test_zhgeqz
  use test_utils
  implicit none

  integer, parameter :: NMAX = 5
  complex*16 :: H(NMAX, NMAX), T(NMAX, NMAX)
  complex*16 :: Q(NMAX, NMAX), Z(NMAX, NMAX)
  complex*16 :: ALPHA(NMAX), BETA(NMAX)
  complex*16 :: WORK(NMAX)
  double precision :: RWORK(NMAX)
  double precision :: H_r(2*NMAX*NMAX), T_r(2*NMAX*NMAX)
  double precision :: Q_r(2*NMAX*NMAX), Z_r(2*NMAX*NMAX)
  double precision :: ALPHA_r(2*NMAX), BETA_r(2*NMAX)
  equivalence (H, H_r)
  equivalence (T, T_r)
  equivalence (Q, Q_r)
  equivalence (Z, Z_r)
  equivalence (ALPHA, ALPHA_r)
  equivalence (BETA, BETA_r)
  integer :: info, n, ilo, ihi, ldh, ldt, ldq, ldz, lwork
  integer :: i, j

  ldh = NMAX
  ldt = NMAX
  ldq = NMAX
  ldz = NMAX

  ! ==================================================================
  ! Test 1: N=0 quick return
  ! ==================================================================
  n = 0
  ilo = 1
  ihi = 0
  lwork = 1
  WORK = (0.0d0, 0.0d0)

  call ZHGEQZ('E', 'N', 'N', n, ilo, ihi, H, ldh, T, ldt, &
               ALPHA, BETA, Q, ldq, Z, ldz, WORK, lwork, RWORK, info)

  call begin_test('n_eq_0')
  call print_int('info', info)
  call end_test()

  ! ==================================================================
  ! Test 2: N=1, trivial case
  ! ==================================================================
  n = 1
  ilo = 1
  ihi = 1
  lwork = 1

  H = (0.0d0, 0.0d0)
  T = (0.0d0, 0.0d0)
  Q = (0.0d0, 0.0d0)
  Z = (0.0d0, 0.0d0)
  ALPHA = (0.0d0, 0.0d0)
  BETA = (0.0d0, 0.0d0)

  H(1,1) = (3.0d0, 1.0d0)
  T(1,1) = (2.0d0, 0.5d0)

  call ZHGEQZ('S', 'I', 'I', n, ilo, ihi, H, ldh, T, ldt, &
               ALPHA, BETA, Q, ldq, Z, ldz, WORK, lwork, RWORK, info)

  call begin_test('n_eq_1')
  call print_int('info', info)
  call print_array('H', H_r(1), 2)
  call print_array('T', T_r(1), 2)
  call print_array('alpha', ALPHA_r(1), 2)
  call print_array('beta', BETA_r(1), 2)
  call print_array('Q', Q_r(1), 2)
  call print_array('Z', Z_r(1), 2)
  call end_test()

  ! ==================================================================
  ! Test 3: 3x3 eigenvalues only (JOB='E'), already Hessenberg-triangular
  ! H is upper Hessenberg, T is upper triangular
  ! ==================================================================
  n = 3
  ilo = 1
  ihi = 3
  lwork = 3

  H = (0.0d0, 0.0d0)
  T = (0.0d0, 0.0d0)
  Q = (0.0d0, 0.0d0)
  Z = (0.0d0, 0.0d0)
  ALPHA = (0.0d0, 0.0d0)
  BETA = (0.0d0, 0.0d0)

  ! Upper Hessenberg H
  H(1,1) = (2.0d0, 1.0d0)
  H(1,2) = (1.0d0, 0.5d0)
  H(1,3) = (0.5d0, -0.5d0)
  H(2,1) = (1.0d0, -1.0d0)
  H(2,2) = (3.0d0, 0.0d0)
  H(2,3) = (1.0d0, 1.0d0)
  ! H(3,1) = 0 (Hessenberg)
  H(3,2) = (0.5d0, 0.5d0)
  H(3,3) = (4.0d0, -1.0d0)

  ! Upper triangular T
  T(1,1) = (3.0d0, 0.0d0)
  T(1,2) = (1.0d0, 0.5d0)
  T(1,3) = (0.5d0, 0.5d0)
  T(2,2) = (2.0d0, 1.0d0)
  T(2,3) = (1.0d0, 0.0d0)
  T(3,3) = (1.0d0, 0.5d0)

  call ZHGEQZ('E', 'N', 'N', n, ilo, ihi, H, ldh, T, ldt, &
               ALPHA, BETA, Q, ldq, Z, ldz, WORK, lwork, RWORK, info)

  call begin_test('eig_only_3x3')
  call print_int('info', info)
  call print_array('alpha', ALPHA_r(1), 2*n)
  call print_array('beta', BETA_r(1), 2*n)
  call end_test()

  ! ==================================================================
  ! Test 4: 3x3 Schur form (JOB='S'), COMPQ='I', COMPZ='I'
  ! Same matrix pair as test 3 (re-initialize)
  ! ==================================================================
  n = 3
  ilo = 1
  ihi = 3
  lwork = 3

  H = (0.0d0, 0.0d0)
  T = (0.0d0, 0.0d0)
  Q = (0.0d0, 0.0d0)
  Z = (0.0d0, 0.0d0)
  ALPHA = (0.0d0, 0.0d0)
  BETA = (0.0d0, 0.0d0)

  H(1,1) = (2.0d0, 1.0d0)
  H(1,2) = (1.0d0, 0.5d0)
  H(1,3) = (0.5d0, -0.5d0)
  H(2,1) = (1.0d0, -1.0d0)
  H(2,2) = (3.0d0, 0.0d0)
  H(2,3) = (1.0d0, 1.0d0)
  H(3,2) = (0.5d0, 0.5d0)
  H(3,3) = (4.0d0, -1.0d0)

  T(1,1) = (3.0d0, 0.0d0)
  T(1,2) = (1.0d0, 0.5d0)
  T(1,3) = (0.5d0, 0.5d0)
  T(2,2) = (2.0d0, 1.0d0)
  T(2,3) = (1.0d0, 0.0d0)
  T(3,3) = (1.0d0, 0.5d0)

  call ZHGEQZ('S', 'I', 'I', n, ilo, ihi, H, ldh, T, ldt, &
               ALPHA, BETA, Q, ldq, Z, ldz, WORK, lwork, RWORK, info)

  call begin_test('schur_3x3')
  call print_int('info', info)
  call print_array('alpha', ALPHA_r(1), 2*n)
  call print_array('beta', BETA_r(1), 2*n)
  ! Print H (Schur form S)
  call print_array('H_col1', H_r(1), 2*n)
  call print_array('H_col2', H_r(2*ldh+1), 2*n)
  call print_array('H_col3', H_r(4*ldh+1), 2*n)
  ! Print T (Schur form P)
  call print_array('T_col1', T_r(1), 2*n)
  call print_array('T_col2', T_r(2*ldt+1), 2*n)
  call print_array('T_col3', T_r(4*ldt+1), 2*n)
  ! Print Q
  call print_array('Q_col1', Q_r(1), 2*n)
  call print_array('Q_col2', Q_r(2*ldq+1), 2*n)
  call print_array('Q_col3', Q_r(4*ldq+1), 2*n)
  ! Print Z
  call print_array('Z_col1', Z_r(1), 2*n)
  call print_array('Z_col2', Z_r(2*ldz+1), 2*n)
  call print_array('Z_col3', Z_r(4*ldz+1), 2*n)
  call end_test()

  ! ==================================================================
  ! Test 5: 4x4 Schur form with COMPQ='I', COMPZ='I'
  ! ==================================================================
  n = 4
  ilo = 1
  ihi = 4
  lwork = 4

  H = (0.0d0, 0.0d0)
  T = (0.0d0, 0.0d0)
  Q = (0.0d0, 0.0d0)
  Z = (0.0d0, 0.0d0)
  ALPHA = (0.0d0, 0.0d0)
  BETA = (0.0d0, 0.0d0)

  ! Upper Hessenberg H
  H(1,1) = (1.0d0, 0.5d0)
  H(1,2) = (2.0d0, -1.0d0)
  H(1,3) = (0.5d0, 0.5d0)
  H(1,4) = (1.0d0, 0.0d0)
  H(2,1) = (0.5d0, 0.3d0)
  H(2,2) = (3.0d0, 1.0d0)
  H(2,3) = (1.0d0, -0.5d0)
  H(2,4) = (0.5d0, 1.0d0)
  ! H(3,1) = 0
  H(3,2) = (0.8d0, -0.2d0)
  H(3,3) = (2.0d0, 0.0d0)
  H(3,4) = (1.5d0, 0.5d0)
  ! H(4,1) = 0, H(4,2) = 0
  H(4,3) = (0.3d0, 0.1d0)
  H(4,4) = (4.0d0, -0.5d0)

  ! Upper triangular T
  T(1,1) = (2.0d0, 0.0d0)
  T(1,2) = (0.5d0, 0.5d0)
  T(1,3) = (0.0d0, 1.0d0)
  T(1,4) = (0.5d0, 0.0d0)
  T(2,2) = (3.0d0, 1.0d0)
  T(2,3) = (1.0d0, 0.0d0)
  T(2,4) = (0.5d0, -0.5d0)
  T(3,3) = (1.0d0, 0.5d0)
  T(3,4) = (0.5d0, 0.5d0)
  T(4,4) = (2.0d0, -1.0d0)

  call ZHGEQZ('S', 'I', 'I', n, ilo, ihi, H, ldh, T, ldt, &
               ALPHA, BETA, Q, ldq, Z, ldz, WORK, lwork, RWORK, info)

  call begin_test('schur_4x4')
  call print_int('info', info)
  call print_array('alpha', ALPHA_r(1), 2*n)
  call print_array('beta', BETA_r(1), 2*n)
  call print_array('H_col1', H_r(1), 2*n)
  call print_array('H_col2', H_r(2*ldh+1), 2*n)
  call print_array('H_col3', H_r(4*ldh+1), 2*n)
  call print_array('H_col4', H_r(6*ldh+1), 2*n)
  call print_array('T_col1', T_r(1), 2*n)
  call print_array('T_col2', T_r(2*ldt+1), 2*n)
  call print_array('T_col3', T_r(4*ldt+1), 2*n)
  call print_array('T_col4', T_r(6*ldt+1), 2*n)
  call print_array('Q_col1', Q_r(1), 2*n)
  call print_array('Q_col2', Q_r(2*ldq+1), 2*n)
  call print_array('Q_col3', Q_r(4*ldq+1), 2*n)
  call print_array('Q_col4', Q_r(6*ldq+1), 2*n)
  call print_array('Z_col1', Z_r(1), 2*n)
  call print_array('Z_col2', Z_r(2*ldz+1), 2*n)
  call print_array('Z_col3', Z_r(4*ldz+1), 2*n)
  call print_array('Z_col4', Z_r(6*ldz+1), 2*n)
  call end_test()

  ! ==================================================================
  ! Test 6: IHI < ILO (skip QZ steps, only set eigenvalues)
  ! ==================================================================
  n = 3
  ilo = 2
  ihi = 1
  lwork = 3

  H = (0.0d0, 0.0d0)
  T = (0.0d0, 0.0d0)
  Q = (0.0d0, 0.0d0)
  Z = (0.0d0, 0.0d0)
  ALPHA = (0.0d0, 0.0d0)
  BETA = (0.0d0, 0.0d0)

  ! Diagonal H and T
  H(1,1) = (5.0d0, 1.0d0)
  H(2,2) = (3.0d0, -1.0d0)
  H(3,3) = (1.0d0, 2.0d0)
  T(1,1) = (2.0d0, 0.0d0)
  T(2,2) = (1.0d0, 0.5d0)
  T(3,3) = (3.0d0, -0.5d0)

  call ZHGEQZ('S', 'I', 'I', n, ilo, ihi, H, ldh, T, ldt, &
               ALPHA, BETA, Q, ldq, Z, ldz, WORK, lwork, RWORK, info)

  call begin_test('ihi_lt_ilo')
  call print_int('info', info)
  call print_array('alpha', ALPHA_r(1), 2*n)
  call print_array('beta', BETA_r(1), 2*n)
  call end_test()

  ! ==================================================================
  ! Test 7: Partial range ILO=2, IHI=3 on 4x4
  ! Already upper triangular outside [ILO,IHI]
  ! ==================================================================
  n = 4
  ilo = 2
  ihi = 3
  lwork = 4

  H = (0.0d0, 0.0d0)
  T = (0.0d0, 0.0d0)
  Q = (0.0d0, 0.0d0)
  Z = (0.0d0, 0.0d0)
  ALPHA = (0.0d0, 0.0d0)
  BETA = (0.0d0, 0.0d0)

  ! H is upper Hessenberg with subdiag only in rows ILO..IHI
  H(1,1) = (1.0d0, 0.0d0)
  H(1,2) = (0.5d0, 0.5d0)
  H(1,3) = (0.0d0, 1.0d0)
  H(1,4) = (0.5d0, 0.0d0)
  H(2,2) = (2.0d0, 1.0d0)
  H(2,3) = (1.0d0, -0.5d0)
  H(2,4) = (0.5d0, 0.5d0)
  H(3,2) = (0.8d0, 0.3d0)
  H(3,3) = (3.0d0, 0.0d0)
  H(3,4) = (1.0d0, 1.0d0)
  H(4,4) = (4.0d0, -1.0d0)

  T(1,1) = (2.0d0, 0.0d0)
  T(1,2) = (0.5d0, 0.0d0)
  T(1,3) = (0.0d0, 0.5d0)
  T(1,4) = (1.0d0, 0.0d0)
  T(2,2) = (1.0d0, 0.5d0)
  T(2,3) = (0.5d0, 0.0d0)
  T(2,4) = (0.0d0, 0.5d0)
  T(3,3) = (3.0d0, 0.0d0)
  T(3,4) = (1.0d0, -0.5d0)
  T(4,4) = (1.0d0, 0.0d0)

  call ZHGEQZ('S', 'I', 'I', n, ilo, ihi, H, ldh, T, ldt, &
               ALPHA, BETA, Q, ldq, Z, ldz, WORK, lwork, RWORK, info)

  call begin_test('partial_4x4')
  call print_int('info', info)
  call print_array('alpha', ALPHA_r(1), 2*n)
  call print_array('beta', BETA_r(1), 2*n)
  call print_array('H_col1', H_r(1), 2*n)
  call print_array('H_col2', H_r(2*ldh+1), 2*n)
  call print_array('H_col3', H_r(4*ldh+1), 2*n)
  call print_array('H_col4', H_r(6*ldh+1), 2*n)
  call print_array('T_col1', T_r(1), 2*n)
  call print_array('T_col2', T_r(2*ldt+1), 2*n)
  call print_array('T_col3', T_r(4*ldt+1), 2*n)
  call print_array('T_col4', T_r(6*ldt+1), 2*n)
  call print_array('Q_col1', Q_r(1), 2*n)
  call print_array('Q_col2', Q_r(2*ldq+1), 2*n)
  call print_array('Q_col3', Q_r(4*ldq+1), 2*n)
  call print_array('Q_col4', Q_r(6*ldq+1), 2*n)
  call print_array('Z_col1', Z_r(1), 2*n)
  call print_array('Z_col2', Z_r(2*ldz+1), 2*n)
  call print_array('Z_col3', Z_r(4*ldz+1), 2*n)
  call print_array('Z_col4', Z_r(6*ldz+1), 2*n)
  call end_test()

  ! ==================================================================
  ! Test 8: JOB='E' with COMPQ='N', COMPZ='N' on 4x4
  ! Only eigenvalues, no Schur vectors
  ! ==================================================================
  n = 4
  ilo = 1
  ihi = 4
  lwork = 4

  H = (0.0d0, 0.0d0)
  T = (0.0d0, 0.0d0)
  Q = (0.0d0, 0.0d0)
  Z = (0.0d0, 0.0d0)
  ALPHA = (0.0d0, 0.0d0)
  BETA = (0.0d0, 0.0d0)

  H(1,1) = (1.0d0, 0.5d0)
  H(1,2) = (2.0d0, -1.0d0)
  H(1,3) = (0.5d0, 0.5d0)
  H(1,4) = (1.0d0, 0.0d0)
  H(2,1) = (0.5d0, 0.3d0)
  H(2,2) = (3.0d0, 1.0d0)
  H(2,3) = (1.0d0, -0.5d0)
  H(2,4) = (0.5d0, 1.0d0)
  H(3,2) = (0.8d0, -0.2d0)
  H(3,3) = (2.0d0, 0.0d0)
  H(3,4) = (1.5d0, 0.5d0)
  H(4,3) = (0.3d0, 0.1d0)
  H(4,4) = (4.0d0, -0.5d0)

  T(1,1) = (2.0d0, 0.0d0)
  T(1,2) = (0.5d0, 0.5d0)
  T(1,3) = (0.0d0, 1.0d0)
  T(1,4) = (0.5d0, 0.0d0)
  T(2,2) = (3.0d0, 1.0d0)
  T(2,3) = (1.0d0, 0.0d0)
  T(2,4) = (0.5d0, -0.5d0)
  T(3,3) = (1.0d0, 0.5d0)
  T(3,4) = (0.5d0, 0.5d0)
  T(4,4) = (2.0d0, -1.0d0)

  call ZHGEQZ('E', 'N', 'N', n, ilo, ihi, H, ldh, T, ldt, &
               ALPHA, BETA, Q, ldq, Z, ldz, WORK, lwork, RWORK, info)

  call begin_test('eig_only_4x4')
  call print_int('info', info)
  call print_array('alpha', ALPHA_r(1), 2*n)
  call print_array('beta', BETA_r(1), 2*n)
  call end_test()

  ! ==================================================================
  ! Test 9: 2x2 - minimal QZ iteration case
  ! ==================================================================
  n = 2
  ilo = 1
  ihi = 2
  lwork = 2

  H = (0.0d0, 0.0d0)
  T = (0.0d0, 0.0d0)
  Q = (0.0d0, 0.0d0)
  Z = (0.0d0, 0.0d0)
  ALPHA = (0.0d0, 0.0d0)
  BETA = (0.0d0, 0.0d0)

  H(1,1) = (1.0d0, 2.0d0)
  H(1,2) = (3.0d0, -1.0d0)
  H(2,1) = (0.5d0, 0.5d0)
  H(2,2) = (4.0d0, 1.0d0)

  T(1,1) = (2.0d0, 0.0d0)
  T(1,2) = (1.0d0, 1.0d0)
  T(2,2) = (3.0d0, -0.5d0)

  call ZHGEQZ('S', 'I', 'I', n, ilo, ihi, H, ldh, T, ldt, &
               ALPHA, BETA, Q, ldq, Z, ldz, WORK, lwork, RWORK, info)

  call begin_test('schur_2x2')
  call print_int('info', info)
  call print_array('alpha', ALPHA_r(1), 2*n)
  call print_array('beta', BETA_r(1), 2*n)
  call print_array('H_col1', H_r(1), 2*n)
  call print_array('H_col2', H_r(2*ldh+1), 2*n)
  call print_array('T_col1', T_r(1), 2*n)
  call print_array('T_col2', T_r(2*ldt+1), 2*n)
  call print_array('Q_col1', Q_r(1), 2*n)
  call print_array('Q_col2', Q_r(2*ldq+1), 2*n)
  call print_array('Z_col1', Z_r(1), 2*n)
  call print_array('Z_col2', Z_r(2*ldz+1), 2*n)
  call end_test()

  ! ==================================================================
  ! Test 10: 3x3 with T(2,2)=0 — triggers zero-diagonal T path
  ! ==================================================================
  n = 3
  ilo = 1
  ihi = 3
  lwork = 3

  H = (0.0d0, 0.0d0)
  T = (0.0d0, 0.0d0)
  Q = (0.0d0, 0.0d0)
  Z = (0.0d0, 0.0d0)
  ALPHA = (0.0d0, 0.0d0)
  BETA = (0.0d0, 0.0d0)

  H(1,1) = (2.0d0, 1.0d0)
  H(1,2) = (1.0d0, 0.5d0)
  H(1,3) = (0.5d0, -0.5d0)
  H(2,1) = (1.0d0, -1.0d0)
  H(2,2) = (3.0d0, 0.0d0)
  H(2,3) = (1.0d0, 1.0d0)
  H(3,2) = (0.5d0, 0.5d0)
  H(3,3) = (4.0d0, -1.0d0)

  T(1,1) = (3.0d0, 0.0d0)
  T(1,2) = (1.0d0, 0.5d0)
  T(1,3) = (0.5d0, 0.5d0)
  T(2,2) = (0.0d0, 0.0d0)
  T(2,3) = (1.0d0, 0.0d0)
  T(3,3) = (1.0d0, 0.5d0)

  call ZHGEQZ('S', 'I', 'I', n, ilo, ihi, H, ldh, T, ldt, &
               ALPHA, BETA, Q, ldq, Z, ldz, WORK, lwork, RWORK, info)

  call begin_test('zero_t_diag_3x3')
  call print_int('info', info)
  call print_array('alpha', ALPHA_r(1), 2*n)
  call print_array('beta', BETA_r(1), 2*n)
  call print_array('H_col1', H_r(1), 2*n)
  call print_array('H_col2', H_r(2*ldh+1), 2*n)
  call print_array('H_col3', H_r(4*ldh+1), 2*n)
  call print_array('T_col1', T_r(1), 2*n)
  call print_array('T_col2', T_r(2*ldt+1), 2*n)
  call print_array('T_col3', T_r(4*ldt+1), 2*n)
  call end_test()

  ! ==================================================================
  ! Test 11: 3x3 with T(3,3)=0 — triggers T(ILAST,ILAST)=0 path
  ! ==================================================================
  n = 3
  ilo = 1
  ihi = 3
  lwork = 3

  H = (0.0d0, 0.0d0)
  T = (0.0d0, 0.0d0)
  Q = (0.0d0, 0.0d0)
  Z = (0.0d0, 0.0d0)
  ALPHA = (0.0d0, 0.0d0)
  BETA = (0.0d0, 0.0d0)

  H(1,1) = (2.0d0, 1.0d0)
  H(1,2) = (1.0d0, 0.5d0)
  H(1,3) = (0.5d0, -0.5d0)
  H(2,1) = (1.0d0, -1.0d0)
  H(2,2) = (3.0d0, 0.0d0)
  H(2,3) = (1.0d0, 1.0d0)
  H(3,2) = (0.5d0, 0.5d0)
  H(3,3) = (4.0d0, -1.0d0)

  T(1,1) = (3.0d0, 0.0d0)
  T(1,2) = (1.0d0, 0.5d0)
  T(1,3) = (0.5d0, 0.5d0)
  T(2,2) = (2.0d0, 1.0d0)
  T(2,3) = (1.0d0, 0.0d0)
  T(3,3) = (0.0d0, 0.0d0)

  call ZHGEQZ('S', 'I', 'I', n, ilo, ihi, H, ldh, T, ldt, &
               ALPHA, BETA, Q, ldq, Z, ldz, WORK, lwork, RWORK, info)

  call begin_test('zero_t_last_3x3')
  call print_int('info', info)
  call print_array('alpha', ALPHA_r(1), 2*n)
  call print_array('beta', BETA_r(1), 2*n)
  call print_array('H_col1', H_r(1), 2*n)
  call print_array('H_col2', H_r(2*ldh+1), 2*n)
  call print_array('H_col3', H_r(4*ldh+1), 2*n)
  call print_array('T_col1', T_r(1), 2*n)
  call print_array('T_col2', T_r(2*ldt+1), 2*n)
  call print_array('T_col3', T_r(4*ldt+1), 2*n)
  call end_test()

  ! ==================================================================
  ! Test 12: Already diagonal pair — immediate deflation
  ! ==================================================================
  n = 3
  ilo = 1
  ihi = 3
  lwork = 3

  H = (0.0d0, 0.0d0)
  T = (0.0d0, 0.0d0)
  Q = (0.0d0, 0.0d0)
  Z = (0.0d0, 0.0d0)
  ALPHA = (0.0d0, 0.0d0)
  BETA = (0.0d0, 0.0d0)

  H(1,1) = (1.0d0, 2.0d0)
  H(1,2) = (0.5d0, 0.5d0)
  H(1,3) = (0.0d0, 1.0d0)
  H(2,2) = (3.0d0, -1.0d0)
  H(2,3) = (1.0d0, 0.0d0)
  H(3,3) = (2.0d0, 0.5d0)

  T(1,1) = (1.0d0, 0.0d0)
  T(1,2) = (0.5d0, 0.0d0)
  T(1,3) = (0.0d0, 0.5d0)
  T(2,2) = (2.0d0, 0.0d0)
  T(2,3) = (1.0d0, 0.0d0)
  T(3,3) = (3.0d0, 0.0d0)

  call ZHGEQZ('E', 'N', 'N', n, ilo, ihi, H, ldh, T, ldt, &
               ALPHA, BETA, Q, ldq, Z, ldz, WORK, lwork, RWORK, info)

  call begin_test('diagonal_3x3')
  call print_int('info', info)
  call print_array('alpha', ALPHA_r(1), 2*n)
  call print_array('beta', BETA_r(1), 2*n)
  call end_test()

  ! ==================================================================
  ! Test 13: COMPQ='V', COMPZ='V' — accumulate into existing Q,Z
  ! ==================================================================
  n = 3
  ilo = 1
  ihi = 3
  lwork = 3

  H = (0.0d0, 0.0d0)
  T = (0.0d0, 0.0d0)
  Q = (0.0d0, 0.0d0)
  Z = (0.0d0, 0.0d0)
  ALPHA = (0.0d0, 0.0d0)
  BETA = (0.0d0, 0.0d0)

  H(1,1) = (2.0d0, 1.0d0)
  H(1,2) = (1.0d0, 0.5d0)
  H(1,3) = (0.5d0, -0.5d0)
  H(2,1) = (1.0d0, -1.0d0)
  H(2,2) = (3.0d0, 0.0d0)
  H(2,3) = (1.0d0, 1.0d0)
  H(3,2) = (0.5d0, 0.5d0)
  H(3,3) = (4.0d0, -1.0d0)

  T(1,1) = (3.0d0, 0.0d0)
  T(1,2) = (1.0d0, 0.5d0)
  T(1,3) = (0.5d0, 0.5d0)
  T(2,2) = (2.0d0, 1.0d0)
  T(2,3) = (1.0d0, 0.0d0)
  T(3,3) = (1.0d0, 0.5d0)

  ! Initialize Q,Z to identity
  do i = 1, n
    Q(i,i) = (1.0d0, 0.0d0)
    Z(i,i) = (1.0d0, 0.0d0)
  end do

  call ZHGEQZ('S', 'V', 'V', n, ilo, ihi, H, ldh, T, ldt, &
               ALPHA, BETA, Q, ldq, Z, ldz, WORK, lwork, RWORK, info)

  call begin_test('accumulate_qz')
  call print_int('info', info)
  call print_array('alpha', ALPHA_r(1), 2*n)
  call print_array('beta', BETA_r(1), 2*n)
  call print_array('Q_col1', Q_r(1), 2*n)
  call print_array('Q_col2', Q_r(2*ldq+1), 2*n)
  call print_array('Q_col3', Q_r(4*ldq+1), 2*n)
  call print_array('Z_col1', Z_r(1), 2*n)
  call print_array('Z_col2', Z_r(2*ldz+1), 2*n)
  call print_array('Z_col3', Z_r(4*ldz+1), 2*n)
  call end_test()

end program
