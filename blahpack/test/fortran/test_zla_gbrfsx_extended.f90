program test_zla_gbrfsx_extended
  use test_utils
  implicit none
  ! Test uses a modified source (pipeline/zla_gbrfsx_extended/base_only.f)
  ! which strips the BLAS_ZGBMV_X / BLAS_ZGBMV2_X calls (extended-precision
  ! reference BLAS not available) and forces Y_PREC_STATE = BASE_RESIDUAL.
  ! The JS translation matches this reduced behavior.

  ! Workspaces / matrices sized for N_MAX = 4, NRHS_MAX = 2
  double precision :: ab_r(48), afb_r(48), b_r(16), y_r(16)
  double precision :: res_r(8), dy_r(8), y_tail_r(8)
  complex*16 :: ab(6,4), afb(6,4), b(4,2), y(4,2)
  complex*16 :: res(4), dy(4), y_tail(4)
  equivalence (ab, ab_r)
  equivalence (afb, afb_r)
  equivalence (b, b_r)
  equivalence (y, y_r)
  equivalence (res, res_r)
  equivalence (dy, dy_r)
  equivalence (y_tail, y_tail_r)
  double precision :: c(4), ayb(4)
  double precision :: berr_out(2)
  ! Declare err_bnds_* as flat 1D so the Fortran caller's leading dimension
  ! matches the `NRHS` passed to the routine (the routine treats these as
  ! `(NRHS, 3)` with LD=NRHS; declaring a static 2D array with LD=2 and
  ! passing NRHS=1 would silently misalign writes).
  double precision :: err_bnds_norm(6), err_bnds_comp(6)
  double precision :: rcond, rthresh, dz_ub
  integer :: ipiv(4), info, n, kl, ku, nrhs, n_norms
  integer :: ithresh, prec_type, trans_type
  logical :: colequ, ignore_cwise
  integer :: i, j

  ! Common constants
  prec_type = 1        ! unused in base-only variant
  ithresh = 10
  rthresh = 0.5d0
  dz_ub = 0.25d0
  rcond = 1.0d0

  ! ============================================================
  ! Test 1: 4x4 tridiagonal, no-transpose, single RHS
  ! ============================================================
  n = 4; kl = 1; ku = 1; nrhs = 1
  trans_type = 111    ! BLAS_NO_TRANS (CHLA_TRANSTYPE -> 'N')
  colequ = .false.
  ignore_cwise = .false.
  n_norms = 2

  ab = (0.0d0, 0.0d0); afb = (0.0d0, 0.0d0)
  ! Original band AB(LDAB=6, N): diag row KU+1=2
  ab(2,1) = (4.0d0,1.0d0);  ab(3,1) = (-1.0d0,0.5d0)
  ab(1,2) = (0.5d0,-0.5d0); ab(2,2) = (4.0d0,1.0d0);  ab(3,2) = (-1.0d0,0.5d0)
  ab(1,3) = (0.5d0,-0.5d0); ab(2,3) = (4.0d0,1.0d0);  ab(3,3) = (-1.0d0,0.5d0)
  ab(1,4) = (0.5d0,-0.5d0); ab(2,4) = (4.0d0,1.0d0)

  b = (0.0d0, 0.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (2.0d0, 1.0d0)
  b(3,1) = (3.0d0,-1.0d0)
  b(4,1) = (4.0d0, 0.5d0)

  ! AFB(LDAFB=6 = 2*KL+KU+1): fill KL+1..2*KL+KU+1
  afb(2,1) = ab(1,1); afb(3,1) = ab(2,1); afb(4,1) = ab(3,1)
  afb(2,2) = ab(1,2); afb(3,2) = ab(2,2); afb(4,2) = ab(3,2)
  afb(2,3) = ab(1,3); afb(3,3) = ab(2,3); afb(4,3) = ab(3,3)
  afb(2,4) = ab(1,4); afb(3,4) = ab(2,4)

  call ZGBTRF(n, n, kl, ku, afb, 6, ipiv, info)

  ! initial Y = solve(A, b) using zgbtrs
  y = (0.0d0, 0.0d0)
  do i = 1, n
    y(i,1) = b(i,1)
  end do
  call ZGBTRS('N', n, kl, ku, 1, afb, 6, ipiv, y, 4, info)

  do i = 1, 4
    c(i) = 1.0d0
    ayb(i) = 0.0d0
  end do
  res = (0.0d0, 0.0d0)
  dy = (0.0d0, 0.0d0)
  y_tail = (0.0d0, 0.0d0)
  err_bnds_norm = 0.0d0
  err_bnds_comp = 0.0d0
  ! Trust entries: LA_LINRX_TRUST_I = 1, NRHS = 1, LD = 1.
  err_bnds_norm(1) = 1.0d0
  err_bnds_comp(1) = 1.0d0
  berr_out = 0.0d0
  info = 0

  call ZLA_GBRFSX_EXTENDED(prec_type, trans_type, n, kl, ku, nrhs, &
       ab, 6, afb, 6, ipiv, colequ, c, b, 4, y, 4, berr_out, n_norms, &
       err_bnds_norm, err_bnds_comp, res, ayb, dy, y_tail, rcond, &
       ithresh, rthresh, dz_ub, ignore_cwise, info)

  call begin_test('tridiag_notrans')
  call print_array('y_r', y_r, 2*n)
  call print_array('berr_out', berr_out, nrhs)
  call print_array('err_bnds_norm_row1', err_bnds_norm(1:3), 3)
  call print_array('err_bnds_comp_row1', err_bnds_comp(1:3), 3)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 2: same tridiagonal, conjugate-transpose
  ! ============================================================
  trans_type = 113    ! BLAS_CONJ_TRANS -> 'C'
  ab = (0.0d0, 0.0d0); afb = (0.0d0, 0.0d0)
  ab(2,1) = (4.0d0,1.0d0);  ab(3,1) = (-1.0d0,0.5d0)
  ab(1,2) = (0.5d0,-0.5d0); ab(2,2) = (4.0d0,1.0d0);  ab(3,2) = (-1.0d0,0.5d0)
  ab(1,3) = (0.5d0,-0.5d0); ab(2,3) = (4.0d0,1.0d0);  ab(3,3) = (-1.0d0,0.5d0)
  ab(1,4) = (0.5d0,-0.5d0); ab(2,4) = (4.0d0,1.0d0)

  b = (0.0d0, 0.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (2.0d0, 1.0d0)
  b(3,1) = (3.0d0,-1.0d0)
  b(4,1) = (4.0d0, 0.5d0)

  afb(2,1) = ab(1,1); afb(3,1) = ab(2,1); afb(4,1) = ab(3,1)
  afb(2,2) = ab(1,2); afb(3,2) = ab(2,2); afb(4,2) = ab(3,2)
  afb(2,3) = ab(1,3); afb(3,3) = ab(2,3); afb(4,3) = ab(3,3)
  afb(2,4) = ab(1,4); afb(3,4) = ab(2,4)

  call ZGBTRF(n, n, kl, ku, afb, 6, ipiv, info)

  y = (0.0d0, 0.0d0)
  do i = 1, n
    y(i,1) = b(i,1)
  end do
  call ZGBTRS('C', n, kl, ku, 1, afb, 6, ipiv, y, 4, info)

  res = (0.0d0, 0.0d0); dy = (0.0d0, 0.0d0); y_tail = (0.0d0, 0.0d0)
  do i = 1, 4
    ayb(i) = 0.0d0
  end do
  err_bnds_norm = 0.0d0; err_bnds_comp = 0.0d0
  err_bnds_norm(1) = 1.0d0
  err_bnds_comp(1) = 1.0d0
  berr_out = 0.0d0
  info = 0

  call ZLA_GBRFSX_EXTENDED(prec_type, trans_type, n, kl, ku, nrhs, &
       ab, 6, afb, 6, ipiv, colequ, c, b, 4, y, 4, berr_out, n_norms, &
       err_bnds_norm, err_bnds_comp, res, ayb, dy, y_tail, rcond, &
       ithresh, rthresh, dz_ub, ignore_cwise, info)

  call begin_test('tridiag_conjtrans')
  call print_array('y_r', y_r, 2*n)
  call print_array('berr_out', berr_out, nrhs)
  call print_array('err_bnds_norm_row1', err_bnds_norm(1:3), 3)
  call print_array('err_bnds_comp_row1', err_bnds_comp(1:3), 3)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 3: Two RHS, no-transpose, colequ=.true.
  ! ============================================================
  trans_type = 111
  nrhs = 2
  colequ = .true.
  c(1) = 1.0d0; c(2) = 0.5d0; c(3) = 2.0d0; c(4) = 1.5d0
  ! Unequilibrate AB columns: row-by-row multiply column j by c(j)
  ab = (0.0d0, 0.0d0); afb = (0.0d0, 0.0d0)
  ab(2,1) = (4.0d0,1.0d0);  ab(3,1) = (-1.0d0,0.5d0)
  ab(1,2) = (0.5d0,-0.5d0); ab(2,2) = (4.0d0,1.0d0);  ab(3,2) = (-1.0d0,0.5d0)
  ab(1,3) = (0.5d0,-0.5d0); ab(2,3) = (4.0d0,1.0d0);  ab(3,3) = (-1.0d0,0.5d0)
  ab(1,4) = (0.5d0,-0.5d0); ab(2,4) = (4.0d0,1.0d0)

  b = (0.0d0, 0.0d0)
  b(1,1) = (1.0d0, 0.0d0); b(2,1) = (2.0d0, 1.0d0)
  b(3,1) = (3.0d0,-1.0d0); b(4,1) = (4.0d0, 0.5d0)
  b(1,2) = (0.0d0, 1.0d0); b(2,2) = (1.0d0, 0.0d0)
  b(3,2) = (-1.0d0,2.0d0); b(4,2) = (2.0d0,-1.0d0)

  afb(2,1) = ab(1,1); afb(3,1) = ab(2,1); afb(4,1) = ab(3,1)
  afb(2,2) = ab(1,2); afb(3,2) = ab(2,2); afb(4,2) = ab(3,2)
  afb(2,3) = ab(1,3); afb(3,3) = ab(2,3); afb(4,3) = ab(3,3)
  afb(2,4) = ab(1,4); afb(3,4) = ab(2,4)

  call ZGBTRF(n, n, kl, ku, afb, 6, ipiv, info)

  y = (0.0d0, 0.0d0)
  do j = 1, nrhs
    do i = 1, n
      y(i,j) = b(i,j)
    end do
  end do
  call ZGBTRS('N', n, kl, ku, nrhs, afb, 6, ipiv, y, 4, info)

  res = (0.0d0, 0.0d0); dy = (0.0d0, 0.0d0); y_tail = (0.0d0, 0.0d0)
  do i = 1, 4
    ayb(i) = 0.0d0
  end do
  err_bnds_norm = 0.0d0; err_bnds_comp = 0.0d0
  ! NRHS=2: element (j,k) sits at flat index j + (k-1)*2.
  err_bnds_norm(1) = 1.0d0; err_bnds_norm(2) = 1.0d0
  err_bnds_comp(1) = 1.0d0; err_bnds_comp(2) = 1.0d0
  berr_out = 0.0d0
  info = 0

  call ZLA_GBRFSX_EXTENDED(prec_type, trans_type, n, kl, ku, nrhs, &
       ab, 6, afb, 6, ipiv, colequ, c, b, 4, y, 4, berr_out, n_norms, &
       err_bnds_norm, err_bnds_comp, res, ayb, dy, y_tail, rcond, &
       ithresh, rthresh, dz_ub, ignore_cwise, info)

  call begin_test('multi_rhs_colequ')
  call print_array('y_r', y_r, 2*n*nrhs)
  call print_array('berr_out', berr_out, nrhs)
  call print_array('err_bnds_norm_j1', [err_bnds_norm(1), err_bnds_norm(3), err_bnds_norm(5)], 3)
  call print_array('err_bnds_norm_j2', [err_bnds_norm(2), err_bnds_norm(4), err_bnds_norm(6)], 3)
  call print_array('err_bnds_comp_j1', [err_bnds_comp(1), err_bnds_comp(3), err_bnds_comp(5)], 3)
  call print_array('err_bnds_comp_j2', [err_bnds_comp(2), err_bnds_comp(4), err_bnds_comp(6)], 3)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 4: 1x1 system (N=1, KL=KU=0)
  ! ============================================================
  n = 1; kl = 0; ku = 0; nrhs = 1
  trans_type = 111
  colequ = .false.
  n_norms = 2

  ab = (0.0d0, 0.0d0); afb = (0.0d0, 0.0d0)
  ab(1,1) = (3.0d0, 2.0d0)
  afb(1,1) = (3.0d0, 2.0d0)

  b = (0.0d0, 0.0d0)
  b(1,1) = (5.0d0, 1.0d0)

  call ZGBTRF(n, n, kl, ku, afb, 6, ipiv, info)
  y = (0.0d0, 0.0d0)
  y(1,1) = b(1,1)
  call ZGBTRS('N', n, kl, ku, 1, afb, 6, ipiv, y, 4, info)

  do i = 1, 4
    c(i) = 1.0d0
    ayb(i) = 0.0d0
  end do
  res = (0.0d0, 0.0d0); dy = (0.0d0, 0.0d0); y_tail = (0.0d0, 0.0d0)
  err_bnds_norm = 0.0d0; err_bnds_comp = 0.0d0
  err_bnds_norm(1) = 1.0d0
  err_bnds_comp(1) = 1.0d0
  berr_out = 0.0d0
  info = 0

  call ZLA_GBRFSX_EXTENDED(prec_type, trans_type, n, kl, ku, nrhs, &
       ab, 6, afb, 6, ipiv, colequ, c, b, 4, y, 4, berr_out, n_norms, &
       err_bnds_norm, err_bnds_comp, res, ayb, dy, y_tail, rcond, &
       ithresh, rthresh, dz_ub, ignore_cwise, info)

  call begin_test('one_by_one')
  call print_array('y_r', y_r, 2)
  call print_array('berr_out', berr_out, 1)
  call print_array('err_bnds_norm_j1', err_bnds_norm(1:3), 3)
  call print_array('err_bnds_comp_j1', err_bnds_comp(1:3), 3)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 5: ignore_cwise=.true., n_norms=0
  ! ============================================================
  n = 4; kl = 1; ku = 1; nrhs = 1
  trans_type = 111
  colequ = .false.
  ignore_cwise = .true.
  n_norms = 0

  ab = (0.0d0, 0.0d0); afb = (0.0d0, 0.0d0)
  ab(2,1) = (4.0d0,1.0d0);  ab(3,1) = (-1.0d0,0.5d0)
  ab(1,2) = (0.5d0,-0.5d0); ab(2,2) = (4.0d0,1.0d0);  ab(3,2) = (-1.0d0,0.5d0)
  ab(1,3) = (0.5d0,-0.5d0); ab(2,3) = (4.0d0,1.0d0);  ab(3,3) = (-1.0d0,0.5d0)
  ab(1,4) = (0.5d0,-0.5d0); ab(2,4) = (4.0d0,1.0d0)

  b = (0.0d0, 0.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (2.0d0, 1.0d0)
  b(3,1) = (3.0d0,-1.0d0)
  b(4,1) = (4.0d0, 0.5d0)

  afb(2,1) = ab(1,1); afb(3,1) = ab(2,1); afb(4,1) = ab(3,1)
  afb(2,2) = ab(1,2); afb(3,2) = ab(2,2); afb(4,2) = ab(3,2)
  afb(2,3) = ab(1,3); afb(3,3) = ab(2,3); afb(4,3) = ab(3,3)
  afb(2,4) = ab(1,4); afb(3,4) = ab(2,4)

  call ZGBTRF(n, n, kl, ku, afb, 6, ipiv, info)

  y = (0.0d0, 0.0d0)
  do i = 1, n
    y(i,1) = b(i,1)
  end do
  call ZGBTRS('N', n, kl, ku, 1, afb, 6, ipiv, y, 4, info)

  do i = 1, 4
    c(i) = 1.0d0
    ayb(i) = 0.0d0
  end do
  res = (0.0d0, 0.0d0); dy = (0.0d0, 0.0d0); y_tail = (0.0d0, 0.0d0)
  err_bnds_norm = 0.0d0; err_bnds_comp = 0.0d0
  berr_out = 0.0d0
  info = 0

  call ZLA_GBRFSX_EXTENDED(prec_type, trans_type, n, kl, ku, nrhs, &
       ab, 6, afb, 6, ipiv, colequ, c, b, 4, y, 4, berr_out, n_norms, &
       err_bnds_norm, err_bnds_comp, res, ayb, dy, y_tail, rcond, &
       ithresh, rthresh, dz_ub, ignore_cwise, info)

  call begin_test('ignore_cwise_nnorms0')
  call print_array('y_r', y_r, 2*n)
  call print_array('berr_out', berr_out, 1)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 6: perturbed initial Y to force multiple iterations
  ! ============================================================
  n = 4; kl = 1; ku = 1; nrhs = 1
  trans_type = 111
  colequ = .false.
  ignore_cwise = .false.
  n_norms = 2

  ab = (0.0d0, 0.0d0); afb = (0.0d0, 0.0d0)
  ab(2,1) = (4.0d0,1.0d0);  ab(3,1) = (-1.0d0,0.5d0)
  ab(1,2) = (0.5d0,-0.5d0); ab(2,2) = (4.0d0,1.0d0);  ab(3,2) = (-1.0d0,0.5d0)
  ab(1,3) = (0.5d0,-0.5d0); ab(2,3) = (4.0d0,1.0d0);  ab(3,3) = (-1.0d0,0.5d0)
  ab(1,4) = (0.5d0,-0.5d0); ab(2,4) = (4.0d0,1.0d0)

  b = (0.0d0, 0.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (2.0d0, 1.0d0)
  b(3,1) = (3.0d0,-1.0d0)
  b(4,1) = (4.0d0, 0.5d0)

  afb(2,1) = ab(1,1); afb(3,1) = ab(2,1); afb(4,1) = ab(3,1)
  afb(2,2) = ab(1,2); afb(3,2) = ab(2,2); afb(4,2) = ab(3,2)
  afb(2,3) = ab(1,3); afb(3,3) = ab(2,3); afb(4,3) = ab(3,3)
  afb(2,4) = ab(1,4); afb(3,4) = ab(2,4)

  call ZGBTRF(n, n, kl, ku, afb, 6, ipiv, info)

  ! intentionally inaccurate initial guess
  y = (0.0d0, 0.0d0)
  y(1,1) = (0.1d0, 0.0d0)
  y(2,1) = (0.2d0, 0.1d0)
  y(3,1) = (0.3d0,-0.1d0)
  y(4,1) = (0.4d0, 0.05d0)

  do i = 1, 4
    c(i) = 1.0d0
    ayb(i) = 0.0d0
  end do
  res = (0.0d0, 0.0d0); dy = (0.0d0, 0.0d0); y_tail = (0.0d0, 0.0d0)
  err_bnds_norm = 0.0d0; err_bnds_comp = 0.0d0
  err_bnds_norm(1) = 1.0d0
  err_bnds_comp(1) = 1.0d0
  berr_out = 0.0d0
  info = 0

  call ZLA_GBRFSX_EXTENDED(prec_type, trans_type, n, kl, ku, nrhs, &
       ab, 6, afb, 6, ipiv, colequ, c, b, 4, y, 4, berr_out, n_norms, &
       err_bnds_norm, err_bnds_comp, res, ayb, dy, y_tail, rcond, &
       ithresh, rthresh, dz_ub, ignore_cwise, info)

  call begin_test('perturbed_y')
  call print_array('y_r', y_r, 2*n)
  call print_array('berr_out', berr_out, 1)
  call print_array('err_bnds_norm_j1', err_bnds_norm(1:3), 3)
  call print_array('err_bnds_comp_j1', err_bnds_comp(1:3), 3)
  call print_int('info', info)
  call end_test()

end program
