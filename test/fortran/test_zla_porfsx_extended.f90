program test_zla_porfsx_extended
  use test_utils
  implicit none

  integer, parameter :: MAXN = 6
  integer, parameter :: NRHSMAX = 3
  integer, parameter :: N_ERR_BNDS = 3

  complex*16 :: a(MAXN*MAXN), af(MAXN*MAXN)
  complex*16 :: b(MAXN*NRHSMAX), y(MAXN*NRHSMAX)
  complex*16 :: res(MAXN), dy(MAXN), y_tail(MAXN)
  double precision :: a_r(2*MAXN*MAXN), af_r(2*MAXN*MAXN)
  double precision :: b_r(2*MAXN*NRHSMAX), y_r(2*MAXN*NRHSMAX)
  equivalence (a, a_r)
  equivalence (af, af_r)
  equivalence (b, b_r)
  equivalence (y, y_r)

  double precision :: c(MAXN), ayb(MAXN), berr_out(NRHSMAX)
  double precision :: err_bnds_norm(NRHSMAX*N_ERR_BNDS)
  double precision :: err_bnds_comp(NRHSMAX*N_ERR_BNDS)
  integer :: n, nrhs, info, k, prec_type, ithresh, n_norms
  double precision :: rcond, rthresh, dz_ub
  logical :: colequ, ignore_cwise

  ! Constant refinement parameters matching zporfsx / LAWN 165 defaults.
  prec_type = 1       ! Double (ignored by the JS port)
  ithresh = 10
  rthresh = 0.5d0
  dz_ub = 0.25d0
  rcond = 1.0d0       ! Well-conditioned test matrices
  ignore_cwise = .false.
  colequ = .false.
  n_norms = 2

  ! ============================================================
  ! Test 1: basic 3x3 upper, single RHS, no column equilibration
  ! A = [[4, 1+i, 0], [1-i, 3, 1], [0, 1, 2]]  (HPD)
  ! ============================================================
  n = 3
  nrhs = 1
  a = (0.0d0, 0.0d0)
  a(1) = (4.0d0, 0.0d0);  a(4) = (1.0d0, 1.0d0);  a(7) = (0.0d0, 0.0d0)
  a(2) = (1.0d0, -1.0d0); a(5) = (3.0d0, 0.0d0);  a(8) = (1.0d0, 0.0d0)
  a(3) = (0.0d0, 0.0d0);  a(6) = (1.0d0, 0.0d0);  a(9) = (2.0d0, 0.0d0)

  b(1) = (1.0d0, 0.0d0); b(2) = (1.0d0, 0.0d0); b(3) = (1.0d0, 0.0d0)

  af = a
  call zpotrf('U', n, af, n, info)
  if (info /= 0) stop 'zpotrf failed u3x3'

  y(1:n) = b(1:n)
  call zpotrs('U', n, nrhs, af, n, y, n, info)
  if (info /= 0) stop 'zpotrs failed u3x3'

  do k = 1, n
    c(k) = 1.0d0
  end do

  err_bnds_norm = 0.0d0
  err_bnds_comp = 0.0d0
  berr_out = 0.0d0

  call zla_porfsx_extended(prec_type, 'U', n, nrhs, a, n, af, n, &
       colequ, c, b, n, y, n, berr_out, n_norms, &
       err_bnds_norm, err_bnds_comp, res, ayb, dy, y_tail, &
       rcond, ithresh, rthresh, dz_ub, ignore_cwise, info)

  call begin_test('basic_upper_3x3')
  call print_int('info', info)
  call print_array('y', y_r, 2*n*nrhs)
  call print_array('berr_out', berr_out, nrhs)
  call print_array('err_bnds_norm', err_bnds_norm, nrhs*N_ERR_BNDS)
  call print_array('err_bnds_comp', err_bnds_comp, nrhs*N_ERR_BNDS)
  call end_test()

  ! ============================================================
  ! Test 2: basic 3x3 lower, single RHS
  ! ============================================================
  n = 3
  nrhs = 1
  a = (0.0d0, 0.0d0)
  a(1) = (4.0d0, 0.0d0);  a(4) = (1.0d0, 1.0d0);  a(7) = (0.0d0, 0.0d0)
  a(2) = (1.0d0, -1.0d0); a(5) = (3.0d0, 0.0d0);  a(8) = (1.0d0, 0.0d0)
  a(3) = (0.0d0, 0.0d0);  a(6) = (1.0d0, 0.0d0);  a(9) = (2.0d0, 0.0d0)

  b(1) = (1.0d0, 0.0d0); b(2) = (1.0d0, 0.0d0); b(3) = (1.0d0, 0.0d0)

  af = a
  call zpotrf('L', n, af, n, info)
  if (info /= 0) stop 'zpotrf failed l3x3'

  y(1:n) = b(1:n)
  call zpotrs('L', n, nrhs, af, n, y, n, info)
  if (info /= 0) stop 'zpotrs failed l3x3'

  do k = 1, n
    c(k) = 1.0d0
  end do

  err_bnds_norm = 0.0d0
  err_bnds_comp = 0.0d0
  berr_out = 0.0d0

  call zla_porfsx_extended(prec_type, 'L', n, nrhs, a, n, af, n, &
       colequ, c, b, n, y, n, berr_out, n_norms, &
       err_bnds_norm, err_bnds_comp, res, ayb, dy, y_tail, &
       rcond, ithresh, rthresh, dz_ub, ignore_cwise, info)

  call begin_test('basic_lower_3x3')
  call print_int('info', info)
  call print_array('y', y_r, 2*n*nrhs)
  call print_array('berr_out', berr_out, nrhs)
  call print_array('err_bnds_norm', err_bnds_norm, nrhs*N_ERR_BNDS)
  call print_array('err_bnds_comp', err_bnds_comp, nrhs*N_ERR_BNDS)
  call end_test()

  ! ============================================================
  ! Test 3: multi-RHS upper (2 RHS)
  ! ============================================================
  n = 3
  nrhs = 2
  a = (0.0d0, 0.0d0)
  a(1) = (4.0d0, 0.0d0);  a(4) = (1.0d0, 1.0d0);  a(7) = (0.0d0, 0.0d0)
  a(2) = (1.0d0, -1.0d0); a(5) = (3.0d0, 0.0d0);  a(8) = (1.0d0, 0.0d0)
  a(3) = (0.0d0, 0.0d0);  a(6) = (1.0d0, 0.0d0);  a(9) = (2.0d0, 0.0d0)

  b(1) = (1.0d0, 0.0d0); b(2) = (2.0d0, 1.0d0); b(3) = (3.0d0, 0.0d0)
  b(4) = (4.0d0, 0.0d0); b(5) = (5.0d0, -1.0d0); b(6) = (6.0d0, 0.0d0)

  af = a
  call zpotrf('U', n, af, n, info)
  if (info /= 0) stop 'zpotrf failed multi'

  y = b
  call zpotrs('U', n, nrhs, af, n, y, n, info)
  if (info /= 0) stop 'zpotrs failed multi'

  do k = 1, n
    c(k) = 1.0d0
  end do

  err_bnds_norm = 0.0d0
  err_bnds_comp = 0.0d0
  berr_out = 0.0d0

  call zla_porfsx_extended(prec_type, 'U', n, nrhs, a, n, af, n, &
       colequ, c, b, n, y, n, berr_out, n_norms, &
       err_bnds_norm, err_bnds_comp, res, ayb, dy, y_tail, &
       rcond, ithresh, rthresh, dz_ub, ignore_cwise, info)

  call begin_test('multi_rhs_upper_3x3')
  call print_int('info', info)
  call print_array('y', y_r, 2*n*nrhs)
  call print_array('berr_out', berr_out, nrhs)
  call print_array('err_bnds_norm', err_bnds_norm, nrhs*N_ERR_BNDS)
  call print_array('err_bnds_comp', err_bnds_comp, nrhs*N_ERR_BNDS)
  call end_test()

  ! ============================================================
  ! Test 4: colequ=.true. (column equilibration active)
  ! Scale c(i)=0.5 so the norms/ratios take the equilibrated branch.
  ! ============================================================
  n = 3
  nrhs = 1
  a = (0.0d0, 0.0d0)
  a(1) = (4.0d0, 0.0d0);  a(4) = (1.0d0, 1.0d0);  a(7) = (0.0d0, 0.0d0)
  a(2) = (1.0d0, -1.0d0); a(5) = (3.0d0, 0.0d0);  a(8) = (1.0d0, 0.0d0)
  a(3) = (0.0d0, 0.0d0);  a(6) = (1.0d0, 0.0d0);  a(9) = (2.0d0, 0.0d0)

  b(1) = (1.0d0, 0.0d0); b(2) = (1.0d0, 0.0d0); b(3) = (1.0d0, 0.0d0)

  af = a
  call zpotrf('U', n, af, n, info)
  if (info /= 0) stop 'zpotrf failed colequ'

  y(1:n) = b(1:n)
  call zpotrs('U', n, nrhs, af, n, y, n, info)
  if (info /= 0) stop 'zpotrs failed colequ'

  do k = 1, n
    c(k) = 0.5d0
  end do
  colequ = .true.

  err_bnds_norm = 0.0d0
  err_bnds_comp = 0.0d0
  berr_out = 0.0d0

  call zla_porfsx_extended(prec_type, 'U', n, nrhs, a, n, af, n, &
       colequ, c, b, n, y, n, berr_out, n_norms, &
       err_bnds_norm, err_bnds_comp, res, ayb, dy, y_tail, &
       rcond, ithresh, rthresh, dz_ub, ignore_cwise, info)

  call begin_test('colequ_upper_3x3')
  call print_int('info', info)
  call print_array('y', y_r, 2*n*nrhs)
  call print_array('berr_out', berr_out, nrhs)
  call print_array('err_bnds_norm', err_bnds_norm, nrhs*N_ERR_BNDS)
  call print_array('err_bnds_comp', err_bnds_comp, nrhs*N_ERR_BNDS)
  call end_test()
  colequ = .false.

  ! ============================================================
  ! Test 5: n_norms=0 (no error bounds written)
  ! ============================================================
  n = 3
  nrhs = 1
  a = (0.0d0, 0.0d0)
  a(1) = (4.0d0, 0.0d0);  a(4) = (1.0d0, 1.0d0);  a(7) = (0.0d0, 0.0d0)
  a(2) = (1.0d0, -1.0d0); a(5) = (3.0d0, 0.0d0);  a(8) = (1.0d0, 0.0d0)
  a(3) = (0.0d0, 0.0d0);  a(6) = (1.0d0, 0.0d0);  a(9) = (2.0d0, 0.0d0)

  b(1) = (1.0d0, 0.0d0); b(2) = (1.0d0, 0.0d0); b(3) = (1.0d0, 0.0d0)

  af = a
  call zpotrf('U', n, af, n, info)
  if (info /= 0) stop 'zpotrf failed nn0'

  y(1:n) = b(1:n)
  call zpotrs('U', n, nrhs, af, n, y, n, info)
  if (info /= 0) stop 'zpotrs failed nn0'

  do k = 1, n
    c(k) = 1.0d0
  end do

  err_bnds_norm = -1.0d0
  err_bnds_comp = -1.0d0
  berr_out = 0.0d0

  call zla_porfsx_extended(prec_type, 'U', n, nrhs, a, n, af, n, &
       colequ, c, b, n, y, n, berr_out, 0, &
       err_bnds_norm, err_bnds_comp, res, ayb, dy, y_tail, &
       rcond, ithresh, rthresh, dz_ub, ignore_cwise, info)

  call begin_test('n_norms_zero_upper_3x3')
  call print_int('info', info)
  call print_array('y', y_r, 2*n*nrhs)
  call print_array('berr_out', berr_out, nrhs)
  call end_test()

  ! ============================================================
  ! Test 6: ignore_cwise=.true.
  ! ============================================================
  n = 3
  nrhs = 1
  a = (0.0d0, 0.0d0)
  a(1) = (4.0d0, 0.0d0);  a(4) = (1.0d0, 1.0d0);  a(7) = (0.0d0, 0.0d0)
  a(2) = (1.0d0, -1.0d0); a(5) = (3.0d0, 0.0d0);  a(8) = (1.0d0, 0.0d0)
  a(3) = (0.0d0, 0.0d0);  a(6) = (1.0d0, 0.0d0);  a(9) = (2.0d0, 0.0d0)

  b(1) = (1.0d0, 0.0d0); b(2) = (1.0d0, 0.0d0); b(3) = (1.0d0, 0.0d0)

  af = a
  call zpotrf('U', n, af, n, info)
  if (info /= 0) stop 'zpotrf failed ic'

  y(1:n) = b(1:n)
  call zpotrs('U', n, nrhs, af, n, y, n, info)
  if (info /= 0) stop 'zpotrs failed ic'

  do k = 1, n
    c(k) = 1.0d0
  end do

  err_bnds_norm = 0.0d0
  err_bnds_comp = 0.0d0
  berr_out = 0.0d0

  call zla_porfsx_extended(prec_type, 'U', n, nrhs, a, n, af, n, &
       colequ, c, b, n, y, n, berr_out, n_norms, &
       err_bnds_norm, err_bnds_comp, res, ayb, dy, y_tail, &
       rcond, ithresh, rthresh, dz_ub, .true., info)

  call begin_test('ignore_cwise_upper_3x3')
  call print_int('info', info)
  call print_array('y', y_r, 2*n*nrhs)
  call print_array('berr_out', berr_out, nrhs)
  call print_array('err_bnds_norm', err_bnds_norm, nrhs*N_ERR_BNDS)
  call print_array('err_bnds_comp', err_bnds_comp, nrhs*N_ERR_BNDS)
  call end_test()

end program
