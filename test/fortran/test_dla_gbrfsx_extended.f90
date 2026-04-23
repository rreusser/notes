program test_dla_gbrfsx_extended
  use test_utils
  implicit none

  ! Dimensions: NMAX=4, NRHS_MAX=2, LDAB=4, LDAFB=6
  integer, parameter :: NMAX = 4, NRHS_MAX = 2, LDAB_ = 4, LDAFB_ = 6
  integer, parameter :: NERRBND = 3

  double precision :: ab(LDAB_, NMAX), afb(LDAFB_, NMAX)
  double precision :: b(NMAX, NRHS_MAX), y(NMAX, NRHS_MAX)
  double precision :: c(NMAX)
  double precision :: berr_out(NRHS_MAX)
  ! Use flat 1D buffers; DLA_GBRFSX_EXTENDED treats them as (NRHS, NERRBND) with leading dim = NRHS passed at call site.
  double precision :: ebn(NRHS_MAX * NERRBND)
  double precision :: ebc(NRHS_MAX * NERRBND)
  double precision :: res(NMAX), ayb(NMAX), dy(NMAX), y_tail(NMAX)
  double precision :: rcond, rthresh, dz_ub
  integer :: ipiv(NMAX)
  integer :: info, n, kl, ku, nrhs, prec_type, trans_type
  integer :: ithresh, n_norms
  logical :: colequ, ignore_cwise
  integer :: i, k

  rthresh = 0.5d0
  dz_ub = 0.25d0
  prec_type = 2
  ithresh = 10
  n_norms = 2
  rcond = 1.0d-2

  ! ============================================================
  ! Test 1: 4x4 tridiag (KL=1, KU=1), single RHS, no-transpose
  n = 4; kl = 1; ku = 1; nrhs = 1
  trans_type = 111
  colequ = .false.
  ignore_cwise = .false.

  ab = 0.0d0; afb = 0.0d0
  ab(2,1) = 4.0d0;  ab(3,1) = -1.0d0
  ab(1,2) = 0.5d0;  ab(2,2) = 4.0d0;  ab(3,2) = -1.0d0
  ab(1,3) = 0.5d0;  ab(2,3) = 4.0d0;  ab(3,3) = -1.0d0
  ab(1,4) = 0.5d0;  ab(2,4) = 4.0d0

  b = 0.0d0
  b(1,1) = 1.0d0; b(2,1) = 2.0d0; b(3,1) = 3.0d0; b(4,1) = 4.0d0

  afb(2,1) = ab(1,1); afb(3,1) = ab(2,1); afb(4,1) = ab(3,1)
  afb(2,2) = ab(1,2); afb(3,2) = ab(2,2); afb(4,2) = ab(3,2)
  afb(2,3) = ab(1,3); afb(3,3) = ab(2,3); afb(4,3) = ab(3,3)
  afb(2,4) = ab(1,4); afb(3,4) = ab(2,4)

  call DGBTRF(n, n, kl, ku, afb, LDAFB_, ipiv, info)

  y = 0.0d0
  do i = 1, n
    y(i,1) = b(i,1)
  end do
  call DGBTRS('N', n, kl, ku, nrhs, afb, LDAFB_, ipiv, y, NMAX, info)

  do i = 1, n
    c(i) = 1.0d0
  end do

  do k = 1, nrhs * NERRBND
    ebn(k) = 1.0d0
    ebc(k) = 1.0d0
  end do
  berr_out = 0.0d0
  info = 0

  call DLA_GBRFSX_EXTENDED(prec_type, trans_type, n, kl, ku, nrhs, &
       ab, LDAB_, afb, LDAFB_, ipiv, colequ, c, b, NMAX, y, NMAX, &
       berr_out, n_norms, ebn, ebc, res, ayb, dy, &
       y_tail, rcond, ithresh, rthresh, dz_ub, ignore_cwise, info)

  call begin_test('tridiag_notrans')
  call print_array('y', y(1:n,1), n)
  call print_array('berr_out', berr_out, nrhs)
  ! LA_LINRX_ERR_I=2 → flat index nrhs+1..2*nrhs
  call print_array('err_norm', ebn(nrhs+1:2*nrhs), nrhs)
  call print_array('err_comp', ebc(nrhs+1:2*nrhs), nrhs)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 2: transpose
  ab = 0.0d0; afb = 0.0d0
  ab(2,1) = 4.0d0;  ab(3,1) = -1.0d0
  ab(1,2) = 0.5d0;  ab(2,2) = 4.0d0;  ab(3,2) = -1.0d0
  ab(1,3) = 0.5d0;  ab(2,3) = 4.0d0;  ab(3,3) = -1.0d0
  ab(1,4) = 0.5d0;  ab(2,4) = 4.0d0

  b = 0.0d0
  b(1,1) = 1.0d0; b(2,1) = 2.0d0; b(3,1) = 3.0d0; b(4,1) = 4.0d0

  afb(2,1) = ab(1,1); afb(3,1) = ab(2,1); afb(4,1) = ab(3,1)
  afb(2,2) = ab(1,2); afb(3,2) = ab(2,2); afb(4,2) = ab(3,2)
  afb(2,3) = ab(1,3); afb(3,3) = ab(2,3); afb(4,3) = ab(3,3)
  afb(2,4) = ab(1,4); afb(3,4) = ab(2,4)

  call DGBTRF(n, n, kl, ku, afb, LDAFB_, ipiv, info)

  y = 0.0d0
  do i = 1, n
    y(i,1) = b(i,1)
  end do
  call DGBTRS('T', n, kl, ku, nrhs, afb, LDAFB_, ipiv, y, NMAX, info)

  trans_type = 112
  do k = 1, nrhs * NERRBND
    ebn(k) = 1.0d0
    ebc(k) = 1.0d0
  end do
  berr_out = 0.0d0
  info = 0
  call DLA_GBRFSX_EXTENDED(prec_type, trans_type, n, kl, ku, nrhs, &
       ab, LDAB_, afb, LDAFB_, ipiv, colequ, c, b, NMAX, y, NMAX, &
       berr_out, n_norms, ebn, ebc, res, ayb, dy, &
       y_tail, rcond, ithresh, rthresh, dz_ub, ignore_cwise, info)

  call begin_test('tridiag_trans')
  call print_array('y', y(1:n,1), n)
  call print_array('berr_out', berr_out, nrhs)
  call print_array('err_norm', ebn(nrhs+1:2*nrhs), nrhs)
  call print_array('err_comp', ebc(nrhs+1:2*nrhs), nrhs)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 3: NRHS=2, colequ=true
  ab = 0.0d0; afb = 0.0d0
  ab(2,1) = 4.0d0;  ab(3,1) = -1.0d0
  ab(1,2) = 0.5d0;  ab(2,2) = 4.0d0;  ab(3,2) = -1.0d0
  ab(1,3) = 0.5d0;  ab(2,3) = 4.0d0;  ab(3,3) = -1.0d0
  ab(1,4) = 0.5d0;  ab(2,4) = 4.0d0

  b = 0.0d0
  b(1,1) = 1.0d0; b(2,1) = 2.0d0; b(3,1) = 3.0d0; b(4,1) = 4.0d0
  b(1,2) = 0.5d0; b(2,2) = 1.5d0; b(3,2) = -1.0d0; b(4,2) = 2.0d0

  nrhs = 2
  afb(2,1) = ab(1,1); afb(3,1) = ab(2,1); afb(4,1) = ab(3,1)
  afb(2,2) = ab(1,2); afb(3,2) = ab(2,2); afb(4,2) = ab(3,2)
  afb(2,3) = ab(1,3); afb(3,3) = ab(2,3); afb(4,3) = ab(3,3)
  afb(2,4) = ab(1,4); afb(3,4) = ab(2,4)

  call DGBTRF(n, n, kl, ku, afb, LDAFB_, ipiv, info)

  y = 0.0d0
  do k = 1, nrhs
    do i = 1, n
      y(i,k) = b(i,k)
    end do
  end do
  call DGBTRS('N', n, kl, ku, nrhs, afb, LDAFB_, ipiv, y, NMAX, info)

  trans_type = 111
  colequ = .true.
  do i = 1, n
    c(i) = 1.0d0 + 0.1d0 * i
  end do
  do k = 1, nrhs * NERRBND
    ebn(k) = 1.0d0
    ebc(k) = 1.0d0
  end do
  berr_out = 0.0d0
  info = 0
  call DLA_GBRFSX_EXTENDED(prec_type, trans_type, n, kl, ku, nrhs, &
       ab, LDAB_, afb, LDAFB_, ipiv, colequ, c, b, NMAX, y, NMAX, &
       berr_out, n_norms, ebn, ebc, res, ayb, dy, &
       y_tail, rcond, ithresh, rthresh, dz_ub, ignore_cwise, info)

  call begin_test('multi_rhs_colequ')
  call print_array('y1', y(1:n,1), n)
  call print_array('y2', y(1:n,2), n)
  call print_array('berr_out', berr_out, nrhs)
  call print_array('err_norm', ebn(nrhs+1:2*nrhs), nrhs)
  call print_array('err_comp', ebc(nrhs+1:2*nrhs), nrhs)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 4: ignore_cwise=true, single RHS
  ab = 0.0d0; afb = 0.0d0
  ab(2,1) = 4.0d0;  ab(3,1) = -1.0d0
  ab(1,2) = 0.5d0;  ab(2,2) = 4.0d0;  ab(3,2) = -1.0d0
  ab(1,3) = 0.5d0;  ab(2,3) = 4.0d0;  ab(3,3) = -1.0d0
  ab(1,4) = 0.5d0;  ab(2,4) = 4.0d0

  b = 0.0d0
  b(1,1) = 1.0d0; b(2,1) = 2.0d0; b(3,1) = 3.0d0; b(4,1) = 4.0d0

  nrhs = 1
  afb(2,1) = ab(1,1); afb(3,1) = ab(2,1); afb(4,1) = ab(3,1)
  afb(2,2) = ab(1,2); afb(3,2) = ab(2,2); afb(4,2) = ab(3,2)
  afb(2,3) = ab(1,3); afb(3,3) = ab(2,3); afb(4,3) = ab(3,3)
  afb(2,4) = ab(1,4); afb(3,4) = ab(2,4)
  call DGBTRF(n, n, kl, ku, afb, LDAFB_, ipiv, info)
  y = 0.0d0
  do i = 1, n
    y(i,1) = b(i,1)
  end do
  call DGBTRS('N', n, kl, ku, nrhs, afb, LDAFB_, ipiv, y, NMAX, info)

  trans_type = 111
  colequ = .false.
  ignore_cwise = .true.
  do i = 1, n
    c(i) = 1.0d0
  end do
  do k = 1, nrhs * NERRBND
    ebn(k) = 1.0d0
    ebc(k) = 1.0d0
  end do
  berr_out = 0.0d0
  info = 0
  call DLA_GBRFSX_EXTENDED(prec_type, trans_type, n, kl, ku, nrhs, &
       ab, LDAB_, afb, LDAFB_, ipiv, colequ, c, b, NMAX, y, NMAX, &
       berr_out, n_norms, ebn, ebc, res, ayb, dy, &
       y_tail, rcond, ithresh, rthresh, dz_ub, ignore_cwise, info)

  call begin_test('ignore_cwise_true')
  call print_array('y', y(1:n,1), n)
  call print_array('berr_out', berr_out, nrhs)
  call print_array('err_norm', ebn(nrhs+1:2*nrhs), nrhs)
  call print_array('err_comp', ebc(nrhs+1:2*nrhs), nrhs)
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 5: n_norms = 0
  ab = 0.0d0; afb = 0.0d0
  ab(2,1) = 4.0d0;  ab(3,1) = -1.0d0
  ab(1,2) = 0.5d0;  ab(2,2) = 4.0d0;  ab(3,2) = -1.0d0
  ab(1,3) = 0.5d0;  ab(2,3) = 4.0d0;  ab(3,3) = -1.0d0
  ab(1,4) = 0.5d0;  ab(2,4) = 4.0d0

  b = 0.0d0
  b(1,1) = 1.0d0; b(2,1) = 2.0d0; b(3,1) = 3.0d0; b(4,1) = 4.0d0

  afb(2,1) = ab(1,1); afb(3,1) = ab(2,1); afb(4,1) = ab(3,1)
  afb(2,2) = ab(1,2); afb(3,2) = ab(2,2); afb(4,2) = ab(3,2)
  afb(2,3) = ab(1,3); afb(3,3) = ab(2,3); afb(4,3) = ab(3,3)
  afb(2,4) = ab(1,4); afb(3,4) = ab(2,4)
  call DGBTRF(n, n, kl, ku, afb, LDAFB_, ipiv, info)
  y = 0.0d0
  do i = 1, n
    y(i,1) = b(i,1)
  end do
  call DGBTRS('N', n, kl, ku, nrhs, afb, LDAFB_, ipiv, y, NMAX, info)

  ignore_cwise = .false.
  do k = 1, nrhs * NERRBND
    ebn(k) = 1.0d0
    ebc(k) = 1.0d0
  end do
  berr_out = 0.0d0
  info = 0
  call DLA_GBRFSX_EXTENDED(prec_type, trans_type, n, kl, ku, nrhs, &
       ab, LDAB_, afb, LDAFB_, ipiv, colequ, c, b, NMAX, y, NMAX, &
       berr_out, 0, ebn, ebc, res, ayb, dy, &
       y_tail, rcond, ithresh, rthresh, dz_ub, ignore_cwise, info)

  call begin_test('n_norms_zero')
  call print_array('y', y(1:n,1), n)
  call print_array('berr_out', berr_out, nrhs)
  call print_int('info', info)
  call end_test()

end program

! ----------------------------------------------------------------
! XBLAS stubs: deliver the same answer as DGBMV (no extra precision).
! ----------------------------------------------------------------
subroutine BLAS_DGBMV_X(trans, m, n, kl, ku, alpha, ab, ldab, x, incx, &
     beta, y, incy, prec_type)
  implicit none
  integer :: trans, m, n, kl, ku, ldab, incx, incy, prec_type
  double precision :: alpha, beta
  double precision :: ab(ldab, *), x(*), y(*)
  character :: t
  if (trans == 111) then
    t = 'N'
  else if (trans == 112) then
    t = 'T'
  else
    t = 'C'
  end if
  call DGBMV(t, m, n, kl, ku, alpha, ab, ldab, x, incx, beta, y, incy)
end subroutine

subroutine BLAS_DGBMV2_X(trans, m, n, kl, ku, alpha, ab, ldab, x, &
     x_tail, incx, beta, y, incy, prec_type)
  implicit none
  integer :: trans, m, n, kl, ku, ldab, incx, incy, prec_type
  double precision :: alpha, beta
  double precision :: ab(ldab, *), x(*), x_tail(*), y(*)
  character :: t
  if (trans == 111) then
    t = 'N'
  else if (trans == 112) then
    t = 'T'
  else
    t = 'C'
  end if
  call DGBMV(t, m, n, kl, ku, alpha, ab, ldab, x, incx, beta, y, incy)
  call DGBMV(t, m, n, kl, ku, alpha, ab, ldab, x_tail, incx, 1.0d0, y, incy)
end subroutine
