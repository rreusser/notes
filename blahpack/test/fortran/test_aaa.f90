! Fortran AAA algorithm — reference implementation using LAPACK ZGESVD.
! Compares convergence behavior against the JavaScript translation.

program test_aaa
  use test_utils
  implicit none

  integer, parameter :: MAX_ITER = 100

  call test_exp_circle()
  call test_lshaped()

contains

  subroutine aaa_core(npts, Z, F, tol, maxm, errvec, niters)
    integer, intent(in) :: npts, maxm
    complex(8), intent(in) :: Z(npts), F(npts)
    double precision, intent(in) :: tol
    double precision, intent(out) :: errvec(maxm)
    integer, intent(out) :: niters

    ! Allocatable to avoid stack overflow on large problems
    complex(8), allocatable :: zj(:), fj(:), wt(:)
    complex(8), allocatable :: CC(:,:), R(:), A_mat(:,:)
    integer, allocatable :: JJ(:)
    complex(8), allocatable :: VT(:,:), Udummy(:,:), WORK(:)
    double precision, allocatable :: svals(:), RWORK(:)

    integer :: nJ, nsup, iter, i, k, ii, jmax, ci
    double precision :: Fnorm, maxval_sq, cur_sq, err, d
    complex(8) :: diff, cmean, numer, denom
    integer :: lwork, info

    allocate(zj(maxm), fj(maxm), wt(maxm))
    allocate(CC(npts, maxm), R(npts), A_mat(npts, maxm))
    allocate(JJ(npts))
    allocate(svals(maxm), VT(maxm, maxm), Udummy(1,1))
    lwork = 8 * (npts + maxm)
    allocate(WORK(lwork), RWORK(5*maxm))

    nJ = npts
    do i = 1, npts
      JJ(i) = i
    end do

    cmean = (0.0d0, 0.0d0)
    do i = 1, npts
      cmean = cmean + F(i)
    end do
    cmean = cmean / dble(npts)
    do i = 1, npts
      R(i) = cmean
    end do

    Fnorm = 0.0d0
    do i = 1, npts
      d = abs(F(i))
      if (d > Fnorm) Fnorm = d
    end do

    nsup = 0
    niters = 0

    do iter = 1, maxm
      niters = iter

      maxval_sq = -1.0d0
      jmax = 1
      do i = 1, npts
        diff = F(i) - R(i)
        cur_sq = dble(diff)**2 + aimag(diff)**2
        if (cur_sq > maxval_sq) then
          maxval_sq = cur_sq
          jmax = i
        end if
      end do

      nsup = nsup + 1
      zj(nsup) = Z(jmax)
      fj(nsup) = F(jmax)

      do i = 1, nJ
        if (JJ(i) == jmax) then
          do k = i, nJ - 1
            JJ(k) = JJ(k + 1)
          end do
          nJ = nJ - 1
          exit
        end if
      end do

      do i = 1, npts
        diff = Z(i) - Z(jmax)
        if (abs(diff) == 0.0d0) then
          CC(i, iter) = (0.0d0, 0.0d0)
        else
          CC(i, iter) = 1.0d0 / diff
        end if
      end do

      do k = 1, nsup
        do ii = 1, nJ
          ci = JJ(ii)
          A_mat(ii, k) = CC(ci, k) * (F(ci) - fj(k))
        end do
      end do

      call ZGESVD('N', 'A', nJ, nsup, A_mat, npts, svals, &
                  Udummy, 1, VT, maxm, WORK, lwork, RWORK, info)

      do k = 1, nsup
        wt(k) = conjg(VT(nsup, k))
      end do

      do i = 1, npts
        R(i) = F(i)
      end do
      do ii = 1, nJ
        ci = JJ(ii)
        numer = (0.0d0, 0.0d0)
        denom = (0.0d0, 0.0d0)
        do k = 1, nsup
          numer = numer + CC(ci, k) * wt(k) * fj(k)
          denom = denom + CC(ci, k) * wt(k)
        end do
        if (abs(denom) > 0.0d0) then
          R(ci) = numer / denom
        end if
      end do

      err = 0.0d0
      do i = 1, npts
        d = abs(F(i) - R(i))
        if (d > err) err = d
      end do
      errvec(iter) = err

      if (err <= tol * Fnorm) exit
    end do

    deallocate(zj, fj, wt, CC, R, A_mat, JJ, svals, VT, Udummy, WORK, RWORK)
  end subroutine

  subroutine test_exp_circle()
    integer, parameter :: NPTS = 200
    complex(8) :: Z(NPTS), F(NPTS)
    double precision :: errvec(MAX_ITER), theta
    integer :: i, niters

    do i = 1, NPTS
      theta = 2.0d0 * 3.141592653589793d0 * dble(i - 1) / dble(NPTS)
      Z(i) = dcmplx(cos(theta), sin(theta))
      F(i) = exp(Z(i))
    end do

    call aaa_core(NPTS, Z, F, 1.0d-13, MAX_ITER, errvec, niters)

    call begin_test('exp_circle')
    call print_int('niters', niters)
    call print_array('errvec', errvec, niters)
    call end_test()
  end subroutine

  subroutine test_lshaped()
    integer, parameter :: MMAX_PTS = 1000
    complex(8) :: Z(MMAX_PTS), F(MMAX_PTS)
    double precision :: errvec(MAX_ITER)
    integer :: npts, niters
    double precision :: x, y, stp
    integer :: idx

    stp = 0.01d0
    idx = 0

    x = 0.0d0
    do while (x <= 2.0d0 + stp/2.0d0)
      idx = idx + 1; Z(idx) = dcmplx(x, 0.0d0); F(idx) = dcmplx(x*x, 0.0d0); x = x + stp
    end do
    y = 0.0d0
    do while (y <= 1.0d0 + stp/2.0d0)
      idx = idx + 1; Z(idx) = dcmplx(2.0d0, y); F(idx) = dcmplx(4.0d0, 0.0d0); y = y + stp
    end do
    x = 2.0d0
    do while (x >= 1.0d0 - stp/2.0d0)
      idx = idx + 1; Z(idx) = dcmplx(x, 1.0d0); F(idx) = dcmplx(x*x, 0.0d0); x = x - stp
    end do
    y = 1.0d0
    do while (y <= 2.0d0 + stp/2.0d0)
      idx = idx + 1; Z(idx) = dcmplx(1.0d0, y); F(idx) = dcmplx(1.0d0, 0.0d0); y = y + stp
    end do
    x = 1.0d0
    do while (x >= 0.0d0 - stp/2.0d0)
      idx = idx + 1; Z(idx) = dcmplx(x, 2.0d0); F(idx) = dcmplx(x*x, 0.0d0); x = x - stp
    end do
    y = 2.0d0
    do while (y >= 0.0d0 - stp/2.0d0)
      idx = idx + 1; Z(idx) = dcmplx(0.0d0, y); F(idx) = dcmplx(0.0d0, 0.0d0); y = y - stp
    end do

    npts = idx

    call aaa_core(npts, Z, F, 1.0d-13, MAX_ITER, errvec, niters)

    call begin_test('x2_lshaped')
    call print_int('npts', npts)
    call print_int('niters', niters)
    call print_array('errvec', errvec, niters)
    call end_test()
  end subroutine

end program
