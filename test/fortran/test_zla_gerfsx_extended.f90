program test_zla_gerfsx_extended
  use test_utils
  implicit none

  integer, parameter :: BLAS_NO_TRANS = 111
  integer, parameter :: BLAS_CONJ_TRANS = 113
  integer, parameter :: PREC_DOUBLE = 2

  call test_notrans_basic()
  call test_conjtrans_basic()
  call test_no_equ()
  call test_n_zero()
  call test_ignore_cwise()

contains

  subroutine factor_and_solve(N, NRHS, A, AF, IPIV, B, Y, INFO)
    integer, intent(in) :: N, NRHS
    complex*16, intent(in) :: A(N, N), B(N, NRHS)
    complex*16, intent(out) :: AF(N, N), Y(N, NRHS)
    integer, intent(out) :: IPIV(N), INFO
    integer :: i, j
    do j = 1, N
      do i = 1, N
        AF(i, j) = A(i, j)
      end do
    end do
    do j = 1, NRHS
      do i = 1, N
        Y(i, j) = B(i, j)
      end do
    end do
    call ZGETRF(N, N, AF, N, IPIV, INFO)
    call ZGETRS('N', N, NRHS, AF, N, IPIV, Y, N, INFO)
  end subroutine

  subroutine test_notrans_basic()
    integer, parameter :: N = 3, NRHS = 1
    complex*16 :: A(N, N), AF(N, N), B(N, NRHS), Y(N, NRHS)
    complex*16 :: RES(N), DY(N), Y_TAIL(N)
    integer :: IPIV(N), INFO
    logical :: COLEQU, IGNORE_CWISE
    double precision :: C(N), AYB(N), BERR_OUT(NRHS)
    double precision :: ERRS_N(NRHS, 3), ERRS_C(NRHS, 3)
    double precision :: RCOND, RTHRESH, DZ_UB
    integer :: ITHRESH
    double precision :: A_r(2*N*N), AF_r(2*N*N), B_r(2*N*NRHS), Y_r(2*N*NRHS)
    double precision :: RES_r(2*N), DY_r(2*N), YT_r(2*N)
    equivalence (A, A_r)
    equivalence (AF, AF_r)
    equivalence (B, B_r)
    equivalence (Y, Y_r)
    equivalence (RES, RES_r)
    equivalence (DY, DY_r)
    equivalence (Y_TAIL, YT_r)
    integer :: i

    A(1,1) = (4.0d0, 1.0d0); A(2,1) = (1.0d0, 0.0d0); A(3,1) = (0.5d0, 0.2d0)
    A(1,2) = (1.0d0,-0.5d0); A(2,2) = (3.0d0, 0.5d0); A(3,2) = (0.2d0, 0.1d0)
    A(1,3) = (0.3d0, 0.1d0); A(2,3) = (0.4d0,-0.2d0); A(3,3) = (2.5d0, 0.3d0)

    B(1,1) = (1.0d0, 0.5d0); B(2,1) = (2.0d0, -0.5d0); B(3,1) = (0.5d0, 1.0d0)

    call factor_and_solve(N, NRHS, A, AF, IPIV, B, Y, INFO)

    COLEQU = .true.
    do i = 1, N
      C(i) = 1.0d0
    end do
    IGNORE_CWISE = .false.
    RCOND = 1.0d-3
    ITHRESH = 10
    RTHRESH = 0.5d0
    DZ_UB = 0.25d0
    BERR_OUT(1) = 0.0d0
    ERRS_N = 0.0d0; ERRS_C = 0.0d0
    do i = 1, N
      Y_TAIL(i) = (0.0d0, 0.0d0)
      RES(i) = (0.0d0, 0.0d0)
      DY(i) = (0.0d0, 0.0d0)
      AYB(i) = 0.0d0
    end do
    INFO = 0

    call ZLA_GERFSX_EXTENDED(PREC_DOUBLE, BLAS_NO_TRANS, N, NRHS, A, N, &
      AF, N, IPIV, COLEQU, C, B, N, Y, N, BERR_OUT, 2, ERRS_N, ERRS_C, &
      RES, AYB, DY, Y_TAIL, RCOND, ITHRESH, RTHRESH, DZ_UB, IGNORE_CWISE, INFO)

    call begin_test('notrans_basic')
    call print_array('Y', Y_r, 2*N*NRHS)
    call print_array('BERR_OUT', BERR_OUT, NRHS)
    call print_array('ERRS_N', ERRS_N(1,1:3), 3)
    call print_array('ERRS_C', ERRS_C(1,1:3), 3)
    call print_scalar('INFO', dble(INFO))
    call end_test()
  end subroutine

  subroutine test_conjtrans_basic()
    integer, parameter :: N = 3, NRHS = 1
    complex*16 :: A(N, N), AF(N, N), B(N, NRHS), Y(N, NRHS)
    complex*16 :: RES(N), DY(N), Y_TAIL(N)
    integer :: IPIV(N), INFO
    logical :: COLEQU, IGNORE_CWISE
    double precision :: C(N), AYB(N), BERR_OUT(NRHS)
    double precision :: ERRS_N(NRHS, 3), ERRS_C(NRHS, 3)
    double precision :: RCOND, RTHRESH, DZ_UB
    integer :: ITHRESH
    double precision :: Y_r(2*N*NRHS)
    equivalence (Y, Y_r)
    integer :: i

    A(1,1) = (4.0d0, 1.0d0); A(2,1) = (1.0d0, 0.0d0); A(3,1) = (0.5d0, 0.2d0)
    A(1,2) = (1.0d0,-0.5d0); A(2,2) = (3.0d0, 0.5d0); A(3,2) = (0.2d0, 0.1d0)
    A(1,3) = (0.3d0, 0.1d0); A(2,3) = (0.4d0,-0.2d0); A(3,3) = (2.5d0, 0.3d0)

    B(1,1) = (1.0d0, 0.5d0); B(2,1) = (2.0d0, -0.5d0); B(3,1) = (0.5d0, 1.0d0)

    ! Factor A
    do i = 1, N*N
      AF(mod(i-1,N)+1, (i-1)/N+1) = A(mod(i-1,N)+1, (i-1)/N+1)
    end do
    call ZGETRF(N, N, AF, N, IPIV, INFO)
    ! Solve A^H * Y = B
    do i = 1, N
      Y(i, 1) = B(i, 1)
    end do
    call ZGETRS('C', N, 1, AF, N, IPIV, Y, N, INFO)

    COLEQU = .false.
    do i = 1, N
      C(i) = 1.0d0
    end do
    IGNORE_CWISE = .false.
    RCOND = 1.0d-3
    ITHRESH = 10
    RTHRESH = 0.5d0
    DZ_UB = 0.25d0
    BERR_OUT(1) = 0.0d0
    ERRS_N = 0.0d0; ERRS_C = 0.0d0
    do i = 1, N
      Y_TAIL(i) = (0.0d0, 0.0d0)
      RES(i) = (0.0d0, 0.0d0)
      DY(i) = (0.0d0, 0.0d0)
      AYB(i) = 0.0d0
    end do
    INFO = 0

    call ZLA_GERFSX_EXTENDED(PREC_DOUBLE, BLAS_CONJ_TRANS, N, NRHS, A, N, &
      AF, N, IPIV, COLEQU, C, B, N, Y, N, BERR_OUT, 2, ERRS_N, ERRS_C, &
      RES, AYB, DY, Y_TAIL, RCOND, ITHRESH, RTHRESH, DZ_UB, IGNORE_CWISE, INFO)

    call begin_test('conjtrans_basic')
    call print_array('Y', Y_r, 2*N*NRHS)
    call print_array('BERR_OUT', BERR_OUT, NRHS)
    call print_array('ERRS_N', ERRS_N(1,1:3), 3)
    call print_array('ERRS_C', ERRS_C(1,1:3), 3)
    call end_test()
  end subroutine

  subroutine test_no_equ()
    ! COLEQU = .false., N_NORMS=1 (only normwise bounds)
    integer, parameter :: N = 2, NRHS = 1
    complex*16 :: A(N, N), AF(N, N), B(N, NRHS), Y(N, NRHS)
    complex*16 :: RES(N), DY(N), Y_TAIL(N)
    integer :: IPIV(N), INFO
    logical :: COLEQU, IGNORE_CWISE
    double precision :: C(N), AYB(N), BERR_OUT(NRHS)
    double precision :: ERRS_N(NRHS, 3), ERRS_C(NRHS, 3)
    double precision :: RCOND, RTHRESH, DZ_UB
    integer :: ITHRESH
    double precision :: Y_r(2*N*NRHS)
    equivalence (Y, Y_r)
    integer :: i

    A(1,1) = (2.0d0, 0.5d0); A(2,1) = (0.5d0, 0.1d0)
    A(1,2) = (0.3d0,-0.1d0); A(2,2) = (3.0d0, 0.2d0)
    B(1,1) = (1.0d0, 0.0d0); B(2,1) = (0.5d0, 1.0d0)

    call factor_and_solve(N, NRHS, A, AF, IPIV, B, Y, INFO)

    COLEQU = .false.
    C = 1.0d0
    IGNORE_CWISE = .false.
    RCOND = 1.0d-2
    ITHRESH = 10
    RTHRESH = 0.5d0
    DZ_UB = 0.25d0
    BERR_OUT(1) = 0.0d0
    ERRS_N = 0.0d0; ERRS_C = 0.0d0
    do i = 1, N
      Y_TAIL(i) = (0.0d0, 0.0d0)
      RES(i) = (0.0d0, 0.0d0)
      DY(i) = (0.0d0, 0.0d0)
      AYB(i) = 0.0d0
    end do
    INFO = 0

    call ZLA_GERFSX_EXTENDED(PREC_DOUBLE, BLAS_NO_TRANS, N, NRHS, A, N, &
      AF, N, IPIV, COLEQU, C, B, N, Y, N, BERR_OUT, 1, ERRS_N, ERRS_C, &
      RES, AYB, DY, Y_TAIL, RCOND, ITHRESH, RTHRESH, DZ_UB, IGNORE_CWISE, INFO)

    call begin_test('no_equ')
    call print_array('Y', Y_r, 2*N*NRHS)
    call print_array('BERR_OUT', BERR_OUT, NRHS)
    call print_array('ERRS_N', ERRS_N(1,1:3), 3)
    call end_test()
  end subroutine

  subroutine test_n_zero()
    integer, parameter :: N = 0, NRHS = 1
    complex*16 :: A(1, 1), AF(1, 1), B(1, 1), Y(1, 1)
    complex*16 :: RES(1), DY(1), Y_TAIL(1)
    integer :: IPIV(1), INFO
    logical :: COLEQU, IGNORE_CWISE
    double precision :: C(1), AYB(1), BERR_OUT(1)
    double precision :: ERRS_N(1, 3), ERRS_C(1, 3)
    double precision :: RCOND, RTHRESH, DZ_UB
    integer :: ITHRESH

    COLEQU = .false.
    IGNORE_CWISE = .false.
    RCOND = 1.0d-3
    ITHRESH = 10
    RTHRESH = 0.5d0
    DZ_UB = 0.25d0
    BERR_OUT(1) = 42.0d0
    ERRS_N(1,1) = 99.0d0; ERRS_N(1,2) = 99.0d0; ERRS_N(1,3) = 99.0d0
    ERRS_C(1,1) = 88.0d0; ERRS_C(1,2) = 88.0d0; ERRS_C(1,3) = 88.0d0
    INFO = 0

    call ZLA_GERFSX_EXTENDED(PREC_DOUBLE, BLAS_NO_TRANS, N, 0, A, 1, &
      AF, 1, IPIV, COLEQU, C, B, 1, Y, 1, BERR_OUT, 2, ERRS_N, ERRS_C, &
      RES, AYB, DY, Y_TAIL, RCOND, ITHRESH, RTHRESH, DZ_UB, IGNORE_CWISE, INFO)

    call begin_test('n_zero')
    call print_scalar('BERR_OUT', BERR_OUT(1))
    call print_scalar('INFO', dble(INFO))
    call end_test()
  end subroutine

  subroutine test_ignore_cwise()
    integer, parameter :: N = 3, NRHS = 1
    complex*16 :: A(N, N), AF(N, N), B(N, NRHS), Y(N, NRHS)
    complex*16 :: RES(N), DY(N), Y_TAIL(N)
    integer :: IPIV(N), INFO
    logical :: COLEQU, IGNORE_CWISE
    double precision :: C(N), AYB(N), BERR_OUT(NRHS)
    double precision :: ERRS_N(NRHS, 3), ERRS_C(NRHS, 3)
    double precision :: RCOND, RTHRESH, DZ_UB
    integer :: ITHRESH
    double precision :: Y_r(2*N*NRHS)
    equivalence (Y, Y_r)
    integer :: i

    A(1,1) = (4.0d0, 1.0d0); A(2,1) = (1.0d0, 0.0d0); A(3,1) = (0.5d0, 0.2d0)
    A(1,2) = (1.0d0,-0.5d0); A(2,2) = (3.0d0, 0.5d0); A(3,2) = (0.2d0, 0.1d0)
    A(1,3) = (0.3d0, 0.1d0); A(2,3) = (0.4d0,-0.2d0); A(3,3) = (2.5d0, 0.3d0)
    B(1,1) = (1.0d0, 0.5d0); B(2,1) = (2.0d0, -0.5d0); B(3,1) = (0.5d0, 1.0d0)

    call factor_and_solve(N, NRHS, A, AF, IPIV, B, Y, INFO)

    COLEQU = .true.
    do i = 1, N
      C(i) = 1.0d0
    end do
    IGNORE_CWISE = .true.
    RCOND = 1.0d-3
    ITHRESH = 10
    RTHRESH = 0.5d0
    DZ_UB = 0.25d0
    BERR_OUT(1) = 0.0d0
    ERRS_N = 0.0d0; ERRS_C = 0.0d0
    do i = 1, N
      Y_TAIL(i) = (0.0d0, 0.0d0)
      RES(i) = (0.0d0, 0.0d0)
      DY(i) = (0.0d0, 0.0d0)
      AYB(i) = 0.0d0
    end do
    INFO = 0

    call ZLA_GERFSX_EXTENDED(PREC_DOUBLE, BLAS_NO_TRANS, N, NRHS, A, N, &
      AF, N, IPIV, COLEQU, C, B, N, Y, N, BERR_OUT, 1, ERRS_N, ERRS_C, &
      RES, AYB, DY, Y_TAIL, RCOND, ITHRESH, RTHRESH, DZ_UB, IGNORE_CWISE, INFO)

    call begin_test('ignore_cwise')
    call print_array('Y', Y_r, 2*N*NRHS)
    call print_array('BERR_OUT', BERR_OUT, NRHS)
    call print_array('ERRS_N', ERRS_N(1,1:3), 3)
    call end_test()
  end subroutine

end program

! External stub for XBLAS extra-precision gemv.  Not available in
! reference LAPACK; falls through to standard ZGEMV. This matches
! the JS implementation behavior (no XBLAS available).
subroutine BLAS_ZGEMV_X(trans_type, M, N, alpha, A, LDA, X, INCX, &
                        beta, Y, INCY, prec_type)
  implicit none
  integer, intent(in) :: trans_type, M, N, LDA, INCX, INCY, prec_type
  complex*16, intent(in) :: alpha, beta
  complex*16, intent(in) :: A(LDA, *), X(*)
  complex*16, intent(inout) :: Y(*)
  character :: trans
  if (trans_type == 111) then
    trans = 'N'
  else if (trans_type == 112) then
    trans = 'T'
  else
    trans = 'C'
  end if
  call ZGEMV(trans, M, N, alpha, A, LDA, X, INCX, beta, Y, INCY)
end subroutine

subroutine BLAS_ZGEMV2_X(trans_type, M, N, alpha, A, LDA, X, X_TAIL, &
                         INCX, beta, Y, INCY, prec_type)
  implicit none
  integer, intent(in) :: trans_type, M, N, LDA, INCX, INCY, prec_type
  complex*16, intent(in) :: alpha, beta
  complex*16, intent(in) :: A(LDA, *), X(*), X_TAIL(*)
  complex*16, intent(inout) :: Y(*)
  character :: trans
  integer :: i, lenx, ix
  complex*16 :: xsum(4096)
  if (trans_type == 111) then
    trans = 'N'
    lenx = N
  else if (trans_type == 112) then
    trans = 'T'
    lenx = M
  else
    trans = 'C'
    lenx = M
  end if
  ix = 1
  do i = 1, lenx
    xsum(i) = X(ix) + X_TAIL(i)
    ix = ix + INCX
  end do
  call ZGEMV(trans, M, N, alpha, A, LDA, xsum, 1, beta, Y, INCY)
end subroutine
