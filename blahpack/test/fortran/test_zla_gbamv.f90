program test_zla_gbamv
  use test_utils
  implicit none

  integer, parameter :: BLAS_NO_TRANS = 111
  integer, parameter :: BLAS_TRANS = 112
  integer, parameter :: BLAS_CONJ_TRANS = 113

  call test_notrans_basic()
  call test_trans_basic()
  call test_conjtrans_basic()
  call test_beta_zero()
  call test_beta_one_alpha_zero()
  call test_alpha_beta_scale()
  call test_neg_incx_incy()
  call test_rect_mlt_n()
  call test_rect_mgt_n()
  call test_kl_zero_ku_zero()

contains

  !--------------------------------------------------------------
  ! Helper 4x4 band matrix: KL=1, KU=2, LDAB=KL+KU+1=4.
  ! Full matrix:
  !   [a11 a12 a13  0 ]
  !   [a21 a22 a23 a24]
  !   [ 0  a32 a33 a34]
  !   [ 0   0  a43 a44]
  !
  ! Band storage (1-based) AB(KU+1+i-j, j) = A(i,j):
  !   Row 1 (2nd superdiag): *    *    a13  a24
  !   Row 2 (1st superdiag): *    a12  a23  a34
  !   Row 3 (diagonal):      a11  a22  a33  a44
  !   Row 4 (subdiag):       a21  a32  a43  *
  !--------------------------------------------------------------

  subroutine test_notrans_basic()
    integer, parameter :: M = 4, N = 4, KL = 1, KU = 2, LDAB = 4
    complex*16 :: AB(LDAB, N), x(N)
    double precision :: y(M)
    double precision :: AB_r(2*LDAB*N), x_r(2*N)
    equivalence (AB, AB_r)
    equivalence (x, x_r)
    double precision :: alpha, beta

    AB = (0.0d0, 0.0d0)
    ! Col 1: rows 3-4 valid
    AB(3,1) = (1.0d0, 2.0d0)   ! a11
    AB(4,1) = (-3.0d0, 1.0d0)  ! a21
    ! Col 2: rows 2-4 valid
    AB(2,2) = (2.0d0, -1.0d0)  ! a12
    AB(3,2) = (1.5d0, 0.0d0)   ! a22
    AB(4,2) = (-2.0d0, 3.0d0)  ! a32
    ! Col 3: rows 1-4 valid
    AB(1,3) = (0.0d0, 4.0d0)   ! a13
    AB(2,3) = (1.0d0, 1.0d0)   ! a23
    AB(3,3) = (2.5d0, -1.5d0)  ! a33
    AB(4,3) = (0.75d0, 0.25d0) ! a43
    ! Col 4: rows 1-3 valid (a24, a34, a44)
    AB(1,4) = (-1.0d0, 0.5d0)  ! a24
    AB(2,4) = (3.0d0, -2.0d0)  ! a34
    AB(3,4) = (0.5d0, 0.5d0)   ! a44

    x(1) = (1.0d0, 1.0d0)
    x(2) = (-2.0d0, 0.5d0)
    x(3) = (0.25d0, -0.75d0)
    x(4) = (1.5d0, 0.0d0)

    y(1) = 1.0d0
    y(2) = -2.0d0
    y(3) = 3.0d0
    y(4) = -0.5d0

    alpha = 1.5d0
    beta = 0.5d0
    call ZLA_GBAMV(BLAS_NO_TRANS, M, N, KL, KU, alpha, AB, LDAB, x, 1, beta, y, 1)
    call begin_test('notrans_basic')
    call print_array('AB', AB_r, 2*LDAB*N)
    call print_array('x', x_r, 2*N)
    call print_array('y', y, M)
    call end_test()
  end subroutine

  subroutine test_trans_basic()
    integer, parameter :: M = 4, N = 4, KL = 1, KU = 2, LDAB = 4
    complex*16 :: AB(LDAB, N), x(M)
    double precision :: y(N)
    double precision :: AB_r(2*LDAB*N), x_r(2*M)
    equivalence (AB, AB_r)
    equivalence (x, x_r)

    AB = (0.0d0, 0.0d0)
    AB(3,1) = (1.0d0, 2.0d0)
    AB(4,1) = (-3.0d0, 1.0d0)
    AB(2,2) = (2.0d0, -1.0d0)
    AB(3,2) = (1.5d0, 0.0d0)
    AB(4,2) = (-2.0d0, 3.0d0)
    AB(1,3) = (0.0d0, 4.0d0)
    AB(2,3) = (1.0d0, 1.0d0)
    AB(3,3) = (2.5d0, -1.5d0)
    AB(4,3) = (0.75d0, 0.25d0)
    AB(1,4) = (-1.0d0, 0.5d0)
    AB(2,4) = (3.0d0, -2.0d0)
    AB(3,4) = (0.5d0, 0.5d0)

    x(1) = (1.0d0, 1.0d0)
    x(2) = (-2.0d0, 0.5d0)
    x(3) = (0.25d0, -0.75d0)
    x(4) = (1.5d0, 0.0d0)

    y(1) = 1.0d0
    y(2) = -2.0d0
    y(3) = 3.0d0
    y(4) = -0.5d0

    call ZLA_GBAMV(BLAS_TRANS, M, N, KL, KU, 1.0d0, AB, LDAB, x, 1, 1.0d0, y, 1)
    call begin_test('trans_basic')
    call print_array('y', y, N)
    call end_test()
  end subroutine

  subroutine test_conjtrans_basic()
    ! |conj(a)|=|a|, so conj-trans matches trans.
    integer, parameter :: M = 4, N = 4, KL = 1, KU = 2, LDAB = 4
    complex*16 :: AB(LDAB, N), x(M)
    double precision :: y(N)
    double precision :: AB_r(2*LDAB*N), x_r(2*M)
    equivalence (AB, AB_r)
    equivalence (x, x_r)

    AB = (0.0d0, 0.0d0)
    AB(3,1) = (1.0d0, 2.0d0)
    AB(4,1) = (-3.0d0, 1.0d0)
    AB(2,2) = (2.0d0, -1.0d0)
    AB(3,2) = (1.5d0, 0.0d0)
    AB(4,2) = (-2.0d0, 3.0d0)
    AB(1,3) = (0.0d0, 4.0d0)
    AB(2,3) = (1.0d0, 1.0d0)
    AB(3,3) = (2.5d0, -1.5d0)
    AB(4,3) = (0.75d0, 0.25d0)
    AB(1,4) = (-1.0d0, 0.5d0)
    AB(2,4) = (3.0d0, -2.0d0)
    AB(3,4) = (0.5d0, 0.5d0)

    x(1) = (1.0d0, 1.0d0)
    x(2) = (-2.0d0, 0.5d0)
    x(3) = (0.25d0, -0.75d0)
    x(4) = (1.5d0, 0.0d0)

    y = 0.0d0

    call ZLA_GBAMV(BLAS_CONJ_TRANS, M, N, KL, KU, 2.0d0, AB, LDAB, x, 1, 0.0d0, y, 1)
    call begin_test('conjtrans_basic')
    call print_array('y', y, N)
    call end_test()
  end subroutine

  subroutine test_beta_zero()
    integer, parameter :: M = 3, N = 3, KL = 1, KU = 1, LDAB = 3
    complex*16 :: AB(LDAB, N), x(N)
    double precision :: y(M)

    AB = (0.0d0, 0.0d0)
    ! Tri-diagonal 3x3
    AB(2,1) = (1.0d0, 0.0d0)   ! a11
    AB(3,1) = (2.0d0, 0.0d0)   ! a21
    AB(1,2) = (3.0d0, 0.0d0)   ! a12
    AB(2,2) = (4.0d0, 0.0d0)   ! a22
    AB(3,2) = (5.0d0, 0.0d0)   ! a32
    AB(1,3) = (6.0d0, 0.0d0)   ! a23
    AB(2,3) = (7.0d0, 0.0d0)   ! a33

    x(1) = (1.0d0, 0.0d0)
    x(2) = (1.0d0, 0.0d0)
    x(3) = (1.0d0, 0.0d0)
    y(1) = 99.0d0
    y(2) = 99.0d0
    y(3) = 99.0d0

    call ZLA_GBAMV(BLAS_NO_TRANS, M, N, KL, KU, 1.0d0, AB, LDAB, x, 1, 0.0d0, y, 1)
    call begin_test('beta_zero')
    call print_array('y', y, M)
    call end_test()
  end subroutine

  subroutine test_beta_one_alpha_zero()
    integer, parameter :: M = 3, N = 3, KL = 1, KU = 1, LDAB = 3
    complex*16 :: AB(LDAB, N), x(N)
    double precision :: y(M)

    AB = (0.0d0, 0.0d0)
    AB(2,1) = (1.0d0, 0.0d0)
    AB(3,1) = (2.0d0, 0.0d0)
    AB(1,2) = (3.0d0, 0.0d0)
    AB(2,2) = (4.0d0, 0.0d0)
    AB(3,2) = (5.0d0, 0.0d0)
    AB(1,3) = (6.0d0, 0.0d0)
    AB(2,3) = (7.0d0, 0.0d0)

    x(1) = (1.0d0, 0.0d0)
    x(2) = (1.0d0, 0.0d0)
    x(3) = (1.0d0, 0.0d0)
    y(1) = 7.0d0
    y(2) = -3.0d0
    y(3) = 2.5d0

    call ZLA_GBAMV(BLAS_NO_TRANS, M, N, KL, KU, 0.0d0, AB, LDAB, x, 1, 1.0d0, y, 1)
    call begin_test('beta_one_alpha_zero')
    call print_array('y', y, M)
    call end_test()
  end subroutine

  subroutine test_alpha_beta_scale()
    integer, parameter :: M = 3, N = 3, KL = 1, KU = 1, LDAB = 3
    complex*16 :: AB(LDAB, N), x(N)
    double precision :: y(M)

    AB = (0.0d0, 0.0d0)
    AB(2,1) = (1.0d0, 1.0d0)
    AB(3,1) = (2.0d0, -1.0d0)
    AB(1,2) = (0.5d0, 0.5d0)
    AB(2,2) = (3.0d0, 2.0d0)
    AB(3,2) = (1.0d0, 0.0d0)
    AB(1,3) = (-1.0d0, 2.0d0)
    AB(2,3) = (2.5d0, -0.5d0)

    x(1) = (1.0d0, 2.0d0)
    x(2) = (-1.0d0, 1.0d0)
    x(3) = (0.5d0, 0.5d0)
    y(1) = 2.0d0
    y(2) = -4.0d0
    y(3) = 1.0d0

    call ZLA_GBAMV(BLAS_NO_TRANS, M, N, KL, KU, 2.0d0, AB, LDAB, x, 1, 0.5d0, y, 1)
    call begin_test('alpha_beta_scale')
    call print_array('y', y, M)
    call end_test()
  end subroutine

  subroutine test_neg_incx_incy()
    integer, parameter :: M = 3, N = 3, KL = 1, KU = 1, LDAB = 3
    complex*16 :: AB(LDAB, N), x(6)
    double precision :: y(6)

    AB = (0.0d0, 0.0d0)
    AB(2,1) = (1.0d0, 1.0d0)
    AB(3,1) = (2.0d0, 0.0d0)
    AB(1,2) = (0.5d0, 0.5d0)
    AB(2,2) = (3.0d0, 0.0d0)
    AB(3,2) = (1.0d0, -1.0d0)
    AB(1,3) = (-1.0d0, 0.0d0)
    AB(2,3) = (2.5d0, 0.0d0)

    x = (0.0d0, 0.0d0)
    x(1) = (1.0d0, 0.0d0)
    x(3) = (2.0d0, 1.0d0)
    x(5) = (-1.0d0, 0.5d0)

    y = 0.0d0
    y(1) = 0.1d0
    y(3) = 0.2d0
    y(5) = 0.3d0

    call ZLA_GBAMV(BLAS_NO_TRANS, M, N, KL, KU, 1.0d0, AB, LDAB, x, -2, 1.0d0, y, -2)
    call begin_test('neg_incx_incy')
    call print_array('y', y, 6)
    call end_test()
  end subroutine

  subroutine test_rect_mlt_n()
    ! M=3 < N=4, KL=1, KU=1, TRANS=N -> LENY=M=3, LENX=N=4
    integer, parameter :: M = 3, N = 4, KL = 1, KU = 1, LDAB = 3
    complex*16 :: AB(LDAB, N), x(N)
    double precision :: y(M)

    AB = (0.0d0, 0.0d0)
    AB(2,1) = (1.0d0, 0.5d0)   ! a11
    AB(3,1) = (-1.0d0, 2.0d0)  ! a21
    AB(1,2) = (2.0d0, -1.0d0)  ! a12
    AB(2,2) = (0.0d0, 1.5d0)   ! a22
    AB(3,2) = (1.0d0, 1.0d0)   ! a32
    AB(1,3) = (3.0d0, 1.0d0)   ! a23
    AB(2,3) = (1.0d0, -2.0d0)  ! a33
    AB(1,4) = (-0.5d0, 0.5d0)  ! a34

    x(1) = (1.0d0, 1.0d0)
    x(2) = (0.5d0, -0.5d0)
    x(3) = (-1.0d0, 0.0d0)
    x(4) = (2.0d0, 1.0d0)
    y(1) = 1.0d0
    y(2) = 2.0d0
    y(3) = -1.0d0

    call ZLA_GBAMV(BLAS_NO_TRANS, M, N, KL, KU, 1.0d0, AB, LDAB, x, 1, 1.0d0, y, 1)
    call begin_test('rect_mlt_n')
    call print_array('y', y, M)
    call end_test()
  end subroutine

  subroutine test_rect_mgt_n()
    ! M=4 > N=3, TRANS=T -> LENY=N=3, LENX=M=4
    integer, parameter :: M = 4, N = 3, KL = 2, KU = 1, LDAB = 4
    complex*16 :: AB(LDAB, N), x(M)
    double precision :: y(N)

    AB = (0.0d0, 0.0d0)
    ! KU=1 -> 1 super-diag; KL=2 -> 2 sub-diags; LDAB=4
    AB(2,1) = (1.0d0, 0.5d0)   ! a11
    AB(3,1) = (-1.0d0, 2.0d0)  ! a21
    AB(4,1) = (0.0d0, 1.0d0)   ! a31
    AB(1,2) = (2.0d0, -1.0d0)  ! a12
    AB(2,2) = (0.0d0, 1.5d0)   ! a22
    AB(3,2) = (1.0d0, 1.0d0)   ! a32
    AB(4,2) = (-1.0d0, 0.0d0)  ! a42
    AB(1,3) = (3.0d0, 1.0d0)   ! a23
    AB(2,3) = (1.0d0, -2.0d0)  ! a33
    AB(3,3) = (-0.5d0, 0.5d0)  ! a43

    x(1) = (1.0d0, 1.0d0)
    x(2) = (0.5d0, -0.5d0)
    x(3) = (-1.0d0, 0.0d0)
    x(4) = (2.0d0, 1.0d0)
    y(1) = 0.0d0
    y(2) = 0.0d0
    y(3) = 0.0d0

    call ZLA_GBAMV(BLAS_TRANS, M, N, KL, KU, 1.0d0, AB, LDAB, x, 1, 0.0d0, y, 1)
    call begin_test('rect_mgt_n')
    call print_array('y', y, N)
    call end_test()
  end subroutine

  subroutine test_n_zero()
    complex*16 :: AB(1, 1), x(1)
    double precision :: y(2)
    AB(1,1) = (0.0d0, 0.0d0)
    x(1) = (0.0d0, 0.0d0)
    y(1) = 5.0d0
    y(2) = 6.0d0
    call ZLA_GBAMV(BLAS_NO_TRANS, 2, 0, 0, 0, 1.0d0, AB, 1, x, 1, 1.0d0, y, 1)
    call begin_test('n_zero')
    call print_array('y', y, 2)
    call end_test()
  end subroutine

  subroutine test_m_zero()
    complex*16 :: AB(1, 2), x(2)
    double precision :: y(1)
    AB = (0.0d0, 0.0d0)
    x(1) = (0.0d0, 0.0d0)
    x(2) = (0.0d0, 0.0d0)
    y(1) = 5.0d0
    call ZLA_GBAMV(BLAS_NO_TRANS, 0, 2, 0, 0, 1.0d0, AB, 1, x, 1, 1.0d0, y, 1)
    call begin_test('m_zero')
    call print_array('y', y, 1)
    call end_test()
  end subroutine

  subroutine test_kl_zero_ku_zero()
    ! Diagonal only: KL=KU=0, LDAB=1
    integer, parameter :: M = 3, N = 3, KL = 0, KU = 0, LDAB = 1
    complex*16 :: AB(LDAB, N), x(N)
    double precision :: y(M)

    AB(1,1) = (2.0d0, 1.0d0)
    AB(1,2) = (-1.0d0, 3.0d0)
    AB(1,3) = (0.5d0, -0.5d0)

    x(1) = (1.0d0, 0.0d0)
    x(2) = (0.0d0, 2.0d0)
    x(3) = (-1.0d0, 1.0d0)
    y(1) = 1.0d0
    y(2) = -1.0d0
    y(3) = 0.5d0

    call ZLA_GBAMV(BLAS_NO_TRANS, M, N, KL, KU, 1.0d0, AB, LDAB, x, 1, 1.0d0, y, 1)
    call begin_test('kl_zero_ku_zero')
    call print_array('y', y, M)
    call end_test()
  end subroutine

end program
