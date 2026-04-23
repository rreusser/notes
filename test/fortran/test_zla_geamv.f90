program test_zla_geamv
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
  call test_alpha_two_beta_half()
  call test_neg_incx_incy()
  call test_rect_mlt_n()
  call test_rect_mgt_n()
  call test_n_zero()
  call test_m_zero()

contains

  subroutine test_notrans_basic()
    ! 3x3 complex matrix, TRANS=N: y := alpha*|A|*|x| + beta*|y|
    integer, parameter :: N = 3, M = 3
    complex*16 :: A(M, N), x(N)
    double precision :: y(M)
    double precision :: A_r(2*M*N), x_r(2*N)
    equivalence (A, A_r)
    equivalence (x, x_r)
    double precision :: alpha, beta

    A(1,1) = (1.0d0, 2.0d0); A(2,1) = (-3.0d0, 1.0d0); A(3,1) = (0.5d0, -0.5d0)
    A(1,2) = (2.0d0,-1.0d0); A(2,2) = (1.5d0, 0.0d0); A(3,2) = (-2.0d0, 3.0d0)
    A(1,3) = (0.0d0, 4.0d0); A(2,3) = (1.0d0, 1.0d0); A(3,3) = (2.5d0,-1.5d0)

    x(1) = (1.0d0, 1.0d0); x(2) = (-2.0d0, 0.5d0); x(3) = (0.25d0,-0.75d0)
    y(1) = 1.0d0; y(2) = -2.0d0; y(3) = 3.0d0

    alpha = 1.5d0
    beta = 0.5d0
    call ZLA_GEAMV(BLAS_NO_TRANS, M, N, alpha, A, M, x, 1, beta, y, 1)
    call begin_test('notrans_basic')
    call print_array('A', A_r, 2*M*N)
    call print_array('x', x_r, 2*N)
    call print_array('y', y, M)
    call end_test()
  end subroutine

  subroutine test_trans_basic()
    integer, parameter :: N = 3, M = 3
    complex*16 :: A(M, N), x(M)
    double precision :: y(N)
    double precision :: A_r(2*M*N), x_r(2*M)
    equivalence (A, A_r)
    equivalence (x, x_r)
    double precision :: alpha, beta

    A(1,1) = (1.0d0, 2.0d0); A(2,1) = (-3.0d0, 1.0d0); A(3,1) = (0.5d0, -0.5d0)
    A(1,2) = (2.0d0,-1.0d0); A(2,2) = (1.5d0, 0.0d0); A(3,2) = (-2.0d0, 3.0d0)
    A(1,3) = (0.0d0, 4.0d0); A(2,3) = (1.0d0, 1.0d0); A(3,3) = (2.5d0,-1.5d0)

    x(1) = (1.0d0, 1.0d0); x(2) = (-2.0d0, 0.5d0); x(3) = (0.25d0,-0.75d0)
    y(1) = 1.0d0; y(2) = -2.0d0; y(3) = 3.0d0

    alpha = 1.0d0
    beta = 1.0d0
    call ZLA_GEAMV(BLAS_TRANS, M, N, alpha, A, M, x, 1, beta, y, 1)
    call begin_test('trans_basic')
    call print_array('A', A_r, 2*M*N)
    call print_array('x', x_r, 2*M)
    call print_array('y', y, N)
    call end_test()
  end subroutine

  subroutine test_conjtrans_basic()
    ! Same as trans since |conj(a)| = |a|
    integer, parameter :: N = 3, M = 3
    complex*16 :: A(M, N), x(M)
    double precision :: y(N)
    double precision :: A_r(2*M*N), x_r(2*M)
    equivalence (A, A_r)
    equivalence (x, x_r)
    double precision :: alpha, beta

    A(1,1) = (1.0d0, 2.0d0); A(2,1) = (-3.0d0, 1.0d0); A(3,1) = (0.5d0, -0.5d0)
    A(1,2) = (2.0d0,-1.0d0); A(2,2) = (1.5d0, 0.0d0); A(3,2) = (-2.0d0, 3.0d0)
    A(1,3) = (0.0d0, 4.0d0); A(2,3) = (1.0d0, 1.0d0); A(3,3) = (2.5d0,-1.5d0)

    x(1) = (1.0d0, 1.0d0); x(2) = (-2.0d0, 0.5d0); x(3) = (0.25d0,-0.75d0)
    y(1) = 0.0d0; y(2) = 0.0d0; y(3) = 0.0d0

    alpha = 2.0d0
    beta = 0.0d0
    call ZLA_GEAMV(BLAS_CONJ_TRANS, M, N, alpha, A, M, x, 1, beta, y, 1)
    call begin_test('conjtrans_basic')
    call print_array('A', A_r, 2*M*N)
    call print_array('x', x_r, 2*M)
    call print_array('y', y, N)
    call end_test()
  end subroutine

  subroutine test_beta_zero()
    integer, parameter :: N = 2, M = 2
    complex*16 :: A(M, N), x(N)
    double precision :: y(M)
    double precision :: A_r(2*M*N), x_r(2*N)
    equivalence (A, A_r)
    equivalence (x, x_r)

    A(1,1) = (1.0d0, 0.0d0); A(2,1) = (2.0d0, 0.0d0)
    A(1,2) = (3.0d0, 0.0d0); A(2,2) = (4.0d0, 0.0d0)
    x(1) = (1.0d0, 0.0d0); x(2) = (1.0d0, 0.0d0)
    y(1) = 99.0d0; y(2) = 99.0d0

    call ZLA_GEAMV(BLAS_NO_TRANS, M, N, 1.0d0, A, M, x, 1, 0.0d0, y, 1)
    call begin_test('beta_zero')
    call print_array('y', y, M)
    call end_test()
  end subroutine

  subroutine test_beta_one_alpha_zero()
    ! Quick return — y unchanged
    integer, parameter :: N = 2, M = 2
    complex*16 :: A(M, N), x(N)
    double precision :: y(M)

    A(1,1) = (1.0d0, 0.0d0); A(2,1) = (2.0d0, 0.0d0)
    A(1,2) = (3.0d0, 0.0d0); A(2,2) = (4.0d0, 0.0d0)
    x(1) = (1.0d0, 0.0d0); x(2) = (1.0d0, 0.0d0)
    y(1) = 7.0d0; y(2) = -3.0d0

    call ZLA_GEAMV(BLAS_NO_TRANS, M, N, 0.0d0, A, M, x, 1, 1.0d0, y, 1)
    call begin_test('beta_one_alpha_zero')
    call print_array('y', y, M)
    call end_test()
  end subroutine

  subroutine test_alpha_two_beta_half()
    integer, parameter :: N = 2, M = 2
    complex*16 :: A(M, N), x(N)
    double precision :: y(M)
    double precision :: A_r(2*M*N), x_r(2*N)
    equivalence (A, A_r)
    equivalence (x, x_r)

    A(1,1) = (1.0d0, 1.0d0); A(2,1) = (2.0d0, -1.0d0)
    A(1,2) = (0.5d0, 0.5d0); A(2,2) = (3.0d0, 2.0d0)
    x(1) = (1.0d0, 2.0d0); x(2) = (-1.0d0, 1.0d0)
    y(1) = 2.0d0; y(2) = -4.0d0

    call ZLA_GEAMV(BLAS_NO_TRANS, M, N, 2.0d0, A, M, x, 1, 0.5d0, y, 1)
    call begin_test('alpha_two_beta_half')
    call print_array('y', y, M)
    call end_test()
  end subroutine

  subroutine test_neg_incx_incy()
    ! Non-unit / negative strides with TRANS=N
    integer, parameter :: N = 2, M = 3
    complex*16 :: A(M, N), x(4)
    double precision :: y(6)
    double precision :: A_r(2*M*N), x_r(8)
    equivalence (A, A_r)
    equivalence (x, x_r)

    A(1,1) = (1.0d0, 0.0d0); A(2,1) = (2.0d0, 0.0d0); A(3,1) = (3.0d0, 0.0d0)
    A(1,2) = (4.0d0, 0.0d0); A(2,2) = (5.0d0, 0.0d0); A(3,2) = (6.0d0, 0.0d0)
    ! X has INCX=2, LENX=N=2, so entries at 1 and 3 (Fortran 1-based) when INCX > 0
    ! With INCX=-2, KX = 1 - (N-1)*(-2) = 1 + 2 = 3; uses X(3), X(1)
    x(1) = (1.0d0, 0.0d0); x(2) = (0.0d0, 0.0d0)
    x(3) = (2.0d0, 0.0d0); x(4) = (0.0d0, 0.0d0)
    ! Y stride=-2, LENY=M=3, KY = 1 - 2*(-2) = 5; uses Y(5), Y(3), Y(1)
    y = 0.0d0
    y(1) = 0.1d0; y(3) = 0.2d0; y(5) = 0.3d0

    call ZLA_GEAMV(BLAS_NO_TRANS, M, N, 1.0d0, A, M, x, -2, 1.0d0, y, -2)
    call begin_test('neg_incx_incy')
    call print_array('y', y, 6)
    call end_test()
  end subroutine

  subroutine test_rect_mlt_n()
    ! M=2 < N=4, TRANS=N, LENX=N=4, LENY=M=2
    integer, parameter :: M = 2, N = 4
    complex*16 :: A(M, N), x(N)
    double precision :: y(M)
    double precision :: A_r(2*M*N), x_r(2*N)
    equivalence (A, A_r)
    equivalence (x, x_r)

    A(1,1) = (1.0d0, 0.5d0); A(2,1) = (-1.0d0, 2.0d0)
    A(1,2) = (2.0d0,-1.0d0); A(2,2) = (0.0d0, 1.5d0)
    A(1,3) = (3.0d0, 1.0d0); A(2,3) = (1.0d0, -2.0d0)
    A(1,4) = (-0.5d0, 0.5d0); A(2,4) = (2.0d0, 0.0d0)
    x(1) = (1.0d0, 1.0d0); x(2) = (0.5d0, -0.5d0)
    x(3) = (-1.0d0, 0.0d0); x(4) = (2.0d0, 1.0d0)
    y(1) = 1.0d0; y(2) = 2.0d0

    call ZLA_GEAMV(BLAS_NO_TRANS, M, N, 1.0d0, A, M, x, 1, 1.0d0, y, 1)
    call begin_test('rect_mlt_n')
    call print_array('y', y, M)
    call end_test()
  end subroutine

  subroutine test_rect_mgt_n()
    ! M=4 > N=2, TRANS=T, LENX=M=4, LENY=N=2
    integer, parameter :: M = 4, N = 2
    complex*16 :: A(M, N), x(M)
    double precision :: y(N)
    double precision :: A_r(2*M*N), x_r(2*M)
    equivalence (A, A_r)
    equivalence (x, x_r)

    A(1,1) = (1.0d0, 0.5d0); A(2,1) = (-1.0d0, 2.0d0); A(3,1) = (0.0d0, 0.0d0); A(4,1) = (2.5d0, -0.5d0)
    A(1,2) = (2.0d0,-1.0d0); A(2,2) = (0.0d0, 1.5d0); A(3,2) = (1.0d0, 1.0d0); A(4,2) = (-1.0d0, 0.0d0)
    x(1) = (1.0d0, 1.0d0); x(2) = (0.5d0, -0.5d0)
    x(3) = (-1.0d0, 0.0d0); x(4) = (2.0d0, 1.0d0)
    y(1) = 0.0d0; y(2) = 0.0d0

    call ZLA_GEAMV(BLAS_TRANS, M, N, 1.0d0, A, M, x, 1, 0.0d0, y, 1)
    call begin_test('rect_mgt_n')
    call print_array('y', y, N)
    call end_test()
  end subroutine

  subroutine test_n_zero()
    ! Quick return N=0
    complex*16 :: A(2, 1), x(1)
    double precision :: y(2)

    A(1,1) = (0.0d0, 0.0d0); A(2,1) = (0.0d0, 0.0d0)
    x(1) = (0.0d0, 0.0d0)
    y(1) = 5.0d0; y(2) = 6.0d0

    call ZLA_GEAMV(BLAS_NO_TRANS, 2, 0, 1.0d0, A, 2, x, 1, 1.0d0, y, 1)
    call begin_test('n_zero')
    call print_array('y', y, 2)
    call end_test()
  end subroutine

  subroutine test_m_zero()
    complex*16 :: A(1, 2), x(2)
    double precision :: y(1)

    A(1,1) = (0.0d0, 0.0d0); A(1,2) = (0.0d0, 0.0d0)
    x(1) = (0.0d0, 0.0d0); x(2) = (0.0d0, 0.0d0)
    y(1) = 5.0d0

    call ZLA_GEAMV(BLAS_NO_TRANS, 0, 2, 1.0d0, A, 1, x, 1, 1.0d0, y, 1)
    call begin_test('m_zero')
    call print_array('y', y, 1)
    call end_test()
  end subroutine

end program
