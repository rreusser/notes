program test_zlarfb_gett
  use test_utils
  implicit none

  ! Tests zlarfb_gett:
  !   - IDENT = 'N' (V1 is unit lower-triangular, stored in A1)
  !   - IDENT = 'I' (V1 is identity, not stored)
  !   - Cases with N > K and N = K
  !   - Edge case M = 0
  !
  ! Dimensions are fixed per case so that the declared LD matches the used
  ! leading dimension (avoiding the EQUIVALENCE stride trap).

  call case_notident_nbig()
  call case_notident_nequal()
  call case_ident_nbig()
  call case_ident_nequal()
  call case_notident_m0()
  call case_ident_m0()

contains

  subroutine fill_T(T, K, LDT)
    integer, intent(in) :: K, LDT
    complex*16, intent(out) :: T(LDT, K)
    integer :: i, j
    do j = 1, K
      do i = 1, LDT
        T(i, j) = cmplx(0.0d0, 0.0d0, kind=8)
      end do
    end do
    ! Upper triangular T with deterministic complex values.
    do j = 1, K
      do i = 1, j
        T(i, j) = cmplx(0.1d0*(i+j) + 0.5d0, -0.07d0*(i-j) + 0.1d0, kind=8)
      end do
    end do
  end subroutine fill_T

  subroutine fill_A(A, K, N, LDA)
    integer, intent(in) :: K, N, LDA
    complex*16, intent(out) :: A(LDA, N)
    integer :: i, j
    ! A is K-by-N (upper-trapezoidal part + below-diagonal for V1).
    do j = 1, N
      do i = 1, LDA
        A(i, j) = cmplx(0.0d0, 0.0d0, kind=8)
      end do
    end do
    do j = 1, N
      do i = 1, K
        A(i, j) = cmplx(0.2d0 + 0.1d0*i - 0.05d0*j, 0.3d0 - 0.04d0*i + 0.02d0*j, kind=8)
      end do
    end do
  end subroutine fill_A

  subroutine fill_B(B, M, N, LDB)
    integer, intent(in) :: M, N, LDB
    complex*16, intent(out) :: B(LDB, N)
    integer :: i, j
    do j = 1, N
      do i = 1, LDB
        B(i, j) = cmplx(0.0d0, 0.0d0, kind=8)
      end do
    end do
    do j = 1, N
      do i = 1, M
        B(i, j) = cmplx(-0.1d0 + 0.03d0*i + 0.07d0*j, 0.2d0 + 0.02d0*i - 0.04d0*j, kind=8)
      end do
    end do
  end subroutine fill_B

  subroutine case_notident_nbig()
    integer, parameter :: K = 3, M = 4, N = 5
    complex*16 :: T(K, K), A(K, N), B(M, N), WORK(K, max(K, N-K))
    double precision :: T_r(2*K*K), A_r(2*K*N), B_r(2*M*N)
    equivalence (T, T_r)
    equivalence (A, A_r)
    equivalence (B, B_r)
    call fill_T(T, K, K)
    call fill_A(A, K, N, K)
    call fill_B(B, M, N, M)
    call zlarfb_gett('N', M, N, K, T, K, A, K, B, M, WORK, K)
    call begin_test('notident_nbig')
    call print_array('A', A_r, 2*K*N)
    call print_array('B', B_r, 2*M*N)
    call end_test()
  end subroutine case_notident_nbig

  subroutine case_notident_nequal()
    integer, parameter :: K = 3, M = 4, N = 3
    complex*16 :: T(K, K), A(K, N), B(M, N), WORK(K, K)
    double precision :: T_r(2*K*K), A_r(2*K*N), B_r(2*M*N)
    equivalence (T, T_r)
    equivalence (A, A_r)
    equivalence (B, B_r)
    call fill_T(T, K, K)
    call fill_A(A, K, N, K)
    call fill_B(B, M, N, M)
    call zlarfb_gett('N', M, N, K, T, K, A, K, B, M, WORK, K)
    call begin_test('notident_nequal')
    call print_array('A', A_r, 2*K*N)
    call print_array('B', B_r, 2*M*N)
    call end_test()
  end subroutine case_notident_nequal

  subroutine case_ident_nbig()
    integer, parameter :: K = 3, M = 4, N = 5
    complex*16 :: T(K, K), A(K, N), B(M, N), WORK(K, max(K, N-K))
    double precision :: T_r(2*K*K), A_r(2*K*N), B_r(2*M*N)
    equivalence (T, T_r)
    equivalence (A, A_r)
    equivalence (B, B_r)
    call fill_T(T, K, K)
    call fill_A(A, K, N, K)
    call fill_B(B, M, N, M)
    call zlarfb_gett('I', M, N, K, T, K, A, K, B, M, WORK, K)
    call begin_test('ident_nbig')
    call print_array('A', A_r, 2*K*N)
    call print_array('B', B_r, 2*M*N)
    call end_test()
  end subroutine case_ident_nbig

  subroutine case_ident_nequal()
    integer, parameter :: K = 3, M = 4, N = 3
    complex*16 :: T(K, K), A(K, N), B(M, N), WORK(K, K)
    double precision :: T_r(2*K*K), A_r(2*K*N), B_r(2*M*N)
    equivalence (T, T_r)
    equivalence (A, A_r)
    equivalence (B, B_r)
    call fill_T(T, K, K)
    call fill_A(A, K, N, K)
    call fill_B(B, M, N, M)
    call zlarfb_gett('I', M, N, K, T, K, A, K, B, M, WORK, K)
    call begin_test('ident_nequal')
    call print_array('A', A_r, 2*K*N)
    call print_array('B', B_r, 2*M*N)
    call end_test()
  end subroutine case_ident_nequal

  subroutine case_notident_m0()
    integer, parameter :: K = 3, M = 1, N = 5
    complex*16 :: T(K, K), A(K, N), B(1, N), WORK(K, max(K, N-K))
    double precision :: T_r(2*K*K), A_r(2*K*N)
    equivalence (T, T_r)
    equivalence (A, A_r)
    call fill_T(T, K, K)
    call fill_A(A, K, N, K)
    ! M = 0: B is declared minimally; not touched
    call zlarfb_gett('N', 0, N, K, T, K, A, K, B, 1, WORK, K)
    call begin_test('notident_m0')
    call print_array('A', A_r, 2*K*N)
    call end_test()
  end subroutine case_notident_m0

  subroutine case_ident_m0()
    integer, parameter :: K = 3, M = 1, N = 5
    complex*16 :: T(K, K), A(K, N), B(1, N), WORK(K, max(K, N-K))
    double precision :: T_r(2*K*K), A_r(2*K*N)
    equivalence (T, T_r)
    equivalence (A, A_r)
    call fill_T(T, K, K)
    call fill_A(A, K, N, K)
    call zlarfb_gett('I', 0, N, K, T, K, A, K, B, 1, WORK, K)
    call begin_test('ident_m0')
    call print_array('A', A_r, 2*K*N)
    call end_test()
  end subroutine case_ident_m0

end program test_zlarfb_gett
