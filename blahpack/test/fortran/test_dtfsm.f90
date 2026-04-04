program test_dtfsm
  use test_utils
  implicit none

  ! Test all 16 dispatch paths for odd (N=3 or M=3) and even (N=4 or M=4) dimensions
  ! Paths: SIDE(L/R) x TRANSR(N/T) x UPLO(L/U) x TRANS(N/T)

  ! SIDE='L': A is MxM triangular, B is MxN
  ! SIDE='R': A is NxN triangular, B is MxN

  ! Left side, odd M=3, N=2
  call run_dtfsm_test(3, 2, 'L', 'N', 'L', 'N', 'N', 'left_odd_NL_N')
  call run_dtfsm_test(3, 2, 'L', 'N', 'L', 'T', 'N', 'left_odd_NL_T')
  call run_dtfsm_test(3, 2, 'L', 'N', 'U', 'N', 'N', 'left_odd_NU_N')
  call run_dtfsm_test(3, 2, 'L', 'N', 'U', 'T', 'N', 'left_odd_NU_T')
  call run_dtfsm_test(3, 2, 'L', 'T', 'L', 'N', 'N', 'left_odd_TL_N')
  call run_dtfsm_test(3, 2, 'L', 'T', 'L', 'T', 'N', 'left_odd_TL_T')
  call run_dtfsm_test(3, 2, 'L', 'T', 'U', 'N', 'N', 'left_odd_TU_N')
  call run_dtfsm_test(3, 2, 'L', 'T', 'U', 'T', 'N', 'left_odd_TU_T')

  ! Left side, even M=4, N=2
  call run_dtfsm_test(4, 2, 'L', 'N', 'L', 'N', 'N', 'left_even_NL_N')
  call run_dtfsm_test(4, 2, 'L', 'N', 'L', 'T', 'N', 'left_even_NL_T')
  call run_dtfsm_test(4, 2, 'L', 'N', 'U', 'N', 'N', 'left_even_NU_N')
  call run_dtfsm_test(4, 2, 'L', 'N', 'U', 'T', 'N', 'left_even_NU_T')
  call run_dtfsm_test(4, 2, 'L', 'T', 'L', 'N', 'N', 'left_even_TL_N')
  call run_dtfsm_test(4, 2, 'L', 'T', 'L', 'T', 'N', 'left_even_TL_T')
  call run_dtfsm_test(4, 2, 'L', 'T', 'U', 'N', 'N', 'left_even_TU_N')
  call run_dtfsm_test(4, 2, 'L', 'T', 'U', 'T', 'N', 'left_even_TU_T')

  ! Right side, odd N=3, M=2
  call run_dtfsm_test(2, 3, 'R', 'N', 'L', 'N', 'N', 'right_odd_NL_N')
  call run_dtfsm_test(2, 3, 'R', 'N', 'L', 'T', 'N', 'right_odd_NL_T')
  call run_dtfsm_test(2, 3, 'R', 'N', 'U', 'N', 'N', 'right_odd_NU_N')
  call run_dtfsm_test(2, 3, 'R', 'N', 'U', 'T', 'N', 'right_odd_NU_T')
  call run_dtfsm_test(2, 3, 'R', 'T', 'L', 'N', 'N', 'right_odd_TL_N')
  call run_dtfsm_test(2, 3, 'R', 'T', 'L', 'T', 'N', 'right_odd_TL_T')
  call run_dtfsm_test(2, 3, 'R', 'T', 'U', 'N', 'N', 'right_odd_TU_N')
  call run_dtfsm_test(2, 3, 'R', 'T', 'U', 'T', 'N', 'right_odd_TU_T')

  ! Right side, even N=4, M=2
  call run_dtfsm_test(2, 4, 'R', 'N', 'L', 'N', 'N', 'right_even_NL_N')
  call run_dtfsm_test(2, 4, 'R', 'N', 'L', 'T', 'N', 'right_even_NL_T')
  call run_dtfsm_test(2, 4, 'R', 'N', 'U', 'N', 'N', 'right_even_NU_N')
  call run_dtfsm_test(2, 4, 'R', 'N', 'U', 'T', 'N', 'right_even_NU_T')
  call run_dtfsm_test(2, 4, 'R', 'T', 'L', 'N', 'N', 'right_even_TL_N')
  call run_dtfsm_test(2, 4, 'R', 'T', 'L', 'T', 'N', 'right_even_TL_T')
  call run_dtfsm_test(2, 4, 'R', 'T', 'U', 'N', 'N', 'right_even_TU_N')
  call run_dtfsm_test(2, 4, 'R', 'T', 'U', 'T', 'N', 'right_even_TU_T')

  ! Unit diagonal tests
  call run_dtfsm_test(3, 2, 'L', 'N', 'L', 'N', 'U', 'left_odd_NL_N_unit')
  call run_dtfsm_test(4, 2, 'L', 'N', 'L', 'N', 'U', 'left_even_NL_N_unit')
  call run_dtfsm_test(2, 3, 'R', 'N', 'U', 'T', 'U', 'right_odd_NU_T_unit')
  call run_dtfsm_test(2, 4, 'R', 'T', 'L', 'N', 'U', 'right_even_TL_N_unit')

  ! Alpha tests
  call run_dtfsm_test_alpha(3, 2, 'L', 'N', 'L', 'N', 'N', 2.5d0, 'left_odd_alpha')
  call run_dtfsm_test_alpha(2, 4, 'R', 'T', 'U', 'N', 'N', 0.5d0, 'right_even_alpha')

  ! M=0 or N=0
  call test_dtfsm_empty()

  ! Alpha=0 test
  call test_dtfsm_alpha_zero()

  ! M=1 special case (left side, odd, normal, lower, notrans)
  call run_dtfsm_test(1, 2, 'L', 'N', 'L', 'N', 'N', 'left_m1_NL_N')
  call run_dtfsm_test(1, 2, 'L', 'N', 'L', 'T', 'N', 'left_m1_NL_T')
  call run_dtfsm_test(1, 2, 'L', 'T', 'L', 'N', 'N', 'left_m1_TL_N')
  call run_dtfsm_test(1, 2, 'L', 'T', 'L', 'T', 'N', 'left_m1_TL_T')

  ! Larger odd (M=5) and even (M=6)
  call run_dtfsm_test(5, 2, 'L', 'N', 'L', 'N', 'N', 'left_5_NL_N')
  call run_dtfsm_test(5, 2, 'L', 'T', 'U', 'T', 'N', 'left_5_TU_T')
  call run_dtfsm_test(6, 2, 'L', 'N', 'U', 'N', 'N', 'left_6_NU_N')
  call run_dtfsm_test(6, 2, 'L', 'T', 'L', 'T', 'N', 'left_6_TL_T')

contains

  subroutine build_triangular(n, uplo, diag_type, full, lda)
    integer, intent(in) :: n, lda
    character, intent(in) :: uplo, diag_type
    double precision, intent(out) :: full(lda,n)
    integer :: i, j
    double precision :: val

    full = 0.0d0
    val = 0.5d0

    if (uplo == 'L') then
      do j = 1, n
        do i = j, n
          if (i == j) then
            if (diag_type == 'U') then
              full(i,j) = 1.0d0
            else
              full(i,j) = dble(n + i)
            end if
          else
            full(i,j) = val
            val = val + 0.5d0
          end if
        end do
      end do
    else
      do j = 1, n
        do i = 1, j
          if (i == j) then
            if (diag_type == 'U') then
              full(i,j) = 1.0d0
            else
              full(i,j) = dble(n + j)
            end if
          else
            full(i,j) = val
            val = val + 0.5d0
          end if
        end do
      end do
    end if
  end subroutine

  subroutine build_b_matrix(m, n, b, ldb)
    integer, intent(in) :: m, n, ldb
    double precision, intent(out) :: b(ldb,n)
    integer :: i, j
    double precision :: val

    b = 0.0d0
    val = 1.0d0
    do j = 1, n
      do i = 1, m
        b(i,j) = val
        val = val + 1.0d0
      end do
    end do
  end subroutine

  subroutine run_dtfsm_test(m, n, side, transr, uplo, trans, diag, label)
    integer, intent(in) :: m, n
    character, intent(in) :: side, transr, uplo, trans, diag
    character(len=*), intent(in) :: label

    call run_dtfsm_test_alpha(m, n, side, transr, uplo, trans, diag, 1.0d0, label)
  end subroutine

  subroutine run_dtfsm_test_alpha(m, n, side, transr, uplo, trans, diag, alpha, label)
    integer, intent(in) :: m, n
    character, intent(in) :: side, transr, uplo, trans, diag
    character(len=*), intent(in) :: label
    double precision, intent(in) :: alpha
    double precision :: full(20,20), rfp(500), b(20,20)
    integer :: info2, nt2, adim, ldb

    ! A dimension depends on side
    if (side == 'L') then
      adim = m
    else
      adim = n
    end if

    nt2 = adim * (adim + 1) / 2
    ldb = max(1, m)

    ! Build triangular matrix
    call build_triangular(adim, uplo, diag, full, 20)

    ! Convert to RFP format
    call dtrttf(transr, uplo, adim, full, 20, rfp, info2)
    if (info2 /= 0) then
      print *, 'DTRTTF failed for ', label, ' info=', info2
      stop
    end if

    ! Build B matrix
    call build_b_matrix(m, n, b, 20)

    call begin_test(label)
    call print_int('M', m)
    call print_int('N', n)
    call print_char('side', side)
    call print_char('transr', transr)
    call print_char('uplo', uplo)
    call print_char('trans', trans)
    call print_char('diag', diag)
    call print_scalar('alpha', alpha)
    call print_array('A', rfp, nt2)
    call print_matrix('B_in', b, 20, m, n)
    call print_int('ldb', ldb)

    call dtfsm(transr, side, uplo, trans, diag, m, n, alpha, rfp, b, 20)

    call print_matrix('B_out', b, 20, m, n)
    call end_test()
  end subroutine

  subroutine test_dtfsm_empty()
    double precision :: rfp(10), b(10,10)

    rfp = 0.0d0
    b = 0.0d0

    ! M=0
    call begin_test('m_zero')
    call print_int('M', 0)
    call print_int('N', 2)
    call dtfsm('N', 'L', 'L', 'N', 'N', 0, 2, 1.0d0, rfp, b, 1)
    call end_test()

    ! N=0
    call begin_test('n_zero')
    call print_int('M', 2)
    call print_int('N', 0)
    call dtfsm('N', 'L', 'L', 'N', 'N', 2, 0, 1.0d0, rfp, b, 2)
    call end_test()
  end subroutine

  subroutine test_dtfsm_alpha_zero()
    double precision :: full(20,20), rfp(500), b(20,20)
    integer :: info2, m, n

    m = 3
    n = 2

    call build_triangular(3, 'L', 'N', full, 20)
    call dtrttf('N', 'L', 3, full, 20, rfp, info2)
    call build_b_matrix(m, n, b, 20)

    call begin_test('alpha_zero')
    call print_int('M', m)
    call print_int('N', n)
    call print_matrix('B_in', b, 20, m, n)

    call dtfsm('N', 'L', 'L', 'N', 'N', m, n, 0.0d0, rfp, b, 20)

    call print_matrix('B_out', b, 20, m, n)
    call end_test()
  end subroutine

end program
