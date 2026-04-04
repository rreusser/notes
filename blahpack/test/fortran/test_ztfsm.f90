program test_ztfsm
  use test_utils
  implicit none

  ! Test all major branches of ZTFSM:
  !   SIDE = L/R
  !   M odd/even (for SIDE=L), N odd/even (for SIDE=R)
  !   TRANSR = N/C
  !   UPLO = L/U
  !   TRANS = N/C
  !   DIAG = N/U

  ! SIDE=L, M odd, all TRANSR/UPLO/TRANS combos
  call test_ztfsm_case(3, 2, 'L', 'N', 'L', 'N', 'N', 'left_modd_NLN')
  call test_ztfsm_case(3, 2, 'L', 'N', 'L', 'C', 'N', 'left_modd_NLC')
  call test_ztfsm_case(3, 2, 'L', 'N', 'U', 'N', 'N', 'left_modd_NUN')
  call test_ztfsm_case(3, 2, 'L', 'N', 'U', 'C', 'N', 'left_modd_NUC')
  call test_ztfsm_case(3, 2, 'L', 'C', 'L', 'N', 'N', 'left_modd_CLN')
  call test_ztfsm_case(3, 2, 'L', 'C', 'L', 'C', 'N', 'left_modd_CLC')
  call test_ztfsm_case(3, 2, 'L', 'C', 'U', 'N', 'N', 'left_modd_CUN')
  call test_ztfsm_case(3, 2, 'L', 'C', 'U', 'C', 'N', 'left_modd_CUC')

  ! SIDE=L, M even, all TRANSR/UPLO/TRANS combos
  call test_ztfsm_case(4, 2, 'L', 'N', 'L', 'N', 'N', 'left_meven_NLN')
  call test_ztfsm_case(4, 2, 'L', 'N', 'L', 'C', 'N', 'left_meven_NLC')
  call test_ztfsm_case(4, 2, 'L', 'N', 'U', 'N', 'N', 'left_meven_NUN')
  call test_ztfsm_case(4, 2, 'L', 'N', 'U', 'C', 'N', 'left_meven_NUC')
  call test_ztfsm_case(4, 2, 'L', 'C', 'L', 'N', 'N', 'left_meven_CLN')
  call test_ztfsm_case(4, 2, 'L', 'C', 'L', 'C', 'N', 'left_meven_CLC')
  call test_ztfsm_case(4, 2, 'L', 'C', 'U', 'N', 'N', 'left_meven_CUN')
  call test_ztfsm_case(4, 2, 'L', 'C', 'U', 'C', 'N', 'left_meven_CUC')

  ! SIDE=R, N odd, all TRANSR/UPLO/TRANS combos
  call test_ztfsm_case(2, 3, 'R', 'N', 'L', 'N', 'N', 'right_nodd_NLN')
  call test_ztfsm_case(2, 3, 'R', 'N', 'L', 'C', 'N', 'right_nodd_NLC')
  call test_ztfsm_case(2, 3, 'R', 'N', 'U', 'N', 'N', 'right_nodd_NUN')
  call test_ztfsm_case(2, 3, 'R', 'N', 'U', 'C', 'N', 'right_nodd_NUC')
  call test_ztfsm_case(2, 3, 'R', 'C', 'L', 'N', 'N', 'right_nodd_CLN')
  call test_ztfsm_case(2, 3, 'R', 'C', 'L', 'C', 'N', 'right_nodd_CLC')
  call test_ztfsm_case(2, 3, 'R', 'C', 'U', 'N', 'N', 'right_nodd_CUN')
  call test_ztfsm_case(2, 3, 'R', 'C', 'U', 'C', 'N', 'right_nodd_CUC')

  ! SIDE=R, N even, all TRANSR/UPLO/TRANS combos
  call test_ztfsm_case(2, 4, 'R', 'N', 'L', 'N', 'N', 'right_neven_NLN')
  call test_ztfsm_case(2, 4, 'R', 'N', 'L', 'C', 'N', 'right_neven_NLC')
  call test_ztfsm_case(2, 4, 'R', 'N', 'U', 'N', 'N', 'right_neven_NUN')
  call test_ztfsm_case(2, 4, 'R', 'N', 'U', 'C', 'N', 'right_neven_NUC')
  call test_ztfsm_case(2, 4, 'R', 'C', 'L', 'N', 'N', 'right_neven_CLN')
  call test_ztfsm_case(2, 4, 'R', 'C', 'L', 'C', 'N', 'right_neven_CLC')
  call test_ztfsm_case(2, 4, 'R', 'C', 'U', 'N', 'N', 'right_neven_CUN')
  call test_ztfsm_case(2, 4, 'R', 'C', 'U', 'C', 'N', 'right_neven_CUC')

  ! Unit diagonal cases
  call test_ztfsm_case(3, 2, 'L', 'N', 'L', 'N', 'U', 'left_modd_NLN_unit')
  call test_ztfsm_case(4, 2, 'L', 'N', 'L', 'N', 'U', 'left_meven_NLN_unit')
  call test_ztfsm_case(2, 3, 'R', 'N', 'L', 'N', 'U', 'right_nodd_NLN_unit')
  call test_ztfsm_case(2, 4, 'R', 'N', 'L', 'N', 'U', 'right_neven_NLN_unit')

  ! M=1 special case (for SIDE=L, M odd)
  call test_ztfsm_m1('N', 'L', 'N', 'N', 'm1_NLN')
  call test_ztfsm_m1('N', 'L', 'C', 'N', 'm1_NLC')
  call test_ztfsm_m1('C', 'L', 'N', 'N', 'm1_CLN')
  call test_ztfsm_m1('C', 'L', 'C', 'N', 'm1_CLC')

  ! Alpha = 0 case
  call test_ztfsm_alpha_zero()

  ! M=0 or N=0 early return
  call test_ztfsm_empty()

  ! Non-trivial alpha
  call test_ztfsm_case_alpha(3, 2, 'L', 'N', 'L', 'N', 'N', &
    (2.0d0, -1.0d0), 'left_alpha')
  call test_ztfsm_case_alpha(2, 3, 'R', 'N', 'L', 'N', 'N', &
    (0.5d0, 1.0d0), 'right_alpha')

contains

  subroutine build_triangular(n, uplo, full)
    integer, intent(in) :: n
    character, intent(in) :: uplo
    complex*16, intent(out) :: full(10,10)
    integer :: i, j
    double precision :: val

    full = (0.0d0, 0.0d0)
    val = 1.0d0
    if (uplo == 'L') then
      do j = 1, n
        do i = j, n
          if (i == j) then
            full(i,j) = dcmplx(n + val, 0.0d0)
          else
            full(i,j) = dcmplx(val, val * 0.5d0)
          end if
          val = val + 1.0d0
        end do
      end do
    else
      do j = 1, n
        do i = 1, j
          if (i == j) then
            full(i,j) = dcmplx(n + val, 0.0d0)
          else
            full(i,j) = dcmplx(val, -val * 0.5d0)
          end if
          val = val + 1.0d0
        end do
      end do
    end if
  end subroutine

  subroutine build_b_matrix(m, n, b, ldb)
    integer, intent(in) :: m, n, ldb
    complex*16, intent(out) :: b(ldb, n)
    integer :: i, j
    double precision :: val

    b = (0.0d0, 0.0d0)
    val = 1.0d0
    do j = 1, n
      do i = 1, m
        b(i,j) = dcmplx(val, val * 0.3d0)
        val = val + 1.0d0
      end do
    end do
  end subroutine

  ! Print MxN submatrix of B column-by-column as flat real array
  subroutine print_b(label, b, ldb, m, n)
    character(len=*), intent(in) :: label
    complex*16, intent(in) :: b(ldb, *)
    integer, intent(in) :: ldb, m, n
    double precision :: flat(200)
    integer :: i, j, idx

    idx = 1
    do j = 1, n
      do i = 1, m
        flat(idx)   = dble(b(i,j))
        flat(idx+1) = dimag(b(i,j))
        idx = idx + 2
      end do
    end do
    call print_array(label, flat, 2*m*n)
  end subroutine

  subroutine test_ztfsm_case(m, n, side, transr, uplo, trans, diag, label)
    integer, intent(in) :: m, n
    character, intent(in) :: side, transr, uplo, trans, diag
    character(len=*), intent(in) :: label
    complex*16 :: full(10,10), rfp(100)
    complex*16, allocatable :: b(:,:)
    double precision :: rfp_r(200)
    equivalence (rfp, rfp_r)
    integer :: info2, nt2, k

    if (side == 'L') then
      k = m
    else
      k = n
    end if
    nt2 = k * (k + 1) / 2

    allocate(b(m, n))

    call build_triangular(k, uplo, full)
    call ztrttf(transr, uplo, k, full, 10, rfp, info2)
    if (info2 /= 0) then
      print *, 'ZTRTTF failed for ', label, ' info=', info2
      stop
    end if

    call build_b_matrix(m, n, b, m)

    call begin_test(label)
    call print_array('a', rfp_r, 2 * nt2)
    call print_b('b_in', b, m, m, n)
    call ztfsm(transr, side, uplo, trans, diag, m, n, (1.0d0, 0.0d0), &
               rfp, b, m)
    call print_b('b_out', b, m, m, n)
    call end_test()

    deallocate(b)
  end subroutine

  subroutine test_ztfsm_case_alpha(m, n, side, transr, uplo, trans, diag, &
                                    alpha, label)
    integer, intent(in) :: m, n
    character, intent(in) :: side, transr, uplo, trans, diag
    complex*16, intent(in) :: alpha
    character(len=*), intent(in) :: label
    complex*16 :: full(10,10), rfp(100)
    complex*16, allocatable :: b(:,:)
    double precision :: rfp_r(200)
    equivalence (rfp, rfp_r)
    integer :: info2, nt2, k

    if (side == 'L') then
      k = m
    else
      k = n
    end if
    nt2 = k * (k + 1) / 2

    allocate(b(m, n))

    call build_triangular(k, uplo, full)
    call ztrttf(transr, uplo, k, full, 10, rfp, info2)
    if (info2 /= 0) then
      print *, 'ZTRTTF failed for ', label, ' info=', info2
      stop
    end if

    call build_b_matrix(m, n, b, m)

    call begin_test(label)
    call print_array('a', rfp_r, 2 * nt2)
    call print_b('b_in', b, m, m, n)
    call ztfsm(transr, side, uplo, trans, diag, m, n, alpha, &
               rfp, b, m)
    call print_b('b_out', b, m, m, n)
    call end_test()

    deallocate(b)
  end subroutine

  subroutine test_ztfsm_m1(transr, uplo, trans, diag, label)
    character, intent(in) :: transr, uplo, trans, diag
    character(len=*), intent(in) :: label
    complex*16 :: full(10,10), rfp(100), b(1,2)
    double precision :: rfp_r(200), b_r(4)
    equivalence (rfp, rfp_r)
    equivalence (b, b_r)
    integer :: info2

    call build_triangular(1, uplo, full)
    call ztrttf(transr, uplo, 1, full, 10, rfp, info2)

    b(1,1) = (3.0d0, 1.0d0)
    b(1,2) = (2.0d0, -1.0d0)

    call begin_test(label)
    call print_array('a', rfp_r, 2)
    call print_array('b_in', b_r, 4)
    call ztfsm(transr, 'L', uplo, trans, diag, 1, 2, (1.0d0, 0.0d0), &
               rfp, b, 1)
    call print_array('b_out', b_r, 4)
    call end_test()
  end subroutine

  subroutine test_ztfsm_alpha_zero()
    complex*16 :: rfp(100), b(3,2)
    double precision :: rfp_r(200), b_r(12)
    equivalence (rfp, rfp_r)
    equivalence (b, b_r)

    rfp = (1.0d0, 0.0d0)
    call build_b_matrix(3, 2, b, 3)

    call begin_test('alpha_zero')
    call print_array('b_in', b_r, 12)
    call ztfsm('N', 'L', 'L', 'N', 'N', 3, 2, (0.0d0, 0.0d0), &
               rfp, b, 3)
    call print_array('b_out', b_r, 12)
    call end_test()
  end subroutine

  subroutine test_ztfsm_empty()
    complex*16 :: rfp(100), b(3,2)
    double precision :: b_r(12)
    equivalence (b, b_r)

    rfp = (1.0d0, 0.0d0)
    b = (99.0d0, 99.0d0)

    call begin_test('m_zero')
    call ztfsm('N', 'L', 'L', 'N', 'N', 0, 2, (1.0d0, 0.0d0), &
               rfp, b, 3)
    call print_array('b_out', b_r, 12)
    call end_test()

    call begin_test('n_zero')
    call ztfsm('N', 'L', 'L', 'N', 'N', 3, 0, (1.0d0, 0.0d0), &
               rfp, b, 3)
    call print_array('b_out', b_r, 12)
    call end_test()
  end subroutine

end program
