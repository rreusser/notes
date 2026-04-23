program test_dtftri
  use test_utils
  implicit none

  ! Test all 8 code paths (odd/even N x transpose/no-transpose x upper/lower)
  ! plus unit diagonal variants and edge cases

  ! Non-unit diagonal tests
  call test_dtftri_case(3, 'N', 'L', 'N', 'lower_odd_normal_nonunit')
  call test_dtftri_case(3, 'N', 'U', 'N', 'upper_odd_normal_nonunit')
  call test_dtftri_case(3, 'T', 'L', 'N', 'lower_odd_trans_nonunit')
  call test_dtftri_case(3, 'T', 'U', 'N', 'upper_odd_trans_nonunit')
  call test_dtftri_case(4, 'N', 'L', 'N', 'lower_even_normal_nonunit')
  call test_dtftri_case(4, 'N', 'U', 'N', 'upper_even_normal_nonunit')
  call test_dtftri_case(4, 'T', 'L', 'N', 'lower_even_trans_nonunit')
  call test_dtftri_case(4, 'T', 'U', 'N', 'upper_even_trans_nonunit')

  ! Unit diagonal tests
  call test_dtftri_case(3, 'N', 'L', 'U', 'lower_odd_normal_unit')
  call test_dtftri_case(3, 'N', 'U', 'U', 'upper_odd_normal_unit')
  call test_dtftri_case(3, 'T', 'L', 'U', 'lower_odd_trans_unit')
  call test_dtftri_case(3, 'T', 'U', 'U', 'upper_odd_trans_unit')
  call test_dtftri_case(4, 'N', 'L', 'U', 'lower_even_normal_unit')
  call test_dtftri_case(4, 'N', 'U', 'U', 'upper_even_normal_unit')
  call test_dtftri_case(4, 'T', 'L', 'U', 'lower_even_trans_unit')
  call test_dtftri_case(4, 'T', 'U', 'U', 'upper_even_trans_unit')

  ! N=0 edge case
  call test_n_zero()

  ! N=1 edge cases
  call test_n_one_nonunit()
  call test_n_one_unit()

  ! Larger: N=5 (odd)
  call test_dtftri_case(5, 'N', 'L', 'N', 'lower_5_normal_nonunit')
  call test_dtftri_case(5, 'T', 'U', 'N', 'upper_5_trans_nonunit')

  ! Singular matrix test (zero on diagonal)
  call test_singular()

contains

  subroutine build_triangular_matrix(n, uplo, diag, full)
    integer, intent(in) :: n
    character, intent(in) :: uplo, diag
    double precision, intent(out) :: full(10,10)
    integer :: i, j
    double precision :: val

    full = 0.0d0
    val = 1.0d0

    if (uplo == 'L') then
      do j = 1, n
        do i = j, n
          if (i == j) then
            if (diag == 'U') then
              full(i,j) = 1.0d0
            else
              full(i,j) = val + dble(n)
            end if
          else
            full(i,j) = val * 0.5d0
          end if
          val = val + 1.0d0
        end do
      end do
    else
      do j = 1, n
        do i = 1, j
          if (i == j) then
            if (diag == 'U') then
              full(i,j) = 1.0d0
            else
              full(i,j) = val + dble(n)
            end if
          else
            full(i,j) = val * 0.5d0
          end if
          val = val + 1.0d0
        end do
      end do
    end if
  end subroutine

  subroutine test_dtftri_case(n, transr, uplo, diag, label)
    integer, intent(in) :: n
    character, intent(in) :: transr, uplo, diag
    character(len=*), intent(in) :: label
    double precision :: full(10,10), rfp(100)
    integer :: info, nt

    nt = n * (n + 1) / 2

    call build_triangular_matrix(n, uplo, diag, full)
    call dtrttf(transr, uplo, n, full, 10, rfp, info)
    if (info /= 0) then
      print *, 'DTRTTF failed with info=', info
      stop
    end if

    call begin_test(label)
    call print_array('input', rfp, nt)

    call dtftri(transr, uplo, diag, n, rfp, info)
    call print_int('info', info)
    call print_array('a', rfp, nt)
    call end_test()
  end subroutine

  subroutine test_n_zero()
    double precision :: a(1)
    integer :: info

    a(1) = 0.0d0
    call dtftri('N', 'L', 'N', 0, a, info)
    call begin_test('n_zero')
    call print_int('info', info)
    call end_test()
  end subroutine

  subroutine test_n_one_nonunit()
    double precision :: a(1)
    integer :: info

    a(1) = 4.0d0
    call begin_test('n_one_nonunit')
    call print_array('input', a, 1)
    call dtftri('N', 'L', 'N', 1, a, info)
    call print_int('info', info)
    call print_array('a', a, 1)
    call end_test()
  end subroutine

  subroutine test_n_one_unit()
    double precision :: a(1)
    integer :: info

    a(1) = 1.0d0
    call begin_test('n_one_unit')
    call print_array('input', a, 1)
    call dtftri('N', 'L', 'U', 1, a, info)
    call print_int('info', info)
    call print_array('a', a, 1)
    call end_test()
  end subroutine

  subroutine test_singular()
    double precision :: full(10,10), rfp(100)
    integer :: info, n, nt

    n = 3
    nt = n * (n + 1) / 2

    ! Build a lower triangular matrix with zero on diagonal
    full = 0.0d0
    full(1,1) = 2.0d0
    full(2,1) = 1.0d0
    full(2,2) = 0.0d0   ! singular!
    full(3,1) = 1.0d0
    full(3,2) = 1.0d0
    full(3,3) = 3.0d0

    call dtrttf('N', 'L', n, full, 10, rfp, info)

    call begin_test('singular')
    call print_array('input', rfp, nt)
    call dtftri('N', 'L', 'N', n, rfp, info)
    call print_int('info', info)
    call end_test()
  end subroutine

end program
