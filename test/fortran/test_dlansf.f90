program test_dlansf
  use test_utils
  implicit none

  ! Test all 8 dispatch paths (2 noe x 2 ifm x 2 ilu) for each of 4 norms.
  ! Use DTRTTF to build RFP arrays from full symmetric matrices.

  ! Odd N=5, Even N=4
  call test_all_combos(5, 'odd5')
  call test_all_combos(4, 'even4')

  ! Edge cases
  call test_n_zero()
  call test_n_one()

  ! Larger odd N=7 for extra coverage
  call test_all_combos(7, 'odd7')

contains

  subroutine test_all_combos(n, prefix)
    integer, intent(in) :: n
    character(len=*), intent(in) :: prefix
    character :: transr, uplo
    character(len=80) :: label
    integer :: it, iu

    do it = 1, 2
      do iu = 1, 2
        if (it == 1) then
          transr = 'N'
        else
          transr = 'T'
        end if
        if (iu == 1) then
          uplo = 'L'
        else
          uplo = 'U'
        end if

        ! Max norm
        write(label, '(A,"_",A1,A1,"_max")') trim(prefix), transr, uplo
        call test_one(n, transr, uplo, 'M', trim(label))

        ! One norm
        write(label, '(A,"_",A1,A1,"_one")') trim(prefix), transr, uplo
        call test_one(n, transr, uplo, '1', trim(label))

        ! Infinity norm
        write(label, '(A,"_",A1,A1,"_inf")') trim(prefix), transr, uplo
        call test_one(n, transr, uplo, 'I', trim(label))

        ! Frobenius norm
        write(label, '(A,"_",A1,A1,"_frob")') trim(prefix), transr, uplo
        call test_one(n, transr, uplo, 'F', trim(label))
      end do
    end do
  end subroutine

  subroutine test_one(n, transr, uplo, norm, label)
    integer, intent(in) :: n
    character, intent(in) :: transr, uplo, norm
    character(len=*), intent(in) :: label
    double precision :: full(20,20), rfp(500), work(50)
    double precision :: result, dlansf
    integer :: info, nt
    external :: dlansf

    nt = n * (n + 1) / 2

    call build_sym_matrix(n, full)
    call dtrttf(transr, uplo, n, full, 20, rfp, info)
    if (info /= 0) then
      print *, 'DTRTTF failed: ', label, ' info=', info
      stop
    end if

    result = dlansf(norm, transr, uplo, n, rfp, work)

    call begin_test(label)
    call print_array('input', rfp, nt)
    call print_scalar('result', result)
    call end_test()
  end subroutine

  subroutine build_sym_matrix(n, h)
    integer, intent(in) :: n
    double precision, intent(out) :: h(20,20)
    integer :: i, j

    h = 0.0d0
    ! Fill with a deterministic symmetric matrix
    do i = 1, n
      do j = 1, n
        if (i == j) then
          h(i,j) = dble(n + i)
        else
          h(i,j) = dble(i + j) * 0.5d0 * (-1.0d0)**(i+j)
        end if
      end do
    end do
  end subroutine

  subroutine test_n_zero()
    double precision :: rfp(1), work(1), result, dlansf
    external :: dlansf

    result = dlansf('M', 'N', 'L', 0, rfp, work)
    call begin_test('n_zero_max')
    call print_scalar('result', result)
    call end_test()

    result = dlansf('1', 'N', 'L', 0, rfp, work)
    call begin_test('n_zero_one')
    call print_scalar('result', result)
    call end_test()

    result = dlansf('F', 'N', 'L', 0, rfp, work)
    call begin_test('n_zero_frob')
    call print_scalar('result', result)
    call end_test()
  end subroutine

  subroutine test_n_one()
    double precision :: rfp(1), work(1), result, dlansf
    external :: dlansf

    rfp(1) = -7.5d0

    result = dlansf('M', 'N', 'L', 1, rfp, work)
    call begin_test('n_one_max')
    call print_array('input', rfp, 1)
    call print_scalar('result', result)
    call end_test()

    result = dlansf('1', 'N', 'L', 1, rfp, work)
    call begin_test('n_one_one')
    call print_array('input', rfp, 1)
    call print_scalar('result', result)
    call end_test()

    result = dlansf('F', 'N', 'L', 1, rfp, work)
    call begin_test('n_one_frob')
    call print_array('input', rfp, 1)
    call print_scalar('result', result)
    call end_test()
  end subroutine

end program
