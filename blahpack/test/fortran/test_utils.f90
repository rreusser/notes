module test_utils
  implicit none
  private
  public :: begin_test, end_test, print_scalar, print_int, print_array, print_int_array, print_matrix, print_char

contains

  subroutine begin_test(name)
    character(*), intent(in) :: name
    write(*, '(A,A,A)', advance='no') '{"name":"', trim(name), '"'
  end subroutine

  subroutine end_test()
    write(*, '(A)') '}'
  end subroutine

  subroutine print_scalar(name, val)
    character(*), intent(in) :: name
    double precision, intent(in) :: val
    write(*, '(A,A,A,ES25.17E3)', advance='no') ',"', trim(name), '":', val
  end subroutine

  subroutine print_int(name, val)
    character(*), intent(in) :: name
    integer, intent(in) :: val
    write(*, '(A,A,A,I0)', advance='no') ',"', trim(name), '":', val
  end subroutine

  subroutine print_char(name, val)
    character(*), intent(in) :: name
    character, intent(in) :: val
    write(*, '(A,A,A,A,A)', advance='no') ',"', trim(name), '":"', val, '"'
  end subroutine

  subroutine print_array(name, arr, n)
    character(*), intent(in) :: name
    integer, intent(in) :: n
    double precision, intent(in) :: arr(n)
    integer :: i
    write(*, '(A,A,A)', advance='no') ',"', trim(name), '":['
    do i = 1, n
      if (i > 1) write(*, '(A)', advance='no') ','
      write(*, '(ES25.17E3)', advance='no') arr(i)
    end do
    write(*, '(A)', advance='no') ']'
  end subroutine

  subroutine print_int_array(name, arr, n)
    character(*), intent(in) :: name
    integer, intent(in) :: n
    integer, intent(in) :: arr(n)
    integer :: i
    write(*, '(A,A,A)', advance='no') ',"', trim(name), '":['
    do i = 1, n
      if (i > 1) write(*, '(A)', advance='no') ','
      write(*, '(I0)', advance='no') arr(i)
    end do
    write(*, '(A)', advance='no') ']'
  end subroutine

  subroutine print_matrix(name, mat, lda, m, n)
    character(*), intent(in) :: name
    integer, intent(in) :: lda, m, n
    double precision, intent(in) :: mat(lda, *)
    integer :: i, j
    ! Print column-major flat array (all columns concatenated)
    write(*, '(A,A,A)', advance='no') ',"', trim(name), '":['
    do j = 1, n
      do i = 1, m
        if (j > 1 .or. i > 1) write(*, '(A)', advance='no') ','
        write(*, '(ES25.17E3)', advance='no') mat(i, j)
      end do
    end do
    write(*, '(A)', advance='no') ']'
  end subroutine

end module
