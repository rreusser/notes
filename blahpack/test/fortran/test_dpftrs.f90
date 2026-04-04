program test_dpftrs
  use test_utils
  implicit none

  ! Test all TRANSR/UPLO combinations for N=3 (odd) and N=4 (even)
  call test_solve(3, 'N', 'L', 1, 'lower_odd_normal_1rhs')
  call test_solve(3, 'N', 'U', 1, 'upper_odd_normal_1rhs')
  call test_solve(3, 'T', 'L', 1, 'lower_odd_trans_1rhs')
  call test_solve(3, 'T', 'U', 1, 'upper_odd_trans_1rhs')
  call test_solve(4, 'N', 'L', 1, 'lower_even_normal_1rhs')
  call test_solve(4, 'N', 'U', 1, 'upper_even_normal_1rhs')
  call test_solve(4, 'T', 'L', 1, 'lower_even_trans_1rhs')
  call test_solve(4, 'T', 'U', 1, 'upper_even_trans_1rhs')

  ! Multiple RHS
  call test_solve(3, 'N', 'L', 2, 'lower_odd_normal_2rhs')
  call test_solve(4, 'T', 'U', 3, 'upper_even_trans_3rhs')

  ! Edge cases
  call test_n_zero()
  call test_nrhs_zero()
  call test_n_one()

  ! Larger odd N=5
  call test_solve_5(5, 'N', 'L', 1, 'lower_5_normal_1rhs')
  call test_solve_5(5, 'T', 'U', 2, 'upper_5_trans_2rhs')

contains

  subroutine test_solve(n, transr, uplo, nrhs, label)
    integer, intent(in) :: n, nrhs
    character, intent(in) :: transr, uplo
    character(len=*), intent(in) :: label
    double precision :: full(10,10), rfp(100), b(10,10), x_exact(10,10)
    integer :: info, nt2, i, j

    nt2 = n * (n + 1) / 2

    ! Build SPD matrix
    call build_spd_matrix(n, full)

    ! Convert to RFP
    call dtrttf(transr, uplo, n, full, 10, rfp, info)
    if (info /= 0) then
      print *, 'DTRTTF failed with info=', info
      stop
    end if

    ! Build known solution and compute RHS: B = A * X_exact
    do j = 1, nrhs
      do i = 1, n
        x_exact(i, j) = dble(i + j)
      end do
    end do

    ! B = full * x_exact (matrix multiply)
    b = 0.0d0
    call dgemm('N', 'N', n, nrhs, n, 1.0d0, full, 10, x_exact, 10, 0.0d0, b, 10)

    ! Factor the RFP matrix
    call dpftrf(transr, uplo, n, rfp, info)
    if (info /= 0) then
      print *, 'DPFTRF failed with info=', info
      stop
    end if

    call begin_test(label)
    call print_array('a', rfp, nt2)
    call print_matrix('b_in', b, 10, n, nrhs)
    call print_matrix('x_exact', x_exact, 10, n, nrhs)

    ! Solve
    call dpftrs(transr, uplo, n, nrhs, rfp, b, 10, info)
    call print_int('info', info)
    call print_matrix('b_out', b, 10, n, nrhs)
    call end_test()
  end subroutine

  subroutine test_n_zero()
    double precision :: a(1), b(1,1)
    integer :: info
    a(1) = 1.0d0
    b(1,1) = 1.0d0
    call dpftrs('N', 'L', 0, 1, a, b, 1, info)
    call begin_test('n_zero')
    call print_int('info', info)
    call end_test()
  end subroutine

  subroutine test_nrhs_zero()
    double precision :: a(6), b(3,1)
    integer :: info
    a = 1.0d0
    b = 0.0d0
    call dpftrs('N', 'L', 3, 0, a, b, 3, info)
    call begin_test('nrhs_zero')
    call print_int('info', info)
    call end_test()
  end subroutine

  subroutine test_n_one()
    double precision :: rfp(1), b(1,1)
    integer :: info

    rfp(1) = 9.0d0
    ! Factor: dpftrf on a 1x1 matrix
    call dpftrf('N', 'L', 1, rfp, info)
    if (info /= 0) then
      print *, 'DPFTRF failed for N=1'
      stop
    end if

    b(1,1) = 18.0d0

    call begin_test('n_one')
    call print_array('a', rfp, 1)
    call print_array('b_in', b(1:1,1), 1)

    call dpftrs('N', 'L', 1, 1, rfp, b, 1, info)
    call print_int('info', info)
    call print_array('b_out', b(1:1,1), 1)
    call end_test()
  end subroutine

  subroutine test_solve_5(n, transr, uplo, nrhs, label)
    integer, intent(in) :: n, nrhs
    character, intent(in) :: transr, uplo
    character(len=*), intent(in) :: label
    double precision :: full(10,10), rfp(100), b(10,10), x_exact(10,10)
    integer :: info, nt2, i, j

    nt2 = n * (n + 1) / 2

    ! Build 5x5 SPD matrix
    call build_spd_5x5(full)

    ! Convert to RFP
    call dtrttf(transr, uplo, n, full, 10, rfp, info)
    if (info /= 0) then
      print *, 'DTRTTF failed with info=', info
      stop
    end if

    ! Build known solution
    do j = 1, nrhs
      do i = 1, n
        x_exact(i, j) = dble(i * j)
      end do
    end do

    ! B = full * x_exact
    b = 0.0d0
    call dgemm('N', 'N', n, nrhs, n, 1.0d0, full, 10, x_exact, 10, 0.0d0, b, 10)

    ! Factor the RFP matrix
    call dpftrf(transr, uplo, n, rfp, info)
    if (info /= 0) then
      print *, 'DPFTRF failed with info=', info
      stop
    end if

    call begin_test(label)
    call print_array('a', rfp, nt2)
    call print_matrix('b_in', b, 10, n, nrhs)
    call print_matrix('x_exact', x_exact, 10, n, nrhs)

    ! Solve
    call dpftrs(transr, uplo, n, nrhs, rfp, b, 10, info)
    call print_int('info', info)
    call print_matrix('b_out', b, 10, n, nrhs)
    call end_test()
  end subroutine

  subroutine build_spd_matrix(n, h)
    integer, intent(in) :: n
    double precision, intent(out) :: h(10,10)

    h = 0.0d0

    if (n == 3) then
      h(1,1) = 10.0d0
      h(2,1) = 3.0d0; h(1,2) = 3.0d0
      h(3,1) = 1.0d0; h(1,3) = 1.0d0
      h(2,2) = 8.0d0
      h(3,2) = 2.0d0; h(2,3) = 2.0d0
      h(3,3) = 6.0d0
    else if (n == 4) then
      h(1,1) = 14.0d0
      h(2,1) = 4.0d0; h(1,2) = 4.0d0
      h(3,1) = 2.0d0; h(1,3) = 2.0d0
      h(4,1) = 1.0d0; h(1,4) = 1.0d0
      h(2,2) = 12.0d0
      h(3,2) = 3.0d0; h(2,3) = 3.0d0
      h(4,2) = 2.0d0; h(2,4) = 2.0d0
      h(3,3) = 10.0d0
      h(4,3) = 1.0d0; h(3,4) = 1.0d0
      h(4,4) = 9.0d0
    end if
  end subroutine

  subroutine build_spd_5x5(h)
    double precision, intent(out) :: h(10,10)

    h = 0.0d0
    h(1,1) = 20.0d0
    h(2,1) = 2.0d0; h(1,2) = 2.0d0
    h(3,1) = 1.0d0; h(1,3) = 1.0d0
    h(4,1) = 3.0d0; h(1,4) = 3.0d0
    h(5,1) = 1.0d0; h(1,5) = 1.0d0
    h(2,2) = 18.0d0
    h(3,2) = 2.0d0; h(2,3) = 2.0d0
    h(4,2) = 1.0d0; h(2,4) = 1.0d0
    h(5,2) = 2.0d0; h(2,5) = 2.0d0
    h(3,3) = 15.0d0
    h(4,3) = 1.0d0; h(3,4) = 1.0d0
    h(5,3) = 3.0d0; h(3,5) = 3.0d0
    h(4,4) = 12.0d0
    h(5,4) = 1.0d0; h(4,5) = 1.0d0
    h(5,5) = 10.0d0
  end subroutine

end program
