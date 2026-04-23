program test_dpftrf
  use test_utils
  implicit none
  double precision :: a(100)
  integer :: info

  ! Test 1-8: Various TRANSR/UPLO combinations for N=3 (odd) and N=4 (even)
  call test_dpftrf_via_trttf(3, 'N', 'L', 'lower_odd_normal')
  call test_dpftrf_via_trttf(3, 'N', 'U', 'upper_odd_normal')
  call test_dpftrf_via_trttf(3, 'T', 'L', 'lower_odd_trans')
  call test_dpftrf_via_trttf(3, 'T', 'U', 'upper_odd_trans')
  call test_dpftrf_via_trttf(4, 'N', 'L', 'lower_even_normal')
  call test_dpftrf_via_trttf(4, 'N', 'U', 'upper_even_normal')
  call test_dpftrf_via_trttf(4, 'T', 'L', 'lower_even_trans')
  call test_dpftrf_via_trttf(4, 'T', 'U', 'upper_even_trans')

  ! Test 9: N=0
  a = 0.0d0
  call dpftrf('N', 'L', 0, a, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 10: N=1
  a(1) = 9.0d0
  call begin_test('n_one')
  call print_array('input', a, 1)
  call dpftrf('N', 'L', 1, a, info)
  call print_int('info', info)
  call print_array('a', a, 1)
  call end_test()

  ! Test 11: Not positive definite (N=3)
  call test_dpftrf_notpd()

  ! Test 12-13: N=5 (larger odd)
  call test_dpftrf_5x5('N', 'L', 'lower_5_normal')
  call test_dpftrf_5x5('T', 'U', 'upper_5_trans')

  ! Test 14-19: Not positive definite for various TRANSR/UPLO combos
  call test_dpftrf_notpd_combo(3, 'N', 'U', 'notpd_odd_normal_upper')
  call test_dpftrf_notpd_combo(3, 'T', 'L', 'notpd_odd_trans_lower')
  call test_dpftrf_notpd_combo(3, 'T', 'U', 'notpd_odd_trans_upper')
  call test_dpftrf_notpd_combo(4, 'N', 'L', 'notpd_even_normal_lower')
  call test_dpftrf_notpd_combo(4, 'N', 'U', 'notpd_even_normal_upper')
  call test_dpftrf_notpd_combo(4, 'T', 'L', 'notpd_even_trans_lower')
  call test_dpftrf_notpd_combo(4, 'T', 'U', 'notpd_even_trans_upper')

contains

  subroutine test_dpftrf_via_trttf(n, transr, uplo, label)
    integer, intent(in) :: n
    character, intent(in) :: transr, uplo
    character(len=*), intent(in) :: label
    double precision :: full(10,10), rfp(100)
    integer :: info2, nt2

    nt2 = n * (n + 1) / 2

    call build_spd_matrix(n, full)
    call dtrttf(transr, uplo, n, full, 10, rfp, info2)
    if (info2 /= 0) then
      print *, 'DTRTTF failed with info=', info2
      stop
    end if

    call begin_test(label)
    call print_array('input', rfp, nt2)

    call dpftrf(transr, uplo, n, rfp, info2)
    call print_int('info', info2)
    call print_array('a', rfp, nt2)
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

  subroutine test_dpftrf_notpd()
    double precision :: full(10,10), rfp(100)
    integer :: info2, nt2

    nt2 = 3 * 4 / 2  ! 6

    full = 0.0d0
    full(1,1) = 1.0d0
    full(2,1) = 2.0d0; full(1,2) = 2.0d0
    full(3,1) = 3.0d0; full(1,3) = 3.0d0
    full(2,2) = 1.0d0
    full(3,2) = 4.0d0; full(2,3) = 4.0d0
    full(3,3) = 1.0d0

    call dtrttf('N', 'L', 3, full, 10, rfp, info2)

    call begin_test('not_posdef')
    call print_array('input', rfp, nt2)

    call dpftrf('N', 'L', 3, rfp, info2)
    call print_int('info', info2)
    call end_test()
  end subroutine

  subroutine test_dpftrf_5x5(transr, uplo, label)
    character, intent(in) :: transr, uplo
    character(len=*), intent(in) :: label
    double precision :: full(10,10), rfp(100)
    integer :: info2, nt2

    nt2 = 5 * 6 / 2  ! 15

    full = 0.0d0
    full(1,1) = 20.0d0
    full(2,1) = 2.0d0; full(1,2) = 2.0d0
    full(3,1) = 1.0d0; full(1,3) = 1.0d0
    full(4,1) = 3.0d0; full(1,4) = 3.0d0
    full(5,1) = 1.0d0; full(1,5) = 1.0d0
    full(2,2) = 18.0d0
    full(3,2) = 2.0d0; full(2,3) = 2.0d0
    full(4,2) = 1.0d0; full(2,4) = 1.0d0
    full(5,2) = 2.0d0; full(2,5) = 2.0d0
    full(3,3) = 15.0d0
    full(4,3) = 1.0d0; full(3,4) = 1.0d0
    full(5,3) = 3.0d0; full(3,5) = 3.0d0
    full(4,4) = 12.0d0
    full(5,4) = 1.0d0; full(4,5) = 1.0d0
    full(5,5) = 10.0d0

    call dtrttf(transr, uplo, 5, full, 10, rfp, info2)
    if (info2 /= 0) then
      print *, 'DTRTTF failed with info=', info2
      stop
    end if

    call begin_test(label)
    call print_array('input', rfp, nt2)

    call dpftrf(transr, uplo, 5, rfp, info2)
    call print_int('info', info2)
    call print_array('a', rfp, nt2)
    call end_test()
  end subroutine

  subroutine test_dpftrf_notpd_combo(n, transr, uplo, label)
    integer, intent(in) :: n
    character, intent(in) :: transr, uplo
    character(len=*), intent(in) :: label
    double precision :: full(10,10), rfp(100)
    integer :: info2, nt2

    nt2 = n * (n + 1) / 2

    call build_notpd_matrix(n, full)

    call dtrttf(transr, uplo, n, full, 10, rfp, info2)
    if (info2 /= 0) then
      print *, 'DTRTTF failed with info=', info2
      stop
    end if

    call begin_test(label)
    call print_array('input', rfp, nt2)

    call dpftrf(transr, uplo, n, rfp, info2)
    call print_int('info', info2)
    call end_test()
  end subroutine

  subroutine build_notpd_matrix(n, h)
    integer, intent(in) :: n
    double precision, intent(out) :: h(10,10)

    h = 0.0d0

    if (n == 3) then
      h(1,1) = 1.0d0
      h(2,1) = 2.0d0; h(1,2) = 2.0d0
      h(3,1) = 3.0d0; h(1,3) = 3.0d0
      h(2,2) = 1.0d0
      h(3,2) = 4.0d0; h(2,3) = 4.0d0
      h(3,3) = 1.0d0
    else if (n == 4) then
      h(1,1) = 1.0d0
      h(2,1) = 2.0d0; h(1,2) = 2.0d0
      h(3,1) = 3.0d0; h(1,3) = 3.0d0
      h(4,1) = 1.0d0; h(1,4) = 1.0d0
      h(2,2) = 1.0d0
      h(3,2) = 4.0d0; h(2,3) = 4.0d0
      h(4,2) = 2.0d0; h(2,4) = 2.0d0
      h(3,3) = 1.0d0
      h(4,3) = 1.0d0; h(3,4) = 1.0d0
      h(4,4) = 1.0d0
    end if
  end subroutine

end program
