program test_zpftrf
  use test_utils
  implicit none
  complex*16 :: a(100)
  double precision :: a_r(200)
  equivalence (a, a_r)
  integer :: info

  ! Test 1-8: Various TRANSR/UPLO combinations for N=3 and N=4
  call test_zpftrf_via_trttf(3, 'N', 'L', 'lower_odd_normal')
  call test_zpftrf_via_trttf(3, 'N', 'U', 'upper_odd_normal')
  call test_zpftrf_via_trttf(3, 'C', 'L', 'lower_odd_conjtrans')
  call test_zpftrf_via_trttf(3, 'C', 'U', 'upper_odd_conjtrans')
  call test_zpftrf_via_trttf(4, 'N', 'L', 'lower_even_normal')
  call test_zpftrf_via_trttf(4, 'N', 'U', 'upper_even_normal')
  call test_zpftrf_via_trttf(4, 'C', 'L', 'lower_even_conjtrans')
  call test_zpftrf_via_trttf(4, 'C', 'U', 'upper_even_conjtrans')

  ! Test 9: N=0
  a = (0.0d0, 0.0d0)
  call zpftrf('N', 'L', 0, a, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 10: N=1
  a_r(1) = 5.0d0
  a_r(2) = 0.0d0
  call begin_test('n_one')
  call print_array('input', a_r, 2)
  call zpftrf('N', 'L', 1, a, info)
  call print_int('info', info)
  call print_array('a', a_r, 2)
  call end_test()

  ! Test 11: Not positive definite (N=3)
  call test_zpftrf_notpd()

  ! Test 12-13: N=5 (larger odd)
  call test_zpftrf_5x5('N', 'L', 'lower_5_normal')
  call test_zpftrf_5x5('C', 'U', 'upper_5_conjtrans')

  ! Test 14-19: Not positive definite for various TRANSR/UPLO combos
  call test_zpftrf_notpd_combo(3, 'N', 'U', 'notpd_odd_normal_upper')
  call test_zpftrf_notpd_combo(3, 'C', 'L', 'notpd_odd_conjtrans_lower')
  call test_zpftrf_notpd_combo(3, 'C', 'U', 'notpd_odd_conjtrans_upper')
  call test_zpftrf_notpd_combo(4, 'N', 'L', 'notpd_even_normal_lower')
  call test_zpftrf_notpd_combo(4, 'N', 'U', 'notpd_even_normal_upper')
  call test_zpftrf_notpd_combo(4, 'C', 'L', 'notpd_even_conjtrans_lower')
  call test_zpftrf_notpd_combo(4, 'C', 'U', 'notpd_even_conjtrans_upper')

contains

  subroutine test_zpftrf_via_trttf(n, transr, uplo, label)
    integer, intent(in) :: n
    character, intent(in) :: transr, uplo
    character(len=*), intent(in) :: label
    complex*16 :: full(10,10), rfp(100)
    double precision :: rfp_r(200)
    equivalence (rfp, rfp_r)
    integer :: info2, nt2

    nt2 = n * (n + 1) / 2

    call build_hpd_matrix(n, full)
    call ztrttf(transr, uplo, n, full, 10, rfp, info2)
    if (info2 /= 0) then
      print *, 'ZTRTTF failed with info=', info2
      stop
    end if

    call begin_test(label)
    call print_array('input', rfp_r, 2 * nt2)

    call zpftrf(transr, uplo, n, rfp, info2)
    call print_int('info', info2)
    call print_array('a', rfp_r, 2 * nt2)
    call end_test()
  end subroutine

  subroutine build_hpd_matrix(n, h)
    integer, intent(in) :: n
    complex*16, intent(out) :: h(10,10)

    h = (0.0d0, 0.0d0)

    if (n == 3) then
      h(1,1) = (10.0d0, 0.0d0)
      h(2,1) = (3.0d0, -1.0d0); h(1,2) = (3.0d0, 1.0d0)
      h(3,1) = (1.0d0, 2.0d0);  h(1,3) = (1.0d0, -2.0d0)
      h(2,2) = (8.0d0, 0.0d0)
      h(3,2) = (2.0d0, -1.0d0); h(2,3) = (2.0d0, 1.0d0)
      h(3,3) = (6.0d0, 0.0d0)
    else if (n == 4) then
      h(1,1) = (14.0d0, 0.0d0)
      h(2,1) = (4.0d0, -2.0d0); h(1,2) = (4.0d0, 2.0d0)
      h(3,1) = (2.0d0, 1.0d0);  h(1,3) = (2.0d0, -1.0d0)
      h(4,1) = (1.0d0, 3.0d0);  h(1,4) = (1.0d0, -3.0d0)
      h(2,2) = (12.0d0, 0.0d0)
      h(3,2) = (3.0d0, -1.0d0); h(2,3) = (3.0d0, 1.0d0)
      h(4,2) = (2.0d0, 2.0d0);  h(2,4) = (2.0d0, -2.0d0)
      h(3,3) = (10.0d0, 0.0d0)
      h(4,3) = (1.0d0, -1.0d0); h(3,4) = (1.0d0, 1.0d0)
      h(4,4) = (9.0d0, 0.0d0)
    end if
  end subroutine

  subroutine test_zpftrf_notpd()
    complex*16 :: full(10,10), rfp(100)
    double precision :: rfp_r(200)
    equivalence (rfp, rfp_r)
    integer :: info2, nt2

    nt2 = 3 * 4 / 2  ! 6

    full = (0.0d0, 0.0d0)
    full(1,1) = (1.0d0, 0.0d0)
    full(2,1) = (2.0d0, -1.0d0); full(1,2) = (2.0d0, 1.0d0)
    full(3,1) = (3.0d0, 0.0d0);  full(1,3) = (3.0d0, 0.0d0)
    full(2,2) = (1.0d0, 0.0d0)
    full(3,2) = (4.0d0, 0.0d0);  full(2,3) = (4.0d0, 0.0d0)
    full(3,3) = (1.0d0, 0.0d0)

    call ztrttf('N', 'L', 3, full, 10, rfp, info2)

    call begin_test('not_posdef')
    call print_array('input', rfp_r, 2 * nt2)

    call zpftrf('N', 'L', 3, rfp, info2)
    call print_int('info', info2)
    call end_test()
  end subroutine

  subroutine test_zpftrf_5x5(transr, uplo, label)
    character, intent(in) :: transr, uplo
    character(len=*), intent(in) :: label
    complex*16 :: full(10,10), rfp(100)
    double precision :: rfp_r(200)
    equivalence (rfp, rfp_r)
    integer :: info2, nt2

    nt2 = 5 * 6 / 2  ! 15

    full = (0.0d0, 0.0d0)
    full(1,1) = (20.0d0, 0.0d0)
    full(2,1) = (2.0d0, -1.0d0); full(1,2) = (2.0d0, 1.0d0)
    full(3,1) = (1.0d0, 3.0d0);  full(1,3) = (1.0d0, -3.0d0)
    full(4,1) = (3.0d0, -2.0d0); full(1,4) = (3.0d0, 2.0d0)
    full(5,1) = (1.0d0, 1.0d0);  full(1,5) = (1.0d0, -1.0d0)
    full(2,2) = (18.0d0, 0.0d0)
    full(3,2) = (2.0d0, -2.0d0); full(2,3) = (2.0d0, 2.0d0)
    full(4,2) = (1.0d0, 1.0d0);  full(2,4) = (1.0d0, -1.0d0)
    full(5,2) = (2.0d0, 0.0d0);  full(2,5) = (2.0d0, 0.0d0)
    full(3,3) = (15.0d0, 0.0d0)
    full(4,3) = (1.0d0, -1.0d0); full(3,4) = (1.0d0, 1.0d0)
    full(5,3) = (3.0d0, 2.0d0);  full(3,5) = (3.0d0, -2.0d0)
    full(4,4) = (12.0d0, 0.0d0)
    full(5,4) = (1.0d0, -2.0d0); full(4,5) = (1.0d0, 2.0d0)
    full(5,5) = (10.0d0, 0.0d0)

    call ztrttf(transr, uplo, 5, full, 10, rfp, info2)
    if (info2 /= 0) then
      print *, 'ZTRTTF failed with info=', info2
      stop
    end if

    call begin_test(label)
    call print_array('input', rfp_r, 2 * nt2)

    call zpftrf(transr, uplo, 5, rfp, info2)
    call print_int('info', info2)
    call print_array('a', rfp_r, 2 * nt2)
    call end_test()
  end subroutine

  subroutine test_zpftrf_notpd_combo(n, transr, uplo, label)
    integer, intent(in) :: n
    character, intent(in) :: transr, uplo
    character(len=*), intent(in) :: label
    complex*16 :: full(10,10), rfp(100)
    double precision :: rfp_r(200)
    equivalence (rfp, rfp_r)
    integer :: info2, nt2

    nt2 = n * (n + 1) / 2

    ! Build a not-positive-definite matrix
    call build_notpd_matrix(n, full)

    call ztrttf(transr, uplo, n, full, 10, rfp, info2)
    if (info2 /= 0) then
      print *, 'ZTRTTF failed with info=', info2
      stop
    end if

    call begin_test(label)
    call print_array('input', rfp_r, 2 * nt2)

    call zpftrf(transr, uplo, n, rfp, info2)
    call print_int('info', info2)
    call end_test()
  end subroutine

  subroutine build_notpd_matrix(n, h)
    integer, intent(in) :: n
    complex*16, intent(out) :: h(10,10)
    integer :: i, j

    h = (0.0d0, 0.0d0)

    if (n == 3) then
      h(1,1) = (1.0d0, 0.0d0)
      h(2,1) = (2.0d0, -1.0d0); h(1,2) = (2.0d0, 1.0d0)
      h(3,1) = (3.0d0, 0.0d0);  h(1,3) = (3.0d0, 0.0d0)
      h(2,2) = (1.0d0, 0.0d0)
      h(3,2) = (4.0d0, 0.0d0);  h(2,3) = (4.0d0, 0.0d0)
      h(3,3) = (1.0d0, 0.0d0)
    else if (n == 4) then
      h(1,1) = (1.0d0, 0.0d0)
      h(2,1) = (2.0d0, -1.0d0); h(1,2) = (2.0d0, 1.0d0)
      h(3,1) = (3.0d0, 0.0d0);  h(1,3) = (3.0d0, 0.0d0)
      h(4,1) = (1.0d0, 1.0d0);  h(1,4) = (1.0d0, -1.0d0)
      h(2,2) = (1.0d0, 0.0d0)
      h(3,2) = (4.0d0, 0.0d0);  h(2,3) = (4.0d0, 0.0d0)
      h(4,2) = (2.0d0, 0.0d0);  h(2,4) = (2.0d0, 0.0d0)
      h(3,3) = (1.0d0, 0.0d0)
      h(4,3) = (1.0d0, -1.0d0); h(3,4) = (1.0d0, 1.0d0)
      h(4,4) = (1.0d0, 0.0d0)
    end if
  end subroutine

end program
