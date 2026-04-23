program test_zpftri
  use test_utils
  implicit none
  complex*16 :: a(100)
  double precision :: a_r(200)
  equivalence (a, a_r)
  integer :: info

  ! Test all 8 TRANSR/UPLO combinations for N=3 (odd) and N=4 (even)
  call test_zpftri_case(3, 'N', 'L', 'lower_odd_normal')
  call test_zpftri_case(3, 'N', 'U', 'upper_odd_normal')
  call test_zpftri_case(3, 'C', 'L', 'lower_odd_conjtrans')
  call test_zpftri_case(3, 'C', 'U', 'upper_odd_conjtrans')
  call test_zpftri_case(4, 'N', 'L', 'lower_even_normal')
  call test_zpftri_case(4, 'N', 'U', 'upper_even_normal')
  call test_zpftri_case(4, 'C', 'L', 'lower_even_conjtrans')
  call test_zpftri_case(4, 'C', 'U', 'upper_even_conjtrans')

  ! Test N=0
  a = (0.0d0, 0.0d0)
  call zpftri('N', 'L', 0, a, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test N=1
  a_r(1) = 5.0d0
  a_r(2) = 0.0d0
  call begin_test('n_one')
  call print_array('input', a_r, 2)
  call zpftrf('N', 'L', 1, a, info)
  call zpftri('N', 'L', 1, a, info)
  call print_int('info', info)
  call print_array('a', a_r, 2)
  call end_test()

  ! Test N=5 (larger odd)
  call test_zpftri_5x5('N', 'L', 'lower_5_normal')
  call test_zpftri_5x5('C', 'U', 'upper_5_conjtrans')
  call test_zpftri_5x5('N', 'U', 'upper_5_normal')
  call test_zpftri_5x5('C', 'L', 'lower_5_conjtrans')

contains

  subroutine test_zpftri_case(n, transr, uplo, label)
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

    ! Factorize first, then invert
    call zpftrf(transr, uplo, n, rfp, info2)
    if (info2 /= 0) then
      print *, 'ZPFTRF failed with info=', info2
      stop
    end if

    call zpftri(transr, uplo, n, rfp, info2)
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

  subroutine test_zpftri_5x5(transr, uplo, label)
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
    if (info2 /= 0) then
      print *, 'ZPFTRF failed with info=', info2
      stop
    end if

    call zpftri(transr, uplo, 5, rfp, info2)
    call print_int('info', info2)
    call print_array('a', rfp_r, 2 * nt2)
    call end_test()
  end subroutine

end program
