program test_dpftri
  use test_utils
  implicit none

  ! Test all 8 code paths: N odd/even x TRANSR N/T x UPLO L/U
  call test_dpftri_case(3, 'N', 'L', 'lower_odd_normal')
  call test_dpftri_case(3, 'N', 'U', 'upper_odd_normal')
  call test_dpftri_case(3, 'T', 'L', 'lower_odd_trans')
  call test_dpftri_case(3, 'T', 'U', 'upper_odd_trans')
  call test_dpftri_case(4, 'N', 'L', 'lower_even_normal')
  call test_dpftri_case(4, 'N', 'U', 'upper_even_normal')
  call test_dpftri_case(4, 'T', 'L', 'lower_even_trans')
  call test_dpftri_case(4, 'T', 'U', 'upper_even_trans')

  ! Test N=0
  call test_dpftri_n0()

  ! Test N=1
  call test_dpftri_n1()

  ! Test N=5 (larger odd)
  call test_dpftri_5x5('N', 'L', 'lower_5_normal')
  call test_dpftri_5x5('T', 'U', 'upper_5_trans')

  ! Test singular (info > 0)
  call test_dpftri_singular()

contains

  subroutine test_dpftri_case(n, transr, uplo, label)
    integer, intent(in) :: n
    character, intent(in) :: transr, uplo
    character(len=*), intent(in) :: label
    double precision :: full(10,10), rfp(100)
    integer :: info, nt

    nt = n * (n + 1) / 2

    call build_spd_matrix(n, full)
    call dtrttf(transr, uplo, n, full, 10, rfp, info)
    if (info /= 0) then
      print *, 'DTRTTF failed with info=', info
      stop
    end if

    call begin_test(label)
    call print_array('input', rfp, nt)

    ! First factorize
    call dpftrf(transr, uplo, n, rfp, info)
    if (info /= 0) then
      print *, 'DPFTRF failed with info=', info
      stop
    end if

    ! Then invert
    call dpftri(transr, uplo, n, rfp, info)
    call print_int('info', info)
    call print_array('a', rfp, nt)
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

  subroutine test_dpftri_n0()
    double precision :: a(1)
    integer :: info

    a = 0.0d0
    call dpftri('N', 'L', 0, a, info)
    call begin_test('n_zero')
    call print_int('info', info)
    call end_test()
  end subroutine

  subroutine test_dpftri_n1()
    double precision :: a(1)
    integer :: info

    ! N=1: a = [9.0], factorize then invert
    a(1) = 9.0d0
    call dpftrf('N', 'L', 1, a, info)
    if (info /= 0) then
      print *, 'DPFTRF N=1 failed with info=', info
      stop
    end if

    call dpftri('N', 'L', 1, a, info)
    call begin_test('n_one')
    call print_int('info', info)
    call print_array('a', a, 1)
    call end_test()
  end subroutine

  subroutine test_dpftri_5x5(transr, uplo, label)
    character, intent(in) :: transr, uplo
    character(len=*), intent(in) :: label
    double precision :: full(10,10), rfp(100)
    integer :: info, nt

    nt = 5 * 6 / 2  ! 15

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

    call dtrttf(transr, uplo, 5, full, 10, rfp, info)
    if (info /= 0) then
      print *, 'DTRTTF failed with info=', info
      stop
    end if

    call begin_test(label)
    call print_array('input', rfp, nt)

    call dpftrf(transr, uplo, 5, rfp, info)
    if (info /= 0) then
      print *, 'DPFTRF 5x5 failed with info=', info
      stop
    end if

    call dpftri(transr, uplo, 5, rfp, info)
    call print_int('info', info)
    call print_array('a', rfp, nt)
    call end_test()
  end subroutine

  subroutine test_dpftri_singular()
    double precision :: rfp(6)
    integer :: info

    ! Create a triangular factor with a zero on the diagonal
    ! This simulates a factored matrix where dtftri will return info > 0
    ! RFP format for N=3, TRANSR='N', UPLO='L': LDA=N=3
    ! The lower triangular factor L in column-major RFP:
    ! Layout: [L(0,0), L(1,0), L(2,0), ?, L(1,1), L(2,1)]
    ! but 0-based in Fortran: A(0:2,0:1)
    ! Set L(1,1) = 0 to trigger singular detection
    rfp(1) = 1.0d0
    rfp(2) = 0.5d0
    rfp(3) = 0.3d0
    rfp(4) = 0.0d0  ! This will be the zero diagonal element
    rfp(5) = 1.0d0
    rfp(6) = 0.2d0

    call dpftri('N', 'L', 3, rfp, info)
    call begin_test('singular')
    call print_int('info', info)
    call end_test()
  end subroutine

end program
