program test_ztftri
  use test_utils
  implicit none
  complex*16 :: a(100)
  double precision :: a_r(200)
  equivalence (a, a_r)
  integer :: info

  ! Test all 8 TRANSR/UPLO combinations for N=3 (odd) and N=4 (even)
  call test_ztftri_case(3, 'N', 'L', 'N', 'lower_odd_normal_nonunit')
  call test_ztftri_case(3, 'N', 'U', 'N', 'upper_odd_normal_nonunit')
  call test_ztftri_case(3, 'C', 'L', 'N', 'lower_odd_conjtrans_nonunit')
  call test_ztftri_case(3, 'C', 'U', 'N', 'upper_odd_conjtrans_nonunit')
  call test_ztftri_case(4, 'N', 'L', 'N', 'lower_even_normal_nonunit')
  call test_ztftri_case(4, 'N', 'U', 'N', 'upper_even_normal_nonunit')
  call test_ztftri_case(4, 'C', 'L', 'N', 'lower_even_conjtrans_nonunit')
  call test_ztftri_case(4, 'C', 'U', 'N', 'upper_even_conjtrans_nonunit')

  ! Unit diagonal cases
  call test_ztftri_case(3, 'N', 'L', 'U', 'lower_odd_normal_unit')
  call test_ztftri_case(3, 'N', 'U', 'U', 'upper_odd_normal_unit')
  call test_ztftri_case(4, 'N', 'L', 'U', 'lower_even_normal_unit')
  call test_ztftri_case(4, 'N', 'U', 'U', 'upper_even_normal_unit')
  call test_ztftri_case(3, 'C', 'L', 'U', 'lower_odd_conjtrans_unit')
  call test_ztftri_case(3, 'C', 'U', 'U', 'upper_odd_conjtrans_unit')
  call test_ztftri_case(4, 'C', 'L', 'U', 'lower_even_conjtrans_unit')
  call test_ztftri_case(4, 'C', 'U', 'U', 'upper_even_conjtrans_unit')

  ! N=5 (larger odd)
  call test_ztftri_5x5('N', 'L', 'N', 'lower_5_normal_nonunit')
  call test_ztftri_5x5('C', 'U', 'N', 'upper_5_conjtrans_nonunit')
  call test_ztftri_5x5('N', 'U', 'N', 'upper_5_normal_nonunit')
  call test_ztftri_5x5('C', 'L', 'N', 'lower_5_conjtrans_nonunit')

  ! N=0
  a = (0.0d0, 0.0d0)
  call ztftri('N', 'L', 'N', 0, a, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! N=1
  a_r(1) = 2.0d0
  a_r(2) = 1.0d0
  call begin_test('n_one')
  call print_array('input', a_r, 2)
  call ztftri('N', 'L', 'N', 1, a, info)
  call print_int('info', info)
  call print_array('a', a_r, 2)
  call end_test()

  ! Singular matrix (zero on diagonal)
  call test_ztftri_singular()

contains

  subroutine test_ztftri_case(n, transr, uplo, diag, label)
    integer, intent(in) :: n
    character, intent(in) :: transr, uplo, diag
    character(len=*), intent(in) :: label
    complex*16 :: full(10,10), rfp(100)
    double precision :: rfp_r(200)
    equivalence (rfp, rfp_r)
    integer :: info2, nt2

    nt2 = n * (n + 1) / 2

    call build_triangular_matrix(n, uplo, diag, full)
    call ztrttf(transr, uplo, n, full, 10, rfp, info2)
    if (info2 /= 0) then
      print *, 'ZTRTTF failed with info=', info2
      stop
    end if

    call begin_test(label)
    call print_array('input', rfp_r, 2 * nt2)

    call ztftri(transr, uplo, diag, n, rfp, info2)
    call print_int('info', info2)
    call print_array('a', rfp_r, 2 * nt2)
    call end_test()
  end subroutine

  subroutine build_triangular_matrix(n, uplo, diag, t)
    integer, intent(in) :: n
    character, intent(in) :: uplo, diag
    complex*16, intent(out) :: t(10,10)
    integer :: i, j

    t = (0.0d0, 0.0d0)

    if (uplo == 'L') then
      ! Lower triangular
      if (n == 3) then
        t(1,1) = (2.0d0, 1.0d0)
        t(2,1) = (0.5d0, -0.3d0)
        t(3,1) = (1.0d0, 0.5d0)
        t(2,2) = (3.0d0, -1.0d0)
        t(3,2) = (0.8d0, 0.2d0)
        t(3,3) = (4.0d0, 0.0d0)
      else if (n == 4) then
        t(1,1) = (3.0d0, 1.0d0)
        t(2,1) = (1.0d0, -0.5d0)
        t(3,1) = (0.5d0, 0.3d0)
        t(4,1) = (0.2d0, -0.1d0)
        t(2,2) = (2.0d0, -1.0d0)
        t(3,2) = (0.7d0, 0.4d0)
        t(4,2) = (0.3d0, 0.2d0)
        t(3,3) = (5.0d0, 0.0d0)
        t(4,3) = (1.0d0, -0.5d0)
        t(4,4) = (4.0d0, 2.0d0)
      end if
    else
      ! Upper triangular
      if (n == 3) then
        t(1,1) = (2.0d0, 1.0d0)
        t(1,2) = (0.5d0, -0.3d0)
        t(1,3) = (1.0d0, 0.5d0)
        t(2,2) = (3.0d0, -1.0d0)
        t(2,3) = (0.8d0, 0.2d0)
        t(3,3) = (4.0d0, 0.0d0)
      else if (n == 4) then
        t(1,1) = (3.0d0, 1.0d0)
        t(1,2) = (1.0d0, -0.5d0)
        t(1,3) = (0.5d0, 0.3d0)
        t(1,4) = (0.2d0, -0.1d0)
        t(2,2) = (2.0d0, -1.0d0)
        t(2,3) = (0.7d0, 0.4d0)
        t(2,4) = (0.3d0, 0.2d0)
        t(3,3) = (5.0d0, 0.0d0)
        t(3,4) = (1.0d0, -0.5d0)
        t(4,4) = (4.0d0, 2.0d0)
      end if
    end if

    ! If unit diagonal, set diagonal to 1
    if (diag == 'U') then
      do i = 1, n
        t(i,i) = (1.0d0, 0.0d0)
      end do
    end if
  end subroutine

  subroutine test_ztftri_5x5(transr, uplo, diag, label)
    character, intent(in) :: transr, uplo, diag
    character(len=*), intent(in) :: label
    complex*16 :: full(10,10), rfp(100)
    double precision :: rfp_r(200)
    equivalence (rfp, rfp_r)
    integer :: info2, nt2

    nt2 = 5 * 6 / 2  ! 15

    call build_triangular_5x5(uplo, full)
    call ztrttf(transr, uplo, 5, full, 10, rfp, info2)
    if (info2 /= 0) then
      print *, 'ZTRTTF failed with info=', info2
      stop
    end if

    call begin_test(label)
    call print_array('input', rfp_r, 2 * nt2)

    call ztftri(transr, uplo, diag, 5, rfp, info2)
    call print_int('info', info2)
    call print_array('a', rfp_r, 2 * nt2)
    call end_test()
  end subroutine

  subroutine build_triangular_5x5(uplo, t)
    character, intent(in) :: uplo
    complex*16, intent(out) :: t(10,10)

    t = (0.0d0, 0.0d0)

    if (uplo == 'L') then
      t(1,1) = (5.0d0, 0.0d0)
      t(2,1) = (1.0d0, -0.5d0)
      t(3,1) = (0.3d0, 0.2d0)
      t(4,1) = (0.1d0, -0.1d0)
      t(5,1) = (0.2d0, 0.3d0)
      t(2,2) = (4.0d0, 1.0d0)
      t(3,2) = (0.5d0, -0.3d0)
      t(4,2) = (0.2d0, 0.1d0)
      t(5,2) = (0.1d0, -0.2d0)
      t(3,3) = (3.0d0, -1.0d0)
      t(4,3) = (0.4d0, 0.2d0)
      t(5,3) = (0.3d0, 0.1d0)
      t(4,4) = (2.0d0, 0.5d0)
      t(5,4) = (0.5d0, -0.4d0)
      t(5,5) = (6.0d0, 0.0d0)
    else
      t(1,1) = (5.0d0, 0.0d0)
      t(1,2) = (1.0d0, -0.5d0)
      t(1,3) = (0.3d0, 0.2d0)
      t(1,4) = (0.1d0, -0.1d0)
      t(1,5) = (0.2d0, 0.3d0)
      t(2,2) = (4.0d0, 1.0d0)
      t(2,3) = (0.5d0, -0.3d0)
      t(2,4) = (0.2d0, 0.1d0)
      t(2,5) = (0.1d0, -0.2d0)
      t(3,3) = (3.0d0, -1.0d0)
      t(3,4) = (0.4d0, 0.2d0)
      t(3,5) = (0.3d0, 0.1d0)
      t(4,4) = (2.0d0, 0.5d0)
      t(4,5) = (0.5d0, -0.4d0)
      t(5,5) = (6.0d0, 0.0d0)
    end if
  end subroutine

  subroutine test_ztftri_singular()
    complex*16 :: full(10,10), rfp(100)
    double precision :: rfp_r(200)
    equivalence (rfp, rfp_r)
    integer :: info2, nt2

    nt2 = 3 * 4 / 2  ! 6

    full = (0.0d0, 0.0d0)
    ! Lower triangular with zero on diagonal at position (2,2)
    full(1,1) = (2.0d0, 1.0d0)
    full(2,1) = (0.5d0, -0.3d0)
    full(3,1) = (1.0d0, 0.5d0)
    full(2,2) = (0.0d0, 0.0d0)   ! singular!
    full(3,2) = (0.8d0, 0.2d0)
    full(3,3) = (4.0d0, 0.0d0)

    call ztrttf('N', 'L', 3, full, 10, rfp, info2)

    call begin_test('singular')
    call print_array('input', rfp_r, 2 * nt2)

    call ztftri('N', 'L', 'N', 3, rfp, info2)
    call print_int('info', info2)
    call end_test()
  end subroutine

end program
