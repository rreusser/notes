program test_zpftrs
  use test_utils
  implicit none

  ! Test zpftrs: Solve A*X = B where A is HPD in RFP format
  ! Strategy: build HPD matrix, convert to RFP, factorize with zpftrf,
  ! then solve with zpftrs. Use LDB=N so B is tightly packed.

  ! Test 1-4: N=3, nrhs=1, all TRANSR/UPLO combos
  call test_solve_3(1, 'N', 'L', 'n3_nrhs1_lower_normal')
  call test_solve_3(1, 'N', 'U', 'n3_nrhs1_upper_normal')
  call test_solve_3(1, 'C', 'L', 'n3_nrhs1_lower_conjtrans')
  call test_solve_3(1, 'C', 'U', 'n3_nrhs1_upper_conjtrans')

  ! Test 5-8: N=4, nrhs=1, all TRANSR/UPLO combos
  call test_solve_4(1, 'N', 'L', 'n4_nrhs1_lower_normal')
  call test_solve_4(1, 'N', 'U', 'n4_nrhs1_upper_normal')
  call test_solve_4(1, 'C', 'L', 'n4_nrhs1_lower_conjtrans')
  call test_solve_4(1, 'C', 'U', 'n4_nrhs1_upper_conjtrans')

  ! Test 9-10: N=3, nrhs=2, multiple RHS
  call test_solve_3(2, 'N', 'L', 'n3_nrhs2_lower_normal')
  call test_solve_3(2, 'C', 'U', 'n3_nrhs2_upper_conjtrans')

  ! Test 11-12: N=4, nrhs=2
  call test_solve_4(2, 'N', 'L', 'n4_nrhs2_lower_normal')
  call test_solve_4(2, 'C', 'U', 'n4_nrhs2_upper_conjtrans')

  ! Test 13: N=0
  call test_n_zero()

  ! Test 14: N=1
  call test_n_one()

contains

  subroutine build_hpd3(h)
    complex*16, intent(out) :: h(3,3)
    h = (0.0d0, 0.0d0)
    h(1,1) = (10.0d0, 0.0d0)
    h(2,1) = (3.0d0, -1.0d0); h(1,2) = (3.0d0, 1.0d0)
    h(3,1) = (1.0d0, 2.0d0);  h(1,3) = (1.0d0, -2.0d0)
    h(2,2) = (8.0d0, 0.0d0)
    h(3,2) = (2.0d0, -1.0d0); h(2,3) = (2.0d0, 1.0d0)
    h(3,3) = (6.0d0, 0.0d0)
  end subroutine

  subroutine build_hpd4(h)
    complex*16, intent(out) :: h(4,4)
    h = (0.0d0, 0.0d0)
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
  end subroutine

  subroutine test_solve_3(nrhs, transr, uplo, label)
    integer, intent(in) :: nrhs
    character, intent(in) :: transr, uplo
    character(len=*), intent(in) :: label
    integer, parameter :: n = 3, nt2 = 6
    complex*16 :: full(n,n), rfp(nt2), b(n,2)
    double precision :: rfp_r(2*nt2), b_r(2*n*2)
    equivalence (rfp, rfp_r)
    integer :: info, i, j

    call build_hpd3(full)
    call ztrttf(transr, uplo, n, full, n, rfp, info)
    if (info /= 0) stop 'ZTRTTF failed'

    call zpftrf(transr, uplo, n, rfp, info)
    if (info /= 0) stop 'ZPFTRF failed'

    ! Build RHS
    b = (0.0d0, 0.0d0)
    do j = 1, nrhs
      do i = 1, n
        b(i,j) = dcmplx(dble(i + j), dble(j - i) * 0.5d0)
      end do
    end do

    ! Pack B into b_r (column-major, tightly packed with LDB=N)
    do j = 1, nrhs
      do i = 1, n
        b_r(2*((j-1)*n + (i-1)) + 1) = dble(b(i,j))
        b_r(2*((j-1)*n + (i-1)) + 2) = dimag(b(i,j))
      end do
    end do

    call begin_test(label)
    call print_array('a', rfp_r, 2 * nt2)
    call print_array('b_in', b_r(1:2*n*nrhs), 2 * n * nrhs)

    call zpftrs(transr, uplo, n, nrhs, rfp, b, n, info)

    ! Pack solution B into b_r
    do j = 1, nrhs
      do i = 1, n
        b_r(2*((j-1)*n + (i-1)) + 1) = dble(b(i,j))
        b_r(2*((j-1)*n + (i-1)) + 2) = dimag(b(i,j))
      end do
    end do

    call print_int('info', info)
    call print_array('b_out', b_r(1:2*n*nrhs), 2 * n * nrhs)
    call end_test()
  end subroutine

  subroutine test_solve_4(nrhs, transr, uplo, label)
    integer, intent(in) :: nrhs
    character, intent(in) :: transr, uplo
    character(len=*), intent(in) :: label
    integer, parameter :: n = 4, nt2 = 10
    complex*16 :: full(n,n), rfp(nt2), b(n,2)
    double precision :: rfp_r(2*nt2), b_r(2*n*2)
    equivalence (rfp, rfp_r)
    integer :: info, i, j

    call build_hpd4(full)
    call ztrttf(transr, uplo, n, full, n, rfp, info)
    if (info /= 0) stop 'ZTRTTF failed'

    call zpftrf(transr, uplo, n, rfp, info)
    if (info /= 0) stop 'ZPFTRF failed'

    ! Build RHS
    b = (0.0d0, 0.0d0)
    do j = 1, nrhs
      do i = 1, n
        b(i,j) = dcmplx(dble(i + j), dble(j - i) * 0.5d0)
      end do
    end do

    ! Pack B into b_r (column-major, tightly packed with LDB=N)
    do j = 1, nrhs
      do i = 1, n
        b_r(2*((j-1)*n + (i-1)) + 1) = dble(b(i,j))
        b_r(2*((j-1)*n + (i-1)) + 2) = dimag(b(i,j))
      end do
    end do

    call begin_test(label)
    call print_array('a', rfp_r, 2 * nt2)
    call print_array('b_in', b_r(1:2*n*nrhs), 2 * n * nrhs)

    call zpftrs(transr, uplo, n, nrhs, rfp, b, n, info)

    ! Pack solution B into b_r
    do j = 1, nrhs
      do i = 1, n
        b_r(2*((j-1)*n + (i-1)) + 1) = dble(b(i,j))
        b_r(2*((j-1)*n + (i-1)) + 2) = dimag(b(i,j))
      end do
    end do

    call print_int('info', info)
    call print_array('b_out', b_r(1:2*n*nrhs), 2 * n * nrhs)
    call end_test()
  end subroutine

  subroutine test_n_zero()
    complex*16 :: a(1), b(1,1)
    integer :: info

    a = (0.0d0, 0.0d0)
    b = (0.0d0, 0.0d0)

    call begin_test('n_zero')
    call zpftrs('N', 'L', 0, 1, a, b, 1, info)
    call print_int('info', info)
    call end_test()
  end subroutine

  subroutine test_n_one()
    complex*16 :: a(1), b(1,1)
    double precision :: a_r(2), b_r(2)
    equivalence (a, a_r)
    equivalence (b, b_r)
    integer :: info

    ! 1x1 HPD: A = (4, 0). Cholesky: L = (2, 0). RFP is just a(1) = (2, 0).
    a(1) = (2.0d0, 0.0d0)
    b(1,1) = (6.0d0, 4.0d0)

    call begin_test('n_one')
    call print_array('a', a_r, 2)
    call print_array('b_in', b_r, 2)
    call zpftrs('N', 'L', 1, 1, a, b, 1, info)
    call print_int('info', info)
    call print_array('b_out', b_r, 2)
    call end_test()
  end subroutine

end program
