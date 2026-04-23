program test_zhfrk
  use test_utils
  implicit none

  ! Test all 16 combinations: TRANSR(N,C) x UPLO(L,U) x TRANS(N,C) x N(odd=3,even=4)
  ! plus edge cases

  ! N=3 (odd), K=2
  call test_zhfrk_combo(3, 2, 'N', 'L', 'N', 'odd_NLN')
  call test_zhfrk_combo(3, 2, 'N', 'L', 'C', 'odd_NLC')
  call test_zhfrk_combo(3, 2, 'N', 'U', 'N', 'odd_NUN')
  call test_zhfrk_combo(3, 2, 'N', 'U', 'C', 'odd_NUC')
  call test_zhfrk_combo(3, 2, 'C', 'L', 'N', 'odd_CLN')
  call test_zhfrk_combo(3, 2, 'C', 'L', 'C', 'odd_CLC')
  call test_zhfrk_combo(3, 2, 'C', 'U', 'N', 'odd_CUN')
  call test_zhfrk_combo(3, 2, 'C', 'U', 'C', 'odd_CUC')

  ! N=4 (even), K=2
  call test_zhfrk_combo(4, 2, 'N', 'L', 'N', 'even_NLN')
  call test_zhfrk_combo(4, 2, 'N', 'L', 'C', 'even_NLC')
  call test_zhfrk_combo(4, 2, 'N', 'U', 'N', 'even_NUN')
  call test_zhfrk_combo(4, 2, 'N', 'U', 'C', 'even_NUC')
  call test_zhfrk_combo(4, 2, 'C', 'L', 'N', 'even_CLN')
  call test_zhfrk_combo(4, 2, 'C', 'L', 'C', 'even_CLC')
  call test_zhfrk_combo(4, 2, 'C', 'U', 'N', 'even_CUN')
  call test_zhfrk_combo(4, 2, 'C', 'U', 'C', 'even_CUC')

  ! Edge: N=0
  call test_zhfrk_n0()

  ! Edge: K=0 with beta=2
  call test_zhfrk_k0()

  ! Edge: alpha=0, beta=0
  call test_zhfrk_alpha0_beta0()

  ! Edge: alpha=0, beta=1 (quick return)
  call test_zhfrk_alpha0_beta1()

  ! N=5 (larger odd), K=3
  call test_zhfrk_combo(5, 3, 'N', 'L', 'N', 'n5_NLN')
  call test_zhfrk_combo(5, 3, 'C', 'U', 'C', 'n5_CUC')

contains

  subroutine test_zhfrk_combo(n, k, transr, uplo, trans, label)
    integer, intent(in) :: n, k
    character, intent(in) :: transr, uplo, trans
    character(len=*), intent(in) :: label
    complex*16 :: c_full(10,10), rfp(100)
    double precision :: rfp_r(200)
    equivalence (rfp, rfp_r)
    integer :: info2, nt2, nrowa, ka, lda
    ! Use an allocatable array so LDA = nrowa exactly
    complex*16, allocatable :: a_mat(:,:)
    double precision, allocatable :: a_r(:)

    nt2 = n * (n + 1) / 2

    if (trans == 'N') then
      nrowa = n
      ka = k
    else
      nrowa = k
      ka = n
    end if
    lda = nrowa

    allocate(a_mat(nrowa, ka))
    allocate(a_r(2 * nrowa * ka))

    ! Build a complex matrix A (nrowa x ka)
    call build_a_matrix(nrowa, ka, a_mat, nrowa)

    ! Build a Hermitian positive definite C matrix and convert to RFP
    call build_hpd_matrix(n, c_full)
    call ztrttf(transr, uplo, n, c_full, 10, rfp, info2)
    if (info2 /= 0) then
      print *, 'ZTRTTF failed with info=', info2
      stop
    end if

    call begin_test(label)

    ! Print A as flat real array via equivalence trick
    ! Copy complex array to real view
    call complex_to_real(a_mat, nrowa, ka, a_r)
    call print_array('A', a_r, 2 * nrowa * ka)
    call print_array('C_input', rfp_r, 2 * nt2)
    call print_int('N', n)
    call print_int('K', k)
    call print_int('LDA', lda)
    call print_scalar('alpha', 2.0d0)
    call print_scalar('beta', 0.5d0)
    call print_char('transr', transr)
    call print_char('uplo', uplo)
    call print_char('trans', trans)

    call zhfrk(transr, uplo, trans, n, k, 2.0d0, a_mat, lda, 0.5d0, rfp)

    call print_array('C_out', rfp_r, 2 * nt2)
    call end_test()

    deallocate(a_mat)
    deallocate(a_r)
  end subroutine

  subroutine complex_to_real(z, m, ncols, r)
    integer, intent(in) :: m, ncols
    complex*16, intent(in) :: z(m, ncols)
    double precision, intent(out) :: r(2 * m * ncols)
    integer :: i, j, idx

    idx = 1
    do j = 1, ncols
      do i = 1, m
        r(idx) = dble(z(i, j))
        r(idx+1) = dimag(z(i, j))
        idx = idx + 2
      end do
    end do
  end subroutine

  subroutine build_a_matrix(m, ncols, a, lda)
    integer, intent(in) :: m, ncols, lda
    complex*16, intent(out) :: a(lda, ncols)
    integer :: i, j
    double precision :: re, im

    a = (0.0d0, 0.0d0)
    do j = 1, ncols
      do i = 1, m
        re = dble(i) + 0.5d0 * dble(j)
        im = dble(j) - 0.25d0 * dble(i)
        a(i,j) = dcmplx(re, im)
      end do
    end do
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
    else if (n == 5) then
      h(1,1) = (20.0d0, 0.0d0)
      h(2,1) = (2.0d0, -1.0d0); h(1,2) = (2.0d0, 1.0d0)
      h(3,1) = (1.0d0, 3.0d0);  h(1,3) = (1.0d0, -3.0d0)
      h(4,1) = (3.0d0, -2.0d0); h(1,4) = (3.0d0, 2.0d0)
      h(5,1) = (1.0d0, 1.0d0);  h(1,5) = (1.0d0, -1.0d0)
      h(2,2) = (18.0d0, 0.0d0)
      h(3,2) = (2.0d0, -2.0d0); h(2,3) = (2.0d0, 2.0d0)
      h(4,2) = (1.0d0, 1.0d0);  h(2,4) = (1.0d0, -1.0d0)
      h(5,2) = (2.0d0, 0.0d0);  h(2,5) = (2.0d0, 0.0d0)
      h(3,3) = (15.0d0, 0.0d0)
      h(4,3) = (1.0d0, -1.0d0); h(3,4) = (1.0d0, 1.0d0)
      h(5,3) = (3.0d0, 2.0d0);  h(3,5) = (3.0d0, -2.0d0)
      h(4,4) = (12.0d0, 0.0d0)
      h(5,4) = (1.0d0, -2.0d0); h(4,5) = (1.0d0, 2.0d0)
      h(5,5) = (10.0d0, 0.0d0)
    end if
  end subroutine

  subroutine test_zhfrk_n0()
    complex*16 :: rfp(10), a_mat(1,1)
    double precision :: rfp_r(20)
    equivalence (rfp, rfp_r)

    a_mat = (0.0d0, 0.0d0)
    rfp = (0.0d0, 0.0d0)

    call begin_test('n_zero')
    call zhfrk('N', 'L', 'N', 0, 0, 1.0d0, a_mat, 1, 1.0d0, rfp)
    call print_array('C_out', rfp_r, 0)
    call end_test()
  end subroutine

  subroutine test_zhfrk_k0()
    complex*16 :: rfp(100), a_mat(3,1)
    double precision :: rfp_r(200)
    equivalence (rfp, rfp_r)
    complex*16 :: c_full(10,10)
    integer :: info2, nt2

    nt2 = 3 * 4 / 2  ! 6

    call build_hpd_matrix(3, c_full)
    call ztrttf('N', 'L', 3, c_full, 10, rfp, info2)

    call begin_test('k_zero')
    call print_array('C_input', rfp_r, 2 * nt2)
    call zhfrk('N', 'L', 'N', 3, 0, 1.0d0, a_mat, 3, 2.0d0, rfp)
    call print_array('C_out', rfp_r, 2 * nt2)
    call end_test()
  end subroutine

  subroutine test_zhfrk_alpha0_beta0()
    complex*16 :: rfp(100), a_mat(3,2)
    double precision :: rfp_r(200)
    equivalence (rfp, rfp_r)
    complex*16 :: c_full(10,10)
    integer :: info2, nt2

    nt2 = 3 * 4 / 2  ! 6

    call build_hpd_matrix(3, c_full)
    call ztrttf('N', 'L', 3, c_full, 10, rfp, info2)

    call begin_test('alpha0_beta0')
    call print_array('C_input', rfp_r, 2 * nt2)
    call zhfrk('N', 'L', 'N', 3, 2, 0.0d0, a_mat, 3, 0.0d0, rfp)
    call print_array('C_out', rfp_r, 2 * nt2)
    call end_test()
  end subroutine

  subroutine test_zhfrk_alpha0_beta1()
    complex*16 :: rfp(100), a_mat(3,2)
    double precision :: rfp_r(200)
    equivalence (rfp, rfp_r)
    complex*16 :: c_full(10,10)
    integer :: info2, nt2

    nt2 = 3 * 4 / 2  ! 6

    call build_hpd_matrix(3, c_full)
    call ztrttf('N', 'L', 3, c_full, 10, rfp, info2)

    call begin_test('alpha0_beta1')
    call print_array('C_input', rfp_r, 2 * nt2)
    call zhfrk('N', 'L', 'N', 3, 2, 0.0d0, a_mat, 3, 1.0d0, rfp)
    call print_array('C_out', rfp_r, 2 * nt2)
    call end_test()
  end subroutine

end program
