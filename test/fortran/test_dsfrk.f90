program test_dsfrk
  use test_utils
  implicit none

  ! Test all 16 combinations: 2 transr x 2 uplo x 2 trans x 2 parity(N)
  ! N=3 (odd), K=2
  call test_dsfrk_combo(3, 2, 'N', 'L', 'N', 'odd_NLN')
  call test_dsfrk_combo(3, 2, 'N', 'L', 'T', 'odd_NLT')
  call test_dsfrk_combo(3, 2, 'N', 'U', 'N', 'odd_NUN')
  call test_dsfrk_combo(3, 2, 'N', 'U', 'T', 'odd_NUT')
  call test_dsfrk_combo(3, 2, 'T', 'L', 'N', 'odd_TLN')
  call test_dsfrk_combo(3, 2, 'T', 'L', 'T', 'odd_TLT')
  call test_dsfrk_combo(3, 2, 'T', 'U', 'N', 'odd_TUN')
  call test_dsfrk_combo(3, 2, 'T', 'U', 'T', 'odd_TUT')

  ! N=4 (even), K=2
  call test_dsfrk_combo(4, 2, 'N', 'L', 'N', 'even_NLN')
  call test_dsfrk_combo(4, 2, 'N', 'L', 'T', 'even_NLT')
  call test_dsfrk_combo(4, 2, 'N', 'U', 'N', 'even_NUN')
  call test_dsfrk_combo(4, 2, 'N', 'U', 'T', 'even_NUT')
  call test_dsfrk_combo(4, 2, 'T', 'L', 'N', 'even_TLN')
  call test_dsfrk_combo(4, 2, 'T', 'L', 'T', 'even_TLT')
  call test_dsfrk_combo(4, 2, 'T', 'U', 'N', 'even_TUN')
  call test_dsfrk_combo(4, 2, 'T', 'U', 'T', 'even_TUT')

  ! N=5 (larger odd), K=3
  call test_dsfrk_combo(5, 3, 'N', 'L', 'N', 'odd5_NLN')
  call test_dsfrk_combo(5, 3, 'T', 'U', 'T', 'odd5_TUT')

  ! Edge cases
  call test_n_zero()
  call test_alpha_zero_beta_one()
  call test_alpha_zero_beta_zero()
  call test_k_zero()

contains

  subroutine test_dsfrk_combo(n, k, transr, uplo, trans, label)
    integer, intent(in) :: n, k
    character, intent(in) :: transr, uplo, trans
    character(len=*), intent(in) :: label
    double precision :: full(10,10), rfp(100), a_mat(10,10)
    integer :: info2, nt2, nrowa, lda, i, j
    double precision :: alpha, beta

    nt2 = n * (n + 1) / 2
    alpha = 2.0d0
    beta = 0.5d0

    ! Build a simple initial symmetric matrix C in full format
    full = 0.0d0
    do j = 1, n
      do i = 1, n
        full(i, j) = dble(i + j)
      end do
    end do

    ! Convert to RFP
    call dtrttf(transr, uplo, n, full, 10, rfp, info2)
    if (info2 /= 0) then
      print *, 'DTRTTF failed for ', label, ' info=', info2
      stop
    end if

    ! Build matrix A
    if (trans == 'N') then
      nrowa = n
      lda = n
    else
      nrowa = k
      lda = k
    end if

    a_mat = 0.0d0
    do j = 1, merge(k, n, trans == 'N')
      do i = 1, nrowa
        a_mat(i, j) = dble(i) * 0.5d0 + dble(j) * 0.3d0
      end do
    end do

    call begin_test(label)
    call print_int('n', n)
    call print_int('k', k)
    call print_scalar('alpha', alpha)
    call print_scalar('beta', beta)
    call print_array('c_in', rfp, nt2)
    call print_matrix('A', a_mat, 10, nrowa, merge(k, n, trans == 'N'))

    call dsfrk(transr, uplo, trans, n, k, alpha, a_mat, 10, beta, rfp)

    call print_array('c_out', rfp, nt2)
    call end_test()
  end subroutine

  subroutine test_n_zero()
    double precision :: rfp(1), a_mat(1,1)
    rfp(1) = 99.0d0
    a_mat(1,1) = 1.0d0

    call begin_test('n_zero')
    call print_array('c_in', rfp, 1)
    call dsfrk('N', 'L', 'N', 0, 1, 1.0d0, a_mat, 1, 0.0d0, rfp)
    call print_array('c_out', rfp, 1)
    call end_test()
  end subroutine

  subroutine test_alpha_zero_beta_one()
    double precision :: full(10,10), rfp(100)
    integer :: info2, nt2, n, k, i, j
    double precision :: a_mat(10,10)

    n = 3
    k = 2
    nt2 = n * (n + 1) / 2

    full = 0.0d0
    do j = 1, n
      do i = 1, n
        full(i, j) = dble(i + j)
      end do
    end do
    call dtrttf('N', 'L', n, full, 10, rfp, info2)

    a_mat = 1.0d0

    call begin_test('alpha_zero_beta_one')
    call print_array('c_in', rfp, nt2)

    ! alpha=0, beta=1 => quick return, C unchanged
    call dsfrk('N', 'L', 'N', n, k, 0.0d0, a_mat, 10, 1.0d0, rfp)

    call print_array('c_out', rfp, nt2)
    call end_test()
  end subroutine

  subroutine test_alpha_zero_beta_zero()
    double precision :: full(10,10), rfp(100)
    integer :: info2, nt2, n, k, i, j
    double precision :: a_mat(10,10)

    n = 3
    k = 2
    nt2 = n * (n + 1) / 2

    full = 0.0d0
    do j = 1, n
      do i = 1, n
        full(i, j) = dble(i + j)
      end do
    end do
    call dtrttf('N', 'L', n, full, 10, rfp, info2)

    a_mat = 1.0d0

    call begin_test('alpha_zero_beta_zero')
    call print_array('c_in', rfp, nt2)

    ! alpha=0, beta=0 => C is set to zero
    call dsfrk('N', 'L', 'N', n, k, 0.0d0, a_mat, 10, 0.0d0, rfp)

    call print_array('c_out', rfp, nt2)
    call end_test()
  end subroutine

  subroutine test_k_zero()
    double precision :: full(10,10), rfp(100)
    integer :: info2, nt2, n, k, i, j
    double precision :: a_mat(10,10)

    n = 3
    k = 0
    nt2 = n * (n + 1) / 2

    full = 0.0d0
    do j = 1, n
      do i = 1, n
        full(i, j) = dble(i + j)
      end do
    end do
    call dtrttf('N', 'L', n, full, 10, rfp, info2)

    a_mat = 0.0d0

    call begin_test('k_zero_beta_one')
    call print_array('c_in', rfp, nt2)

    ! K=0, beta=1 => quick return, C unchanged
    call dsfrk('N', 'L', 'N', n, k, 1.0d0, a_mat, 10, 1.0d0, rfp)

    call print_array('c_out', rfp, nt2)
    call end_test()
  end subroutine

end program
