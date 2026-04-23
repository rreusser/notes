program test_ztrmm
  use test_utils
  implicit none
  complex*16 :: a(4, 4), b(4, 4), alpha
  double precision :: b_r(32)
  equivalence (b, b_r)
  integer :: m, n, lda, ldb

  lda = 4
  ldb = 4

  ! Test 1: left side, upper, no transpose, non-unit diagonal, 2x2
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (2.0d0, 1.0d0)
  a(1,2) = (3.0d0, 1.0d0)
  a(2,2) = (4.0d0, 2.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (0.0d0, 1.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  call ztrmm('L', 'U', 'N', 'N', m, n, alpha, a, lda, b, ldb)
  call begin_test('ztrmm_left_upper_notrans')
  call print_cmatrix('b', b_r, ldb, m, n)
  call end_test()

  ! Test 2: left side, lower, no transpose, non-unit diagonal, 2x2
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (2.0d0, 1.0d0)
  a(2,1) = (3.0d0, 1.0d0)
  a(2,2) = (4.0d0, 2.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (0.0d0, 1.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  call ztrmm('L', 'L', 'N', 'N', m, n, alpha, a, lda, b, ldb)
  call begin_test('ztrmm_left_lower_notrans')
  call print_cmatrix('b', b_r, ldb, m, n)
  call end_test()

  ! Test 3: right side, upper, no transpose, non-unit diagonal, 2x2
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (2.0d0, 1.0d0)
  a(1,2) = (3.0d0, 1.0d0)
  a(2,2) = (4.0d0, 2.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (0.0d0, 1.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  call ztrmm('R', 'U', 'N', 'N', m, n, alpha, a, lda, b, ldb)
  call begin_test('ztrmm_right_upper_notrans')
  call print_cmatrix('b', b_r, ldb, m, n)
  call end_test()

  ! Test 4: left side, upper, conj-transpose, non-unit, 2x2
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (2.0d0, 1.0d0)
  a(1,2) = (3.0d0, 1.0d0)
  a(2,2) = (4.0d0, 2.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (0.0d0, 1.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  call ztrmm('L', 'U', 'C', 'N', m, n, alpha, a, lda, b, ldb)
  call begin_test('ztrmm_left_upper_conjtrans')
  call print_cmatrix('b', b_r, ldb, m, n)
  call end_test()

  ! Test 5: alpha=0 (should zero B)
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (2.0d0, 1.0d0)
  a(1,2) = (3.0d0, 1.0d0)
  a(2,2) = (4.0d0, 2.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (0.0d0, 1.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  alpha = (0.0d0, 0.0d0)
  call ztrmm('L', 'U', 'N', 'N', m, n, alpha, a, lda, b, ldb)
  call begin_test('ztrmm_alpha_zero')
  call print_cmatrix('b', b_r, ldb, m, n)
  call end_test()

  ! Test 6: complex alpha
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 0.0d0)
  a(1,2) = (0.0d0, 0.0d0)
  a(2,2) = (1.0d0, 0.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (2.0d0, 0.0d0)
  b(2,2) = (0.0d0, 2.0d0)
  alpha = (0.0d0, 1.0d0)
  call ztrmm('L', 'U', 'N', 'N', m, n, alpha, a, lda, b, ldb)
  call begin_test('ztrmm_complex_alpha')
  call print_cmatrix('b', b_r, ldb, m, n)
  call end_test()

  ! Test 7: M=0 quick return
  b = (0.0d0, 0.0d0)
  b(1,1) = (99.0d0, 99.0d0)
  alpha = (1.0d0, 0.0d0)
  call ztrmm('L', 'U', 'N', 'N', 0, 2, alpha, a, lda, b, ldb)
  call begin_test('ztrmm_m_zero')
  call print_cmatrix('b', b_r, ldb, 1, 1)
  call end_test()

  ! Test 8: unit diagonal, left, upper
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (99.0d0, 99.0d0)
  a(1,2) = (2.0d0, 1.0d0)
  a(2,2) = (99.0d0, 99.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (0.0d0, 1.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  call ztrmm('L', 'U', 'N', 'U', m, n, alpha, a, lda, b, ldb)
  call begin_test('ztrmm_unit_diag')
  call print_cmatrix('b', b_r, ldb, m, n)
  call end_test()

  ! Test 9: left, upper, transpose (not conjugate), non-unit, 2x2
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (2.0d0, 1.0d0)
  a(1,2) = (3.0d0, 1.0d0)
  a(2,2) = (4.0d0, 2.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (0.0d0, 1.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  call ztrmm('L', 'U', 'T', 'N', m, n, alpha, a, lda, b, ldb)
  call begin_test('ztrmm_left_upper_trans')
  call print_cmatrix('b', b_r, ldb, m, n)
  call end_test()

  ! Test 10: right, lower, no transpose, non-unit, 2x2
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (2.0d0, 1.0d0)
  a(2,1) = (3.0d0, 1.0d0)
  a(2,2) = (4.0d0, 2.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (0.0d0, 1.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  call ztrmm('R', 'L', 'N', 'N', m, n, alpha, a, lda, b, ldb)
  call begin_test('ztrmm_right_lower_notrans')
  call print_cmatrix('b', b_r, ldb, m, n)
  call end_test()

  ! Test 11: left, lower, transpose, non-unit, 2x2
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (2.0d0, 1.0d0)
  a(2,1) = (3.0d0, 1.0d0)
  a(2,2) = (4.0d0, 2.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (0.0d0, 1.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  call ztrmm('L', 'L', 'T', 'N', m, n, alpha, a, lda, b, ldb)
  call begin_test('ztrmm_left_lower_trans')
  call print_cmatrix('b', b_r, ldb, m, n)
  call end_test()

  ! Test 12: left, lower, conjugate transpose, non-unit, 2x2
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (2.0d0, 1.0d0)
  a(2,1) = (3.0d0, 1.0d0)
  a(2,2) = (4.0d0, 2.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (0.0d0, 1.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  call ztrmm('L', 'L', 'C', 'N', m, n, alpha, a, lda, b, ldb)
  call begin_test('ztrmm_left_lower_conjtrans')
  call print_cmatrix('b', b_r, ldb, m, n)
  call end_test()

  ! Test 13: right, upper, transpose, non-unit, 2x2
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (2.0d0, 1.0d0)
  a(1,2) = (3.0d0, 1.0d0)
  a(2,2) = (4.0d0, 2.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (0.0d0, 1.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  call ztrmm('R', 'U', 'T', 'N', m, n, alpha, a, lda, b, ldb)
  call begin_test('ztrmm_right_upper_trans')
  call print_cmatrix('b', b_r, ldb, m, n)
  call end_test()

  ! Test 14: right, upper, conjugate transpose, non-unit, 2x2
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (2.0d0, 1.0d0)
  a(1,2) = (3.0d0, 1.0d0)
  a(2,2) = (4.0d0, 2.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (0.0d0, 1.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  call ztrmm('R', 'U', 'C', 'N', m, n, alpha, a, lda, b, ldb)
  call begin_test('ztrmm_right_upper_conjtrans')
  call print_cmatrix('b', b_r, ldb, m, n)
  call end_test()

  ! Test 15: right, lower, transpose, non-unit, 2x2
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (2.0d0, 1.0d0)
  a(2,1) = (3.0d0, 1.0d0)
  a(2,2) = (4.0d0, 2.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (0.0d0, 1.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  call ztrmm('R', 'L', 'T', 'N', m, n, alpha, a, lda, b, ldb)
  call begin_test('ztrmm_right_lower_trans')
  call print_cmatrix('b', b_r, ldb, m, n)
  call end_test()

  ! Test 16: right, lower, conjugate transpose, non-unit, 2x2
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  b = (0.0d0, 0.0d0)
  a(1,1) = (2.0d0, 1.0d0)
  a(2,1) = (3.0d0, 1.0d0)
  a(2,2) = (4.0d0, 2.0d0)
  b(1,1) = (1.0d0, 0.0d0)
  b(2,1) = (0.0d0, 1.0d0)
  b(1,2) = (0.0d0, 1.0d0)
  b(2,2) = (1.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  call ztrmm('R', 'L', 'C', 'N', m, n, alpha, a, lda, b, ldb)
  call begin_test('ztrmm_right_lower_conjtrans')
  call print_cmatrix('b', b_r, ldb, m, n)
  call end_test()

contains
  subroutine print_cmatrix(name, arr, lda_val, m_val, n_val)
    character(*), intent(in) :: name
    integer, intent(in) :: lda_val, m_val, n_val
    double precision, intent(in) :: arr(2*lda_val, *)
    integer :: i, j
    logical :: first
    write(*, '(A,A,A)', advance='no') ',"', trim(name), '":['
    first = .true.
    do j = 1, n_val
      do i = 1, m_val
        if (.not. first) write(*, '(A)', advance='no') ','
        first = .false.
        write(*, '(ES25.17E3)', advance='no') arr(2*(i-1)+1, j)
        write(*, '(A)', advance='no') ','
        write(*, '(ES25.17E3)', advance='no') arr(2*(i-1)+2, j)
      end do
    end do
    write(*, '(A)', advance='no') ']'
  end subroutine

end program
