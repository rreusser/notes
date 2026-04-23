program test_zgerc
  use test_utils
  implicit none
  complex*16 :: x(10), y(10), alpha
  complex*16 :: a(4, 4)
  double precision :: x_r(20), y_r(20), a_r(32)
  equivalence (x, x_r)
  equivalence (y, y_r)
  equivalence (a, a_r)
  integer :: m, n, lda

  lda = 4

  ! Test 1: basic 2x2 with unit stride
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 1.0d0)
  a(2,1) = (2.0d0, 2.0d0)
  a(1,2) = (3.0d0, 3.0d0)
  a(2,2) = (4.0d0, 4.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (0.0d0, 1.0d0)
  y(1) = (1.0d0, 1.0d0)
  y(2) = (0.0d0, 2.0d0)
  alpha = (1.0d0, 0.0d0)
  call zgerc(m, n, alpha, x, 1, y, 1, a, lda)
  call begin_test('zgerc_basic')
  call print_cmatrix('a', a_r, lda, m, n)
  call end_test()

  ! Test 2: n=0 quick return
  a = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 1.0d0)
  a(2,1) = (2.0d0, 2.0d0)
  x(1) = (5.0d0, 5.0d0)
  y(1) = (6.0d0, 6.0d0)
  alpha = (1.0d0, 0.0d0)
  call zgerc(2, 0, alpha, x, 1, y, 1, a, lda)
  call begin_test('zgerc_n_zero')
  call print_cmatrix('a', a_r, lda, 2, 1)
  call end_test()

  ! Test 3: m=0 quick return
  a = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 1.0d0)
  a(1,2) = (2.0d0, 2.0d0)
  call zgerc(0, 2, alpha, x, 1, y, 1, a, lda)
  call begin_test('zgerc_m_zero')
  call print_cmatrix('a', a_r, lda, 1, 2)
  call end_test()

  ! Test 4: alpha=0 quick return
  a = (0.0d0, 0.0d0)
  a(1,1) = (7.0d0, 7.0d0)
  a(2,1) = (8.0d0, 8.0d0)
  a(1,2) = (9.0d0, 9.0d0)
  a(2,2) = (10.0d0, 10.0d0)
  alpha = (0.0d0, 0.0d0)
  call zgerc(2, 2, alpha, x, 1, y, 1, a, lda)
  call begin_test('zgerc_alpha_zero')
  call print_cmatrix('a', a_r, lda, 2, 2)
  call end_test()

  ! Test 5: complex alpha
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 0.0d0)
  a(2,1) = (0.0d0, 0.0d0)
  a(1,2) = (0.0d0, 0.0d0)
  a(2,2) = (1.0d0, 0.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (0.0d0, 1.0d0)
  y(1) = (1.0d0, 0.0d0)
  y(2) = (0.0d0, 1.0d0)
  alpha = (0.0d0, 1.0d0)
  call zgerc(m, n, alpha, x, 1, y, 1, a, lda)
  call begin_test('zgerc_complex_alpha')
  call print_cmatrix('a', a_r, lda, m, n)
  call end_test()

  ! Test 6: non-unit strides incx=2, incy=2
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 2.0d0)
  x(2) = (99.0d0, 99.0d0)
  x(3) = (3.0d0, 4.0d0)
  y(1) = (5.0d0, 6.0d0)
  y(2) = (99.0d0, 99.0d0)
  y(3) = (7.0d0, 8.0d0)
  alpha = (1.0d0, 0.0d0)
  call zgerc(m, n, alpha, x, 2, y, 2, a, lda)
  call begin_test('zgerc_stride')
  call print_cmatrix('a', a_r, lda, m, n)
  call end_test()

  ! Test 7: 3x2 non-square
  m = 3
  n = 2
  a = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 0.0d0)
  a(2,1) = (0.0d0, 0.0d0)
  a(3,1) = (0.0d0, 0.0d0)
  a(1,2) = (0.0d0, 0.0d0)
  a(2,2) = (1.0d0, 0.0d0)
  a(3,2) = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (2.0d0, 0.0d0)
  x(3) = (3.0d0, 0.0d0)
  y(1) = (1.0d0, 1.0d0)
  y(2) = (2.0d0, 0.0d0)
  alpha = (1.0d0, 0.0d0)
  call zgerc(m, n, alpha, x, 1, y, 1, a, lda)
  call begin_test('zgerc_nonsquare')
  call print_cmatrix('a', a_r, lda, m, n)
  call end_test()

contains
  subroutine print_cmatrix(name, arr, lda_val, m_val, n_val)
    character(*), intent(in) :: name
    integer, intent(in) :: lda_val, m_val, n_val
    double precision, intent(in) :: arr(2*lda_val, *)
    integer :: i, j, idx
    logical :: first
    write(*, '(A,A,A)', advance='no') ',"', trim(name), '":['
    first = .true.
    do j = 1, n_val
      do i = 1, m_val
        if (.not. first) write(*, '(A)', advance='no') ','
        first = .false.
        ! real part
        write(*, '(ES25.17E3)', advance='no') arr(2*(i-1)+1, j)
        write(*, '(A)', advance='no') ','
        ! imag part
        write(*, '(ES25.17E3)', advance='no') arr(2*(i-1)+2, j)
      end do
    end do
    write(*, '(A)', advance='no') ']'
  end subroutine

end program
