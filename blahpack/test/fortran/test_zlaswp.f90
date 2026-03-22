program test_zlaswp
  use test_utils
  implicit none
  double precision :: a_r(400)
  complex*16 :: a(200)
  equivalence (a, a_r)
  integer :: ipiv(5)
  integer :: i

  ! Test 1: basic forward pivots, 3x2 complex matrix
  ! A = [(1+2i) (7+8i); (3+4i) (9+10i); (5+6i) (11+12i)] col-major
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 2.0d0); a(2) = (3.0d0, 4.0d0); a(3) = (5.0d0, 6.0d0)
  a(4) = (7.0d0, 8.0d0); a(5) = (9.0d0, 10.0d0); a(6) = (11.0d0, 12.0d0)
  ipiv(1) = 3; ipiv(2) = 2
  call zlaswp(2, a, 3, 1, 2, ipiv, 1)
  call begin_test('basic_forward')
  call print_array('a', a_r, 12)
  call end_test()

  ! Test 2: no swap (ipiv(k) == k)
  a(1) = (1.0d0, 2.0d0); a(2) = (3.0d0, 4.0d0); a(3) = (5.0d0, 6.0d0)
  a(4) = (7.0d0, 8.0d0); a(5) = (9.0d0, 10.0d0); a(6) = (11.0d0, 12.0d0)
  ipiv(1) = 1; ipiv(2) = 2
  call zlaswp(2, a, 3, 1, 2, ipiv, 1)
  call begin_test('no_swap')
  call print_array('a', a_r, 12)
  call end_test()

  ! Test 3: reverse pivots (incx = -1)
  a(1) = (1.0d0, 2.0d0); a(2) = (3.0d0, 4.0d0); a(3) = (5.0d0, 6.0d0)
  a(4) = (7.0d0, 8.0d0); a(5) = (9.0d0, 10.0d0); a(6) = (11.0d0, 12.0d0)
  ipiv(1) = 3; ipiv(2) = 2
  call zlaswp(2, a, 3, 1, 2, ipiv, -1)
  call begin_test('reverse_pivots')
  call print_array('a', a_r, 12)
  call end_test()

  ! Test 4: incx = 0 (no-op)
  a(1) = (1.0d0, 2.0d0); a(2) = (3.0d0, 4.0d0); a(3) = (5.0d0, 6.0d0)
  a(4) = (7.0d0, 8.0d0); a(5) = (9.0d0, 10.0d0); a(6) = (11.0d0, 12.0d0)
  ipiv(1) = 3
  call zlaswp(2, a, 3, 1, 2, ipiv, 0)
  call begin_test('incx_zero')
  call print_array('a', a_r, 12)
  call end_test()

  ! Test 5: two sequential swaps on 3x1 complex matrix
  a = (0.0d0, 0.0d0)
  a(1) = (10.0d0, 1.0d0); a(2) = (20.0d0, 2.0d0); a(3) = (30.0d0, 3.0d0)
  ipiv(1) = 2; ipiv(2) = 3
  call zlaswp(1, a, 3, 1, 2, ipiv, 1)
  call begin_test('two_swaps')
  call print_array('a', a_r, 6)
  call end_test()

  ! Test 6: block-tiled path (N=40 > 32 columns), 3 rows
  a = (0.0d0, 0.0d0)
  do i = 1, 120
    a(i) = cmplx(dble(i), dble(i) + 0.5d0, kind=8)
  end do
  ipiv(1) = 3; ipiv(2) = 2
  call zlaswp(40, a, 3, 1, 2, ipiv, 1)
  call begin_test('block_tiled')
  call print_array('a', a_r, 240)
  call end_test()

end program
