program test_dlaswp
  use test_utils
  implicit none
  double precision :: a(200)
  integer :: ipiv(5)
  integer :: i

  ! Test 1: basic forward pivots, 3x2 matrix
  ! A = [1 4; 2 5; 3 6] col-major, swap row 1↔3, row 2↔2
  a = 0.0d0
  a(1) = 1.0d0; a(2) = 2.0d0; a(3) = 3.0d0
  a(4) = 4.0d0; a(5) = 5.0d0; a(6) = 6.0d0
  ipiv(1) = 3; ipiv(2) = 2
  call dlaswp(2, a, 3, 1, 2, ipiv, 1)
  call begin_test('basic_forward')
  call print_array('a', a, 6)
  call end_test()

  ! Test 2: no swap (ipiv(k) == k)
  a(1) = 1.0d0; a(2) = 2.0d0; a(3) = 3.0d0
  a(4) = 4.0d0; a(5) = 5.0d0; a(6) = 6.0d0
  ipiv(1) = 1; ipiv(2) = 2
  call dlaswp(2, a, 3, 1, 2, ipiv, 1)
  call begin_test('no_swap')
  call print_array('a', a, 6)
  call end_test()

  ! Test 3: reverse pivots (incx = -1)
  a(1) = 1.0d0; a(2) = 2.0d0; a(3) = 3.0d0
  a(4) = 4.0d0; a(5) = 5.0d0; a(6) = 6.0d0
  ipiv(1) = 3; ipiv(2) = 2
  call dlaswp(2, a, 3, 1, 2, ipiv, -1)
  call begin_test('reverse_pivots')
  call print_array('a', a, 6)
  call end_test()

  ! Test 4: incx = 0 (no-op)
  a(1) = 1.0d0; a(2) = 2.0d0; a(3) = 3.0d0
  a(4) = 4.0d0; a(5) = 5.0d0; a(6) = 6.0d0
  ipiv(1) = 3
  call dlaswp(2, a, 3, 1, 2, ipiv, 0)
  call begin_test('incx_zero')
  call print_array('a', a, 6)
  call end_test()

  ! Test 5: larger matrix to exercise the n32 block tiling (N > 32)
  ! Use a 3x1 matrix (small in rows, 1 column — just verifying row swaps)
  a = 0.0d0
  a(1) = 10.0d0; a(2) = 20.0d0; a(3) = 30.0d0
  ipiv(1) = 2; ipiv(2) = 3
  call dlaswp(1, a, 3, 1, 2, ipiv, 1)
  call begin_test('two_swaps')
  call print_array('a', a, 3)
  call end_test()

  ! Test 6: block-tiled path (N=40 > 32 columns), 3 rows
  ! A(i,j) = (i-1)*40 + j for a 3x40 matrix, col-major
  a = 0.0d0
  do i = 1, 40
    a((i-1)*3 + 1) = dble((i-1)*3 + 1)
    a((i-1)*3 + 2) = dble((i-1)*3 + 2)
    a((i-1)*3 + 3) = dble((i-1)*3 + 3)
  end do
  ipiv(1) = 3; ipiv(2) = 2
  call dlaswp(40, a, 3, 1, 2, ipiv, 1)
  call begin_test('block_tiled')
  call print_array('a', a, 120)
  call end_test()

  ! Test 7: reverse pivots forward comparison
  ! Same setup as basic_forward but with incx=-1
  ! Fortran DLASWP with incx=-1 applies pivots in reverse order
  a = 0.0d0
  a(1) = 1.0d0; a(2) = 2.0d0; a(3) = 3.0d0
  a(4) = 4.0d0; a(5) = 5.0d0; a(6) = 6.0d0
  ipiv(1) = 3; ipiv(2) = 2
  call dlaswp(2, a, 3, 1, 2, ipiv, -1)
  call begin_test('reverse_pivots')
  call print_array('a', a, 6)
  call end_test()

end program
