program test_zlascl2
  use test_utils
  implicit none
  double precision :: d(4)
  complex*16 :: x(16)
  double precision :: xr(32)
  equivalence (x, xr)
  integer :: i

  ! Test 1: basic 3x3 matrix with complex entries
  ! X column-major 3x3, D = [2, 3, 4]
  x(1) = dcmplx(1.0d0, 2.0d0)
  x(2) = dcmplx(3.0d0, 4.0d0)
  x(3) = dcmplx(5.0d0, 6.0d0)
  x(4) = dcmplx(7.0d0, 8.0d0)
  x(5) = dcmplx(9.0d0, 10.0d0)
  x(6) = dcmplx(11.0d0, 12.0d0)
  x(7) = dcmplx(13.0d0, 14.0d0)
  x(8) = dcmplx(15.0d0, 16.0d0)
  x(9) = dcmplx(17.0d0, 18.0d0)
  d(1) = 2.0d0
  d(2) = 3.0d0
  d(3) = 4.0d0
  call zlascl2(3, 3, d, x, 3)
  call begin_test('basic_3x3')
  call print_array('x', xr, 18)
  call end_test()

  ! Test 2: M=0 (quick return)
  x(1) = dcmplx(99.0d0, 88.0d0)
  call zlascl2(0, 3, d, x, 3)
  call begin_test('m_zero')
  call print_array('x', xr, 2)
  call end_test()

  ! Test 3: N=0 (quick return)
  x(1) = dcmplx(99.0d0, 88.0d0)
  call zlascl2(3, 0, d, x, 3)
  call begin_test('n_zero')
  call print_array('x', xr, 2)
  call end_test()

  ! Test 4: single element
  x(1) = dcmplx(5.0d0, -3.0d0)
  d(1) = 3.0d0
  call zlascl2(1, 1, d, x, 1)
  call begin_test('single_element')
  call print_array('x', xr, 2)
  call end_test()

  ! Test 5: rectangular 2x3 (m < n)
  x(1) = dcmplx(1.0d0, 0.5d0)
  x(2) = dcmplx(2.0d0, 1.0d0)
  x(3) = dcmplx(3.0d0, 1.5d0)
  x(4) = dcmplx(4.0d0, 2.0d0)
  x(5) = dcmplx(5.0d0, 2.5d0)
  x(6) = dcmplx(6.0d0, 3.0d0)
  d(1) = 0.5d0
  d(2) = 2.0d0
  call zlascl2(2, 3, d, x, 2)
  call begin_test('rect_2x3')
  call print_array('x', xr, 12)
  call end_test()

  ! Test 6: rectangular 3x2 (m > n)
  x(1) = dcmplx(1.0d0, -1.0d0)
  x(2) = dcmplx(2.0d0, -2.0d0)
  x(3) = dcmplx(3.0d0, -3.0d0)
  x(4) = dcmplx(4.0d0, -4.0d0)
  x(5) = dcmplx(5.0d0, -5.0d0)
  x(6) = dcmplx(6.0d0, -6.0d0)
  d(1) = 10.0d0
  d(2) = 20.0d0
  d(3) = 30.0d0
  call zlascl2(3, 2, d, x, 3)
  call begin_test('rect_3x2')
  call print_array('x', xr, 12)
  call end_test()

  ! Test 7: negative and zero values in D
  x(1) = dcmplx(1.0d0, 2.0d0)
  x(2) = dcmplx(3.0d0, 4.0d0)
  x(3) = dcmplx(5.0d0, 6.0d0)
  x(4) = dcmplx(7.0d0, 8.0d0)
  d(1) = -1.0d0
  d(2) = 0.0d0
  call zlascl2(2, 2, d, x, 2)
  call begin_test('negative_zero_d')
  call print_array('x', xr, 8)
  call end_test()

  ! Test 8: LDX > M (leading dimension larger than rows)
  ! X is 4-by-3 in memory, but only rows 1..2 are scaled
  do i = 1, 12
    x(i) = dcmplx(0.0d0, 0.0d0)
  end do
  x(1) = dcmplx(1.0d0, 0.1d0)
  x(2) = dcmplx(2.0d0, 0.2d0)
  x(3) = dcmplx(99.0d0, 99.0d0)
  x(4) = dcmplx(99.0d0, 99.0d0)
  x(5) = dcmplx(3.0d0, 0.3d0)
  x(6) = dcmplx(4.0d0, 0.4d0)
  x(7) = dcmplx(99.0d0, 99.0d0)
  x(8) = dcmplx(99.0d0, 99.0d0)
  x(9) = dcmplx(5.0d0, 0.5d0)
  x(10) = dcmplx(6.0d0, 0.6d0)
  x(11) = dcmplx(99.0d0, 99.0d0)
  x(12) = dcmplx(99.0d0, 99.0d0)
  d(1) = 2.0d0
  d(2) = 3.0d0
  call zlascl2(2, 3, d, x, 4)
  call begin_test('ldx_gt_m')
  call print_array('x', xr, 24)
  call end_test()

end program
