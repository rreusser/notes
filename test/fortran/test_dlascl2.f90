program test_dlascl2
  use test_utils
  implicit none
  double precision :: d(4), x(16)
  integer :: i

  ! Test 1: basic 3x3 matrix
  ! X = [1 4 7; 2 5 8; 3 6 9] column-major, D = [2, 3, 4]
  do i = 1, 9
    x(i) = dble(i)
  end do
  d(1) = 2.0d0
  d(2) = 3.0d0
  d(3) = 4.0d0
  call dlascl2(3, 3, d, x, 3)
  call begin_test('basic_3x3')
  call print_array('x', x, 9)
  call end_test()

  ! Test 2: M=0 (quick return, nothing scaled)
  x(1) = 99.0d0
  call dlascl2(0, 3, d, x, 3)
  call begin_test('m_zero')
  call print_array('x', x, 1)
  call end_test()

  ! Test 3: N=0 (quick return, nothing scaled)
  x(1) = 99.0d0
  call dlascl2(3, 0, d, x, 3)
  call begin_test('n_zero')
  call print_array('x', x, 1)
  call end_test()

  ! Test 4: M=1, N=1 (single element)
  x(1) = 5.0d0
  d(1) = 3.0d0
  call dlascl2(1, 1, d, x, 1)
  call begin_test('single_element')
  call print_array('x', x, 1)
  call end_test()

  ! Test 5: rectangular 2x3 (m < n)
  ! X = [1 3 5; 2 4 6] column-major, D = [0.5, 2.0]
  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0
  x(4) = 4.0d0; x(5) = 5.0d0; x(6) = 6.0d0
  d(1) = 0.5d0
  d(2) = 2.0d0
  call dlascl2(2, 3, d, x, 2)
  call begin_test('rect_2x3')
  call print_array('x', x, 6)
  call end_test()

  ! Test 6: rectangular 3x2 (m > n)
  ! X = [1 4; 2 5; 3 6] column-major, D = [10, 20, 30]
  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0
  x(4) = 4.0d0; x(5) = 5.0d0; x(6) = 6.0d0
  d(1) = 10.0d0
  d(2) = 20.0d0
  d(3) = 30.0d0
  call dlascl2(3, 2, d, x, 3)
  call begin_test('rect_3x2')
  call print_array('x', x, 6)
  call end_test()

  ! Test 7: with negative values and zeros in D
  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0; x(4) = 4.0d0
  d(1) = -1.0d0
  d(2) = 0.0d0
  call dlascl2(2, 2, d, x, 2)
  call begin_test('negative_zero_d')
  call print_array('x', x, 4)
  call end_test()

  ! Test 8: with LDX > M (leading dimension larger than rows used)
  ! X is 4-by-3 in memory but we only use rows 1..2
  x = 0.0d0
  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 99.0d0; x(4) = 99.0d0
  x(5) = 3.0d0; x(6) = 4.0d0; x(7) = 99.0d0; x(8) = 99.0d0
  x(9) = 5.0d0; x(10) = 6.0d0; x(11) = 99.0d0; x(12) = 99.0d0
  d(1) = 2.0d0
  d(2) = 3.0d0
  call dlascl2(2, 3, d, x, 4)
  call begin_test('ldx_gt_m')
  call print_array('x', x, 12)
  call end_test()

end program
