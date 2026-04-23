program test_dlacpy
  use test_utils
  implicit none
  double precision :: a(16), b(16)
  integer :: i

  ! Test 1: copy all, 3x3
  ! A = [1 4 7; 2 5 8; 3 6 9] column-major
  a = 0.0d0; b = 0.0d0
  do i = 1, 9
    a(i) = dble(i)
  end do
  call dlacpy('A', 3, 3, a, 3, b, 3)
  call begin_test('all_3x3')
  call print_array('b', b, 9)
  call end_test()

  ! Test 2: copy upper, 3x3
  b = 0.0d0
  call dlacpy('U', 3, 3, a, 3, b, 3)
  call begin_test('upper_3x3')
  call print_array('b', b, 9)
  call end_test()

  ! Test 3: copy lower, 3x3
  b = 0.0d0
  call dlacpy('L', 3, 3, a, 3, b, 3)
  call begin_test('lower_3x3')
  call print_array('b', b, 9)
  call end_test()

  ! Test 4: rectangular 2x3 (m < n), copy all
  b = 0.0d0
  call dlacpy('A', 2, 3, a, 3, b, 3)
  call begin_test('all_2x3')
  call print_array('b', b, 9)
  call end_test()

  ! Test 5: m=0 (quick — nothing copied)
  b(1) = 99.0d0
  call dlacpy('A', 0, 3, a, 3, b, 3)
  call begin_test('m_zero')
  call print_array('b', b, 1)
  call end_test()

  ! Test 6: n=0
  b(1) = 99.0d0
  call dlacpy('A', 3, 0, a, 3, b, 3)
  call begin_test('n_zero')
  call print_array('b', b, 1)
  call end_test()

  ! Test 7: upper, rectangular 4x3
  a = 0.0d0; b = 0.0d0
  do i = 1, 12
    a(i) = dble(i)
  end do
  call dlacpy('U', 4, 3, a, 4, b, 4)
  call begin_test('upper_4x3')
  call print_array('b', b, 12)
  call end_test()

end program
