program test_dscal
  use test_utils
  implicit none
  double precision :: dx(20)
  integer :: i

  ! Test 1: basic scaling, incx=1
  dx = 0.0d0
  dx(1:5) = (/1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0/)
  call dscal(5, 2.0d0, dx, 1)
  call begin_test('basic')
  call print_array('dx', dx, 5)
  call end_test()

  ! Test 2: da=0 (should zero out the vector)
  dx(1:5) = (/1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0/)
  call dscal(5, 0.0d0, dx, 1)
  call begin_test('da_zero')
  call print_array('dx', dx, 5)
  call end_test()

  ! Test 3: da=1 (no-op)
  dx(1:5) = (/1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0/)
  call dscal(5, 1.0d0, dx, 1)
  call begin_test('da_one')
  call print_array('dx', dx, 5)
  call end_test()

  ! Test 4: n=0 (no-op)
  dx(1:5) = (/1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0/)
  call dscal(0, 2.0d0, dx, 1)
  call begin_test('n_zero')
  call print_array('dx', dx, 5)
  call end_test()

  ! Test 5: n=1
  dx(1) = 7.0d0
  call dscal(1, 3.0d0, dx, 1)
  call begin_test('n_one')
  call print_array('dx', dx, 1)
  call end_test()

  ! Test 6: non-unit stride (incx=2)
  dx = 0.0d0
  dx(1) = 1.0d0; dx(3) = 2.0d0; dx(5) = 3.0d0
  dx(2) = 99.0d0; dx(4) = 99.0d0
  call dscal(3, 4.0d0, dx, 2)
  call begin_test('stride')
  call print_array('dx', dx, 6)
  call end_test()

  ! Test 7: larger n (exercises 5-unrolled loop), n=12
  do i = 1, 12
    dx(i) = dble(i)
  end do
  call dscal(12, 3.0d0, dx, 1)
  call begin_test('unrolled')
  call print_array('dx', dx, 12)
  call end_test()

end program
