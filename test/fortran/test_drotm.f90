program test_drotm
  use test_utils
  implicit none
  double precision :: dx(20), dy(20), dparam(5)

  ! Test 1: flag=-1, full matrix [h11,h12;h21,h22]
  dx = 0.0d0; dy = 0.0d0
  dx(1:3) = (/1.0d0, 2.0d0, 3.0d0/)
  dy(1:3) = (/4.0d0, 5.0d0, 6.0d0/)
  dparam(1) = -1.0d0
  dparam(2) = 2.0d0   ! h11
  dparam(3) = -1.0d0  ! h21
  dparam(4) = 3.0d0   ! h12
  dparam(5) = 0.5d0   ! h22
  call drotm(3, dx, 1, dy, 1, dparam)
  call begin_test('flag_neg1')
  call print_array('dx', dx, 3)
  call print_array('dy', dy, 3)
  call end_test()

  ! Test 2: flag=0, [1,h12;h21,1]
  dx(1:3) = (/1.0d0, 2.0d0, 3.0d0/)
  dy(1:3) = (/4.0d0, 5.0d0, 6.0d0/)
  dparam(1) = 0.0d0
  dparam(3) = -0.5d0  ! h21
  dparam(4) = 0.25d0  ! h12
  call drotm(3, dx, 1, dy, 1, dparam)
  call begin_test('flag_zero')
  call print_array('dx', dx, 3)
  call print_array('dy', dy, 3)
  call end_test()

  ! Test 3: flag=1, [h11,1;-1,h22]
  dx(1:3) = (/1.0d0, 2.0d0, 3.0d0/)
  dy(1:3) = (/4.0d0, 5.0d0, 6.0d0/)
  dparam(1) = 1.0d0
  dparam(2) = 0.5d0   ! h11
  dparam(5) = 2.0d0   ! h22
  call drotm(3, dx, 1, dy, 1, dparam)
  call begin_test('flag_one')
  call print_array('dx', dx, 3)
  call print_array('dy', dy, 3)
  call end_test()

  ! Test 4: flag=-2, identity (no-op)
  dx(1:3) = (/1.0d0, 2.0d0, 3.0d0/)
  dy(1:3) = (/4.0d0, 5.0d0, 6.0d0/)
  dparam(1) = -2.0d0
  call drotm(3, dx, 1, dy, 1, dparam)
  call begin_test('flag_neg2')
  call print_array('dx', dx, 3)
  call print_array('dy', dy, 3)
  call end_test()

  ! Test 5: n=0
  dx(1:3) = (/1.0d0, 2.0d0, 3.0d0/)
  dy(1:3) = (/4.0d0, 5.0d0, 6.0d0/)
  dparam(1) = -1.0d0
  dparam(2) = 2.0d0; dparam(3) = -1.0d0; dparam(4) = 3.0d0; dparam(5) = 0.5d0
  call drotm(0, dx, 1, dy, 1, dparam)
  call begin_test('n_zero')
  call print_array('dx', dx, 3)
  call print_array('dy', dy, 3)
  call end_test()

  ! Test 6: stride=2
  dx = 0.0d0; dy = 0.0d0
  dx(1) = 1.0d0; dx(3) = 2.0d0
  dy(1) = 3.0d0; dy(3) = 4.0d0
  dparam(1) = -1.0d0
  dparam(2) = 2.0d0; dparam(3) = -1.0d0; dparam(4) = 3.0d0; dparam(5) = 0.5d0
  call drotm(2, dx, 2, dy, 2, dparam)
  call begin_test('stride2')
  call print_array('dx', dx, 4)
  call print_array('dy', dy, 4)
  call end_test()

  ! Test 7: negative stride
  dx = 0.0d0; dy = 0.0d0
  dx(1:3) = (/1.0d0, 2.0d0, 3.0d0/)
  dy(1:3) = (/4.0d0, 5.0d0, 6.0d0/)
  dparam(1) = 0.0d0
  dparam(3) = -0.5d0; dparam(4) = 0.25d0
  call drotm(3, dx, -1, dy, 1, dparam)
  call begin_test('neg_stride')
  call print_array('dx', dx, 3)
  call print_array('dy', dy, 3)
  call end_test()

end program
