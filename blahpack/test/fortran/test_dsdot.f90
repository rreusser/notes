program test_dsdot
  use test_utils
  implicit none
  real :: sx(20), sy(20)
  double precision :: result, dsdot
  integer :: i

  ! Test 1: basic dot product
  sx = 0.0; sy = 0.0
  sx(1:5) = (/1.0, 2.0, 3.0, 4.0, 5.0/)
  sy(1:5) = (/2.0, 3.0, 4.0, 5.0, 6.0/)
  result = dsdot(5, sx, 1, sy, 1)
  call begin_test('basic')
  call print_scalar('result', result)
  call end_test()

  ! Test 2: n=0
  result = dsdot(0, sx, 1, sy, 1)
  call begin_test('n_zero')
  call print_scalar('result', result)
  call end_test()

  ! Test 3: n=1
  sx(1) = 3.0; sy(1) = 7.0
  result = dsdot(1, sx, 1, sy, 1)
  call begin_test('n_one')
  call print_scalar('result', result)
  call end_test()

  ! Test 4: stride=2
  sx = 0.0; sy = 0.0
  sx(1) = 1.0; sx(3) = 2.0; sx(5) = 3.0
  sy(1) = 4.0; sy(3) = 5.0; sy(5) = 6.0
  result = dsdot(3, sx, 2, sy, 2)
  call begin_test('stride')
  call print_scalar('result', result)
  call end_test()

  ! Test 5: negative stride
  sx = 0.0; sy = 0.0
  sx(1:3) = (/1.0, 2.0, 3.0/)
  sy(1:3) = (/4.0, 5.0, 6.0/)
  result = dsdot(3, sx, -1, sy, 1)
  call begin_test('neg_inc')
  call print_scalar('result', result)
  call end_test()

end program
