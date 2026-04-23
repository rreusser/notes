program test_dzasum
  use test_utils
  implicit none
  double precision :: zx_r(20)
  complex*16 :: zx(10)
  equivalence (zx, zx_r)
  double precision :: result, dzasum

  ! Test 1: basic, n=3, incx=1
  zx(1) = dcmplx(1.0d0, 2.0d0)
  zx(2) = dcmplx(3.0d0, -4.0d0)
  zx(3) = dcmplx(-5.0d0, 6.0d0)
  result = dzasum(3, zx, 1)
  call begin_test('basic')
  call print_scalar('result', result)
  call end_test()

  ! Test 2: n=0
  result = dzasum(0, zx, 1)
  call begin_test('n_zero')
  call print_scalar('result', result)
  call end_test()

  ! Test 3: n=1
  zx(1) = dcmplx(3.0d0, 4.0d0)
  result = dzasum(1, zx, 1)
  call begin_test('n_one')
  call print_scalar('result', result)
  call end_test()

  ! Test 4: incx=2
  zx(1) = dcmplx(1.0d0, 1.0d0)
  zx(2) = dcmplx(99.0d0, 99.0d0)
  zx(3) = dcmplx(2.0d0, 3.0d0)
  result = dzasum(2, zx, 2)
  call begin_test('stride2')
  call print_scalar('result', result)
  call end_test()

end program
