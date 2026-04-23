program test_dzsum1
  use test_utils
  implicit none

  double precision :: result, dzsum1
  external dzsum1
  double precision :: zx_r(20)
  complex*16 :: zx(10)
  equivalence (zx, zx_r)

  ! DZSUM1: sum of |z_i| using genuine complex absolute value (cabs)

  ! Test 1: basic 3-element vector
  ! zx = [(3,4), (1,0), (0,1)] => |3+4i|=5, |1|=1, |i|=1 => sum=7
  zx(1) = (3.0d0, 4.0d0)
  zx(2) = (1.0d0, 0.0d0)
  zx(3) = (0.0d0, 1.0d0)
  result = dzsum1(3, zx, 1)
  call begin_test('basic_3')
  call print_scalar('result', result)
  call end_test()

  ! Test 2: stride=2
  ! zx = [(3,4), skip, (5,12), skip, (8,15)]
  zx(1) = (3.0d0, 4.0d0)
  zx(2) = (99.0d0, 99.0d0)
  zx(3) = (5.0d0, 12.0d0)
  zx(4) = (99.0d0, 99.0d0)
  zx(5) = (8.0d0, 15.0d0)
  ! |3+4i|=5, |5+12i|=13, |8+15i|=17 => sum=35
  result = dzsum1(3, zx, 2)
  call begin_test('stride2')
  call print_scalar('result', result)
  call end_test()

  ! Test 3: N=0 => result should be 0
  result = dzsum1(0, zx, 1)
  call begin_test('n_zero')
  call print_scalar('result', result)
  call end_test()

  ! Test 4: N=1
  zx(1) = (6.0d0, 8.0d0)
  result = dzsum1(1, zx, 1)
  call begin_test('n_one')
  call print_scalar('result', result)
  call end_test()

  ! Test 5: all zeros
  zx(1) = (0.0d0, 0.0d0)
  zx(2) = (0.0d0, 0.0d0)
  zx(3) = (0.0d0, 0.0d0)
  result = dzsum1(3, zx, 1)
  call begin_test('all_zeros')
  call print_scalar('result', result)
  call end_test()

  ! Test 6: purely real
  zx(1) = (3.0d0, 0.0d0)
  zx(2) = (-4.0d0, 0.0d0)
  zx(3) = (5.0d0, 0.0d0)
  result = dzsum1(3, zx, 1)
  call begin_test('purely_real')
  call print_scalar('result', result)
  call end_test()

  ! Test 7: purely imaginary
  zx(1) = (0.0d0, 2.0d0)
  zx(2) = (0.0d0, -3.0d0)
  result = dzsum1(2, zx, 1)
  call begin_test('purely_imag')
  call print_scalar('result', result)
  call end_test()

end program
