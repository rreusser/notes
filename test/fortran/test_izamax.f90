program test_izamax
  use test_utils
  implicit none
  complex*16 :: zx(20)
  integer :: izamax, result

  ! Test 1: basic, n=4, incx=1
  ! |Re| + |Im| values: 1+2=3, 5+1=6, 2+3=5, 4+0=4
  ! Max is element 2 (1-based), value = 6
  zx = (0.0d0, 0.0d0)
  zx(1) = (1.0d0, 2.0d0)
  zx(2) = (5.0d0, 1.0d0)
  zx(3) = (2.0d0, 3.0d0)
  zx(4) = (4.0d0, 0.0d0)
  result = izamax(4, zx, 1)
  call begin_test('basic')
  call print_int('result', result)
  call end_test()

  ! Test 2: n=0 (should return 0)
  result = izamax(0, zx, 1)
  call begin_test('n_zero')
  call print_int('result', result)
  call end_test()

  ! Test 3: n=1 (should return 1)
  result = izamax(1, zx, 1)
  call begin_test('n_one')
  call print_int('result', result)
  call end_test()

  ! Test 4: non-unit stride, incx=2
  ! Elements accessed: zx(1), zx(3), zx(5)
  ! |Re| + |Im|: 1+2=3, 2+3=5, 10+10=20
  zx = (0.0d0, 0.0d0)
  zx(1) = (1.0d0, 2.0d0)
  zx(2) = (99.0d0, 99.0d0)
  zx(3) = (2.0d0, 3.0d0)
  zx(4) = (99.0d0, 99.0d0)
  zx(5) = (10.0d0, 10.0d0)
  result = izamax(3, zx, 2)
  call begin_test('stride')
  call print_int('result', result)
  call end_test()

  ! Test 5: all equal magnitudes
  ! |Re| + |Im| = 5 for all
  zx = (0.0d0, 0.0d0)
  zx(1) = (3.0d0, 2.0d0)
  zx(2) = (1.0d0, 4.0d0)
  zx(3) = (5.0d0, 0.0d0)
  result = izamax(3, zx, 1)
  call begin_test('equal')
  call print_int('result', result)
  call end_test()

  ! Test 6: negative components
  ! |Re| + |Im|: |-3|+|-4|=7, |1|+|1|=2, |-2|+|5|=7
  ! First max is element 1
  zx = (0.0d0, 0.0d0)
  zx(1) = (-3.0d0, -4.0d0)
  zx(2) = (1.0d0, 1.0d0)
  zx(3) = (-2.0d0, 5.0d0)
  result = izamax(3, zx, 1)
  call begin_test('negative')
  call print_int('result', result)
  call end_test()

end program
