program test_izmax1
  use test_utils
  implicit none

  integer :: result, izmax1
  external izmax1
  double precision :: zx_r(20)
  complex*16 :: zx(10)
  equivalence (zx, zx_r)

  ! IZMAX1: index of element with maximum |z| (genuine cabs), 1-based in Fortran

  ! Test 1: basic 3-element vector
  ! zx = [(1,0), (3,4), (2,0)] => |1|=1, |3+4i|=5, |2|=2 => max at index 2
  zx(1) = (1.0d0, 0.0d0)
  zx(2) = (3.0d0, 4.0d0)
  zx(3) = (2.0d0, 0.0d0)
  result = izmax1(3, zx, 1)
  call begin_test('basic_3')
  call print_int('result', result)
  call end_test()

  ! Test 2: max at first element
  zx(1) = (5.0d0, 12.0d0)
  zx(2) = (3.0d0, 4.0d0)
  zx(3) = (1.0d0, 0.0d0)
  result = izmax1(3, zx, 1)
  call begin_test('max_first')
  call print_int('result', result)
  call end_test()

  ! Test 3: max at last element
  zx(1) = (1.0d0, 0.0d0)
  zx(2) = (2.0d0, 0.0d0)
  zx(3) = (3.0d0, 0.0d0)
  zx(4) = (0.0d0, 10.0d0)
  result = izmax1(4, zx, 1)
  call begin_test('max_last')
  call print_int('result', result)
  call end_test()

  ! Test 4: N=1
  zx(1) = (7.0d0, 3.0d0)
  result = izmax1(1, zx, 1)
  call begin_test('n_one')
  call print_int('result', result)
  call end_test()

  ! Test 5: stride=2
  zx(1) = (1.0d0, 0.0d0)
  zx(2) = (99.0d0, 99.0d0)
  zx(3) = (3.0d0, 4.0d0)
  zx(4) = (99.0d0, 99.0d0)
  zx(5) = (2.0d0, 0.0d0)
  result = izmax1(3, zx, 2)
  call begin_test('stride2')
  call print_int('result', result)
  call end_test()

  ! Test 6: all equal magnitudes
  zx(1) = (3.0d0, 4.0d0)
  zx(2) = (0.0d0, 5.0d0)
  zx(3) = (5.0d0, 0.0d0)
  result = izmax1(3, zx, 1)
  call begin_test('equal_magnitudes')
  call print_int('result', result)
  call end_test()

end program
