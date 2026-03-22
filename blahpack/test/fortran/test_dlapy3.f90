program test_dlapy3
  use test_utils
  implicit none

  double precision :: dlapy3
  external :: dlapy3
  double precision :: result

  ! Test 1: (3, 4, 0) -> 5
  result = dlapy3(3.0d0, 4.0d0, 0.0d0)
  call begin_test('dlapy3_3_4_0')
  call print_scalar('result', result)
  call end_test()

  ! Test 2: (1, 1, 1) -> sqrt(3)
  result = dlapy3(1.0d0, 1.0d0, 1.0d0)
  call begin_test('dlapy3_1_1_1')
  call print_scalar('result', result)
  call end_test()

  ! Test 3: (0, 0, 0) -> 0
  result = dlapy3(0.0d0, 0.0d0, 0.0d0)
  call begin_test('dlapy3_0_0_0')
  call print_scalar('result', result)
  call end_test()

  ! Test 4: large values (overflow-safe)
  result = dlapy3(1.0d300, 1.0d300, 1.0d300)
  call begin_test('dlapy3_large')
  call print_scalar('result', result)
  call end_test()

  ! Test 5: small values
  result = dlapy3(1.0d-300, 2.0d-300, 3.0d-300)
  call begin_test('dlapy3_small')
  call print_scalar('result', result)
  call end_test()

  ! Test 6: mixed
  result = dlapy3(2.0d0, 3.0d0, 6.0d0)
  call begin_test('dlapy3_2_3_6')
  call print_scalar('result', result)
  call end_test()

  ! Test 7: negative values
  result = dlapy3(-3.0d0, -4.0d0, 0.0d0)
  call begin_test('dlapy3_neg')
  call print_scalar('result', result)
  call end_test()

  ! Test 8: one non-zero
  result = dlapy3(5.0d0, 0.0d0, 0.0d0)
  call begin_test('dlapy3_single')
  call print_scalar('result', result)
  call end_test()

end program
