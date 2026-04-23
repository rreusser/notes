program test_dlaisnan
  use test_utils
  implicit none

  logical :: dlaisnan
  external :: dlaisnan
  double precision :: val, zero
  integer :: res

  zero = 0.0d0

  ! Test 1: NaN != NaN should be true
  val = zero / zero
  if (dlaisnan(val, val)) then
    res = 1
  else
    res = 0
  end if
  call begin_test('dlaisnan_nan')
  call print_int('result', res)
  call end_test()

  ! Test 2: 0.0 == 0.0 should be false (not NaN)
  val = 0.0d0
  if (dlaisnan(val, val)) then
    res = 1
  else
    res = 0
  end if
  call begin_test('dlaisnan_zero')
  call print_int('result', res)
  call end_test()

  ! Test 3: 1.0 == 1.0 should be false
  val = 1.0d0
  if (dlaisnan(val, val)) then
    res = 1
  else
    res = 0
  end if
  call begin_test('dlaisnan_one')
  call print_int('result', res)
  call end_test()

  ! Test 4: inf == inf should be false
  val = 1.0d0 / zero
  if (dlaisnan(val, val)) then
    res = 1
  else
    res = 0
  end if
  call begin_test('dlaisnan_inf')
  call print_int('result', res)
  call end_test()

end program
