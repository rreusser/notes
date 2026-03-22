program test_disnan
  use test_utils
  implicit none

  logical :: disnan
  external :: disnan
  double precision :: val
  integer :: res

  ! Use IEEE_VALUE to get a proper NaN
  double precision :: zero

  zero = 0.0d0

  ! Test 1: NaN
  val = zero / zero
  if (disnan(val)) then
    res = 1
  else
    res = 0
  end if
  call begin_test('disnan_nan')
  call print_int('result', res)
  call end_test()

  ! Test 2: zero
  val = 0.0d0
  if (disnan(val)) then
    res = 1
  else
    res = 0
  end if
  call begin_test('disnan_zero')
  call print_int('result', res)
  call end_test()

  ! Test 3: one
  val = 1.0d0
  if (disnan(val)) then
    res = 1
  else
    res = 0
  end if
  call begin_test('disnan_one')
  call print_int('result', res)
  call end_test()

  ! Test 4: infinity
  val = 1.0d0 / zero
  if (disnan(val)) then
    res = 1
  else
    res = 0
  end if
  call begin_test('disnan_inf')
  call print_int('result', res)
  call end_test()

  ! Test 5: negative infinity
  val = -1.0d0 / zero
  if (disnan(val)) then
    res = 1
  else
    res = 0
  end if
  call begin_test('disnan_neginf')
  call print_int('result', res)
  call end_test()

  ! Test 6: large value
  val = 1.0d300
  if (disnan(val)) then
    res = 1
  else
    res = 0
  end if
  call begin_test('disnan_large')
  call print_int('result', res)
  call end_test()

end program
