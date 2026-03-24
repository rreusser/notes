program test_dpttrf
  use test_utils
  implicit none
  double precision :: d(10), e(10)
  integer :: info

  ! Test 1: basic 5x5 positive definite tridiagonal
  ! Matrix:
  !   4  -1   0   0   0
  !  -1   4  -1   0   0
  !   0  -1   4  -1   0
  !   0   0  -1   4  -1
  !   0   0   0  -1   4
  d(1:5) = (/4.0d0, 4.0d0, 4.0d0, 4.0d0, 4.0d0/)
  e(1:4) = (/-1.0d0, -1.0d0, -1.0d0, -1.0d0/)
  call dpttrf(5, d, e, info)
  call begin_test('basic_5x5')
  call print_array('d', d, 5)
  call print_array('e', e, 4)
  call print_int('info', info)
  call end_test()

  ! Test 2: n=1 (single element)
  d(1) = 3.0d0
  call dpttrf(1, d, e, info)
  call begin_test('n_one')
  call print_array('d', d, 1)
  call print_int('info', info)
  call end_test()

  ! Test 3: n=0 (quick return)
  call dpttrf(0, d, e, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 4: non-positive-definite (d(1) <= 0)
  d(1:3) = (/-1.0d0, 4.0d0, 4.0d0/)
  e(1:2) = (/1.0d0, 1.0d0/)
  call dpttrf(3, d, e, info)
  call begin_test('not_posdef_first')
  call print_array('d', d, 3)
  call print_array('e', e, 2)
  call print_int('info', info)
  call end_test()

  ! Test 5: non-positive-definite (fails mid-factorization)
  ! d(1) = 1, e(1) = 2 => d(2) = 1 - (2/1)*2 = 1 - 4 = -3
  d(1:3) = (/1.0d0, 1.0d0, 4.0d0/)
  e(1:2) = (/2.0d0, 1.0d0/)
  call dpttrf(3, d, e, info)
  call begin_test('not_posdef_mid')
  call print_array('d', d, 3)
  call print_array('e', e, 2)
  call print_int('info', info)
  call end_test()

  ! Test 6: larger 8x8 to exercise the 4-unrolled loop
  d(1:8) = (/5.0d0, 5.0d0, 5.0d0, 5.0d0, 5.0d0, 5.0d0, 5.0d0, 5.0d0/)
  e(1:7) = (/1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0/)
  call dpttrf(8, d, e, info)
  call begin_test('unrolled_8x8')
  call print_array('d', d, 8)
  call print_array('e', e, 7)
  call print_int('info', info)
  call end_test()

  ! Test 7: n=2
  d(1:2) = (/4.0d0, 4.0d0/)
  e(1:1) = (/2.0d0/)
  call dpttrf(2, d, e, info)
  call begin_test('n_two')
  call print_array('d', d, 2)
  call print_array('e', e, 1)
  call print_int('info', info)
  call end_test()

end program
