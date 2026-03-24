program test_dptsv
  use test_utils
  implicit none
  double precision :: d(10), e(10), b(10, 4)
  integer :: info

  ! Test 1: basic 5x5 tridiagonal, single RHS
  ! Matrix:
  !   4  -1   0   0   0
  !  -1   4  -1   0   0
  !   0  -1   4  -1   0
  !   0   0  -1   4  -1
  !   0   0   0  -1   4
  d(1:5) = (/4.0d0, 4.0d0, 4.0d0, 4.0d0, 4.0d0/)
  e(1:4) = (/-1.0d0, -1.0d0, -1.0d0, -1.0d0/)
  b(1:5, 1) = (/1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0/)
  call dptsv(5, 1, d, e, b, 10, info)
  call begin_test('basic_5x5_single_rhs')
  call print_array('d', d, 5)
  call print_array('e', e, 4)
  call print_array('b', b(1:5, 1), 5)
  call print_int('info', info)
  call end_test()

  ! Test 2: 4x4 with 2 RHS
  d(1:4) = (/3.0d0, 3.0d0, 3.0d0, 3.0d0/)
  e(1:3) = (/1.0d0, 1.0d0, 1.0d0/)
  b(1:4, 1) = (/1.0d0, 0.0d0, 0.0d0, 1.0d0/)
  b(1:4, 2) = (/2.0d0, 1.0d0, 1.0d0, 2.0d0/)
  call dptsv(4, 2, d, e, b, 10, info)
  call begin_test('multi_rhs')
  call print_array('d', d, 4)
  call print_array('e', e, 3)
  call print_array('b1', b(1:4, 1), 4)
  call print_array('b2', b(1:4, 2), 4)
  call print_int('info', info)
  call end_test()

  ! Test 3: n=1
  d(1) = 5.0d0
  b(1, 1) = 10.0d0
  call dptsv(1, 1, d, e, b, 10, info)
  call begin_test('n_one')
  call print_array('d', d, 1)
  call print_array('b', b(1:1, 1), 1)
  call print_int('info', info)
  call end_test()

  ! Test 4: n=0 (quick return)
  call dptsv(0, 1, d, e, b, 10, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 5: non-positive-definite matrix (should return INFO > 0)
  d(1:3) = (/-1.0d0, 4.0d0, 4.0d0/)
  e(1:2) = (/1.0d0, 1.0d0/)
  b(1:3, 1) = (/1.0d0, 2.0d0, 3.0d0/)
  call dptsv(3, 1, d, e, b, 10, info)
  call begin_test('not_posdef')
  call print_int('info', info)
  call end_test()

end program
