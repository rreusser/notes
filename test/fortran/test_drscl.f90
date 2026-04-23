program test_drscl
  use test_utils
  implicit none
  double precision :: x(10)
  integer :: i

  ! Test 1: basic reciprocal scaling, sa=2, x=[2,4,6,8], expect [1,2,3,4]
  x = 0.0d0
  do i = 1, 4
    x(i) = dble(i) * 2.0d0
  end do
  call drscl(4, 2.0d0, x, 1)
  call begin_test('basic')
  call print_array('x', x, 4)
  call end_test()

  ! Test 2: sa=0.5, x=[1,2,3], expect [2,4,6]
  x = 0.0d0
  x(1) = 1.0d0
  x(2) = 2.0d0
  x(3) = 3.0d0
  call drscl(3, 0.5d0, x, 1)
  call begin_test('half')
  call print_array('x', x, 3)
  call end_test()

  ! Test 3: n=0 quick return
  x = 0.0d0
  x(1) = 99.0d0
  call drscl(0, 2.0d0, x, 1)
  call begin_test('n_zero')
  call print_array('x', x, 1)
  call end_test()

  ! Test 4: n=1
  x = 0.0d0
  x(1) = 10.0d0
  call drscl(1, 5.0d0, x, 1)
  call begin_test('n_one')
  call print_array('x', x, 1)
  call end_test()

  ! Test 5: non-unit stride (incx=2)
  x = 0.0d0
  x(1) = 10.0d0
  x(2) = 99.0d0
  x(3) = 20.0d0
  x(4) = 99.0d0
  x(5) = 30.0d0
  call drscl(3, 10.0d0, x, 2)
  call begin_test('stride2')
  call print_array('x', x, 5)
  call end_test()

  ! Test 6: sa=1 (identity)
  x = 0.0d0
  x(1) = 3.0d0
  x(2) = 7.0d0
  call drscl(2, 1.0d0, x, 1)
  call begin_test('identity')
  call print_array('x', x, 2)
  call end_test()

  ! Test 7: large sa — does NOT trigger iterative scaling
  x = 0.0d0
  x(1) = 1.0d0
  x(2) = 2.0d0
  call drscl(2, 1.0d300, x, 1)
  call begin_test('large_sa')
  call print_array('x', x, 2)
  call end_test()

  ! Test 8: small sa — does NOT trigger iterative scaling
  x = 0.0d0
  x(1) = 1.0d-300
  x(2) = 2.0d-300
  call drscl(2, 1.0d-300, x, 1)
  call begin_test('small_sa')
  call print_array('x', x, 2)
  call end_test()

  ! Test 10: very large sa (>BIGNUM) triggers SMLNUM pre-multiply branch
  x = 0.0d0
  x(1) = 1.0d0
  x(2) = 2.0d0
  call drscl(2, 1.0d308, x, 1)
  call begin_test('very_large_sa')
  call print_array('x', x, 2)
  call end_test()

  ! Test 11: very small sa (<SMLNUM) triggers BIGNUM pre-multiply branch
  x = 0.0d0
  x(1) = 1.0d-308
  x(2) = 2.0d-308
  call drscl(2, 1.0d-309, x, 1)
  call begin_test('very_small_sa')
  call print_array('x', x, 2)
  call end_test()

  ! Test 9: negative sa
  x = 0.0d0
  x(1) = 6.0d0
  x(2) = -9.0d0
  x(3) = 12.0d0
  call drscl(3, -3.0d0, x, 1)
  call begin_test('negative_sa')
  call print_array('x', x, 3)
  call end_test()

end program
