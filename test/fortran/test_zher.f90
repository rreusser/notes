program test_zher
  use test_utils
  implicit none
  complex*16 :: A(25), x(10)
  double precision :: A_r(50), x_r(20)
  equivalence (A, A_r)
  equivalence (x, x_r)
  double precision :: alpha

  ! Test 1: upper triangle, 2x2, alpha=1
  ! Hermitian A (upper stored):
  !   A = [ 2    (1+1i) ]
  !       [ .     3     ]
  ! x = [(1,0), (0,1)]
  ! A := alpha*x*x^H + A
  A = (0.0d0, 0.0d0)
  A(1) = (2.0d0, 0.0d0)
  A(3) = (1.0d0, 1.0d0)
  A(4) = (3.0d0, 0.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (0.0d0, 1.0d0)
  alpha = 1.0d0
  call zher('U', 2, alpha, x, 1, A, 2)
  call begin_test('upper_basic')
  call print_array('A', A_r, 8)
  call end_test()

  ! Test 2: lower triangle, 2x2, alpha=1
  A = (0.0d0, 0.0d0)
  A(1) = (2.0d0, 0.0d0)
  A(2) = (1.0d0, -1.0d0)
  A(4) = (3.0d0, 0.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (0.0d0, 1.0d0)
  alpha = 1.0d0
  call zher('L', 2, alpha, x, 1, A, 2)
  call begin_test('lower_basic')
  call print_array('A', A_r, 8)
  call end_test()

  ! Test 3: N=0 quick return
  A(1) = (99.0d0, 88.0d0)
  alpha = 1.0d0
  call zher('U', 0, alpha, x, 1, A, 1)
  call begin_test('n_zero')
  call print_array('A', A_r, 2)
  call end_test()

  ! Test 4: alpha=0 quick return
  A(1) = (99.0d0, 88.0d0)
  alpha = 0.0d0
  call zher('U', 1, alpha, x, 1, A, 1)
  call begin_test('alpha_zero')
  call print_array('A', A_r, 2)
  call end_test()

  ! Test 5: N=1
  A(1) = (5.0d0, 0.0d0)
  x(1) = (2.0d0, 3.0d0)
  alpha = 1.0d0
  call zher('U', 1, alpha, x, 1, A, 1)
  call begin_test('n_one')
  call print_array('A', A_r, 2)
  call end_test()

  ! Test 6: upper, non-unit stride incx=2
  A = (0.0d0, 0.0d0)
  A(1) = (2.0d0, 0.0d0)
  A(4) = (1.0d0, 1.0d0)
  A(5) = (3.0d0, 0.0d0)
  A(7) = (2.0d0, -1.0d0)
  A(8) = (1.0d0, 2.0d0)
  A(9) = (4.0d0, 0.0d0)
  x = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(3) = (0.0d0, 1.0d0)
  x(5) = (1.0d0, 1.0d0)
  alpha = 2.0d0
  call zher('U', 3, alpha, x, 2, A, 3)
  call begin_test('upper_stride')
  call print_array('A', A_r, 18)
  call end_test()

  ! Test 7: lower, non-unit stride incx=2
  A = (0.0d0, 0.0d0)
  A(1) = (2.0d0, 0.0d0)
  A(2) = (1.0d0, -1.0d0)
  A(3) = (2.0d0, 1.0d0)
  A(5) = (3.0d0, 0.0d0)
  A(6) = (1.0d0, -2.0d0)
  A(9) = (4.0d0, 0.0d0)
  x = (0.0d0, 0.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(3) = (0.0d0, 1.0d0)
  x(5) = (1.0d0, 1.0d0)
  alpha = 2.0d0
  call zher('L', 3, alpha, x, 2, A, 3)
  call begin_test('lower_stride')
  call print_array('A', A_r, 18)
  call end_test()

  ! Test 8: alpha=2, upper 3x3
  A = (0.0d0, 0.0d0)
  A(1) = (1.0d0, 0.0d0)
  A(4) = (2.0d0, 1.0d0)
  A(5) = (3.0d0, 0.0d0)
  A(7) = (0.0d0, -1.0d0)
  A(8) = (1.0d0, 1.0d0)
  A(9) = (5.0d0, 0.0d0)
  x(1) = (1.0d0, 1.0d0)
  x(2) = (2.0d0, 0.0d0)
  x(3) = (0.0d0, 3.0d0)
  alpha = 2.0d0
  call zher('U', 3, alpha, x, 1, A, 3)
  call begin_test('upper_alpha2')
  call print_array('A', A_r, 18)
  call end_test()

  ! Test 9: x with zeros (tests skip branch)
  A = (0.0d0, 0.0d0)
  A(1) = (1.0d0, 0.5d0)
  A(3) = (2.0d0, 1.0d0)
  A(4) = (3.0d0, 0.7d0)
  x(1) = (0.0d0, 0.0d0)
  x(2) = (1.0d0, 0.0d0)
  alpha = 1.0d0
  call zher('U', 2, alpha, x, 1, A, 2)
  call begin_test('upper_zeros')
  call print_array('A', A_r, 8)
  call end_test()

  ! Test 10: lower, x with zeros
  A = (0.0d0, 0.0d0)
  A(1) = (1.0d0, 0.5d0)
  A(2) = (2.0d0, -1.0d0)
  A(4) = (3.0d0, 0.7d0)
  x(1) = (0.0d0, 0.0d0)
  x(2) = (1.0d0, 0.0d0)
  alpha = 1.0d0
  call zher('L', 2, alpha, x, 1, A, 2)
  call begin_test('lower_zeros')
  call print_array('A', A_r, 8)
  call end_test()

  ! Test 11: negative stride
  A = (0.0d0, 0.0d0)
  A(1) = (2.0d0, 0.0d0)
  A(3) = (1.0d0, 1.0d0)
  A(4) = (3.0d0, 0.0d0)
  x(1) = (0.0d0, 1.0d0)
  x(2) = (1.0d0, 0.0d0)
  alpha = 1.0d0
  call zher('U', 2, alpha, x, -1, A, 2)
  call begin_test('negative_stride')
  call print_array('A', A_r, 8)
  call end_test()

end program
