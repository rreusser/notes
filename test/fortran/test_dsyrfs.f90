program test_dsyrfs
  use test_utils
  implicit none
  double precision :: a(100), af(100), b(100), x(100), work(300)
  double precision :: ferr(10), berr(10)
  integer :: ipiv(10), iwork(10), info, i

  ! Test 1: 3x3 upper triangle, single RHS
  ! A = [4 2 1; 2 5 3; 1 3 6] (symmetric positive definite, stored upper)
  ! b = [1; 2; 3]
  a = 0.0d0; af = 0.0d0; b = 0.0d0; x = 0.0d0
  ! Column-major upper triangle
  a(1) = 4.0d0; a(2) = 0.0d0; a(3) = 0.0d0
  a(4) = 2.0d0; a(5) = 5.0d0; a(6) = 0.0d0
  a(7) = 1.0d0; a(8) = 3.0d0; a(9) = 6.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0

  af(1:9) = a(1:9)
  call dsytrf('U', 3, af, 3, ipiv, work, 100, info)

  x(1:3) = b(1:3)
  call dsytrs('U', 3, 1, af, 3, ipiv, x, 3, info)

  call dsyrfs('U', 3, 1, a, 3, af, 3, ipiv, b, 3, x, 3, &
              ferr, berr, work, iwork, info)
  call begin_test('upper_3x3')
  call print_array('x', x, 3)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_int('info', info)
  call end_test()

  ! Test 2: 3x3 lower triangle, single RHS
  ! Same matrix but stored as lower triangle
  a = 0.0d0; af = 0.0d0; b = 0.0d0; x = 0.0d0
  a(1) = 4.0d0; a(2) = 2.0d0; a(3) = 1.0d0
  a(4) = 0.0d0; a(5) = 5.0d0; a(6) = 3.0d0
  a(7) = 0.0d0; a(8) = 0.0d0; a(9) = 6.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0

  af(1:9) = a(1:9)
  call dsytrf('L', 3, af, 3, ipiv, work, 100, info)

  x(1:3) = b(1:3)
  call dsytrs('L', 3, 1, af, 3, ipiv, x, 3, info)

  call dsyrfs('L', 3, 1, a, 3, af, 3, ipiv, b, 3, x, 3, &
              ferr, berr, work, iwork, info)
  call begin_test('lower_3x3')
  call print_array('x', x, 3)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_int('info', info)
  call end_test()

  ! Test 3: Multiple RHS (NRHS=2), upper
  a = 0.0d0; af = 0.0d0; b = 0.0d0; x = 0.0d0
  a(1) = 4.0d0; a(2) = 0.0d0; a(3) = 0.0d0
  a(4) = 2.0d0; a(5) = 5.0d0; a(6) = 0.0d0
  a(7) = 1.0d0; a(8) = 3.0d0; a(9) = 6.0d0
  ! b1 = [1;0;0], b2 = [0;1;0]
  b(1) = 1.0d0; b(2) = 0.0d0; b(3) = 0.0d0
  b(4) = 0.0d0; b(5) = 1.0d0; b(6) = 0.0d0

  af(1:9) = a(1:9)
  call dsytrf('U', 3, af, 3, ipiv, work, 100, info)

  x(1:6) = b(1:6)
  call dsytrs('U', 3, 2, af, 3, ipiv, x, 3, info)

  call dsyrfs('U', 3, 2, a, 3, af, 3, ipiv, b, 3, x, 3, &
              ferr, berr, work, iwork, info)
  call begin_test('multi_rhs')
  call print_array('x', x, 6)
  call print_array('ferr', ferr, 2)
  call print_array('berr', berr, 2)
  call print_int('info', info)
  call end_test()

  ! Test 4: N=0 quick return
  call dsyrfs('U', 0, 1, a, 1, af, 1, ipiv, b, 1, x, 1, &
              ferr, berr, work, iwork, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 5: NRHS=0 quick return
  call dsyrfs('U', 3, 0, a, 3, af, 3, ipiv, b, 3, x, 3, &
              ferr, berr, work, iwork, info)
  call begin_test('nrhs_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: 1x1 system
  a = 0.0d0; af = 0.0d0; b = 0.0d0; x = 0.0d0
  a(1) = 5.0d0
  b(1) = 10.0d0
  af(1) = 5.0d0
  call dsytrf('U', 1, af, 1, ipiv, work, 100, info)
  x(1) = b(1)
  call dsytrs('U', 1, 1, af, 1, ipiv, x, 1, info)

  call dsyrfs('U', 1, 1, a, 1, af, 1, ipiv, b, 1, x, 1, &
              ferr, berr, work, iwork, info)
  call begin_test('one_by_one')
  call print_array('x', x, 1)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_int('info', info)
  call end_test()

  ! Test 7: Ill-conditioned symmetric matrix (Hilbert-like, upper)
  a = 0.0d0; af = 0.0d0; b = 0.0d0; x = 0.0d0
  ! H(i,j) = 1/(i+j-1), upper triangle stored column-major
  a(1) = 1.0d0;       a(2) = 0.0d0;       a(3) = 0.0d0
  a(4) = 0.5d0;       a(5) = 1.0d0/3.0d0; a(6) = 0.0d0
  a(7) = 1.0d0/3.0d0; a(8) = 0.25d0;      a(9) = 0.2d0
  b(1) = 1.0d0; b(2) = 1.0d0; b(3) = 1.0d0

  af(1:9) = a(1:9)
  call dsytrf('U', 3, af, 3, ipiv, work, 100, info)

  x(1:3) = b(1:3)
  call dsytrs('U', 3, 1, af, 3, ipiv, x, 3, info)

  call dsyrfs('U', 3, 1, a, 3, af, 3, ipiv, b, 3, x, 3, &
              ferr, berr, work, iwork, info)
  call begin_test('hilbert_upper')
  call print_array('x', x, 3)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_int('info', info)
  call end_test()

end program
