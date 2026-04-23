program test_dgerfs
  use test_utils
  implicit none
  double precision :: a(100), af(100), b(100), x(100), work(300)
  double precision :: ferr(10), berr(10)
  integer :: ipiv(10), iwork(10), info, i

  ! Test 1: 3x3 system, single RHS, no-transpose
  ! A = [2 1 1; 4 3 3; 8 7 9], b = [1; 1; 1]
  a = 0.0d0; af = 0.0d0; b = 0.0d0; x = 0.0d0
  a(1) = 2.0d0; a(2) = 4.0d0; a(3) = 8.0d0
  a(4) = 1.0d0; a(5) = 3.0d0; a(6) = 7.0d0
  a(7) = 1.0d0; a(8) = 3.0d0; a(9) = 9.0d0
  b(1) = 1.0d0; b(2) = 1.0d0; b(3) = 1.0d0

  ! Copy A to AF, then factorize AF
  af(1:9) = a(1:9)
  call dgetrf(3, 3, af, 3, ipiv, info)

  ! Initial solve
  x(1:3) = b(1:3)
  call dgetrs('N', 3, 1, af, 3, ipiv, x, 3, info)

  ! Now refine
  call dgerfs('N', 3, 1, a, 3, af, 3, ipiv, b, 3, x, 3, &
              ferr, berr, work, iwork, info)
  call begin_test('basic_3x3')
  call print_array('x', x, 3)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_int('info', info)
  call end_test()

  ! Test 2: 3x3 system, transpose
  a = 0.0d0; af = 0.0d0; b = 0.0d0; x = 0.0d0
  a(1) = 2.0d0; a(2) = 4.0d0; a(3) = 8.0d0
  a(4) = 1.0d0; a(5) = 3.0d0; a(6) = 7.0d0
  a(7) = 1.0d0; a(8) = 3.0d0; a(9) = 9.0d0
  b(1) = 1.0d0; b(2) = 1.0d0; b(3) = 1.0d0

  af(1:9) = a(1:9)
  call dgetrf(3, 3, af, 3, ipiv, info)

  x(1:3) = b(1:3)
  call dgetrs('T', 3, 1, af, 3, ipiv, x, 3, info)

  call dgerfs('T', 3, 1, a, 3, af, 3, ipiv, b, 3, x, 3, &
              ferr, berr, work, iwork, info)
  call begin_test('transpose_3x3')
  call print_array('x', x, 3)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_int('info', info)
  call end_test()

  ! Test 3: Multiple RHS (NRHS=2)
  a = 0.0d0; af = 0.0d0; b = 0.0d0; x = 0.0d0
  a(1) = 2.0d0; a(2) = 4.0d0; a(3) = 8.0d0
  a(4) = 1.0d0; a(5) = 3.0d0; a(6) = 7.0d0
  a(7) = 1.0d0; a(8) = 3.0d0; a(9) = 9.0d0
  ! b1 = [1;0;0], b2 = [0;1;0]
  b(1) = 1.0d0; b(2) = 0.0d0; b(3) = 0.0d0
  b(4) = 0.0d0; b(5) = 1.0d0; b(6) = 0.0d0

  af(1:9) = a(1:9)
  call dgetrf(3, 3, af, 3, ipiv, info)

  x(1:6) = b(1:6)
  call dgetrs('N', 3, 2, af, 3, ipiv, x, 3, info)

  call dgerfs('N', 3, 2, a, 3, af, 3, ipiv, b, 3, x, 3, &
              ferr, berr, work, iwork, info)
  call begin_test('multi_rhs')
  call print_array('x', x, 6)
  call print_array('ferr', ferr, 2)
  call print_array('berr', berr, 2)
  call print_int('info', info)
  call end_test()

  ! Test 4: N=0 quick return
  call dgerfs('N', 0, 1, a, 1, af, 1, ipiv, b, 1, x, 1, &
              ferr, berr, work, iwork, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 5: NRHS=0 quick return
  call dgerfs('N', 3, 0, a, 3, af, 3, ipiv, b, 3, x, 3, &
              ferr, berr, work, iwork, info)
  call begin_test('nrhs_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: Ill-conditioned 3x3 (Hilbert-like matrix)
  a = 0.0d0; af = 0.0d0; b = 0.0d0; x = 0.0d0
  ! A is a 3x3 Hilbert matrix: A(i,j) = 1/(i+j-1)
  a(1) = 1.0d0;       a(2) = 0.5d0;       a(3) = 1.0d0/3.0d0
  a(4) = 0.5d0;       a(5) = 1.0d0/3.0d0; a(6) = 0.25d0
  a(7) = 1.0d0/3.0d0; a(8) = 0.25d0;      a(9) = 0.2d0
  b(1) = 1.0d0; b(2) = 1.0d0; b(3) = 1.0d0

  af(1:9) = a(1:9)
  call dgetrf(3, 3, af, 3, ipiv, info)

  x(1:3) = b(1:3)
  call dgetrs('N', 3, 1, af, 3, ipiv, x, 3, info)

  call dgerfs('N', 3, 1, a, 3, af, 3, ipiv, b, 3, x, 3, &
              ferr, berr, work, iwork, info)
  call begin_test('hilbert_3x3')
  call print_array('x', x, 3)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_int('info', info)
  call end_test()

  ! Test 7: 1x1 system
  a = 0.0d0; af = 0.0d0; b = 0.0d0; x = 0.0d0
  a(1) = 5.0d0
  b(1) = 10.0d0
  af(1) = 5.0d0
  call dgetrf(1, 1, af, 1, ipiv, info)
  x(1) = b(1)
  call dgetrs('N', 1, 1, af, 1, ipiv, x, 1, info)

  call dgerfs('N', 1, 1, a, 1, af, 1, ipiv, b, 1, x, 1, &
              ferr, berr, work, iwork, info)
  call begin_test('one_by_one')
  call print_array('x', x, 1)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_int('info', info)
  call end_test()

end program
