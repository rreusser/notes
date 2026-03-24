program test_dsymm
  use test_utils
  implicit none
  double precision :: a(16), b(16), c(16)

  ! Test 1: SIDE='L', UPLO='U', basic 3x2
  ! A is 3x3 symmetric (upper stored), B is 3x2, C is 3x2
  ! A = [2 1 3; 1 4 2; 3 2 5] (upper: A(1,1)=2, A(1,2)=1, A(1,3)=3, A(2,2)=4, A(2,3)=2, A(3,3)=5)
  ! B = [1 4; 2 5; 3 6]
  ! C = alpha*A*B + beta*C = 1.0*A*B + 0.0*C
  ! A*B col 1: [2*1+1*2+3*3, 1*1+4*2+2*3, 3*1+2*2+5*3] = [13, 15, 22]
  ! A*B col 2: [2*4+1*5+3*6, 1*4+4*5+2*6, 3*4+2*5+5*6] = [31, 36, 52]
  a = 0.0d0; b = 0.0d0; c = 0.0d0
  a(1) = 2.0d0; a(2) = 1.0d0; a(3) = 3.0d0  ! col 1 (but only upper matters: a(1)=2)
  ! Wait, column-major: A(1,1)=a(1), A(2,1)=a(2), A(3,1)=a(3), A(1,2)=a(4), ...
  ! For upper: use A(i,j) where i<=j
  a = 0.0d0
  a(1) = 2.0d0  ! A(1,1)
  a(4) = 1.0d0  ! A(1,2)
  a(5) = 4.0d0  ! A(2,2)
  a(7) = 3.0d0  ! A(1,3)
  a(8) = 2.0d0  ! A(2,3)
  a(9) = 5.0d0  ! A(3,3)
  ! LDA=3

  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0  ! col 1
  b(4) = 4.0d0; b(5) = 5.0d0; b(6) = 6.0d0  ! col 2

  call dsymm('L', 'U', 3, 2, 1.0d0, a, 3, b, 3, 0.0d0, c, 3)
  call begin_test('left_upper_basic')
  call print_matrix('C', c, 3, 3, 2)
  call end_test()

  ! Test 2: SIDE='L', UPLO='L', same matrix but lower stored
  a = 0.0d0
  a(1) = 2.0d0  ! A(1,1)
  a(2) = 1.0d0  ! A(2,1)
  a(3) = 3.0d0  ! A(3,1)
  a(5) = 4.0d0  ! A(2,2)
  a(6) = 2.0d0  ! A(3,2)
  a(9) = 5.0d0  ! A(3,3)

  c = 0.0d0
  call dsymm('L', 'L', 3, 2, 1.0d0, a, 3, b, 3, 0.0d0, c, 3)
  call begin_test('left_lower_basic')
  call print_matrix('C', c, 3, 3, 2)
  call end_test()

  ! Test 3: SIDE='R', UPLO='U', 2x3 * 3x3sym
  ! B is 2x3, A is 3x3 symmetric (upper stored)
  ! C = alpha*B*A + beta*C
  ! Reuse same A (upper stored) from test 1
  a = 0.0d0
  a(1) = 2.0d0; a(4) = 1.0d0; a(5) = 4.0d0
  a(7) = 3.0d0; a(8) = 2.0d0; a(9) = 5.0d0
  ! B is 2x3: B = [1 3 5; 2 4 6]
  b = 0.0d0
  b(1) = 1.0d0; b(2) = 2.0d0  ! col 1
  b(3) = 3.0d0; b(4) = 4.0d0  ! col 2
  b(5) = 5.0d0; b(6) = 6.0d0  ! col 3
  ! LDB=2

  c = 0.0d0
  call dsymm('R', 'U', 2, 3, 1.0d0, a, 3, b, 2, 0.0d0, c, 2)
  call begin_test('right_upper_basic')
  call print_matrix('C', c, 2, 2, 3)
  call end_test()

  ! Test 4: SIDE='R', UPLO='L'
  a = 0.0d0
  a(1) = 2.0d0; a(2) = 1.0d0; a(3) = 3.0d0
  a(5) = 4.0d0; a(6) = 2.0d0; a(9) = 5.0d0

  c = 0.0d0
  call dsymm('R', 'L', 2, 3, 1.0d0, a, 3, b, 2, 0.0d0, c, 2)
  call begin_test('right_lower_basic')
  call print_matrix('C', c, 2, 2, 3)
  call end_test()

  ! Test 5: alpha=2, beta=3
  a = 0.0d0
  a(1) = 2.0d0; a(4) = 1.0d0; a(5) = 4.0d0
  a(7) = 3.0d0; a(8) = 2.0d0; a(9) = 5.0d0

  b = 0.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  b(4) = 4.0d0; b(5) = 5.0d0; b(6) = 6.0d0

  c = 0.0d0
  c(1) = 1.0d0; c(2) = 1.0d0; c(3) = 1.0d0
  c(4) = 1.0d0; c(5) = 1.0d0; c(6) = 1.0d0

  call dsymm('L', 'U', 3, 2, 2.0d0, a, 3, b, 3, 3.0d0, c, 3)
  call begin_test('alpha_beta_scaling')
  call print_matrix('C', c, 3, 3, 2)
  call end_test()

  ! Test 6: alpha=0, beta=2 — just scale C
  c = 0.0d0
  c(1) = 1.0d0; c(2) = 2.0d0; c(3) = 3.0d0; c(4) = 4.0d0
  call dsymm('L', 'U', 2, 2, 0.0d0, a, 3, b, 3, 2.0d0, c, 2)
  call begin_test('alpha_zero')
  call print_matrix('C', c, 2, 2, 2)
  call end_test()

  ! Test 7: M=0 quick return
  c(1) = 99.0d0
  call dsymm('L', 'U', 0, 2, 1.0d0, a, 1, b, 1, 0.0d0, c, 1)
  call begin_test('m_zero')
  call print_scalar('C1', c(1))
  call end_test()

  ! Test 8: N=0 quick return
  c(1) = 99.0d0
  call dsymm('L', 'U', 2, 0, 1.0d0, a, 2, b, 2, 0.0d0, c, 2)
  call begin_test('n_zero')
  call print_scalar('C1', c(1))
  call end_test()

  ! Test 9: N=1, M=1 scalar-like
  a = 0.0d0; b = 0.0d0; c = 0.0d0
  a(1) = 3.0d0
  b(1) = 5.0d0
  call dsymm('L', 'U', 1, 1, 2.0d0, a, 1, b, 1, 0.0d0, c, 1)
  call begin_test('scalar')
  call print_scalar('C1', c(1))
  call end_test()

  ! Test 10: beta=0 overwrites C
  c = 0.0d0
  c(1) = 999.0d0; c(2) = 999.0d0; c(3) = 999.0d0; c(4) = 999.0d0
  a = 0.0d0; a(1) = 1.0d0; a(2) = 0.0d0; a(3) = 0.0d0; a(4) = 1.0d0
  b = 0.0d0; b(1) = 2.0d0; b(2) = 3.0d0; b(3) = 4.0d0; b(4) = 5.0d0
  call dsymm('L', 'L', 2, 2, 1.0d0, a, 2, b, 2, 0.0d0, c, 2)
  call begin_test('beta_zero')
  call print_matrix('C', c, 2, 2, 2)
  call end_test()

end program
