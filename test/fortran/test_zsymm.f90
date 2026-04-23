program test_zsymm
  use test_utils
  implicit none
  complex*16 :: a(16), b(16), c(16)
  double precision :: a_r(32), b_r(32), c_r(32)
  equivalence (a, a_r)
  equivalence (b, b_r)
  equivalence (c, c_r)

  ! Test 1: SIDE='L', UPLO='U', basic 3x2
  ! A is 3x3 symmetric (upper stored), B is 3x2, C is 3x2
  ! A = [2+i 1+i 3-2i; 1+i 4-i 2+i; 3-2i 2+i 5+2i]  (symmetric, not Hermitian!)
  a = (0.0d0, 0.0d0)
  a(1) = (2.0d0, 1.0d0)    ! A(1,1)
  a(4) = (1.0d0, 1.0d0)    ! A(1,2)
  a(5) = (4.0d0, -1.0d0)   ! A(2,2)
  a(7) = (3.0d0, -2.0d0)   ! A(1,3)
  a(8) = (2.0d0, 1.0d0)    ! A(2,3)
  a(9) = (5.0d0, 2.0d0)    ! A(3,3)

  b = (0.0d0, 0.0d0)
  b(1) = (1.0d0, 0.5d0); b(2) = (2.0d0, -1.0d0); b(3) = (3.0d0, 1.0d0)
  b(4) = (4.0d0, 2.0d0); b(5) = (5.0d0, 0.0d0);  b(6) = (6.0d0, -0.5d0)

  c = (0.0d0, 0.0d0)
  call zsymm('L', 'U', 3, 2, (1.0d0,0.0d0), a, 3, b, 3, (0.0d0,0.0d0), c, 3)
  call begin_test('left_upper_basic')
  call print_array('C', c_r, 12)
  call end_test()

  ! Test 2: SIDE='L', UPLO='L', same symmetric matrix stored in lower triangle
  a = (0.0d0, 0.0d0)
  a(1) = (2.0d0, 1.0d0)    ! A(1,1)
  a(2) = (1.0d0, 1.0d0)    ! A(2,1) = A(1,2) (symmetric, same value, NO conjugate)
  a(3) = (3.0d0, -2.0d0)   ! A(3,1) = A(1,3)
  a(5) = (4.0d0, -1.0d0)   ! A(2,2)
  a(6) = (2.0d0, 1.0d0)    ! A(3,2) = A(2,3)
  a(9) = (5.0d0, 2.0d0)    ! A(3,3)

  c = (0.0d0, 0.0d0)
  call zsymm('L', 'L', 3, 2, (1.0d0,0.0d0), a, 3, b, 3, (0.0d0,0.0d0), c, 3)
  call begin_test('left_lower_basic')
  call print_array('C', c_r, 12)
  call end_test()

  ! Test 3: SIDE='R', UPLO='U', M=2, N=3
  a = (0.0d0, 0.0d0)
  a(1) = (2.0d0, 1.0d0); a(4) = (1.0d0, 1.0d0); a(5) = (4.0d0, -1.0d0)
  a(7) = (3.0d0, -2.0d0); a(8) = (2.0d0, 1.0d0); a(9) = (5.0d0, 2.0d0)

  b = (0.0d0, 0.0d0)
  b(1) = (1.0d0, 0.5d0); b(2) = (2.0d0, -1.0d0)
  b(3) = (3.0d0, 1.0d0); b(4) = (4.0d0, 2.0d0)
  b(5) = (5.0d0, 0.0d0); b(6) = (6.0d0, -0.5d0)

  c = (0.0d0, 0.0d0)
  call zsymm('R', 'U', 2, 3, (1.0d0,0.0d0), a, 3, b, 2, (0.0d0,0.0d0), c, 2)
  call begin_test('right_upper_basic')
  call print_array('C', c_r, 12)
  call end_test()

  ! Test 4: SIDE='R', UPLO='L'
  a = (0.0d0, 0.0d0)
  a(1) = (2.0d0, 1.0d0); a(2) = (1.0d0, 1.0d0); a(3) = (3.0d0, -2.0d0)
  a(5) = (4.0d0, -1.0d0); a(6) = (2.0d0, 1.0d0); a(9) = (5.0d0, 2.0d0)

  c = (0.0d0, 0.0d0)
  call zsymm('R', 'L', 2, 3, (1.0d0,0.0d0), a, 3, b, 2, (0.0d0,0.0d0), c, 2)
  call begin_test('right_lower_basic')
  call print_array('C', c_r, 12)
  call end_test()

  ! Test 5: complex alpha=(2,1), complex beta=(0.5,-0.5)
  a = (0.0d0, 0.0d0)
  a(1) = (2.0d0, 1.0d0); a(4) = (1.0d0, 1.0d0); a(5) = (4.0d0, -1.0d0)
  a(7) = (3.0d0, -2.0d0); a(8) = (2.0d0, 1.0d0); a(9) = (5.0d0, 2.0d0)

  b = (0.0d0, 0.0d0)
  b(1) = (1.0d0, 0.5d0); b(2) = (2.0d0, -1.0d0); b(3) = (3.0d0, 1.0d0)
  b(4) = (4.0d0, 2.0d0); b(5) = (5.0d0, 0.0d0);  b(6) = (6.0d0, -0.5d0)

  c = (0.0d0, 0.0d0)
  c(1) = (1.0d0, 1.0d0); c(2) = (2.0d0, -1.0d0); c(3) = (0.5d0, 0.5d0)
  c(4) = (1.0d0, 0.0d0); c(5) = (0.0d0, 2.0d0);  c(6) = (3.0d0, -1.0d0)

  call zsymm('L', 'U', 3, 2, (2.0d0,1.0d0), a, 3, b, 3, (0.5d0,-0.5d0), c, 3)
  call begin_test('complex_alpha_beta')
  call print_array('C', c_r, 12)
  call end_test()

  ! Test 6: alpha=0, beta=(2,0) — just scale C
  c = (0.0d0, 0.0d0)
  c(1) = (1.0d0, 2.0d0); c(2) = (3.0d0, 4.0d0)
  c(3) = (5.0d0, 6.0d0); c(4) = (7.0d0, 8.0d0)

  call zsymm('L', 'U', 2, 2, (0.0d0,0.0d0), a, 3, b, 2, (2.0d0,0.0d0), c, 2)
  call begin_test('alpha_zero')
  call print_array('C', c_r, 8)
  call end_test()

  ! Test 7: M=0 quick return
  c(1) = (99.0d0, 0.0d0)
  call zsymm('L', 'U', 0, 2, (1.0d0,0.0d0), a, 1, b, 1, (0.0d0,0.0d0), c, 1)
  call begin_test('m_zero')
  call print_array('C', c_r, 2)
  call end_test()

  ! Test 8: N=0 quick return
  c(1) = (99.0d0, 0.0d0)
  call zsymm('L', 'U', 2, 0, (1.0d0,0.0d0), a, 2, b, 2, (0.0d0,0.0d0), c, 2)
  call begin_test('n_zero')
  call print_array('C', c_r, 2)
  call end_test()

  ! Test 9: 1x1 scalar case
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0); c = (0.0d0, 0.0d0)
  a(1) = (3.0d0, 1.0d0)
  b(1) = (5.0d0, 2.0d0)
  call zsymm('L', 'U', 1, 1, (2.0d0,1.0d0), a, 1, b, 1, (0.0d0,0.0d0), c, 1)
  call begin_test('scalar')
  call print_array('C', c_r, 2)
  call end_test()

  ! Test 10: beta=0 overwrites C
  c = (0.0d0, 0.0d0)
  c(1) = (999.0d0, 999.0d0); c(2) = (999.0d0, 999.0d0)
  c(3) = (999.0d0, 999.0d0); c(4) = (999.0d0, 999.0d0)
  a = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 0.5d0); a(3) = (0.0d0, 0.0d0)
  a(4) = (1.0d0, -0.5d0)
  b = (0.0d0, 0.0d0)
  b(1) = (2.0d0, 1.0d0); b(2) = (3.0d0, -1.0d0)
  b(3) = (4.0d0, 0.5d0); b(4) = (5.0d0, 2.0d0)
  call zsymm('L', 'L', 2, 2, (1.0d0,0.0d0), a, 2, b, 2, (0.0d0,0.0d0), c, 2)
  call begin_test('beta_zero')
  call print_array('C', c_r, 8)
  call end_test()

  ! Test 11: alpha=0, beta=0 — zero out C
  c = (0.0d0, 0.0d0)
  c(1) = (99.0d0, 88.0d0); c(2) = (77.0d0, 66.0d0)
  c(3) = (55.0d0, 44.0d0); c(4) = (33.0d0, 22.0d0)
  call zsymm('L', 'U', 2, 2, (0.0d0,0.0d0), a, 2, b, 2, (0.0d0,0.0d0), c, 2)
  call begin_test('alpha_zero_beta_zero')
  call print_array('C', c_r, 8)
  call end_test()

  ! Test 12: SIDE='L', UPLO='L', nonzero beta
  a = (0.0d0, 0.0d0)
  a(1) = (2.0d0, 1.0d0)
  a(2) = (1.0d0, 1.0d0)
  a(3) = (3.0d0, -2.0d0)
  a(5) = (4.0d0, -1.0d0)
  a(6) = (2.0d0, 1.0d0)
  a(9) = (5.0d0, 2.0d0)

  b = (0.0d0, 0.0d0)
  b(1) = (1.0d0, 0.5d0); b(2) = (2.0d0, -1.0d0); b(3) = (3.0d0, 1.0d0)
  b(4) = (4.0d0, 2.0d0); b(5) = (5.0d0, 0.0d0);  b(6) = (6.0d0, -0.5d0)

  c = (0.0d0, 0.0d0)
  c(1) = (1.0d0, 1.0d0); c(2) = (2.0d0, -1.0d0); c(3) = (0.5d0, 0.5d0)
  c(4) = (1.0d0, 0.0d0); c(5) = (0.0d0, 2.0d0);  c(6) = (3.0d0, -1.0d0)
  call zsymm('L', 'L', 3, 2, (1.0d0,0.0d0), a, 3, b, 3, (0.5d0,0.0d0), c, 3)
  call begin_test('left_lower_nonzero_beta')
  call print_array('C', c_r, 12)
  call end_test()

  ! Test 13: SIDE='R', UPLO='U', nonzero beta
  a = (0.0d0, 0.0d0)
  a(1) = (2.0d0, 1.0d0); a(4) = (1.0d0, 1.0d0); a(5) = (4.0d0, -1.0d0)
  a(7) = (3.0d0, -2.0d0); a(8) = (2.0d0, 1.0d0); a(9) = (5.0d0, 2.0d0)

  b = (0.0d0, 0.0d0)
  b(1) = (1.0d0, 0.5d0); b(2) = (2.0d0, -1.0d0)
  b(3) = (3.0d0, 1.0d0); b(4) = (4.0d0, 2.0d0)
  b(5) = (5.0d0, 0.0d0); b(6) = (6.0d0, -0.5d0)

  c = (0.0d0, 0.0d0)
  c(1) = (1.0d0, 1.0d0); c(2) = (2.0d0, -1.0d0)
  c(3) = (0.5d0, 0.5d0); c(4) = (1.0d0, 0.0d0)
  c(5) = (0.0d0, 2.0d0); c(6) = (3.0d0, -1.0d0)
  call zsymm('R', 'U', 2, 3, (1.0d0,0.0d0), a, 3, b, 2, (0.5d0,0.5d0), c, 2)
  call begin_test('right_upper_nonzero_beta')
  call print_array('C', c_r, 12)
  call end_test()

end program
