program test_dsygs2
  use test_utils
  implicit none
  double precision :: a(16), b(16)
  integer :: info

  ! Test 1: ITYPE=1, UPLO='U', N=3
  ! A = [4 2 1; 2 5 3; 1 3 6] symmetric, upper stored
  ! B = Cholesky factor of [4 2 0; 2 5 1; 0 1 3] => U^T*U
  ! First compute B's Cholesky factor:
  b = 0.0d0
  b(1) = 4.0d0; b(2) = 2.0d0; b(4) = 2.0d0; b(5) = 5.0d0
  b(6) = 1.0d0; b(8) = 1.0d0; b(9) = 3.0d0
  call dpotrf('U', 3, b, 3, info)

  a = 0.0d0
  a(1) = 4.0d0; a(4) = 2.0d0; a(5) = 5.0d0
  a(7) = 1.0d0; a(8) = 3.0d0; a(9) = 6.0d0
  call dsygs2(1, 'U', 3, a, 3, b, 3, info)
  call begin_test('itype1_upper')
  call print_int('info', info)
  call print_matrix('A', a, 3, 3, 3)
  call end_test()

  ! Test 2: ITYPE=1, UPLO='L', N=3
  b = 0.0d0
  b(1) = 4.0d0; b(2) = 2.0d0; b(4) = 2.0d0; b(5) = 5.0d0
  b(6) = 1.0d0; b(8) = 1.0d0; b(9) = 3.0d0
  call dpotrf('L', 3, b, 3, info)

  a = 0.0d0
  a(1) = 4.0d0; a(2) = 2.0d0; a(3) = 1.0d0
  a(5) = 5.0d0; a(6) = 3.0d0; a(9) = 6.0d0
  call dsygs2(1, 'L', 3, a, 3, b, 3, info)
  call begin_test('itype1_lower')
  call print_int('info', info)
  call print_matrix('A', a, 3, 3, 3)
  call end_test()

  ! Test 3: ITYPE=2, UPLO='U', N=3
  b = 0.0d0
  b(1) = 4.0d0; b(2) = 2.0d0; b(4) = 2.0d0; b(5) = 5.0d0
  b(6) = 1.0d0; b(8) = 1.0d0; b(9) = 3.0d0
  call dpotrf('U', 3, b, 3, info)

  a = 0.0d0
  a(1) = 4.0d0; a(4) = 2.0d0; a(5) = 5.0d0
  a(7) = 1.0d0; a(8) = 3.0d0; a(9) = 6.0d0
  call dsygs2(2, 'U', 3, a, 3, b, 3, info)
  call begin_test('itype2_upper')
  call print_int('info', info)
  call print_matrix('A', a, 3, 3, 3)
  call end_test()

  ! Test 4: ITYPE=2, UPLO='L', N=3
  b = 0.0d0
  b(1) = 4.0d0; b(2) = 2.0d0; b(4) = 2.0d0; b(5) = 5.0d0
  b(6) = 1.0d0; b(8) = 1.0d0; b(9) = 3.0d0
  call dpotrf('L', 3, b, 3, info)

  a = 0.0d0
  a(1) = 4.0d0; a(2) = 2.0d0; a(3) = 1.0d0
  a(5) = 5.0d0; a(6) = 3.0d0; a(9) = 6.0d0
  call dsygs2(2, 'L', 3, a, 3, b, 3, info)
  call begin_test('itype2_lower')
  call print_int('info', info)
  call print_matrix('A', a, 3, 3, 3)
  call end_test()

  ! Test 5: ITYPE=3, UPLO='U', N=3
  b = 0.0d0
  b(1) = 4.0d0; b(2) = 2.0d0; b(4) = 2.0d0; b(5) = 5.0d0
  b(6) = 1.0d0; b(8) = 1.0d0; b(9) = 3.0d0
  call dpotrf('U', 3, b, 3, info)

  a = 0.0d0
  a(1) = 4.0d0; a(4) = 2.0d0; a(5) = 5.0d0
  a(7) = 1.0d0; a(8) = 3.0d0; a(9) = 6.0d0
  call dsygs2(3, 'U', 3, a, 3, b, 3, info)
  call begin_test('itype3_upper')
  call print_int('info', info)
  call print_matrix('A', a, 3, 3, 3)
  call end_test()

  ! Test 6: N=0 quick return
  call dsygs2(1, 'U', 0, a, 1, b, 1, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 7: N=1
  a(1) = 9.0d0
  b(1) = 3.0d0
  call dsygs2(1, 'U', 1, a, 1, b, 1, info)
  call begin_test('n_one')
  call print_int('info', info)
  call print_scalar('A11', a(1))
  call end_test()

end program
