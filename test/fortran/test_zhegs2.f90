program test_zhegs2
  use test_utils
  implicit none
  complex*16 :: a(16), b(16)
  double precision :: a_r(32), b_r(32)
  equivalence (a, a_r)
  equivalence (b, b_r)
  integer :: info

  ! Use a 3x3 Hermitian positive definite B for Cholesky:
  ! B = [4 1+i 0; 1-i 5 2-i; 0 2+i 6]
  ! A = [10 2+i 1-2i; 2-i 8 3+i; 1+2i 3-i 7] Hermitian

  ! Test 1: ITYPE=1, UPLO='U', N=3
  b = (0.0d0, 0.0d0)
  b(1) = (4.0d0, 0.0d0)
  b(2) = (1.0d0, -1.0d0)
  b(4) = (1.0d0, 1.0d0)
  b(5) = (5.0d0, 0.0d0)
  b(6) = (2.0d0, 1.0d0)
  b(8) = (2.0d0, -1.0d0)
  b(9) = (6.0d0, 0.0d0)
  call zpotrf('U', 3, b, 3, info)

  a = (0.0d0, 0.0d0)
  a(1) = (10.0d0, 0.0d0)
  a(4) = (2.0d0, 1.0d0)
  a(5) = (8.0d0, 0.0d0)
  a(7) = (1.0d0, -2.0d0)
  a(8) = (3.0d0, 1.0d0)
  a(9) = (7.0d0, 0.0d0)

  call zhegs2(1, 'U', 3, a, 3, b, 3, info)
  call begin_test('itype1_upper')
  call print_int('info', info)
  call print_array('A', a_r, 18)
  call end_test()

  ! Test 2: ITYPE=1, UPLO='L', N=3
  b = (0.0d0, 0.0d0)
  b(1) = (4.0d0, 0.0d0)
  b(2) = (1.0d0, -1.0d0)
  b(4) = (1.0d0, 1.0d0)
  b(5) = (5.0d0, 0.0d0)
  b(6) = (2.0d0, 1.0d0)
  b(8) = (2.0d0, -1.0d0)
  b(9) = (6.0d0, 0.0d0)
  call zpotrf('L', 3, b, 3, info)

  a = (0.0d0, 0.0d0)
  a(1) = (10.0d0, 0.0d0)
  a(2) = (2.0d0, -1.0d0)
  a(3) = (1.0d0, 2.0d0)
  a(5) = (8.0d0, 0.0d0)
  a(6) = (3.0d0, -1.0d0)
  a(9) = (7.0d0, 0.0d0)

  call zhegs2(1, 'L', 3, a, 3, b, 3, info)
  call begin_test('itype1_lower')
  call print_int('info', info)
  call print_array('A', a_r, 18)
  call end_test()

  ! Test 3: ITYPE=2, UPLO='U', N=3
  b = (0.0d0, 0.0d0)
  b(1) = (4.0d0, 0.0d0)
  b(2) = (1.0d0, -1.0d0)
  b(4) = (1.0d0, 1.0d0)
  b(5) = (5.0d0, 0.0d0)
  b(6) = (2.0d0, 1.0d0)
  b(8) = (2.0d0, -1.0d0)
  b(9) = (6.0d0, 0.0d0)
  call zpotrf('U', 3, b, 3, info)

  a = (0.0d0, 0.0d0)
  a(1) = (10.0d0, 0.0d0)
  a(4) = (2.0d0, 1.0d0)
  a(5) = (8.0d0, 0.0d0)
  a(7) = (1.0d0, -2.0d0)
  a(8) = (3.0d0, 1.0d0)
  a(9) = (7.0d0, 0.0d0)

  call zhegs2(2, 'U', 3, a, 3, b, 3, info)
  call begin_test('itype2_upper')
  call print_int('info', info)
  call print_array('A', a_r, 18)
  call end_test()

  ! Test 4: ITYPE=2, UPLO='L', N=3
  b = (0.0d0, 0.0d0)
  b(1) = (4.0d0, 0.0d0)
  b(2) = (1.0d0, -1.0d0)
  b(4) = (1.0d0, 1.0d0)
  b(5) = (5.0d0, 0.0d0)
  b(6) = (2.0d0, 1.0d0)
  b(8) = (2.0d0, -1.0d0)
  b(9) = (6.0d0, 0.0d0)
  call zpotrf('L', 3, b, 3, info)

  a = (0.0d0, 0.0d0)
  a(1) = (10.0d0, 0.0d0)
  a(2) = (2.0d0, -1.0d0)
  a(3) = (1.0d0, 2.0d0)
  a(5) = (8.0d0, 0.0d0)
  a(6) = (3.0d0, -1.0d0)
  a(9) = (7.0d0, 0.0d0)

  call zhegs2(2, 'L', 3, a, 3, b, 3, info)
  call begin_test('itype2_lower')
  call print_int('info', info)
  call print_array('A', a_r, 18)
  call end_test()

  ! Test 5: ITYPE=3, UPLO='U', N=3
  b = (0.0d0, 0.0d0)
  b(1) = (4.0d0, 0.0d0)
  b(2) = (1.0d0, -1.0d0)
  b(4) = (1.0d0, 1.0d0)
  b(5) = (5.0d0, 0.0d0)
  b(6) = (2.0d0, 1.0d0)
  b(8) = (2.0d0, -1.0d0)
  b(9) = (6.0d0, 0.0d0)
  call zpotrf('U', 3, b, 3, info)

  a = (0.0d0, 0.0d0)
  a(1) = (10.0d0, 0.0d0)
  a(4) = (2.0d0, 1.0d0)
  a(5) = (8.0d0, 0.0d0)
  a(7) = (1.0d0, -2.0d0)
  a(8) = (3.0d0, 1.0d0)
  a(9) = (7.0d0, 0.0d0)

  call zhegs2(3, 'U', 3, a, 3, b, 3, info)
  call begin_test('itype3_upper')
  call print_int('info', info)
  call print_array('A', a_r, 18)
  call end_test()

  ! Test 6: ITYPE=3, UPLO='L', N=3
  b = (0.0d0, 0.0d0)
  b(1) = (4.0d0, 0.0d0)
  b(2) = (1.0d0, -1.0d0)
  b(4) = (1.0d0, 1.0d0)
  b(5) = (5.0d0, 0.0d0)
  b(6) = (2.0d0, 1.0d0)
  b(8) = (2.0d0, -1.0d0)
  b(9) = (6.0d0, 0.0d0)
  call zpotrf('L', 3, b, 3, info)

  a = (0.0d0, 0.0d0)
  a(1) = (10.0d0, 0.0d0)
  a(2) = (2.0d0, -1.0d0)
  a(3) = (1.0d0, 2.0d0)
  a(5) = (8.0d0, 0.0d0)
  a(6) = (3.0d0, -1.0d0)
  a(9) = (7.0d0, 0.0d0)

  call zhegs2(3, 'L', 3, a, 3, b, 3, info)
  call begin_test('itype3_lower')
  call print_int('info', info)
  call print_array('A', a_r, 18)
  call end_test()

  ! Test 7: N=0 quick return
  call zhegs2(1, 'U', 0, a, 1, b, 1, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 8: N=1
  a(1) = (9.0d0, 0.0d0)
  b(1) = (3.0d0, 0.0d0)
  call zhegs2(1, 'U', 1, a, 1, b, 1, info)
  call begin_test('n_one')
  call print_int('info', info)
  call print_array('A', a_r, 2)
  call end_test()

end program
