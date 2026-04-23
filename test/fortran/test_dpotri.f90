program test_dpotri
  use test_utils
  implicit none
  double precision :: a(25), a_orig(25)
  integer :: info

  ! Test 1: UPLO='U', 3x3 SPD matrix
  ! A = [4 2 1; 2 5 3; 1 3 6] (diagonally dominant => SPD)
  ! First compute Cholesky factorization, then invert
  a = 0.0d0
  a(1) = 4.0d0; a(4) = 2.0d0; a(7) = 1.0d0
  a(2) = 2.0d0; a(5) = 5.0d0; a(8) = 3.0d0
  a(3) = 1.0d0; a(6) = 3.0d0; a(9) = 6.0d0
  ! Save original for verification
  a_orig(1:9) = a(1:9)
  call dpotrf('U', 3, a, 3, info)
  call begin_test('upper_3x3_chol')
  call print_int('info', info)
  call print_matrix('chol', a, 3, 3, 3)
  call end_test()

  ! Now call dpotri to get the inverse
  call dpotri('U', 3, a, 3, info)
  call begin_test('upper_3x3')
  call print_int('info', info)
  call print_matrix('a', a, 3, 3, 3)
  call end_test()

  ! Test 2: UPLO='L', 3x3 SPD matrix
  ! Same matrix A = [4 2 1; 2 5 3; 1 3 6]
  a = 0.0d0
  a(1) = 4.0d0; a(4) = 2.0d0; a(7) = 1.0d0
  a(2) = 2.0d0; a(5) = 5.0d0; a(8) = 3.0d0
  a(3) = 1.0d0; a(6) = 3.0d0; a(9) = 6.0d0
  call dpotrf('L', 3, a, 3, info)
  call begin_test('lower_3x3_chol')
  call print_int('info', info)
  call print_matrix('chol', a, 3, 3, 3)
  call end_test()

  call dpotri('L', 3, a, 3, info)
  call begin_test('lower_3x3')
  call print_int('info', info)
  call print_matrix('a', a, 3, 3, 3)
  call end_test()

  ! Test 3: N=1 edge case
  a(1) = 9.0d0
  call dpotrf('U', 1, a, 1, info)
  call dpotri('U', 1, a, 1, info)
  call begin_test('n_one')
  call print_int('info', info)
  call print_matrix('a', a, 1, 1, 1)
  call end_test()

  ! Test 4: N=0 quick return
  call dpotri('U', 0, a, 1, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 5: UPLO='U', 4x4 SPD matrix (exercises blocking if NB <= 4)
  ! A = [10 1 2 3; 1 10 1 2; 2 1 10 1; 3 2 1 10]
  a = 0.0d0
  a(1)  = 10.0d0; a(5)  = 1.0d0; a(9)  = 2.0d0; a(13) = 3.0d0
  a(2)  = 1.0d0;  a(6)  = 10.0d0; a(10) = 1.0d0; a(14) = 2.0d0
  a(3)  = 2.0d0;  a(7)  = 1.0d0;  a(11) = 10.0d0; a(15) = 1.0d0
  a(4)  = 3.0d0;  a(8)  = 2.0d0;  a(12) = 1.0d0;  a(16) = 10.0d0
  call dpotrf('U', 4, a, 4, info)
  call dpotri('U', 4, a, 4, info)
  call begin_test('upper_4x4')
  call print_int('info', info)
  call print_matrix('a', a, 4, 4, 4)
  call end_test()

  ! Test 6: UPLO='L', 4x4 SPD matrix
  a = 0.0d0
  a(1)  = 10.0d0; a(5)  = 1.0d0; a(9)  = 2.0d0; a(13) = 3.0d0
  a(2)  = 1.0d0;  a(6)  = 10.0d0; a(10) = 1.0d0; a(14) = 2.0d0
  a(3)  = 2.0d0;  a(7)  = 1.0d0;  a(11) = 10.0d0; a(15) = 1.0d0
  a(4)  = 3.0d0;  a(8)  = 2.0d0;  a(12) = 1.0d0;  a(16) = 10.0d0
  call dpotrf('L', 4, a, 4, info)
  call dpotri('L', 4, a, 4, info)
  call begin_test('lower_4x4')
  call print_int('info', info)
  call print_matrix('a', a, 4, 4, 4)
  call end_test()

end program
