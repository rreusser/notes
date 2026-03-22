program test_zlanhe
  use test_utils
  implicit none
  complex*16 :: a(5, 5)
  double precision :: work(10)
  double precision :: result
  double precision :: zlanhe
  external :: zlanhe
  integer :: n, lda

  lda = 5

  ! 4x4 Hermitian matrix (upper triangle stored):
  ! A = [  2.0+0i    3.0+1i  -1.0+2i   4.0-3i ]
  !     [  3.0-1i    5.0+0i   2.0+0.5i -6.0+1i ]
  !     [ -1.0-2i    2.0-0.5i 7.0+0i    1.0+4i ]
  !     [  4.0+3i   -6.0-1i   1.0-4i   8.0+0i ]
  ! Note: diagonal elements are real (imaginary part = 0)
  n = 4
  a = (0.0d0, 0.0d0)
  ! Fill upper triangle
  a(1,1) = (2.0d0, 0.0d0)
  a(1,2) = (3.0d0, 1.0d0)
  a(2,2) = (5.0d0, 0.0d0)
  a(1,3) = (-1.0d0, 2.0d0)
  a(2,3) = (2.0d0, 0.5d0)
  a(3,3) = (7.0d0, 0.0d0)
  a(1,4) = (4.0d0, -3.0d0)
  a(2,4) = (-6.0d0, 1.0d0)
  a(3,4) = (1.0d0, 4.0d0)
  a(4,4) = (8.0d0, 0.0d0)

  ! Test 1: max norm ('M'), uplo='U'
  result = zlanhe('M', 'U', n, a, lda, work)
  call begin_test('zlanhe_max_U')
  call print_scalar('result', result)
  call end_test()

  ! Test 2: one norm ('1'), uplo='U'
  result = zlanhe('1', 'U', n, a, lda, work)
  call begin_test('zlanhe_one_U')
  call print_scalar('result', result)
  call end_test()

  ! Test 3: one norm ('O'), uplo='U'
  result = zlanhe('O', 'U', n, a, lda, work)
  call begin_test('zlanhe_one_O_U')
  call print_scalar('result', result)
  call end_test()

  ! Test 4: infinity norm ('I'), uplo='U'
  result = zlanhe('I', 'U', n, a, lda, work)
  call begin_test('zlanhe_inf_U')
  call print_scalar('result', result)
  call end_test()

  ! Test 5: Frobenius norm ('F'), uplo='U'
  result = zlanhe('F', 'U', n, a, lda, work)
  call begin_test('zlanhe_frob_U')
  call print_scalar('result', result)
  call end_test()

  ! Test 6: Frobenius norm ('E'), uplo='U'
  result = zlanhe('E', 'U', n, a, lda, work)
  call begin_test('zlanhe_frob_E_U')
  call print_scalar('result', result)
  call end_test()

  ! Now fill lower triangle for uplo='L' tests
  a = (0.0d0, 0.0d0)
  ! For Hermitian with lower triangle, A(i,j) = conj(A(j,i))
  ! So the lower triangle of the same matrix:
  a(1,1) = (2.0d0, 0.0d0)
  a(2,1) = (3.0d0, -1.0d0)   ! conj of a(1,2) = conj(3+i) = 3-i
  a(2,2) = (5.0d0, 0.0d0)
  a(3,1) = (-1.0d0, -2.0d0)  ! conj of a(1,3) = conj(-1+2i) = -1-2i
  a(3,2) = (2.0d0, -0.5d0)   ! conj of a(2,3) = conj(2+0.5i) = 2-0.5i
  a(3,3) = (7.0d0, 0.0d0)
  a(4,1) = (4.0d0, 3.0d0)    ! conj of a(1,4) = conj(4-3i) = 4+3i
  a(4,2) = (-6.0d0, -1.0d0)  ! conj of a(2,4) = conj(-6+i) = -6-i
  a(4,3) = (1.0d0, -4.0d0)   ! conj of a(3,4) = conj(1+4i) = 1-4i
  a(4,4) = (8.0d0, 0.0d0)

  ! Test 7: max norm ('M'), uplo='L'
  result = zlanhe('M', 'L', n, a, lda, work)
  call begin_test('zlanhe_max_L')
  call print_scalar('result', result)
  call end_test()

  ! Test 8: one norm ('1'), uplo='L'
  result = zlanhe('1', 'L', n, a, lda, work)
  call begin_test('zlanhe_one_L')
  call print_scalar('result', result)
  call end_test()

  ! Test 9: infinity norm ('I'), uplo='L'
  result = zlanhe('I', 'L', n, a, lda, work)
  call begin_test('zlanhe_inf_L')
  call print_scalar('result', result)
  call end_test()

  ! Test 10: Frobenius norm ('F'), uplo='L'
  result = zlanhe('F', 'L', n, a, lda, work)
  call begin_test('zlanhe_frob_L')
  call print_scalar('result', result)
  call end_test()

  ! Test 11: N=0 quick return
  result = zlanhe('M', 'U', 0, a, lda, work)
  call begin_test('zlanhe_n_zero')
  call print_scalar('result', result)
  call end_test()

  ! Test 12: N=1 matrix, uplo='U'
  a = (0.0d0, 0.0d0)
  a(1,1) = (-5.5d0, 0.0d0)
  result = zlanhe('M', 'U', 1, a, lda, work)
  call begin_test('zlanhe_1x1_max')
  call print_scalar('result', result)
  call end_test()

  result = zlanhe('1', 'U', 1, a, lda, work)
  call begin_test('zlanhe_1x1_one')
  call print_scalar('result', result)
  call end_test()

  result = zlanhe('I', 'U', 1, a, lda, work)
  call begin_test('zlanhe_1x1_inf')
  call print_scalar('result', result)
  call end_test()

  result = zlanhe('F', 'U', 1, a, lda, work)
  call begin_test('zlanhe_1x1_frob')
  call print_scalar('result', result)
  call end_test()

  ! Test 13: 3x3 Hermitian, purely real diagonal, complex off-diagonal, uplo='U'
  n = 3
  a = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 0.0d0)
  a(1,2) = (2.0d0, 3.0d0)
  a(2,2) = (4.0d0, 0.0d0)
  a(1,3) = (5.0d0, -1.0d0)
  a(2,3) = (0.0d0, 6.0d0)
  a(3,3) = (9.0d0, 0.0d0)

  result = zlanhe('M', 'U', n, a, lda, work)
  call begin_test('zlanhe_3x3_max_U')
  call print_scalar('result', result)
  call end_test()

  result = zlanhe('1', 'U', n, a, lda, work)
  call begin_test('zlanhe_3x3_one_U')
  call print_scalar('result', result)
  call end_test()

  result = zlanhe('I', 'U', n, a, lda, work)
  call begin_test('zlanhe_3x3_inf_U')
  call print_scalar('result', result)
  call end_test()

  result = zlanhe('F', 'U', n, a, lda, work)
  call begin_test('zlanhe_3x3_frob_U')
  call print_scalar('result', result)
  call end_test()

  ! Test 14: Same 3x3 matrix, lower triangle
  a = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 0.0d0)
  a(2,1) = (2.0d0, -3.0d0)
  a(2,2) = (4.0d0, 0.0d0)
  a(3,1) = (5.0d0, 1.0d0)
  a(3,2) = (0.0d0, -6.0d0)
  a(3,3) = (9.0d0, 0.0d0)

  result = zlanhe('M', 'L', n, a, lda, work)
  call begin_test('zlanhe_3x3_max_L')
  call print_scalar('result', result)
  call end_test()

  result = zlanhe('1', 'L', n, a, lda, work)
  call begin_test('zlanhe_3x3_one_L')
  call print_scalar('result', result)
  call end_test()

  result = zlanhe('I', 'L', n, a, lda, work)
  call begin_test('zlanhe_3x3_inf_L')
  call print_scalar('result', result)
  call end_test()

  result = zlanhe('F', 'L', n, a, lda, work)
  call begin_test('zlanhe_3x3_frob_L')
  call print_scalar('result', result)
  call end_test()

end program
