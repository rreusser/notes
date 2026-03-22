program test_dlansy
  use test_utils
  implicit none
  double precision :: a(5, 5)
  double precision :: work(10)
  double precision :: result
  double precision :: dlansy
  external :: dlansy
  integer :: n, lda

  lda = 5

  ! 4x4 symmetric matrix (upper triangle stored):
  ! A = [  2.0   3.0  -1.0   4.0 ]
  !     [  3.0   5.0   2.0  -6.0 ]
  !     [ -1.0   2.0   7.0   1.0 ]
  !     [  4.0  -6.0   1.0   8.0 ]
  n = 4
  a = 0.0d0
  ! Fill upper triangle
  a(1,1) =  2.0d0
  a(1,2) =  3.0d0
  a(2,2) =  5.0d0
  a(1,3) = -1.0d0
  a(2,3) =  2.0d0
  a(3,3) =  7.0d0
  a(1,4) =  4.0d0
  a(2,4) = -6.0d0
  a(3,4) =  1.0d0
  a(4,4) =  8.0d0

  ! Test 1: max norm ('M'), uplo='U'
  result = dlansy('M', 'U', n, a, lda, work)
  call begin_test('dlansy_max_U')
  call print_scalar('result', result)
  call end_test()

  ! Test 2: one norm ('1'), uplo='U'
  result = dlansy('1', 'U', n, a, lda, work)
  call begin_test('dlansy_one_U')
  call print_scalar('result', result)
  call end_test()

  ! Test 3: one norm ('O'), uplo='U'
  result = dlansy('O', 'U', n, a, lda, work)
  call begin_test('dlansy_one_O_U')
  call print_scalar('result', result)
  call end_test()

  ! Test 4: infinity norm ('I'), uplo='U'
  ! For symmetric matrices, I-norm = 1-norm
  result = dlansy('I', 'U', n, a, lda, work)
  call begin_test('dlansy_inf_U')
  call print_scalar('result', result)
  call end_test()

  ! Test 5: Frobenius norm ('F'), uplo='U'
  result = dlansy('F', 'U', n, a, lda, work)
  call begin_test('dlansy_frob_U')
  call print_scalar('result', result)
  call end_test()

  ! Test 6: Frobenius norm ('E'), uplo='U'
  result = dlansy('E', 'U', n, a, lda, work)
  call begin_test('dlansy_frob_E_U')
  call print_scalar('result', result)
  call end_test()

  ! Now fill lower triangle for uplo='L' tests
  a = 0.0d0
  a(1,1) =  2.0d0
  a(2,1) =  3.0d0
  a(2,2) =  5.0d0
  a(3,1) = -1.0d0
  a(3,2) =  2.0d0
  a(3,3) =  7.0d0
  a(4,1) =  4.0d0
  a(4,2) = -6.0d0
  a(4,3) =  1.0d0
  a(4,4) =  8.0d0

  ! Test 7: max norm ('M'), uplo='L'
  result = dlansy('M', 'L', n, a, lda, work)
  call begin_test('dlansy_max_L')
  call print_scalar('result', result)
  call end_test()

  ! Test 8: one norm ('1'), uplo='L'
  result = dlansy('1', 'L', n, a, lda, work)
  call begin_test('dlansy_one_L')
  call print_scalar('result', result)
  call end_test()

  ! Test 9: infinity norm ('I'), uplo='L'
  result = dlansy('I', 'L', n, a, lda, work)
  call begin_test('dlansy_inf_L')
  call print_scalar('result', result)
  call end_test()

  ! Test 10: Frobenius norm ('F'), uplo='L'
  result = dlansy('F', 'L', n, a, lda, work)
  call begin_test('dlansy_frob_L')
  call print_scalar('result', result)
  call end_test()

  ! Test 11: N=0 quick return
  result = dlansy('M', 'U', 0, a, lda, work)
  call begin_test('dlansy_n_zero')
  call print_scalar('result', result)
  call end_test()

  ! Test 12: N=1 matrix, uplo='U'
  a = 0.0d0
  a(1,1) = -5.5d0
  result = dlansy('M', 'U', 1, a, lda, work)
  call begin_test('dlansy_1x1_max')
  call print_scalar('result', result)
  call end_test()

  result = dlansy('1', 'U', 1, a, lda, work)
  call begin_test('dlansy_1x1_one')
  call print_scalar('result', result)
  call end_test()

  result = dlansy('I', 'U', 1, a, lda, work)
  call begin_test('dlansy_1x1_inf')
  call print_scalar('result', result)
  call end_test()

  result = dlansy('F', 'U', 1, a, lda, work)
  call begin_test('dlansy_1x1_frob')
  call print_scalar('result', result)
  call end_test()

  ! Test 13: 5x5 symmetric with varied values, uplo='U'
  n = 5
  a = 0.0d0
  a(1,1) =  1.0d0
  a(1,2) =  2.0d0
  a(2,2) =  3.0d0
  a(1,3) =  4.0d0
  a(2,3) =  5.0d0
  a(3,3) =  6.0d0
  a(1,4) =  7.0d0
  a(2,4) =  8.0d0
  a(3,4) =  9.0d0
  a(4,4) = 10.0d0
  a(1,5) = 11.0d0
  a(2,5) = 12.0d0
  a(3,5) = 13.0d0
  a(4,5) = 14.0d0
  a(5,5) = 15.0d0

  result = dlansy('M', 'U', n, a, lda, work)
  call begin_test('dlansy_5x5_max_U')
  call print_scalar('result', result)
  call end_test()

  result = dlansy('1', 'U', n, a, lda, work)
  call begin_test('dlansy_5x5_one_U')
  call print_scalar('result', result)
  call end_test()

  result = dlansy('I', 'U', n, a, lda, work)
  call begin_test('dlansy_5x5_inf_U')
  call print_scalar('result', result)
  call end_test()

  result = dlansy('F', 'U', n, a, lda, work)
  call begin_test('dlansy_5x5_frob_U')
  call print_scalar('result', result)
  call end_test()

  ! Test 14: same 5x5 symmetric, stored in lower triangle
  a = 0.0d0
  a(1,1) =  1.0d0
  a(2,1) =  2.0d0
  a(2,2) =  3.0d0
  a(3,1) =  4.0d0
  a(3,2) =  5.0d0
  a(3,3) =  6.0d0
  a(4,1) =  7.0d0
  a(4,2) =  8.0d0
  a(4,3) =  9.0d0
  a(4,4) = 10.0d0
  a(5,1) = 11.0d0
  a(5,2) = 12.0d0
  a(5,3) = 13.0d0
  a(5,4) = 14.0d0
  a(5,5) = 15.0d0

  result = dlansy('M', 'L', n, a, lda, work)
  call begin_test('dlansy_5x5_max_L')
  call print_scalar('result', result)
  call end_test()

  result = dlansy('1', 'L', n, a, lda, work)
  call begin_test('dlansy_5x5_one_L')
  call print_scalar('result', result)
  call end_test()

  result = dlansy('I', 'L', n, a, lda, work)
  call begin_test('dlansy_5x5_inf_L')
  call print_scalar('result', result)
  call end_test()

  result = dlansy('F', 'L', n, a, lda, work)
  call begin_test('dlansy_5x5_frob_L')
  call print_scalar('result', result)
  call end_test()

end program
