program test_dlange
  use test_utils
  implicit none
  double precision :: a(4, 5)
  double precision :: work(10)
  double precision :: result
  double precision :: dlange
  external :: dlange
  integer :: m, n, lda

  lda = 4

  ! 3x4 matrix with mixed positive/negative values:
  ! A = [  1.0  -4.0   7.0  -2.0 ]
  !     [ -3.0   5.0  -8.0   6.0 ]
  !     [  2.0  -1.0   9.0  -3.0 ]
  m = 3
  n = 4
  a = 0.0d0
  a(1,1) =  1.0d0
  a(2,1) = -3.0d0
  a(3,1) =  2.0d0
  a(1,2) = -4.0d0
  a(2,2) =  5.0d0
  a(3,2) = -1.0d0
  a(1,3) =  7.0d0
  a(2,3) = -8.0d0
  a(3,3) =  9.0d0
  a(1,4) = -2.0d0
  a(2,4) =  6.0d0
  a(3,4) = -3.0d0

  ! Test 1: max norm ('M')
  ! max(|a_ij|) = 9.0
  result = dlange('M', m, n, a, lda, work)
  call begin_test('dlange_max')
  call print_scalar('result', result)
  call end_test()

  ! Test 2: one norm ('1')
  ! Column sums: |1|+|3|+|2|=6, |4|+|5|+|1|=10, |7|+|8|+|9|=24, |2|+|6|+|3|=11
  ! max = 24
  result = dlange('1', m, n, a, lda, work)
  call begin_test('dlange_one')
  call print_scalar('result', result)
  call end_test()

  ! Test 3: one norm with 'O'
  result = dlange('O', m, n, a, lda, work)
  call begin_test('dlange_one_O')
  call print_scalar('result', result)
  call end_test()

  ! Test 4: infinity norm ('I')
  ! Row sums: |1|+|4|+|7|+|2|=14, |3|+|5|+|8|+|6|=22, |2|+|1|+|9|+|3|=15
  ! max = 22
  result = dlange('I', m, n, a, lda, work)
  call begin_test('dlange_inf')
  call print_scalar('result', result)
  call end_test()

  ! Test 5: Frobenius norm ('F')
  ! sqrt(1+9+4+16+25+1+49+64+81+4+36+9) = sqrt(299)
  result = dlange('F', m, n, a, lda, work)
  call begin_test('dlange_frob')
  call print_scalar('result', result)
  call end_test()

  ! Test 6: Frobenius norm with 'E'
  result = dlange('E', m, n, a, lda, work)
  call begin_test('dlange_frob_E')
  call print_scalar('result', result)
  call end_test()

  ! Test 7: M=0 quick return
  result = dlange('M', 0, 4, a, lda, work)
  call begin_test('dlange_m_zero')
  call print_scalar('result', result)
  call end_test()

  ! Test 8: N=0 quick return
  result = dlange('1', 3, 0, a, lda, work)
  call begin_test('dlange_n_zero')
  call print_scalar('result', result)
  call end_test()

  ! Test 9: 1x1 matrix
  a = 0.0d0
  a(1,1) = -5.5d0
  result = dlange('M', 1, 1, a, lda, work)
  call begin_test('dlange_1x1_max')
  call print_scalar('result', result)
  call end_test()

  result = dlange('F', 1, 1, a, lda, work)
  call begin_test('dlange_1x1_frob')
  call print_scalar('result', result)
  call end_test()

  result = dlange('1', 1, 1, a, lda, work)
  call begin_test('dlange_1x1_one')
  call print_scalar('result', result)
  call end_test()

  result = dlange('I', 1, 1, a, lda, work)
  call begin_test('dlange_1x1_inf')
  call print_scalar('result', result)
  call end_test()

  ! Test 10: larger 4x5 matrix to exercise more paths
  a = 0.0d0
  a(1,1) =  2.0d0
  a(2,1) = -1.0d0
  a(3,1) =  0.0d0
  a(4,1) =  3.0d0
  a(1,2) =  4.0d0
  a(2,2) = -6.0d0
  a(3,2) =  1.0d0
  a(4,2) =  0.5d0
  a(1,3) = -7.0d0
  a(2,3) =  2.0d0
  a(3,3) =  8.0d0
  a(4,3) = -4.0d0
  a(1,4) =  1.0d0
  a(2,4) =  0.0d0
  a(3,4) = -3.0d0
  a(4,4) =  5.0d0
  a(1,5) =  0.0d0
  a(2,5) =  9.0d0
  a(3,5) = -2.0d0
  a(4,5) =  1.0d0

  m = 4
  n = 5

  result = dlange('M', m, n, a, lda, work)
  call begin_test('dlange_4x5_max')
  call print_scalar('result', result)
  call end_test()

  result = dlange('1', m, n, a, lda, work)
  call begin_test('dlange_4x5_one')
  call print_scalar('result', result)
  call end_test()

  result = dlange('I', m, n, a, lda, work)
  call begin_test('dlange_4x5_inf')
  call print_scalar('result', result)
  call end_test()

  result = dlange('F', m, n, a, lda, work)
  call begin_test('dlange_4x5_frob')
  call print_scalar('result', result)
  call end_test()

end program
