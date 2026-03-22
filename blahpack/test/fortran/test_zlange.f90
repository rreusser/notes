program test_zlange
  use test_utils
  implicit none
  complex*16 :: a(4, 4)
  double precision :: work(10)
  double precision :: result
  double precision :: zlange
  external :: zlange
  integer :: m, n, lda

  lda = 4

  ! A = [ (1+2i) (5+6i) ]   abs: [ sqrt(5)  sqrt(61) ]
  !     [ (3+4i) (7+8i) ]        [ 5        sqrt(113) ]

  ! Test 1: max norm ('M')
  m = 2
  n = 2
  a = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 2.0d0)
  a(2,1) = (3.0d0, 4.0d0)
  a(1,2) = (5.0d0, 6.0d0)
  a(2,2) = (7.0d0, 8.0d0)
  result = zlange('M', m, n, a, lda, work)
  call begin_test('zlange_max')
  call print_scalar('result', result)
  call end_test()

  ! Test 2: one norm ('1')
  result = zlange('1', m, n, a, lda, work)
  call begin_test('zlange_one')
  call print_scalar('result', result)
  call end_test()

  ! Test 3: infinity norm ('I')
  result = zlange('I', m, n, a, lda, work)
  call begin_test('zlange_inf')
  call print_scalar('result', result)
  call end_test()

  ! Test 4: Frobenius norm ('F')
  result = zlange('F', m, n, a, lda, work)
  call begin_test('zlange_frob')
  call print_scalar('result', result)
  call end_test()

  ! Test 5: M=0 quick return
  result = zlange('M', 0, 2, a, lda, work)
  call begin_test('zlange_m_zero')
  call print_scalar('result', result)
  call end_test()

  ! Test 6: N=0 quick return
  result = zlange('M', 2, 0, a, lda, work)
  call begin_test('zlange_n_zero')
  call print_scalar('result', result)
  call end_test()

  ! Test 7: 1x1 matrix
  a = (0.0d0, 0.0d0)
  a(1,1) = (3.0d0, 4.0d0)
  result = zlange('F', 1, 1, a, lda, work)
  call begin_test('zlange_1x1')
  call print_scalar('result', result)
  call end_test()

end program
