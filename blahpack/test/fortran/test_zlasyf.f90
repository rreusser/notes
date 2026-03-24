program test_zlasyf
  use test_utils
  implicit none

  integer, parameter :: MAXN = 10
  double precision :: A_r(2*MAXN*MAXN), W_r(2*MAXN*MAXN)
  complex*16 :: A(MAXN,MAXN), W(MAXN,MAXN)
  equivalence (A, A_r)
  equivalence (W, W_r)
  integer :: ipiv(MAXN), info, kb, n, nb, lda, ldw

  ! ZLASYF: partial factorization of complex symmetric matrix
  ! using Bunch-Kaufman diagonal pivoting

  ! Test 1: 4x4 upper, nb=4
  n = 4
  nb = 4
  lda = MAXN
  ldw = MAXN
  A = (0.0d0, 0.0d0)
  ! Symmetric matrix (not Hermitian): A(i,j) = A(j,i)
  A(1,1) = (4.0d0, 1.0d0)
  A(1,2) = (1.0d0, 2.0d0); A(2,2) = (5.0d0, 0.0d0)
  A(1,3) = (2.0d0, 0.0d0); A(2,3) = (1.0d0, 1.0d0); A(3,3) = (6.0d0, 2.0d0)
  A(1,4) = (0.0d0, 1.0d0); A(2,4) = (0.0d0, 0.0d0); A(3,4) = (1.0d0, -1.0d0); A(4,4) = (3.0d0, 0.0d0)
  W = (0.0d0, 0.0d0)
  ipiv = 0
  call zlasyf('U', n, nb, kb, A, lda, ipiv, W, ldw, info)
  call begin_test('upper_4x4')
  call print_array('A', A_r, 2*lda*n)
  call print_int_array('ipiv', ipiv, n)
  call print_int('info', info)
  call print_int('kb', kb)
  call end_test()

  ! Test 2: 4x4 lower, nb=4
  n = 4
  nb = 4
  lda = MAXN
  ldw = MAXN
  A = (0.0d0, 0.0d0)
  A(1,1) = (4.0d0, 1.0d0)
  A(2,1) = (1.0d0, 2.0d0); A(2,2) = (5.0d0, 0.0d0)
  A(3,1) = (2.0d0, 0.0d0); A(3,2) = (1.0d0, 1.0d0); A(3,3) = (6.0d0, 2.0d0)
  A(4,1) = (0.0d0, 1.0d0); A(4,2) = (0.0d0, 0.0d0); A(4,3) = (1.0d0, -1.0d0); A(4,4) = (3.0d0, 0.0d0)
  W = (0.0d0, 0.0d0)
  ipiv = 0
  call zlasyf('L', n, nb, kb, A, lda, ipiv, W, ldw, info)
  call begin_test('lower_4x4')
  call print_array('A', A_r, 2*lda*n)
  call print_int_array('ipiv', ipiv, n)
  call print_int('info', info)
  call print_int('kb', kb)
  call end_test()

  ! Test 3: 3x3 upper, nb=2 (partial panel)
  n = 3
  nb = 2
  lda = MAXN
  ldw = MAXN
  A = (0.0d0, 0.0d0)
  A(1,1) = (5.0d0, 0.0d0)
  A(1,2) = (2.0d0, 1.0d0); A(2,2) = (4.0d0, 0.0d0)
  A(1,3) = (1.0d0, 0.0d0); A(2,3) = (1.0d0, -1.0d0); A(3,3) = (6.0d0, 1.0d0)
  W = (0.0d0, 0.0d0)
  ipiv = 0
  call zlasyf('U', n, nb, kb, A, lda, ipiv, W, ldw, info)
  call begin_test('upper_3x3_nb2')
  call print_array('A', A_r, 2*lda*n)
  call print_int_array('ipiv', ipiv, n)
  call print_int('info', info)
  call print_int('kb', kb)
  call end_test()

  ! Test 4: 1x1 matrix
  n = 1
  nb = 1
  lda = MAXN
  ldw = MAXN
  A = (0.0d0, 0.0d0)
  A(1,1) = (7.0d0, 3.0d0)
  W = (0.0d0, 0.0d0)
  ipiv = 0
  call zlasyf('U', n, nb, kb, A, lda, ipiv, W, ldw, info)
  call begin_test('upper_1x1')
  call print_array('A', A_r, 2*lda*n)
  call print_int_array('ipiv', ipiv, 1)
  call print_int('info', info)
  call print_int('kb', kb)
  call end_test()

  ! Test 5: 2x2 with pivot swap (upper)
  n = 2
  nb = 2
  lda = MAXN
  ldw = MAXN
  A = (0.0d0, 0.0d0)
  ! Make |A(1,1)| small, |A(2,2)| large to avoid 2x2 pivot
  A(1,1) = (0.1d0, 0.0d0)
  A(1,2) = (3.0d0, 4.0d0); A(2,2) = (10.0d0, 0.0d0)
  W = (0.0d0, 0.0d0)
  ipiv = 0
  call zlasyf('U', n, nb, kb, A, lda, ipiv, W, ldw, info)
  call begin_test('upper_2x2_pivot')
  call print_array('A', A_r, 2*lda*n)
  call print_int_array('ipiv', ipiv, n)
  call print_int('info', info)
  call print_int('kb', kb)
  call end_test()

  ! Test 6: 3x3 lower, nb=3
  n = 3
  nb = 3
  lda = MAXN
  ldw = MAXN
  A = (0.0d0, 0.0d0)
  A(1,1) = (5.0d0, 0.0d0)
  A(2,1) = (2.0d0, 1.0d0); A(2,2) = (4.0d0, 0.0d0)
  A(3,1) = (1.0d0, 0.0d0); A(3,2) = (1.0d0, -1.0d0); A(3,3) = (6.0d0, 1.0d0)
  W = (0.0d0, 0.0d0)
  ipiv = 0
  call zlasyf('L', n, nb, kb, A, lda, ipiv, W, ldw, info)
  call begin_test('lower_3x3')
  call print_array('A', A_r, 2*lda*n)
  call print_int_array('ipiv', ipiv, n)
  call print_int('info', info)
  call print_int('kb', kb)
  call end_test()

end program
