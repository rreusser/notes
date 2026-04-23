program test_zhecon_3
  use test_utils
  implicit none

  integer, parameter :: NMAX = 6
  complex*16 :: A(NMAX, NMAX), E(NMAX), WORK(2*NMAX), FWORK(NMAX*32)
  double precision :: A_r(2*NMAX*NMAX), E_r(2*NMAX), anorm, rcond
  equivalence (A, A_r)
  equivalence (E, E_r)
  integer :: IPIV(NMAX), INFO, n, LWORK, i, j
  double precision :: colsum

  LWORK = NMAX * 32

  ! Test 1: 4x4 well-conditioned Hermitian (upper)
  n = 4
  A = (0.0d0, 0.0d0)
  E = (0.0d0, 0.0d0)
  A(1,1) = (4.0d0, 0.0d0)
  A(1,2) = (1.0d0, 2.0d0)
  A(1,3) = (3.0d0, -1.0d0)
  A(1,4) = (0.5d0, 0.5d0)
  A(2,2) = (5.0d0, 0.0d0)
  A(2,3) = (2.0d0, 1.0d0)
  A(2,4) = (1.0d0, -2.0d0)
  A(3,3) = (6.0d0, 0.0d0)
  A(3,4) = (3.0d0, 0.0d0)
  A(4,4) = (8.0d0, 0.0d0)
  anorm = 0.0d0
  do j = 1, n
    colsum = 0.0d0
    do i = 1, n
      if (i .le. j) then
        colsum = colsum + abs(A(i,j))
      else
        colsum = colsum + abs(A(j,i))
      end if
    end do
    if (colsum > anorm) anorm = colsum
  end do
  call ZHETRF_RK('U', n, A, NMAX, E, IPIV, FWORK, LWORK, INFO)
  call ZHECON_3('U', n, A, NMAX, E, IPIV, anorm, rcond, WORK, INFO)
  call begin_test('upper_4x4')
  call print_int('n', n)
  call print_int('lda', NMAX)
  call print_scalar('anorm', anorm)
  call print_array('A_factored', A_r, 2*NMAX*n)
  call print_array('e', E_r, 2*n)
  call print_int_array('ipiv', IPIV, n)
  call print_int('info', INFO)
  call print_scalar('rcond', rcond)
  call end_test()

  ! Test 2: same matrix lower
  n = 4
  A = (0.0d0, 0.0d0)
  E = (0.0d0, 0.0d0)
  A(1,1) = (4.0d0, 0.0d0)
  A(2,1) = (1.0d0, -2.0d0)
  A(2,2) = (5.0d0, 0.0d0)
  A(3,1) = (3.0d0, 1.0d0)
  A(3,2) = (2.0d0, -1.0d0)
  A(3,3) = (6.0d0, 0.0d0)
  A(4,1) = (0.5d0, -0.5d0)
  A(4,2) = (1.0d0, 2.0d0)
  A(4,3) = (3.0d0, 0.0d0)
  A(4,4) = (8.0d0, 0.0d0)
  anorm = 0.0d0
  do j = 1, n
    colsum = 0.0d0
    do i = 1, n
      if (i .ge. j) then
        colsum = colsum + abs(A(i,j))
      else
        colsum = colsum + abs(A(j,i))
      end if
    end do
    if (colsum > anorm) anorm = colsum
  end do
  call ZHETRF_RK('L', n, A, NMAX, E, IPIV, FWORK, LWORK, INFO)
  call ZHECON_3('L', n, A, NMAX, E, IPIV, anorm, rcond, WORK, INFO)
  call begin_test('lower_4x4')
  call print_int('n', n)
  call print_int('lda', NMAX)
  call print_scalar('anorm', anorm)
  call print_array('A_factored', A_r, 2*NMAX*n)
  call print_array('e', E_r, 2*n)
  call print_int_array('ipiv', IPIV, n)
  call print_int('info', INFO)
  call print_scalar('rcond', rcond)
  call end_test()

  ! Test 3: 3x3 identity (upper)
  n = 3
  A = (0.0d0, 0.0d0)
  E = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.0d0)
  A(2,2) = (1.0d0, 0.0d0)
  A(3,3) = (1.0d0, 0.0d0)
  anorm = 1.0d0
  call ZHETRF_RK('U', n, A, NMAX, E, IPIV, FWORK, LWORK, INFO)
  call ZHECON_3('U', n, A, NMAX, E, IPIV, anorm, rcond, WORK, INFO)
  call begin_test('identity_upper')
  call print_int('n', n)
  call print_int('lda', NMAX)
  call print_scalar('anorm', anorm)
  call print_array('A_factored', A_r, 2*NMAX*n)
  call print_array('e', E_r, 2*n)
  call print_int_array('ipiv', IPIV, n)
  call print_int('info', INFO)
  call print_scalar('rcond', rcond)
  call end_test()

  ! Test 4: 3x3 identity (lower)
  n = 3
  A = (0.0d0, 0.0d0)
  E = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.0d0)
  A(2,2) = (1.0d0, 0.0d0)
  A(3,3) = (1.0d0, 0.0d0)
  anorm = 1.0d0
  call ZHETRF_RK('L', n, A, NMAX, E, IPIV, FWORK, LWORK, INFO)
  call ZHECON_3('L', n, A, NMAX, E, IPIV, anorm, rcond, WORK, INFO)
  call begin_test('identity_lower')
  call print_int('n', n)
  call print_int('lda', NMAX)
  call print_scalar('anorm', anorm)
  call print_array('A_factored', A_r, 2*NMAX*n)
  call print_array('e', E_r, 2*n)
  call print_int_array('ipiv', IPIV, n)
  call print_int('info', INFO)
  call print_scalar('rcond', rcond)
  call end_test()

  ! Test 5: indefinite forces 2x2 (upper)
  n = 4
  A = (0.0d0, 0.0d0)
  E = (0.0d0, 0.0d0)
  A(1,1) = (0.0d0, 0.0d0)
  A(1,2) = (1.0d0, 0.0d0)
  A(1,3) = (2.0d0, 0.5d0)
  A(1,4) = (3.0d0, -1.0d0)
  A(2,2) = (0.0d0, 0.0d0)
  A(2,3) = (4.0d0, 0.0d0)
  A(2,4) = (5.0d0, 1.0d0)
  A(3,3) = (0.0d0, 0.0d0)
  A(3,4) = (6.0d0, 0.0d0)
  A(4,4) = (0.0d0, 0.0d0)
  anorm = 0.0d0
  do j = 1, n
    colsum = 0.0d0
    do i = 1, n
      if (i .le. j) then
        colsum = colsum + abs(A(i,j))
      else
        colsum = colsum + abs(A(j,i))
      end if
    end do
    if (colsum > anorm) anorm = colsum
  end do
  call ZHETRF_RK('U', n, A, NMAX, E, IPIV, FWORK, LWORK, INFO)
  call ZHECON_3('U', n, A, NMAX, E, IPIV, anorm, rcond, WORK, INFO)
  call begin_test('indef_upper')
  call print_int('n', n)
  call print_int('lda', NMAX)
  call print_scalar('anorm', anorm)
  call print_array('A_factored', A_r, 2*NMAX*n)
  call print_array('e', E_r, 2*n)
  call print_int_array('ipiv', IPIV, n)
  call print_int('info', INFO)
  call print_scalar('rcond', rcond)
  call end_test()

  ! Test 6: N=0
  n = 0
  call ZHECON_3('U', n, A, NMAX, E, IPIV, 0.0d0, rcond, WORK, INFO)
  call begin_test('n_zero')
  call print_int('n', n)
  call print_int('lda', NMAX)
  call print_int('info', INFO)
  call print_scalar('rcond', rcond)
  call end_test()

  ! Test 7: N=1 (upper)
  n = 1
  A = (0.0d0, 0.0d0)
  E = (0.0d0, 0.0d0)
  A(1,1) = (5.0d0, 0.0d0)
  anorm = 5.0d0
  call ZHETRF_RK('U', n, A, NMAX, E, IPIV, FWORK, LWORK, INFO)
  call ZHECON_3('U', n, A, NMAX, E, IPIV, anorm, rcond, WORK, INFO)
  call begin_test('n_one_upper')
  call print_int('n', n)
  call print_int('lda', NMAX)
  call print_scalar('anorm', anorm)
  call print_array('A_factored', A_r, 2*NMAX*n)
  call print_array('e', E_r, 2*n)
  call print_int_array('ipiv', IPIV, n)
  call print_int('info', INFO)
  call print_scalar('rcond', rcond)
  call end_test()

end program
