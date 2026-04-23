program test_zhecon
  use test_utils
  implicit none

  integer, parameter :: NMAX = 6
  complex*16 :: A(NMAX, NMAX), WORK(2*NMAX), FWORK(NMAX*32)
  double precision :: A_r(2*NMAX*NMAX), anorm, rcond
  equivalence (A, A_r)
  integer :: IPIV(NMAX), INFO, n, LWORK, i, j
  double precision :: colsum

  LWORK = NMAX * 32

  ! Test 1: 4x4 well-conditioned Hermitian (upper)
  ! Compute 1-norm manually: max column sum of |A| using Hermitian symmetry
  n = 4
  A = (0.0d0, 0.0d0)
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
  ! Compute 1-norm = max over j of sum_i |A(i,j)|
  ! For Hermitian: full matrix has A(i,j) = conj(A(j,i))
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
  call ZHETRF('U', n, A, NMAX, IPIV, FWORK, LWORK, INFO)
  call begin_test('upper_4x4')
  call print_int('n', n)
  call print_scalar('anorm', anorm)
  call print_int('info_trf', INFO)
  call print_array('A_factored', A_r, 2*NMAX*n)
  call print_int_array('ipiv', IPIV, n)
  call ZHECON('U', n, A, NMAX, IPIV, anorm, rcond, WORK, INFO)
  call print_int('info', INFO)
  call print_scalar('rcond', rcond)
  call end_test()

  ! Test 2: 4x4 Hermitian (lower) - same matrix stored lower
  n = 4
  A = (0.0d0, 0.0d0)
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
  call ZHETRF('L', n, A, NMAX, IPIV, FWORK, LWORK, INFO)
  call begin_test('lower_4x4')
  call print_int('n', n)
  call print_scalar('anorm', anorm)
  call print_int('info_trf', INFO)
  call print_array('A_factored', A_r, 2*NMAX*n)
  call print_int_array('ipiv', IPIV, n)
  call ZHECON('L', n, A, NMAX, IPIV, anorm, rcond, WORK, INFO)
  call print_int('info', INFO)
  call print_scalar('rcond', rcond)
  call end_test()

  ! Test 3: Identity 3x3 (upper)
  n = 3
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.0d0)
  A(2,2) = (1.0d0, 0.0d0)
  A(3,3) = (1.0d0, 0.0d0)
  anorm = 1.0d0
  call ZHETRF('U', n, A, NMAX, IPIV, FWORK, LWORK, INFO)
  call begin_test('identity_upper')
  call print_int('n', n)
  call print_scalar('anorm', anorm)
  call print_int('info_trf', INFO)
  call print_array('A_factored', A_r, 2*NMAX*n)
  call print_int_array('ipiv', IPIV, n)
  call ZHECON('U', n, A, NMAX, IPIV, anorm, rcond, WORK, INFO)
  call print_int('info', INFO)
  call print_scalar('rcond', rcond)
  call end_test()

  ! Test 4: Identity 3x3 (lower)
  n = 3
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.0d0)
  A(2,2) = (1.0d0, 0.0d0)
  A(3,3) = (1.0d0, 0.0d0)
  anorm = 1.0d0
  call ZHETRF('L', n, A, NMAX, IPIV, FWORK, LWORK, INFO)
  call begin_test('identity_lower')
  call print_int('n', n)
  call print_scalar('anorm', anorm)
  call print_int('info_trf', INFO)
  call print_array('A_factored', A_r, 2*NMAX*n)
  call print_int_array('ipiv', IPIV, n)
  call ZHECON('L', n, A, NMAX, IPIV, anorm, rcond, WORK, INFO)
  call print_int('info', INFO)
  call print_scalar('rcond', rcond)
  call end_test()

  ! Test 5: N=1 (upper)
  n = 1
  A(1,1) = (5.0d0, 0.0d0)
  anorm = 5.0d0
  call ZHETRF('U', n, A, NMAX, IPIV, FWORK, LWORK, INFO)
  call begin_test('n1_upper')
  call print_int('n', n)
  call print_scalar('anorm', anorm)
  call print_int('info_trf', INFO)
  call print_array('A_factored', A_r, 2*n)
  call print_int_array('ipiv', IPIV, n)
  call ZHECON('U', n, A, NMAX, IPIV, anorm, rcond, WORK, INFO)
  call print_int('info', INFO)
  call print_scalar('rcond', rcond)
  call end_test()

  ! Test 6: 6x6 with 2x2 pivots (lower)
  n = 6
  A = (0.0d0, 0.0d0)
  A(1,1) = (0.01d0, 0.0d0)
  A(2,1) = (5.0d0, -1.0d0)
  A(2,2) = (0.02d0, 0.0d0)
  A(3,1) = (1.0d0, 1.0d0)
  A(3,2) = (2.0d0, -1.0d0)
  A(3,3) = (8.0d0, 0.0d0)
  A(4,1) = (0.5d0, -0.5d0)
  A(4,2) = (1.0d0, 1.0d0)
  A(4,3) = (3.0d0, 0.0d0)
  A(4,4) = (7.0d0, 0.0d0)
  A(5,1) = (2.0d0, 0.0d0)
  A(5,2) = (1.5d0, -0.5d0)
  A(5,3) = (0.0d0, -2.0d0)
  A(5,4) = (1.0d0, 0.5d0)
  A(5,5) = (6.0d0, 0.0d0)
  A(6,1) = (1.0d0, -1.0d0)
  A(6,2) = (0.0d0, -3.0d0)
  A(6,3) = (1.0d0, 0.0d0)
  A(6,4) = (2.0d0, -2.0d0)
  A(6,5) = (0.5d0, 1.0d0)
  A(6,6) = (5.0d0, 0.0d0)
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
  call ZHETRF('L', n, A, NMAX, IPIV, FWORK, LWORK, INFO)
  call begin_test('lower_6x6')
  call print_int('n', n)
  call print_scalar('anorm', anorm)
  call print_int('info_trf', INFO)
  call print_array('A_factored', A_r, 2*NMAX*n)
  call print_int_array('ipiv', IPIV, n)
  call ZHECON('L', n, A, NMAX, IPIV, anorm, rcond, WORK, INFO)
  call print_int('info', INFO)
  call print_scalar('rcond', rcond)
  call end_test()

  ! Test 7: 6x6 with 2x2 pivots (upper)
  n = 6
  A = (0.0d0, 0.0d0)
  A(1,1) = (5.0d0, 0.0d0)
  A(1,2) = (0.5d0, 1.0d0)
  A(1,3) = (1.0d0, 0.5d0)
  A(1,4) = (2.0d0, -2.0d0)
  A(1,5) = (0.0d0, -3.0d0)
  A(1,6) = (1.0d0, -1.0d0)
  A(2,2) = (6.0d0, 0.0d0)
  A(2,3) = (1.5d0, 0.5d0)
  A(2,4) = (0.0d0, 2.0d0)
  A(2,5) = (1.0d0, -0.5d0)
  A(2,6) = (2.0d0, 0.0d0)
  A(3,3) = (7.0d0, 0.0d0)
  A(3,4) = (3.0d0, 0.0d0)
  A(3,5) = (1.0d0, -1.0d0)
  A(3,6) = (0.5d0, 0.5d0)
  A(4,4) = (8.0d0, 0.0d0)
  A(4,5) = (2.0d0, 1.0d0)
  A(4,6) = (1.0d0, 2.0d0)
  A(5,5) = (0.02d0, 0.0d0)
  A(5,6) = (5.0d0, 1.0d0)
  A(6,6) = (0.01d0, 0.0d0)
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
  call ZHETRF('U', n, A, NMAX, IPIV, FWORK, LWORK, INFO)
  call begin_test('upper_6x6')
  call print_int('n', n)
  call print_scalar('anorm', anorm)
  call print_int('info_trf', INFO)
  call print_array('A_factored', A_r, 2*NMAX*n)
  call print_int_array('ipiv', IPIV, n)
  call ZHECON('U', n, A, NMAX, IPIV, anorm, rcond, WORK, INFO)
  call print_int('info', INFO)
  call print_scalar('rcond', rcond)
  call end_test()

end program
