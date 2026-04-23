program test_zhpsvx
  use test_utils
  implicit none

  integer, parameter :: NMAX = 6
  integer, parameter :: APMAX = NMAX*(NMAX+1)/2
  complex*16 :: AP(APMAX), AFP(APMAX), B(NMAX, 3), X(NMAX, 3)
  complex*16 :: WORK(2*NMAX)
  complex*16 :: Bpk(NMAX*3), Xpk(NMAX*3)
  double precision :: AP_r(2*APMAX), AFP_r(2*APMAX)
  double precision :: B_r(2*NMAX*3), X_r(2*NMAX*3)
  double precision :: Bpk_r(2*NMAX*3), Xpk_r(2*NMAX*3)
  double precision :: FERR(3), BERR(3), RWORK(NMAX), RCOND
  equivalence (AP, AP_r)
  equivalence (AFP, AFP_r)
  equivalence (B, B_r)
  equivalence (X, X_r)
  equivalence (Bpk, Bpk_r)
  equivalence (Xpk, Xpk_r)
  integer :: IPIV(NMAX), INFO, n, nrhs, nn, i, j

  ! Test 1: FACT='N', UPLO='U', 3x3, 1 RHS
  n = 3
  nrhs = 1
  nn = n*(n+1)/2
  AP = (0.0d0, 0.0d0)
  AFP = (0.0d0, 0.0d0)
  ! Upper packed: (1,1), (1,2), (2,2), (1,3), (2,3), (3,3)
  AP(1) = (4.0d0, 0.0d0)    ! A(1,1) real diagonal
  AP(2) = (1.0d0, 2.0d0)    ! A(1,2)
  AP(3) = (5.0d0, 0.0d0)    ! A(2,2) real diagonal
  AP(4) = (2.0d0, -1.0d0)   ! A(1,3)
  AP(5) = (3.0d0, 1.0d0)    ! A(2,3)
  AP(6) = (6.0d0, 0.0d0)    ! A(3,3) real diagonal
  B = (0.0d0, 0.0d0)
  B(1,1) = (1.0d0, 0.0d0)
  B(2,1) = (0.0d0, 1.0d0)
  B(3,1) = (1.0d0, -1.0d0)
  X = (0.0d0, 0.0d0)
  IPIV = 0
  call ZHPSVX('N', 'U', n, nrhs, AP, AFP, IPIV, B, NMAX, X, NMAX, &
              RCOND, FERR, BERR, WORK, RWORK, INFO)
  call begin_test('fact_n_upper')
  call print_int('n', n)
  call print_int('nrhs', nrhs)
  call print_array('AP', AP_r, 2*nn)
  do j = 1, nrhs
    do i = 1, n
      Bpk(i + (j-1)*n) = B(i, j)
    end do
  end do
  call print_array('b', Bpk_r, 2*n*nrhs)
  do j = 1, nrhs
    do i = 1, n
      Xpk(i + (j-1)*n) = X(i, j)
    end do
  end do
  call print_array('x', Xpk_r, 2*n*nrhs)
  call print_int('info', INFO)
  call print_scalar('rcond', RCOND)
  call print_array('ferr', FERR, nrhs)
  call print_array('berr', BERR, nrhs)
  call print_array('afp', AFP_r, 2*nn)
  call print_int_array('ipiv', IPIV, n)
  call end_test()

  ! Test 2: FACT='N', UPLO='L', 3x3, 1 RHS
  n = 3
  nrhs = 1
  nn = n*(n+1)/2
  AP = (0.0d0, 0.0d0)
  AFP = (0.0d0, 0.0d0)
  ! Lower packed: (1,1), (2,1), (3,1), (2,2), (3,2), (3,3)
  AP(1) = (4.0d0, 0.0d0)    ! A(1,1)
  AP(2) = (1.0d0, -2.0d0)   ! A(2,1) = conj(A(1,2))
  AP(3) = (2.0d0, 1.0d0)    ! A(3,1) = conj(A(1,3))
  AP(4) = (5.0d0, 0.0d0)    ! A(2,2)
  AP(5) = (3.0d0, -1.0d0)   ! A(3,2) = conj(A(2,3))
  AP(6) = (6.0d0, 0.0d0)    ! A(3,3)
  B = (0.0d0, 0.0d0)
  B(1,1) = (1.0d0, 0.0d0)
  B(2,1) = (0.0d0, 1.0d0)
  B(3,1) = (1.0d0, -1.0d0)
  X = (0.0d0, 0.0d0)
  IPIV = 0
  call ZHPSVX('N', 'L', n, nrhs, AP, AFP, IPIV, B, NMAX, X, NMAX, &
              RCOND, FERR, BERR, WORK, RWORK, INFO)
  call begin_test('fact_n_lower')
  call print_int('n', n)
  call print_int('nrhs', nrhs)
  call print_array('AP', AP_r, 2*nn)
  do j = 1, nrhs
    do i = 1, n
      Bpk(i + (j-1)*n) = B(i, j)
    end do
  end do
  call print_array('b', Bpk_r, 2*n*nrhs)
  do j = 1, nrhs
    do i = 1, n
      Xpk(i + (j-1)*n) = X(i, j)
    end do
  end do
  call print_array('x', Xpk_r, 2*n*nrhs)
  call print_int('info', INFO)
  call print_scalar('rcond', RCOND)
  call print_array('ferr', FERR, nrhs)
  call print_array('berr', BERR, nrhs)
  call print_array('afp', AFP_r, 2*nn)
  call print_int_array('ipiv', IPIV, n)
  call end_test()

  ! Test 3: FACT='F', UPLO='U', reuse factorization from test 1
  n = 3
  nrhs = 1
  nn = n*(n+1)/2
  AP = (0.0d0, 0.0d0)
  AFP = (0.0d0, 0.0d0)
  AP(1) = (4.0d0, 0.0d0)
  AP(2) = (1.0d0, 2.0d0)
  AP(3) = (5.0d0, 0.0d0)
  AP(4) = (2.0d0, -1.0d0)
  AP(5) = (3.0d0, 1.0d0)
  AP(6) = (6.0d0, 0.0d0)
  ! Factor first
  AFP(1:nn) = AP(1:nn)
  call ZHPTRF('U', n, AFP, IPIV, INFO)
  ! Solve with FACT='F'
  B = (0.0d0, 0.0d0)
  B(1,1) = (1.0d0, 0.0d0)
  B(2,1) = (0.0d0, 1.0d0)
  B(3,1) = (1.0d0, -1.0d0)
  X = (0.0d0, 0.0d0)
  call ZHPSVX('F', 'U', n, nrhs, AP, AFP, IPIV, B, NMAX, X, NMAX, &
              RCOND, FERR, BERR, WORK, RWORK, INFO)
  call begin_test('fact_f_upper')
  call print_int('n', n)
  call print_int('nrhs', nrhs)
  do j = 1, nrhs
    do i = 1, n
      Xpk(i + (j-1)*n) = X(i, j)
    end do
  end do
  call print_array('x', Xpk_r, 2*n*nrhs)
  call print_int('info', INFO)
  call print_scalar('rcond', RCOND)
  call print_array('ferr', FERR, nrhs)
  call print_array('berr', BERR, nrhs)
  call end_test()

  ! Test 4: FACT='F', UPLO='L', reuse factorization
  n = 3
  nrhs = 1
  nn = n*(n+1)/2
  AP = (0.0d0, 0.0d0)
  AFP = (0.0d0, 0.0d0)
  AP(1) = (4.0d0, 0.0d0)
  AP(2) = (1.0d0, -2.0d0)
  AP(3) = (2.0d0, 1.0d0)
  AP(4) = (5.0d0, 0.0d0)
  AP(5) = (3.0d0, -1.0d0)
  AP(6) = (6.0d0, 0.0d0)
  AFP(1:nn) = AP(1:nn)
  call ZHPTRF('L', n, AFP, IPIV, INFO)
  B = (0.0d0, 0.0d0)
  B(1,1) = (1.0d0, 0.0d0)
  B(2,1) = (0.0d0, 1.0d0)
  B(3,1) = (1.0d0, -1.0d0)
  X = (0.0d0, 0.0d0)
  call ZHPSVX('F', 'L', n, nrhs, AP, AFP, IPIV, B, NMAX, X, NMAX, &
              RCOND, FERR, BERR, WORK, RWORK, INFO)
  call begin_test('fact_f_lower')
  call print_int('n', n)
  call print_int('nrhs', nrhs)
  do j = 1, nrhs
    do i = 1, n
      Xpk(i + (j-1)*n) = X(i, j)
    end do
  end do
  call print_array('x', Xpk_r, 2*n*nrhs)
  call print_int('info', INFO)
  call print_scalar('rcond', RCOND)
  call print_array('ferr', FERR, nrhs)
  call print_array('berr', BERR, nrhs)
  call end_test()

  ! Test 5: N=0 quick return
  call ZHPSVX('N', 'U', 0, 1, AP, AFP, IPIV, B, NMAX, X, NMAX, &
              RCOND, FERR, BERR, WORK, RWORK, INFO)
  call begin_test('n_zero')
  call print_int('info', INFO)
  call end_test()

  ! Test 6: N=1, UPLO='U'
  n = 1
  nrhs = 1
  AP = (0.0d0, 0.0d0)
  AFP = (0.0d0, 0.0d0)
  AP(1) = (4.0d0, 0.0d0)
  B = (0.0d0, 0.0d0)
  B(1,1) = (8.0d0, 4.0d0)
  X = (0.0d0, 0.0d0)
  IPIV = 0
  call ZHPSVX('N', 'U', n, nrhs, AP, AFP, IPIV, B, NMAX, X, NMAX, &
              RCOND, FERR, BERR, WORK, RWORK, INFO)
  call begin_test('n_one_upper')
  Xpk(1) = X(1,1)
  call print_array('x', Xpk_r, 2)
  call print_int('info', INFO)
  call print_scalar('rcond', RCOND)
  call print_array('ferr', FERR, 1)
  call print_array('berr', BERR, 1)
  call end_test()

  ! Test 7: Singular matrix (INFO > 0)
  n = 3
  nrhs = 1
  nn = n*(n+1)/2
  AP = (0.0d0, 0.0d0)
  AFP = (0.0d0, 0.0d0)
  ! Upper packed: A(2,2) = 0, A(1,2) = 0 => singular
  AP(1) = (4.0d0, 0.0d0)
  AP(2) = (0.0d0, 0.0d0)
  AP(3) = (0.0d0, 0.0d0)
  AP(4) = (0.0d0, 0.0d0)
  AP(5) = (0.0d0, 0.0d0)
  AP(6) = (6.0d0, 0.0d0)
  B = (0.0d0, 0.0d0)
  B(1,1) = (1.0d0, 0.0d0)
  B(2,1) = (2.0d0, 0.0d0)
  B(3,1) = (3.0d0, 0.0d0)
  X = (0.0d0, 0.0d0)
  IPIV = 0
  call ZHPSVX('N', 'U', n, nrhs, AP, AFP, IPIV, B, NMAX, X, NMAX, &
              RCOND, FERR, BERR, WORK, RWORK, INFO)
  call begin_test('singular')
  call print_int('info', INFO)
  call print_scalar('rcond', RCOND)
  call end_test()

  ! Test 8: Multi-RHS (2 right-hand sides), UPLO='U'
  n = 3
  nrhs = 2
  nn = n*(n+1)/2
  AP = (0.0d0, 0.0d0)
  AFP = (0.0d0, 0.0d0)
  AP(1) = (4.0d0, 0.0d0)
  AP(2) = (1.0d0, 2.0d0)
  AP(3) = (5.0d0, 0.0d0)
  AP(4) = (2.0d0, -1.0d0)
  AP(5) = (3.0d0, 1.0d0)
  AP(6) = (6.0d0, 0.0d0)
  B = (0.0d0, 0.0d0)
  B(1,1) = (1.0d0, 0.0d0)
  B(2,1) = (0.0d0, 1.0d0)
  B(3,1) = (1.0d0, -1.0d0)
  B(1,2) = (2.0d0, 1.0d0)
  B(2,2) = (1.0d0, -1.0d0)
  B(3,2) = (0.0d0, 2.0d0)
  X = (0.0d0, 0.0d0)
  IPIV = 0
  call ZHPSVX('N', 'U', n, nrhs, AP, AFP, IPIV, B, NMAX, X, NMAX, &
              RCOND, FERR, BERR, WORK, RWORK, INFO)
  call begin_test('multi_rhs_upper')
  call print_int('n', n)
  call print_int('nrhs', nrhs)
  do j = 1, nrhs
    do i = 1, n
      Xpk(i + (j-1)*n) = X(i, j)
    end do
  end do
  call print_array('x', Xpk_r, 2*n*nrhs)
  call print_int('info', INFO)
  call print_scalar('rcond', RCOND)
  call print_array('ferr', FERR, nrhs)
  call print_array('berr', BERR, nrhs)
  call print_array('afp', AFP_r, 2*nn)
  call print_int_array('ipiv', IPIV, n)
  call end_test()

  ! Test 9: Multi-RHS (2 right-hand sides), UPLO='L'
  n = 3
  nrhs = 2
  nn = n*(n+1)/2
  AP = (0.0d0, 0.0d0)
  AFP = (0.0d0, 0.0d0)
  AP(1) = (4.0d0, 0.0d0)
  AP(2) = (1.0d0, -2.0d0)
  AP(3) = (2.0d0, 1.0d0)
  AP(4) = (5.0d0, 0.0d0)
  AP(5) = (3.0d0, -1.0d0)
  AP(6) = (6.0d0, 0.0d0)
  B = (0.0d0, 0.0d0)
  B(1,1) = (1.0d0, 0.0d0)
  B(2,1) = (0.0d0, 1.0d0)
  B(3,1) = (1.0d0, -1.0d0)
  B(1,2) = (2.0d0, 1.0d0)
  B(2,2) = (1.0d0, -1.0d0)
  B(3,2) = (0.0d0, 2.0d0)
  X = (0.0d0, 0.0d0)
  IPIV = 0
  call ZHPSVX('N', 'L', n, nrhs, AP, AFP, IPIV, B, NMAX, X, NMAX, &
              RCOND, FERR, BERR, WORK, RWORK, INFO)
  call begin_test('multi_rhs_lower')
  call print_int('n', n)
  call print_int('nrhs', nrhs)
  do j = 1, nrhs
    do i = 1, n
      Xpk(i + (j-1)*n) = X(i, j)
    end do
  end do
  call print_array('x', Xpk_r, 2*n*nrhs)
  call print_int('info', INFO)
  call print_scalar('rcond', RCOND)
  call print_array('ferr', FERR, nrhs)
  call print_array('berr', BERR, nrhs)
  call print_array('afp', AFP_r, 2*nn)
  call print_int_array('ipiv', IPIV, n)
  call end_test()

  ! Test 10: 4x4, Upper, well-conditioned
  n = 4
  nrhs = 1
  nn = n*(n+1)/2
  AP = (0.0d0, 0.0d0)
  AFP = (0.0d0, 0.0d0)
  ! Upper packed: (1,1),(1,2),(2,2),(1,3),(2,3),(3,3),(1,4),(2,4),(3,4),(4,4)
  AP(1)  = (4.0d0, 0.0d0)
  AP(2)  = (0.5d0, 0.3d0)
  AP(3)  = (5.0d0, 0.0d0)
  AP(4)  = (0.3d0, -0.2d0)
  AP(5)  = (0.4d0, 0.1d0)
  AP(6)  = (6.0d0, 0.0d0)
  AP(7)  = (0.2d0, 0.1d0)
  AP(8)  = (0.1d0, -0.3d0)
  AP(9)  = (0.5d0, 0.2d0)
  AP(10) = (7.0d0, 0.0d0)
  B = (0.0d0, 0.0d0)
  B(1,1) = (1.0d0, 0.0d0)
  B(2,1) = (0.0d0, 1.0d0)
  B(3,1) = (2.0d0, -1.0d0)
  B(4,1) = (1.0d0, 1.0d0)
  X = (0.0d0, 0.0d0)
  IPIV = 0
  call ZHPSVX('N', 'U', n, nrhs, AP, AFP, IPIV, B, NMAX, X, NMAX, &
              RCOND, FERR, BERR, WORK, RWORK, INFO)
  call begin_test('upper_4x4')
  call print_int('n', n)
  call print_int('nrhs', nrhs)
  do j = 1, nrhs
    do i = 1, n
      Xpk(i + (j-1)*n) = X(i, j)
    end do
  end do
  call print_array('x', Xpk_r, 2*n*nrhs)
  call print_int('info', INFO)
  call print_scalar('rcond', RCOND)
  call print_array('ferr', FERR, nrhs)
  call print_array('berr', BERR, nrhs)
  call print_array('afp', AFP_r, 2*nn)
  call print_int_array('ipiv', IPIV, n)
  call end_test()

end program
