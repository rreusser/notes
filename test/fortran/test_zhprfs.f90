program test_zhprfs
  use test_utils
  implicit none

  integer, parameter :: NMAX = 4, LWMAX = 256
  complex*16 :: AP(NMAX*(NMAX+1)/2), AFP(NMAX*(NMAX+1)/2)
  complex*16 :: B(NMAX, 2), X(NMAX, 2), WORK(LWMAX)
  double precision :: FERR(2), BERR(2), RWORK(NMAX)
  double precision :: X_r(2*NMAX*2), B_r(2*NMAX*2)
  double precision :: AP_r(2*NMAX*(NMAX+1)/2), AFP_r(2*NMAX*(NMAX+1)/2)
  equivalence (X, X_r)
  equivalence (B, B_r)
  equivalence (AP, AP_r)
  equivalence (AFP, AFP_r)
  integer :: IPIV(NMAX), INFO, n, nrhs, nap

  ! Test 1: Upper, 3x3, 1 RHS
  n = 3
  nrhs = 1
  nap = n*(n+1)/2
  AP(1) = (4.0d0, 0.0d0)
  AP(2) = (1.0d0, 2.0d0)
  AP(3) = (6.0d0, 0.0d0)
  AP(4) = (3.0d0, -1.0d0)
  AP(5) = (2.0d0, 1.0d0)
  AP(6) = (5.0d0, 0.0d0)
  AFP(1:nap) = AP(1:nap)
  call ZHPTRF('U', n, AFP, IPIV, INFO)
  B(1,1) = (1.0d0, 0.0d0)
  B(2,1) = (2.0d0, 1.0d0)
  B(3,1) = (-1.0d0, 3.0d0)
  X(:,1) = B(:,1)
  call ZHPTRS('U', n, nrhs, AFP, IPIV, X, NMAX, INFO)
  call ZHPRFS('U', n, nrhs, AP, AFP, IPIV, B, NMAX, X, NMAX, &
              FERR, BERR, WORK, RWORK, INFO)
  call begin_test('upper_3x3')
  call print_int('info', INFO)
  call print_int('N', n)
  call print_int('nrhs', nrhs)
  call print_array('AP', AP_r, 2*nap)
  call print_array('AFP', AFP_r, 2*nap)
  call print_int_array('IPIV', IPIV, n)
  call print_array('B', B_r, 2*NMAX*nrhs)
  call print_array('X', X_r, 2*NMAX*nrhs)
  call print_array('ferr', FERR, nrhs)
  call print_array('berr', BERR, nrhs)
  call end_test()

  ! Test 2: Lower, 3x3, 1 RHS (same matrix, lower triangle stored)
  n = 3
  nrhs = 1
  nap = n*(n+1)/2
  AP(1) = (4.0d0, 0.0d0)
  AP(2) = (1.0d0, -2.0d0)
  AP(3) = (3.0d0, 1.0d0)
  AP(4) = (6.0d0, 0.0d0)
  AP(5) = (2.0d0, -1.0d0)
  AP(6) = (5.0d0, 0.0d0)
  AFP(1:nap) = AP(1:nap)
  call ZHPTRF('L', n, AFP, IPIV, INFO)
  B(1,1) = (1.0d0, 0.0d0)
  B(2,1) = (2.0d0, 1.0d0)
  B(3,1) = (-1.0d0, 3.0d0)
  X(:,1) = B(:,1)
  call ZHPTRS('L', n, nrhs, AFP, IPIV, X, NMAX, INFO)
  call ZHPRFS('L', n, nrhs, AP, AFP, IPIV, B, NMAX, X, NMAX, &
              FERR, BERR, WORK, RWORK, INFO)
  call begin_test('lower_3x3')
  call print_int('info', INFO)
  call print_int('N', n)
  call print_int('nrhs', nrhs)
  call print_array('AP', AP_r, 2*nap)
  call print_array('AFP', AFP_r, 2*nap)
  call print_int_array('IPIV', IPIV, n)
  call print_array('B', B_r, 2*NMAX*nrhs)
  call print_array('X', X_r, 2*NMAX*nrhs)
  call print_array('ferr', FERR, nrhs)
  call print_array('berr', BERR, nrhs)
  call end_test()

  ! Test 3: Upper, 3x3, 2 RHS
  n = 3
  nrhs = 2
  nap = n*(n+1)/2
  AP(1) = (4.0d0, 0.0d0)
  AP(2) = (1.0d0, 2.0d0)
  AP(3) = (6.0d0, 0.0d0)
  AP(4) = (3.0d0, -1.0d0)
  AP(5) = (2.0d0, 1.0d0)
  AP(6) = (5.0d0, 0.0d0)
  AFP(1:nap) = AP(1:nap)
  call ZHPTRF('U', n, AFP, IPIV, INFO)
  B(1,1) = (1.0d0, 0.0d0)
  B(2,1) = (2.0d0, 1.0d0)
  B(3,1) = (-1.0d0, 3.0d0)
  B(1,2) = (0.0d0, 1.0d0)
  B(2,2) = (1.0d0, 0.0d0)
  B(3,2) = (2.0d0, -1.0d0)
  X(:,1) = B(:,1)
  X(:,2) = B(:,2)
  call ZHPTRS('U', n, nrhs, AFP, IPIV, X, NMAX, INFO)
  call ZHPRFS('U', n, nrhs, AP, AFP, IPIV, B, NMAX, X, NMAX, &
              FERR, BERR, WORK, RWORK, INFO)
  call begin_test('upper_3x3_2rhs')
  call print_int('info', INFO)
  call print_int('N', n)
  call print_int('nrhs', nrhs)
  call print_array('AP', AP_r, 2*nap)
  call print_array('AFP', AFP_r, 2*nap)
  call print_int_array('IPIV', IPIV, n)
  call print_array('B', B_r, 2*NMAX*nrhs)
  call print_array('X', X_r, 2*NMAX*nrhs)
  call print_array('ferr', FERR, nrhs)
  call print_array('berr', BERR, nrhs)
  call end_test()

  ! Test 4: N=0
  FERR(1) = 999.0d0
  BERR(1) = 999.0d0
  call ZHPRFS('U', 0, 1, AP, AFP, IPIV, B, NMAX, X, NMAX, &
              FERR, BERR, WORK, RWORK, INFO)
  call begin_test('n0')
  call print_int('info', INFO)
  call print_array('ferr', FERR, 1)
  call print_array('berr', BERR, 1)
  call end_test()

  ! Test 5: N=1
  n = 1
  nrhs = 1
  nap = 1
  AP(1) = (5.0d0, 0.0d0)
  AFP(1) = (5.0d0, 0.0d0)
  IPIV(1) = 1
  B(1,1) = (10.0d0, 5.0d0)
  X(1,1) = (2.0d0, 1.0d0)
  call ZHPRFS('U', n, nrhs, AP, AFP, IPIV, B, NMAX, X, NMAX, &
              FERR, BERR, WORK, RWORK, INFO)
  call begin_test('n1')
  call print_int('info', INFO)
  call print_int('N', n)
  call print_int('nrhs', nrhs)
  call print_array('AP', AP_r, 2*nap)
  call print_array('AFP', AFP_r, 2*nap)
  call print_int_array('IPIV', IPIV, n)
  call print_array('B', B_r, 2*NMAX*nrhs)
  call print_array('X', X_r, 2*NMAX*nrhs)
  call print_array('ferr', FERR, nrhs)
  call print_array('berr', BERR, nrhs)
  call end_test()

end program
