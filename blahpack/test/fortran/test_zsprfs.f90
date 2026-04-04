program test_zsprfs
  use test_utils
  implicit none

  integer, parameter :: NMAX = 4
  integer, parameter :: APMAX = NMAX*(NMAX+1)/2
  integer, parameter :: NRHSMAX = 2
  complex*16 :: AP(APMAX), AFP(APMAX), B(NMAX, NRHSMAX), X(NMAX, NRHSMAX)
  complex*16 :: WORK(2*NMAX)
  double precision :: FERR(NRHSMAX), BERR(NRHSMAX), RWORK(NMAX)
  double precision :: AP_r(2*APMAX), AFP_r(2*APMAX)
  equivalence (AP, AP_r)
  equivalence (AFP, AFP_r)
  complex*16 :: Bpk(NMAX*NRHSMAX), Xpk(NMAX*NRHSMAX), Xinit(NMAX*NRHSMAX)
  double precision :: Bpk_r(2*NMAX*NRHSMAX), Xpk_r(2*NMAX*NRHSMAX)
  double precision :: Xinit_r(2*NMAX*NRHSMAX)
  equivalence (Bpk, Bpk_r)
  equivalence (Xpk, Xpk_r)
  equivalence (Xinit, Xinit_r)
  integer :: IPIV(NMAX), INFO, n, nrhs, nn, i, j

  ! Test 1: Upper, 3x3, 1 RHS
  n = 3
  nrhs = 1
  nn = n*(n+1)/2
  AP = (0.0d0, 0.0d0)
  ! Upper packed: (1,1), (1,2), (2,2), (1,3), (2,3), (3,3)
  AP(1) = (4.0d0, 1.0d0)
  AP(2) = (1.0d0, 2.0d0)
  AP(3) = (5.0d0, -1.0d0)
  AP(4) = (2.0d0, -1.0d0)
  AP(5) = (3.0d0, 1.0d0)
  AP(6) = (6.0d0, 2.0d0)

  B(1,1) = (1.0d0, 0.5d0)
  B(2,1) = (2.0d0, -1.0d0)
  B(3,1) = (0.0d0, 1.0d0)

  AFP(1:nn) = AP(1:nn)
  call ZSPTRF('U', n, AFP, IPIV, INFO)
  if (INFO .ne. 0) stop 'ZSPTRF failed test 1'

  ! Store B flat
  do j = 1, nrhs
    do i = 1, n
      Bpk(i + (j-1)*n) = B(i, j)
    end do
  end do

  ! Solve initial X = AFP \ B
  X(1:n, 1:nrhs) = B(1:n, 1:nrhs)
  call ZSPTRS('U', n, nrhs, AFP, IPIV, X, NMAX, INFO)

  ! Save initial X
  do j = 1, nrhs
    do i = 1, n
      Xinit(i + (j-1)*n) = X(i, j)
    end do
  end do

  ! Call ZSPRFS
  call ZSPRFS('U', n, nrhs, AP, AFP, IPIV, B, NMAX, X, NMAX, &
              FERR, BERR, WORK, RWORK, INFO)

  ! Store refined X flat
  do j = 1, nrhs
    do i = 1, n
      Xpk(i + (j-1)*n) = X(i, j)
    end do
  end do

  call begin_test('upper_3x3')
  call print_int('info', INFO)
  call print_array('AP', AP_r, 2*nn)
  call print_array('AFP', AFP_r, 2*nn)
  call print_int_array('ipiv', IPIV, n)
  call print_array('B', Bpk_r, 2*n*nrhs)
  call print_array('Xinit', Xinit_r, 2*n*nrhs)
  call print_array('X', Xpk_r, 2*n*nrhs)
  call print_array('ferr', FERR, nrhs)
  call print_array('berr', BERR, nrhs)
  call end_test()

  ! Test 2: Lower, 3x3, 1 RHS
  n = 3
  nrhs = 1
  nn = n*(n+1)/2
  AP = (0.0d0, 0.0d0)
  ! Lower packed: (1,1), (2,1), (3,1), (2,2), (3,2), (3,3)
  AP(1) = (4.0d0, 1.0d0)
  AP(2) = (1.0d0, 2.0d0)
  AP(3) = (2.0d0, -1.0d0)
  AP(4) = (5.0d0, -1.0d0)
  AP(5) = (3.0d0, 1.0d0)
  AP(6) = (6.0d0, 2.0d0)

  B(1,1) = (1.0d0, 0.5d0)
  B(2,1) = (2.0d0, -1.0d0)
  B(3,1) = (0.0d0, 1.0d0)

  AFP(1:nn) = AP(1:nn)
  call ZSPTRF('L', n, AFP, IPIV, INFO)
  if (INFO .ne. 0) stop 'ZSPTRF failed test 2'

  do j = 1, nrhs
    do i = 1, n
      Bpk(i + (j-1)*n) = B(i, j)
    end do
  end do

  X(1:n, 1:nrhs) = B(1:n, 1:nrhs)
  call ZSPTRS('L', n, nrhs, AFP, IPIV, X, NMAX, INFO)

  do j = 1, nrhs
    do i = 1, n
      Xinit(i + (j-1)*n) = X(i, j)
    end do
  end do

  call ZSPRFS('L', n, nrhs, AP, AFP, IPIV, B, NMAX, X, NMAX, &
              FERR, BERR, WORK, RWORK, INFO)

  do j = 1, nrhs
    do i = 1, n
      Xpk(i + (j-1)*n) = X(i, j)
    end do
  end do

  call begin_test('lower_3x3')
  call print_int('info', INFO)
  call print_array('AP', AP_r, 2*nn)
  call print_array('AFP', AFP_r, 2*nn)
  call print_int_array('ipiv', IPIV, n)
  call print_array('B', Bpk_r, 2*n*nrhs)
  call print_array('Xinit', Xinit_r, 2*n*nrhs)
  call print_array('X', Xpk_r, 2*n*nrhs)
  call print_array('ferr', FERR, nrhs)
  call print_array('berr', BERR, nrhs)
  call end_test()

  ! Test 3: Upper, 3x3, 2 RHS (NRHS > 1)
  n = 3
  nrhs = 2
  nn = n*(n+1)/2
  AP = (0.0d0, 0.0d0)
  AP(1) = (4.0d0, 1.0d0)
  AP(2) = (1.0d0, 2.0d0)
  AP(3) = (5.0d0, -1.0d0)
  AP(4) = (2.0d0, -1.0d0)
  AP(5) = (3.0d0, 1.0d0)
  AP(6) = (6.0d0, 2.0d0)

  B(1,1) = (1.0d0, 0.5d0)
  B(2,1) = (2.0d0, -1.0d0)
  B(3,1) = (0.0d0, 1.0d0)
  B(1,2) = (0.5d0, 0.0d0)
  B(2,2) = (-1.0d0, 2.0d0)
  B(3,2) = (1.0d0, -0.5d0)

  AFP(1:nn) = AP(1:nn)
  call ZSPTRF('U', n, AFP, IPIV, INFO)
  if (INFO .ne. 0) stop 'ZSPTRF failed test 3'

  do j = 1, nrhs
    do i = 1, n
      Bpk(i + (j-1)*n) = B(i, j)
    end do
  end do

  X(1:n, 1:nrhs) = B(1:n, 1:nrhs)
  call ZSPTRS('U', n, nrhs, AFP, IPIV, X, NMAX, INFO)

  do j = 1, nrhs
    do i = 1, n
      Xinit(i + (j-1)*n) = X(i, j)
    end do
  end do

  call ZSPRFS('U', n, nrhs, AP, AFP, IPIV, B, NMAX, X, NMAX, &
              FERR, BERR, WORK, RWORK, INFO)

  do j = 1, nrhs
    do i = 1, n
      Xpk(i + (j-1)*n) = X(i, j)
    end do
  end do

  call begin_test('upper_3x3_2rhs')
  call print_int('info', INFO)
  call print_array('AP', AP_r, 2*nn)
  call print_array('AFP', AFP_r, 2*nn)
  call print_int_array('ipiv', IPIV, n)
  call print_array('B', Bpk_r, 2*n*nrhs)
  call print_array('Xinit', Xinit_r, 2*n*nrhs)
  call print_array('X', Xpk_r, 2*n*nrhs)
  call print_array('ferr', FERR, nrhs)
  call print_array('berr', BERR, nrhs)
  call end_test()

  ! Test 4: N=0
  FERR = 99.0d0
  BERR = 99.0d0
  call ZSPRFS('U', 0, 1, AP, AFP, IPIV, B, NMAX, X, NMAX, &
              FERR, BERR, WORK, RWORK, INFO)
  call begin_test('n0')
  call print_int('info', INFO)
  call print_array('ferr', FERR, 1)
  call print_array('berr', BERR, 1)
  call end_test()

  ! Test 5: N=1
  n = 1
  nrhs = 1
  AP(1) = (3.0d0, 1.0d0)
  B(1,1) = (1.0d0, 1.0d0)

  AFP(1) = AP(1)
  call ZSPTRF('U', n, AFP, IPIV, INFO)
  if (INFO .ne. 0) stop 'ZSPTRF failed test 5'

  do j = 1, nrhs
    do i = 1, n
      Bpk(i + (j-1)*n) = B(i, j)
    end do
  end do

  X(1:n, 1:nrhs) = B(1:n, 1:nrhs)
  call ZSPTRS('U', n, nrhs, AFP, IPIV, X, NMAX, INFO)

  do j = 1, nrhs
    do i = 1, n
      Xinit(i + (j-1)*n) = X(i, j)
    end do
  end do

  call ZSPRFS('U', n, nrhs, AP, AFP, IPIV, B, NMAX, X, NMAX, &
              FERR, BERR, WORK, RWORK, INFO)

  do j = 1, nrhs
    do i = 1, n
      Xpk(i + (j-1)*n) = X(i, j)
    end do
  end do

  call begin_test('n1')
  call print_int('info', INFO)
  call print_array('AP', AP_r, 2)
  call print_array('AFP', AFP_r, 2)
  call print_int_array('ipiv', IPIV, n)
  call print_array('B', Bpk_r, 2*n*nrhs)
  call print_array('Xinit', Xinit_r, 2*n*nrhs)
  call print_array('X', Xpk_r, 2*n*nrhs)
  call print_array('ferr', FERR, nrhs)
  call print_array('berr', BERR, nrhs)
  call end_test()

end program
