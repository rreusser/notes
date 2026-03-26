program test_zsyrfs
  use test_utils
  implicit none

  integer, parameter :: NMAX = 4, LWMAX = 256
  complex*16 :: A(NMAX, NMAX), AF(NMAX, NMAX), B(NMAX, 1), X(NMAX, 1)
  complex*16 :: WORK(LWMAX)
  double precision :: FERR(1), BERR(1), RWORK(NMAX)
  double precision :: X_r(2*NMAX), AF_r(2*NMAX*NMAX), B_r(2*NMAX), A_r(2*NMAX*NMAX)
  equivalence (X, X_r)
  complex*16 :: AFflat(NMAX*NMAX)
  double precision :: AFflat_r(2*NMAX*NMAX)
  equivalence (AFflat, AFflat_r)
  complex*16 :: Aflat(NMAX*NMAX)
  double precision :: Aflat_r(2*NMAX*NMAX)
  equivalence (Aflat, Aflat_r)
  complex*16 :: Bflat(NMAX)
  double precision :: Bflat_r(2*NMAX)
  equivalence (Bflat, Bflat_r)
  complex*16 :: Xinit(NMAX)
  double precision :: Xinit_r(2*NMAX)
  equivalence (Xinit, Xinit_r)
  integer :: IPIV(NMAX), INFO, n, nrhs, i, j

  ! Test 1: Upper, 4x4 symmetric (not Hermitian), 1 RHS
  n = 4
  nrhs = 1
  A = (0.0d0, 0.0d0)
  A(1,1) = (2.0d0, 1.0d0)
  A(1,2) = (1.0d0, 2.0d0)
  A(1,3) = (3.0d0, -1.0d0)
  A(1,4) = (0.5d0, 0.5d0)
  A(2,2) = (5.0d0, -1.0d0)
  A(2,3) = (2.0d0, 1.0d0)
  A(2,4) = (1.0d0, -2.0d0)
  A(3,3) = (4.0d0, 2.0d0)
  A(3,4) = (3.0d0, 0.0d0)
  A(4,4) = (6.0d0, -3.0d0)

  ! Pack A column-major flat
  do j = 1, n
    do i = 1, n
      Aflat((j-1)*n+i) = A(i, j)
    end do
  end do

  AF = A
  call ZSYTRF('U', n, AF, NMAX, IPIV, WORK, LWMAX, INFO)

  ! Pack AF column-major flat
  do j = 1, n
    do i = 1, n
      AFflat((j-1)*n+i) = AF(i, j)
    end do
  end do

  B(1,1) = (1.0d0, 0.0d0)
  B(2,1) = (2.0d0, 1.0d0)
  B(3,1) = (-1.0d0, 3.0d0)
  B(4,1) = (0.5d0, -0.5d0)
  do i = 1, n
    Bflat(i) = B(i, 1)
  end do

  X = B
  call ZSYTRS('U', n, nrhs, AF, NMAX, IPIV, X, NMAX, INFO)
  ! Save initial X before refinement
  do i = 1, n
    Xinit(i) = X(i, 1)
  end do

  call ZSYRFS('U', n, nrhs, A, NMAX, AF, NMAX, IPIV, B, NMAX, X, NMAX, &
              FERR, BERR, WORK, RWORK, INFO)
  call begin_test('upper_4x4')
  call print_int('info', INFO)
  call print_array('A', Aflat_r, 2*n*n)
  call print_array('AF', AFflat_r, 2*n*n)
  call print_int_array('ipiv', IPIV, n)
  call print_array('B', Bflat_r, 2*n)
  call print_array('Xinit', Xinit_r, 2*n)
  call print_array('X', X_r, 2*n)
  call print_array('ferr', FERR, 1)
  call print_array('berr', BERR, 1)
  call end_test()

  ! Test 2: Lower, 4x4 symmetric, 1 RHS
  n = 4
  nrhs = 1
  A = (0.0d0, 0.0d0)
  A(1,1) = (2.0d0, 1.0d0)
  A(2,1) = (1.0d0, 2.0d0)
  A(2,2) = (5.0d0, -1.0d0)
  A(3,1) = (3.0d0, -1.0d0)
  A(3,2) = (2.0d0, 1.0d0)
  A(3,3) = (4.0d0, 2.0d0)
  A(4,1) = (0.5d0, 0.5d0)
  A(4,2) = (1.0d0, -2.0d0)
  A(4,3) = (3.0d0, 0.0d0)
  A(4,4) = (6.0d0, -3.0d0)

  do j = 1, n
    do i = 1, n
      Aflat((j-1)*n+i) = A(i, j)
    end do
  end do

  AF = A
  call ZSYTRF('L', n, AF, NMAX, IPIV, WORK, LWMAX, INFO)

  do j = 1, n
    do i = 1, n
      AFflat((j-1)*n+i) = AF(i, j)
    end do
  end do

  B(1,1) = (1.0d0, 0.0d0)
  B(2,1) = (2.0d0, 1.0d0)
  B(3,1) = (-1.0d0, 3.0d0)
  B(4,1) = (0.5d0, -0.5d0)
  do i = 1, n
    Bflat(i) = B(i, 1)
  end do

  X = B
  call ZSYTRS('L', n, nrhs, AF, NMAX, IPIV, X, NMAX, INFO)
  do i = 1, n
    Xinit(i) = X(i, 1)
  end do

  call ZSYRFS('L', n, nrhs, A, NMAX, AF, NMAX, IPIV, B, NMAX, X, NMAX, &
              FERR, BERR, WORK, RWORK, INFO)
  call begin_test('lower_4x4')
  call print_int('info', INFO)
  call print_array('A', Aflat_r, 2*n*n)
  call print_array('AF', AFflat_r, 2*n*n)
  call print_int_array('ipiv', IPIV, n)
  call print_array('B', Bflat_r, 2*n)
  call print_array('Xinit', Xinit_r, 2*n)
  call print_array('X', X_r, 2*n)
  call print_array('ferr', FERR, 1)
  call print_array('berr', BERR, 1)
  call end_test()

  ! Test 3: N=0
  FERR = 99.0d0
  BERR = 99.0d0
  call ZSYRFS('U', 0, 1, A, NMAX, AF, NMAX, IPIV, B, NMAX, X, NMAX, &
              FERR, BERR, WORK, RWORK, INFO)
  call begin_test('n0')
  call print_int('info', INFO)
  call print_array('ferr', FERR, 1)
  call print_array('berr', BERR, 1)
  call end_test()

end program
