program test_zsysvx
  use test_utils
  implicit none

  integer, parameter :: NMAX = 4, LWMAX = 256
  complex*16 :: A(NMAX, NMAX), AF(NMAX, NMAX), B(NMAX, 2), X(NMAX, 2)
  complex*16 :: WORK(LWMAX)
  double precision :: FERR(2), BERR(2), RWORK(NMAX), RCOND
  ! Flat output packing
  complex*16 :: Aflat(NMAX*NMAX), Bflat(NMAX*2), Xflat(NMAX*2)
  double precision :: Aflat_r(2*NMAX*NMAX), Bflat_r(2*NMAX*2), Xflat_r(2*NMAX*2)
  equivalence (Aflat, Aflat_r)
  equivalence (Bflat, Bflat_r)
  equivalence (Xflat, Xflat_r)
  integer :: IPIV(NMAX), INFO, n, nrhs, i, j

  ! Test 1: Not-factored, upper, 4x4, 1 RHS
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

  do j = 1, n
    do i = 1, n
      Aflat((j-1)*n+i) = A(i, j)
    end do
  end do

  B(1,1) = (1.0d0, 0.0d0)
  B(2,1) = (2.0d0, 1.0d0)
  B(3,1) = (-1.0d0, 3.0d0)
  B(4,1) = (0.5d0, -0.5d0)
  do i = 1, n
    Bflat(i) = B(i, 1)
  end do

  AF = (0.0d0, 0.0d0)
  IPIV = 0
  call ZSYSVX('N', 'U', n, nrhs, A, NMAX, AF, NMAX, IPIV, B, NMAX, &
               X, NMAX, RCOND, FERR, BERR, WORK, LWMAX, RWORK, INFO)
  do i = 1, n
    Xflat(i) = X(i, 1)
  end do
  call begin_test('upper_4x4_1rhs')
  call print_int('n', n)
  call print_int('info', INFO)
  call print_array('A', Aflat_r, 2*n*n)
  call print_array('B', Bflat_r, 2*n)
  call print_array('X', Xflat_r, 2*n)
  call print_scalar('rcond', RCOND)
  call print_array('ferr', FERR, nrhs)
  call print_array('berr', BERR, nrhs)
  call end_test()

  ! Test 2: Not-factored, lower, 4x4, 2 RHS
  n = 4
  nrhs = 2
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

  B(1,1) = (1.0d0, 0.0d0)
  B(2,1) = (2.0d0, 1.0d0)
  B(3,1) = (-1.0d0, 3.0d0)
  B(4,1) = (0.5d0, -0.5d0)
  B(1,2) = (0.0d0, 1.0d0)
  B(2,2) = (1.0d0, 0.0d0)
  B(3,2) = (2.0d0, -1.0d0)
  B(4,2) = (-1.0d0, 2.0d0)
  do j = 1, nrhs
    do i = 1, n
      Bflat((j-1)*n+i) = B(i, j)
    end do
  end do

  AF = (0.0d0, 0.0d0)
  IPIV = 0
  call ZSYSVX('N', 'L', n, nrhs, A, NMAX, AF, NMAX, IPIV, B, NMAX, &
               X, NMAX, RCOND, FERR, BERR, WORK, LWMAX, RWORK, INFO)
  do j = 1, nrhs
    do i = 1, n
      Xflat((j-1)*n+i) = X(i, j)
    end do
  end do
  call begin_test('lower_4x4_2rhs')
  call print_int('n', n)
  call print_int('info', INFO)
  call print_array('A', Aflat_r, 2*n*n)
  call print_array('B', Bflat_r, 2*n*nrhs)
  call print_array('X', Xflat_r, 2*n*nrhs)
  call print_scalar('rcond', RCOND)
  call print_array('ferr', FERR, nrhs)
  call print_array('berr', BERR, nrhs)
  call end_test()

  ! Test 3: N=0
  call ZSYSVX('N', 'U', 0, 1, A, NMAX, AF, NMAX, IPIV, B, NMAX, &
               X, NMAX, RCOND, FERR, BERR, WORK, LWMAX, RWORK, INFO)
  call begin_test('n0')
  call print_int('info', INFO)
  call end_test()

end program
