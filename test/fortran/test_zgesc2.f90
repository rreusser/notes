program test_zgesc2
  use test_utils
  implicit none

  integer, parameter :: NMAX = 4
  complex*16 :: A(NMAX, NMAX), RHS(NMAX)
  complex*16 :: Apk(NMAX*NMAX), RHSpk(NMAX)
  double precision :: Apk_r(2*NMAX*NMAX), RHS_r(2*NMAX)
  equivalence (Apk, Apk_r)
  equivalence (RHSpk, RHS_r)
  double precision :: SCALE
  integer :: IPIV(NMAX), JPIV(NMAX), INFO
  integer :: N, i, j

  ! Test 1: 2x2 complex system
  N = 2
  A = (0.0d0, 0.0d0)
  A(1,1) = (4.0d0, 1.0d0); A(1,2) = (2.0d0, -1.0d0)
  A(2,1) = (1.0d0, 2.0d0); A(2,2) = (3.0d0, 0.5d0)
  RHS(1) = (10.0d0, 3.0d0); RHS(2) = (7.0d0, 4.0d0)
  call ZGETC2(N, A, NMAX, IPIV, JPIV, INFO)
  call ZGESC2(N, A, NMAX, RHS, IPIV, JPIV, SCALE)
  ! Pack the N-by-N submatrix for output
  do j = 1, N
    do i = 1, N
      Apk(i + (j-1)*N) = A(i, j)
    end do
  end do
  do i = 1, N
    RHSpk(i) = RHS(i)
  end do
  call begin_test('basic_2x2')
  call print_scalar('scale', SCALE)
  call print_array('rhs', RHS_r, 2*N)
  call print_array('A', Apk_r, 2*N*N)
  call print_int_array('ipiv', IPIV, N)
  call print_int_array('jpiv', JPIV, N)
  call end_test()

  ! Test 2: 3x3 complex system
  N = 3
  A = (0.0d0, 0.0d0)
  A(1,1) = (2.0d0, 1.0d0); A(1,2) = (1.0d0, 0.5d0); A(1,3) = (1.0d0, -1.0d0)
  A(2,1) = (4.0d0, -1.0d0); A(2,2) = (3.0d0, 2.0d0); A(2,3) = (3.0d0, 0.0d0)
  A(3,1) = (8.0d0, 0.0d0); A(3,2) = (7.0d0, -0.5d0); A(3,3) = (9.0d0, 1.0d0)
  RHS(1) = (4.0d0, 0.5d0); RHS(2) = (10.0d0, 1.0d0); RHS(3) = (24.0d0, 0.5d0)
  call ZGETC2(N, A, NMAX, IPIV, JPIV, INFO)
  call ZGESC2(N, A, NMAX, RHS, IPIV, JPIV, SCALE)
  do j = 1, N
    do i = 1, N
      Apk(i + (j-1)*N) = A(i, j)
    end do
  end do
  do i = 1, N
    RHSpk(i) = RHS(i)
  end do
  call begin_test('basic_3x3')
  call print_scalar('scale', SCALE)
  call print_array('rhs', RHS_r, 2*N)
  call print_array('A', Apk_r, 2*N*N)
  call print_int_array('ipiv', IPIV, N)
  call print_int_array('jpiv', JPIV, N)
  call end_test()

  ! Test 3: 4x4 complex system (diagonally dominant)
  N = 4
  A = (0.0d0, 0.0d0)
  A(1,1) = (10.0d0, 1.0d0); A(1,2) = (1.0d0, 0.5d0)
  A(1,3) = (2.0d0, -1.0d0); A(1,4) = (0.5d0, 0.0d0)
  A(2,1) = (1.0d0, -0.5d0); A(2,2) = (8.0d0, 2.0d0)
  A(2,3) = (1.0d0, 0.0d0); A(2,4) = (1.5d0, 1.0d0)
  A(3,1) = (2.0d0, 1.0d0); A(3,2) = (1.0d0, 0.0d0)
  A(3,3) = (12.0d0, -1.0d0); A(3,4) = (2.0d0, 0.5d0)
  A(4,1) = (0.5d0, 0.0d0); A(4,2) = (1.5d0, -1.0d0)
  A(4,3) = (2.0d0, 0.5d0); A(4,4) = (9.0d0, 3.0d0)
  RHS(1) = (13.5d0, 0.5d0); RHS(2) = (11.5d0, 2.5d0)
  RHS(3) = (17.0d0, -0.5d0); RHS(4) = (13.0d0, 2.5d0)
  call ZGETC2(N, A, NMAX, IPIV, JPIV, INFO)
  call ZGESC2(N, A, NMAX, RHS, IPIV, JPIV, SCALE)
  do j = 1, N
    do i = 1, N
      Apk(i + (j-1)*N) = A(i, j)
    end do
  end do
  do i = 1, N
    RHSpk(i) = RHS(i)
  end do
  call begin_test('basic_4x4')
  call print_scalar('scale', SCALE)
  call print_array('rhs', RHS_r, 2*N)
  call print_array('A', Apk_r, 2*N*N)
  call print_int_array('ipiv', IPIV, N)
  call print_int_array('jpiv', JPIV, N)
  call end_test()

  ! Test 4: N=1 (edge case)
  N = 1
  A = (0.0d0, 0.0d0)
  A(1,1) = (5.0d0, 2.0d0)
  RHS(1) = (15.0d0, 6.0d0)
  call ZGETC2(N, A, NMAX, IPIV, JPIV, INFO)
  call ZGESC2(N, A, NMAX, RHS, IPIV, JPIV, SCALE)
  RHSpk(1) = RHS(1)
  call begin_test('n_equals_1')
  call print_scalar('scale', SCALE)
  call print_array('rhs', RHS_r, 2*N)
  call end_test()

end program
