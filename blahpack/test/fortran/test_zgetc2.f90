program test_zgetc2
  use test_utils
  implicit none
  integer, parameter :: NMAX = 4

  complex*16 :: A(NMAX, NMAX), Apk(NMAX*NMAX)
  double precision :: Apk_r(2*NMAX*NMAX)
  equivalence (Apk, Apk_r)

  integer :: IPIV(NMAX), JPIV(NMAX), INFO
  integer :: i, j, n

  ! Test 1: 2x2 complex matrix
  n = 2
  A(1,1) = (1.0d0, 2.0d0); A(1,2) = (3.0d0, 4.0d0)
  A(2,1) = (5.0d0, 6.0d0); A(2,2) = (7.0d0, 8.0d0)
  call ZGETC2(n, A, NMAX, IPIV, JPIV, INFO)
  ! Pack the n-by-n submatrix into contiguous array
  do j = 1, n
    do i = 1, n
      Apk(i + (j-1)*n) = A(i, j)
    end do
  end do
  call begin_test('basic_2x2')
  call print_int('info', INFO)
  call print_array('A', Apk_r, 2*n*n)
  call print_int_array('ipiv', IPIV, n)
  call print_int_array('jpiv', JPIV, n)
  call end_test()

  ! Test 2: 3x3 complex matrix
  n = 3
  A(1,1) = (1.0d0, 0.5d0); A(1,2) = (2.0d0, 1.0d0); A(1,3) = (0.5d0, 3.0d0)
  A(2,1) = (4.0d0, 2.0d0); A(2,2) = (5.0d0, 0.0d0); A(2,3) = (6.0d0, 1.5d0)
  A(3,1) = (7.0d0, 1.0d0); A(3,2) = (8.0d0, 3.0d0); A(3,3) = (10.0d0, 2.0d0)
  call ZGETC2(n, A, NMAX, IPIV, JPIV, INFO)
  do j = 1, n
    do i = 1, n
      Apk(i + (j-1)*n) = A(i, j)
    end do
  end do
  call begin_test('basic_3x3')
  call print_int('info', INFO)
  call print_array('A', Apk_r, 2*n*n)
  call print_int_array('ipiv', IPIV, n)
  call print_int_array('jpiv', JPIV, n)
  call end_test()

  ! Test 3: 4x4 complex matrix with complete pivoting needed
  n = 4
  A(1,1) = (0.1d0, 0.2d0); A(1,2) = (0.3d0, 0.4d0)
  A(1,3) = (0.5d0, 0.6d0); A(1,4) = (10.0d0, 1.0d0)
  A(2,1) = (0.7d0, 0.8d0); A(2,2) = (0.9d0, 1.0d0)
  A(2,3) = (1.1d0, 1.2d0); A(2,4) = (1.3d0, 1.4d0)
  A(3,1) = (1.5d0, 1.6d0); A(3,2) = (1.7d0, 1.8d0)
  A(3,3) = (1.9d0, 2.0d0); A(3,4) = (2.1d0, 2.2d0)
  A(4,1) = (2.3d0, 2.4d0); A(4,2) = (2.5d0, 2.6d0)
  A(4,3) = (2.7d0, 2.8d0); A(4,4) = (2.9d0, 3.0d0)
  call ZGETC2(n, A, NMAX, IPIV, JPIV, INFO)
  do j = 1, n
    do i = 1, n
      Apk(i + (j-1)*n) = A(i, j)
    end do
  end do
  call begin_test('basic_4x4')
  call print_int('info', INFO)
  call print_array('A', Apk_r, 2*n*n)
  call print_int_array('ipiv', IPIV, n)
  call print_int_array('jpiv', JPIV, n)
  call end_test()

  ! Test 4: N=1
  n = 1
  A(1,1) = (5.0d0, 3.0d0)
  call ZGETC2(n, A, NMAX, IPIV, JPIV, INFO)
  do j = 1, n
    do i = 1, n
      Apk(i + (j-1)*n) = A(i, j)
    end do
  end do
  call begin_test('n_equals_1')
  call print_int('info', INFO)
  call print_array('A', Apk_r, 2*n*n)
  call print_int_array('ipiv', IPIV, n)
  call print_int_array('jpiv', JPIV, n)
  call end_test()

  ! Test 5: Near-singular complex matrix
  n = 2
  A(1,1) = (1.0d-200, 0.0d0); A(1,2) = (1.0d0, 0.0d0)
  A(2,1) = (0.0d0, 1.0d0);    A(2,2) = (1.0d0, 1.0d0)
  call ZGETC2(n, A, NMAX, IPIV, JPIV, INFO)
  do j = 1, n
    do i = 1, n
      Apk(i + (j-1)*n) = A(i, j)
    end do
  end do
  call begin_test('near_singular')
  call print_int('info', INFO)
  call print_array('A', Apk_r, 2*n*n)
  call print_int_array('ipiv', IPIV, n)
  call print_int_array('jpiv', JPIV, n)
  call end_test()

end program
