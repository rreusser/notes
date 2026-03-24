program test_dlacn2
  use test_utils
  implicit none

  double precision :: v(10), x(10), est
  integer :: isgn(10), isave(3), kase, n, i, j, iter
  integer, parameter :: lda = 10
  double precision :: a(lda,10)

  ! Test 1: 3x3 identity matrix — 1-norm is exactly 1
  n = 3
  a = 0.0d0
  do i = 1, n
    a(i, i) = 1.0d0
  end do
  kase = 0
  est = 0.0d0
  isave = 0
  iter = 0
  do
    call dlacn2(n, v, x, isgn, est, kase, isave)
    if (kase .eq. 0) exit
    iter = iter + 1
    if (iter > 20) exit
    if (kase .eq. 1) then
      ! x = A * x
      call dgemv('N', n, n, 1.0d0, a, lda, x, 1, 0.0d0, v, 1)
      x(1:n) = v(1:n)
    else
      ! x = A^T * x
      call dgemv('T', n, n, 1.0d0, a, lda, x, 1, 0.0d0, v, 1)
      x(1:n) = v(1:n)
    end if
  end do
  call begin_test('identity_3x3')
  call print_scalar('est', est)
  call print_int('iterations', iter)
  call end_test()

  ! Test 2: 4x4 matrix with known 1-norm
  ! A = [[1, 2, 3, 4], [0, 1, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1]]
  ! Column sums: 1, 3, 4, 5 => 1-norm = 5
  n = 4
  a = 0.0d0
  a(1,1) = 1.0d0; a(1,2) = 2.0d0; a(1,3) = 3.0d0; a(1,4) = 4.0d0
  a(2,2) = 1.0d0; a(3,3) = 1.0d0; a(4,4) = 1.0d0
  kase = 0
  est = 0.0d0
  isave = 0
  iter = 0
  do
    call dlacn2(n, v, x, isgn, est, kase, isave)
    if (kase .eq. 0) exit
    iter = iter + 1
    if (iter > 20) exit
    if (kase .eq. 1) then
      call dgemv('N', n, n, 1.0d0, a, lda, x, 1, 0.0d0, v, 1)
      x(1:n) = v(1:n)
    else
      call dgemv('T', n, n, 1.0d0, a, lda, x, 1, 0.0d0, v, 1)
      x(1:n) = v(1:n)
    end if
  end do
  call begin_test('upper_tri_4x4')
  call print_scalar('est', est)
  call print_int('iterations', iter)
  call end_test()

  ! Test 3: 1x1 matrix
  n = 1
  a(1,1) = 7.0d0
  kase = 0
  est = 0.0d0
  isave = 0
  iter = 0
  do
    call dlacn2(n, v, x, isgn, est, kase, isave)
    if (kase .eq. 0) exit
    iter = iter + 1
    if (iter > 20) exit
    if (kase .eq. 1) then
      x(1) = a(1,1) * x(1)
    else
      x(1) = a(1,1) * x(1)
    end if
  end do
  call begin_test('1x1')
  call print_scalar('est', est)
  call print_int('iterations', iter)
  call end_test()

  ! Test 4: 5x5 diagonal matrix — 1-norm = max diagonal
  n = 5
  a = 0.0d0
  a(1,1) = 2.0d0; a(2,2) = -5.0d0; a(3,3) = 3.0d0
  a(4,4) = 1.0d0; a(5,5) = 4.0d0
  kase = 0
  est = 0.0d0
  isave = 0
  iter = 0
  do
    call dlacn2(n, v, x, isgn, est, kase, isave)
    if (kase .eq. 0) exit
    iter = iter + 1
    if (iter > 20) exit
    if (kase .eq. 1) then
      call dgemv('N', n, n, 1.0d0, a, lda, x, 1, 0.0d0, v, 1)
      x(1:n) = v(1:n)
    else
      call dgemv('T', n, n, 1.0d0, a, lda, x, 1, 0.0d0, v, 1)
      x(1:n) = v(1:n)
    end if
  end do
  call begin_test('diag_5x5')
  call print_scalar('est', est)
  call print_int('iterations', iter)
  call end_test()

  ! Test 5: 3x3 dense matrix
  ! A = [[1, -2, 3], [4, 5, -6], [-7, 8, 9]]
  ! Column sums: |1|+|4|+|7| = 12, |2|+|5|+|8| = 15, |3|+|6|+|9| = 18
  ! 1-norm = 18
  n = 3
  a = 0.0d0
  a(1,1) = 1.0d0; a(2,1) = 4.0d0; a(3,1) = -7.0d0
  a(1,2) = -2.0d0; a(2,2) = 5.0d0; a(3,2) = 8.0d0
  a(1,3) = 3.0d0; a(2,3) = -6.0d0; a(3,3) = 9.0d0
  kase = 0
  est = 0.0d0
  isave = 0
  iter = 0
  do
    call dlacn2(n, v, x, isgn, est, kase, isave)
    if (kase .eq. 0) exit
    iter = iter + 1
    if (iter > 20) exit
    if (kase .eq. 1) then
      call dgemv('N', n, n, 1.0d0, a, lda, x, 1, 0.0d0, v, 1)
      x(1:n) = v(1:n)
    else
      call dgemv('T', n, n, 1.0d0, a, lda, x, 1, 0.0d0, v, 1)
      x(1:n) = v(1:n)
    end if
  end do
  call begin_test('dense_3x3')
  call print_scalar('est', est)
  call print_int('iterations', iter)
  call end_test()

end program
