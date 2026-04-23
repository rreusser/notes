program test_zlacn2
  use test_utils
  implicit none

  complex*16 :: v(10), x(10)
  double precision :: est, v_r(20), x_r(20)
  integer :: isave(3), kase, n, i, iter
  complex*16 :: a(10,10), tmp(10)
  equivalence (v, v_r)
  equivalence (x, x_r)

  ! Test 1: 3x3 identity matrix — 1-norm is exactly 1
  n = 3
  a = (0.0d0, 0.0d0)
  do i = 1, n
    a(i, i) = (1.0d0, 0.0d0)
  end do
  kase = 0
  est = 0.0d0
  isave = 0
  iter = 0
  do
    call zlacn2(n, v, x, est, kase, isave)
    if (kase .eq. 0) exit
    iter = iter + 1
    if (iter > 20) exit
    if (kase .eq. 1) then
      ! x = A * x
      call zgemv('N', n, n, (1.0d0,0.0d0), a, 10, x, 1, (0.0d0,0.0d0), tmp, 1)
      x(1:n) = tmp(1:n)
    else
      ! x = A**H * x
      call zgemv('C', n, n, (1.0d0,0.0d0), a, 10, x, 1, (0.0d0,0.0d0), tmp, 1)
      x(1:n) = tmp(1:n)
    end if
  end do
  call begin_test('identity_3x3')
  call print_scalar('est', est)
  call print_int('iterations', iter)
  call end_test()

  ! Test 2: 1x1 matrix with complex value
  n = 1
  a(1,1) = (3.0d0, 4.0d0)
  kase = 0
  est = 0.0d0
  isave = 0
  iter = 0
  do
    call zlacn2(n, v, x, est, kase, isave)
    if (kase .eq. 0) exit
    iter = iter + 1
    if (iter > 20) exit
    if (kase .eq. 1) then
      x(1) = a(1,1) * x(1)
    else
      x(1) = conjg(a(1,1)) * x(1)
    end if
  end do
  call begin_test('complex_1x1')
  call print_scalar('est', est)
  call print_int('iterations', iter)
  call end_test()

  ! Test 3: 4x4 diagonal with complex entries
  ! 1-norm = max column sum of abs values = max(|diag|)
  n = 4
  a = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 2.0d0)   ! |a| = sqrt(5) ~ 2.236
  a(2,2) = (3.0d0, 4.0d0)   ! |a| = 5.0
  a(3,3) = (0.0d0, 1.0d0)   ! |a| = 1.0
  a(4,4) = (2.0d0, 0.0d0)   ! |a| = 2.0
  kase = 0
  est = 0.0d0
  isave = 0
  iter = 0
  do
    call zlacn2(n, v, x, est, kase, isave)
    if (kase .eq. 0) exit
    iter = iter + 1
    if (iter > 20) exit
    if (kase .eq. 1) then
      call zgemv('N', n, n, (1.0d0,0.0d0), a, 10, x, 1, (0.0d0,0.0d0), tmp, 1)
      x(1:n) = tmp(1:n)
    else
      call zgemv('C', n, n, (1.0d0,0.0d0), a, 10, x, 1, (0.0d0,0.0d0), tmp, 1)
      x(1:n) = tmp(1:n)
    end if
  end do
  call begin_test('complex_diag_4x4')
  call print_scalar('est', est)
  call print_int('iterations', iter)
  call end_test()

  ! Test 4: 3x3 dense complex matrix
  ! A = [[1+i, 2-i, 0], [0, 3+2i, -1+i], [4-3i, 0, 2+i]]
  ! Col sums: |1+i|+|0|+|4-3i| = 1.414+0+5.0 = 6.414
  !           |2-i|+|3+2i|+|0| = 2.236+3.606+0 = 5.842
  !           |0|+|-1+i|+|2+i| = 0+1.414+2.236 = 3.650
  ! 1-norm ~ 6.414
  n = 3
  a = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 1.0d0);  a(1,2) = (2.0d0, -1.0d0); a(1,3) = (0.0d0, 0.0d0)
  a(2,1) = (0.0d0, 0.0d0);  a(2,2) = (3.0d0, 2.0d0);  a(2,3) = (-1.0d0, 1.0d0)
  a(3,1) = (4.0d0, -3.0d0); a(3,2) = (0.0d0, 0.0d0);  a(3,3) = (2.0d0, 1.0d0)
  kase = 0
  est = 0.0d0
  isave = 0
  iter = 0
  do
    call zlacn2(n, v, x, est, kase, isave)
    if (kase .eq. 0) exit
    iter = iter + 1
    if (iter > 20) exit
    if (kase .eq. 1) then
      call zgemv('N', n, n, (1.0d0,0.0d0), a, 10, x, 1, (0.0d0,0.0d0), tmp, 1)
      x(1:n) = tmp(1:n)
    else
      call zgemv('C', n, n, (1.0d0,0.0d0), a, 10, x, 1, (0.0d0,0.0d0), tmp, 1)
      x(1:n) = tmp(1:n)
    end if
  end do
  call begin_test('dense_3x3')
  call print_scalar('est', est)
  call print_int('iterations', iter)
  call end_test()

  ! Test 5: 5x5 upper triangular complex matrix
  n = 5
  a = (0.0d0, 0.0d0)
  a(1,1) = (2.0d0, 1.0d0)
  a(1,2) = (1.0d0, 0.0d0); a(2,2) = (3.0d0, -1.0d0)
  a(1,3) = (0.0d0, 1.0d0); a(2,3) = (2.0d0, 0.0d0); a(3,3) = (1.0d0, 1.0d0)
  a(1,4) = (1.0d0, 1.0d0); a(2,4) = (0.0d0, 0.0d0); a(3,4) = (1.0d0, -1.0d0); a(4,4) = (4.0d0, 0.0d0)
  a(1,5) = (0.0d0, 0.0d0); a(2,5) = (1.0d0, 2.0d0); a(3,5) = (0.0d0, 0.0d0); a(4,5) = (2.0d0, 1.0d0); a(5,5) = (1.0d0, -2.0d0)
  kase = 0
  est = 0.0d0
  isave = 0
  iter = 0
  do
    call zlacn2(n, v, x, est, kase, isave)
    if (kase .eq. 0) exit
    iter = iter + 1
    if (iter > 20) exit
    if (kase .eq. 1) then
      call zgemv('N', n, n, (1.0d0,0.0d0), a, 10, x, 1, (0.0d0,0.0d0), tmp, 1)
      x(1:n) = tmp(1:n)
    else
      call zgemv('C', n, n, (1.0d0,0.0d0), a, 10, x, 1, (0.0d0,0.0d0), tmp, 1)
      x(1:n) = tmp(1:n)
    end if
  end do
  call begin_test('upper_tri_5x5')
  call print_scalar('est', est)
  call print_int('iterations', iter)
  call end_test()

end program
