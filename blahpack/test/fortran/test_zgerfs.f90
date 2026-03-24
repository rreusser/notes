program test_zgerfs
  use test_utils
  implicit none
  double precision :: a_r(200), af_r(200), b_r(200), x_r(200), work_r(400)
  complex*16 :: a(100), af(100), b(100), x(100), work(200)
  equivalence (a, a_r)
  equivalence (af, af_r)
  equivalence (b, b_r)
  equivalence (x, x_r)
  equivalence (work, work_r)
  integer :: ipiv(10), info, n, nrhs, i, j
  double precision :: ferr(10), berr(10), rwork(200)

  ! Test 1: TRANS='N', 3x3 well-conditioned, single RHS
  ! A = [(4+1i) (1+0.5i) (0.5+0.1i);
  !      (1-1i) (3+2i)   (1+0.3i);
  !      (0.5+0.2i) (1-0.5i) (2+1i)]
  ! exact x = [1+0i; 1+0i; 1+0i], b = A*x
  n = 3; nrhs = 1
  a(1) = (4.0d0, 1.0d0);   a(2) = (1.0d0, -1.0d0);  a(3) = (0.5d0, 0.2d0)
  a(4) = (1.0d0, 0.5d0);   a(5) = (3.0d0, 2.0d0);   a(6) = (1.0d0, -0.5d0)
  a(7) = (0.5d0, 0.1d0);   a(8) = (1.0d0, 0.3d0);   a(9) = (2.0d0, 1.0d0)
  ! b = A * [1;1;1]
  do i = 1, n
    b(i) = (0.0d0, 0.0d0)
    do j = 1, n
      b(i) = b(i) + a(i + (j-1)*n)
    end do
  end do
  ! Factor A into AF
  af(1:n*n) = a(1:n*n)
  call zgetrf(n, n, af, n, ipiv, info)
  ! Solve for x
  x(1:n) = b(1:n)
  call zgetrs('N', n, nrhs, af, n, ipiv, x, n, info)
  ! Now refine
  call zgerfs('N', n, nrhs, a, n, af, n, ipiv, b, n, x, n, &
              ferr, berr, work, rwork, info)
  call begin_test('trans_N')
  call print_array('x', x_r, 2*n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 2: TRANS='C' (conjugate transpose), same matrix
  n = 3; nrhs = 1
  a(1) = (4.0d0, 1.0d0);   a(2) = (1.0d0, -1.0d0);  a(3) = (0.5d0, 0.2d0)
  a(4) = (1.0d0, 0.5d0);   a(5) = (3.0d0, 2.0d0);   a(6) = (1.0d0, -0.5d0)
  a(7) = (0.5d0, 0.1d0);   a(8) = (1.0d0, 0.3d0);   a(9) = (2.0d0, 1.0d0)
  ! b = A^H * [1;1;1] = conjg(A)^T * [1;1;1] = sum of conj(col k)
  do i = 1, n
    b(i) = (0.0d0, 0.0d0)
    do j = 1, n
      b(i) = b(i) + conjg(a(j + (i-1)*n))
    end do
  end do
  af(1:n*n) = a(1:n*n)
  call zgetrf(n, n, af, n, ipiv, info)
  x(1:n) = b(1:n)
  call zgetrs('C', n, nrhs, af, n, ipiv, x, n, info)
  call zgerfs('C', n, nrhs, a, n, af, n, ipiv, b, n, x, n, &
              ferr, berr, work, rwork, info)
  call begin_test('trans_C')
  call print_array('x', x_r, 2*n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 3: N=0 quick return
  call zgerfs('N', 0, 1, a, 1, af, 1, ipiv, b, 1, x, 1, &
              ferr, berr, work, rwork, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 4: NRHS=0 quick return
  call zgerfs('N', 3, 0, a, 3, af, 3, ipiv, b, 3, x, 3, &
              ferr, berr, work, rwork, info)
  call begin_test('nrhs_zero')
  call print_int('info', info)
  call end_test()

  ! Test 5: Multiple RHS (nrhs=2)
  n = 3; nrhs = 2
  a(1) = (4.0d0, 1.0d0);   a(2) = (1.0d0, -1.0d0);  a(3) = (0.5d0, 0.2d0)
  a(4) = (1.0d0, 0.5d0);   a(5) = (3.0d0, 2.0d0);   a(6) = (1.0d0, -0.5d0)
  a(7) = (0.5d0, 0.1d0);   a(8) = (1.0d0, 0.3d0);   a(9) = (2.0d0, 1.0d0)
  ! RHS 1: b = A * [1;1;1]
  do i = 1, n
    b(i) = (0.0d0, 0.0d0)
    do j = 1, n
      b(i) = b(i) + a(i + (j-1)*n)
    end do
  end do
  ! RHS 2: b = A * [1+i; 2-i; 0.5+0.5i]
  do i = 1, n
    b(n+i) = a(i)*dcmplx(1.0d0,1.0d0) + a(n+i)*dcmplx(2.0d0,-1.0d0) + &
             a(2*n+i)*dcmplx(0.5d0,0.5d0)
  end do
  af(1:n*n) = a(1:n*n)
  call zgetrf(n, n, af, n, ipiv, info)
  x(1:n*nrhs) = b(1:n*nrhs)
  call zgetrs('N', n, nrhs, af, n, ipiv, x, n, info)
  call zgerfs('N', n, nrhs, a, n, af, n, ipiv, b, n, x, n, &
              ferr, berr, work, rwork, info)
  call begin_test('multi_rhs')
  call print_array('x', x_r, 2*n*nrhs)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

  ! Test 6: TRANS='T' (transpose, not conjugate)
  n = 3; nrhs = 1
  a(1) = (4.0d0, 1.0d0);   a(2) = (1.0d0, -1.0d0);  a(3) = (0.5d0, 0.2d0)
  a(4) = (1.0d0, 0.5d0);   a(5) = (3.0d0, 2.0d0);   a(6) = (1.0d0, -0.5d0)
  a(7) = (0.5d0, 0.1d0);   a(8) = (1.0d0, 0.3d0);   a(9) = (2.0d0, 1.0d0)
  ! b = A^T * [1;1;1] = sum of each column of A (not conjugated)
  do i = 1, n
    b(i) = (0.0d0, 0.0d0)
    do j = 1, n
      b(i) = b(i) + a(j + (i-1)*n)
    end do
  end do
  af(1:n*n) = a(1:n*n)
  call zgetrf(n, n, af, n, ipiv, info)
  x(1:n) = b(1:n)
  call zgetrs('T', n, nrhs, af, n, ipiv, x, n, info)
  call zgerfs('T', n, nrhs, a, n, af, n, ipiv, b, n, x, n, &
              ferr, berr, work, rwork, info)
  call begin_test('trans_T')
  call print_array('x', x_r, 2*n)
  call print_array('ferr', ferr, nrhs)
  call print_array('berr', berr, nrhs)
  call print_int('info', info)
  call end_test()

end program
