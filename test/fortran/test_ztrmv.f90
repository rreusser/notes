program test_ztrmv
  use test_utils
  implicit none
  complex*16 :: a(4, 4), x(10)
  double precision :: x_r(20), a_r(32)
  equivalence (x, x_r)
  equivalence (a, a_r)
  integer :: n

  ! Test 1: upper triangular, no transpose, non-unit diagonal, N=2
  ! A = [[2+1i, 3+1i], [0, 4+2i]]  (upper triangular, col-major)
  ! x = [1+0i, 1+1i]
  ! x := A*x
  ! x(1) = A(1,1)*x(1) + A(1,2)*x(2) = (2+1i)*(1) + (3+1i)*(1+1i)
  !      = (2+i) + (3+i+3i-1) = (2+i) + (2+4i) = 4+5i
  ! x(2) = A(2,2)*x(2) = (4+2i)*(1+i) = 4+4i+2i-2 = 2+6i
  n = 2
  a = (0.0d0, 0.0d0)
  a(1,1) = (2.0d0, 1.0d0)
  a(1,2) = (3.0d0, 1.0d0)
  a(2,2) = (4.0d0, 2.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (1.0d0, 1.0d0)
  call ztrmv('U', 'N', 'N', n, a, 4, x, 1)
  call begin_test('ztrmv_upper_no_trans')
  call print_array('x', x_r, 2*n)
  call end_test()

  ! Test 2: lower triangular, no transpose, non-unit diagonal, N=2
  ! A = [[2+1i, 0], [3+1i, 4+2i]]  (lower triangular, col-major)
  ! x = [1+0i, 1+1i]
  ! x := A*x
  ! x(1) = A(1,1)*x(1) = (2+i)*(1) = 2+i
  ! x(2) = A(2,1)*x(1) + A(2,2)*x(2) = (3+i)*(1) + (4+2i)*(1+i)
  !      = (3+i) + (4+4i+2i-2) = (3+i) + (2+6i) = 5+7i
  n = 2
  a = (0.0d0, 0.0d0)
  a(1,1) = (2.0d0, 1.0d0)
  a(2,1) = (3.0d0, 1.0d0)
  a(2,2) = (4.0d0, 2.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (1.0d0, 1.0d0)
  call ztrmv('L', 'N', 'N', n, a, 4, x, 1)
  call begin_test('ztrmv_lower_no_trans')
  call print_array('x', x_r, 2*n)
  call end_test()

  ! Test 3: upper triangular, unit diagonal, N=2
  ! A = [[1, 3+1i], [0, 1]]  (unit diagonal)
  ! x = [1+0i, 1+1i]
  ! x := A*x
  ! x(1) = x(1) + A(1,2)*x(2) = 1 + (3+i)*(1+i) = 1 + (2+4i) = 3+4i
  ! x(2) = x(2) = 1+i
  n = 2
  a = (0.0d0, 0.0d0)
  a(1,1) = (99.0d0, 99.0d0)  ! should be ignored
  a(1,2) = (3.0d0, 1.0d0)
  a(2,2) = (99.0d0, 99.0d0)  ! should be ignored
  x(1) = (1.0d0, 0.0d0)
  x(2) = (1.0d0, 1.0d0)
  call ztrmv('U', 'N', 'U', n, a, 4, x, 1)
  call begin_test('ztrmv_unit_diag')
  call print_array('x', x_r, 2*n)
  call end_test()

  ! Test 4: upper triangular, transpose (A^T), non-unit, N=2
  ! A = [[2+1i, 3+1i], [0, 4+2i]]
  ! x := A^T * x  (no conjugate)
  ! A^T = [[2+1i, 0], [3+1i, 4+2i]]
  ! x(1) = (2+i)*x(1) = (2+i)*(1) = 2+i
  ! x(2) = (3+i)*x(1) + (4+2i)*x(2) = (3+i)*1 + (4+2i)*(1+i)
  !       = (3+i) + (2+6i) = 5+7i
  n = 2
  a = (0.0d0, 0.0d0)
  a(1,1) = (2.0d0, 1.0d0)
  a(1,2) = (3.0d0, 1.0d0)
  a(2,2) = (4.0d0, 2.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (1.0d0, 1.0d0)
  call ztrmv('U', 'T', 'N', n, a, 4, x, 1)
  call begin_test('ztrmv_upper_trans')
  call print_array('x', x_r, 2*n)
  call end_test()

  ! Test 5: upper triangular, conjugate transpose (A^H), non-unit, N=2
  ! A = [[2+1i, 3+1i], [0, 4+2i]]
  ! x := A^H * x
  ! A^H = [[2-1i, 0], [3-1i, 4-2i]]
  ! x(1) = (2-i)*x(1) = (2-i)*(1) = 2-i
  ! x(2) = (3-i)*x(1) + (4-2i)*x(2) = (3-i)*1 + (4-2i)*(1+i)
  !       = (3-i) + (4+4i-2i+2) = (3-i) + (6+2i) = 9+i
  n = 2
  a = (0.0d0, 0.0d0)
  a(1,1) = (2.0d0, 1.0d0)
  a(1,2) = (3.0d0, 1.0d0)
  a(2,2) = (4.0d0, 2.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (1.0d0, 1.0d0)
  call ztrmv('U', 'C', 'N', n, a, 4, x, 1)
  call begin_test('ztrmv_upper_conjtrans')
  call print_array('x', x_r, 2*n)
  call end_test()

  ! Test 6: N=0 quick return
  x(1) = (5.0d0, 5.0d0)
  call ztrmv('U', 'N', 'N', 0, a, 4, x, 1)
  call begin_test('ztrmv_n_zero')
  call print_array('x', x_r, 2)
  call end_test()

  ! Test 7: N=1, upper, non-unit
  n = 1
  a(1,1) = (3.0d0, 2.0d0)
  x(1) = (2.0d0, 1.0d0)
  call ztrmv('U', 'N', 'N', n, a, 4, x, 1)
  call begin_test('ztrmv_n_one')
  call print_array('x', x_r, 2)
  call end_test()

  ! Test 8: non-unit stride incx=2, upper, no transpose, N=2
  n = 2
  a = (0.0d0, 0.0d0)
  a(1,1) = (2.0d0, 0.0d0)
  a(1,2) = (1.0d0, 1.0d0)
  a(2,2) = (3.0d0, 0.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (99.0d0, 99.0d0)
  x(3) = (0.0d0, 1.0d0)
  call ztrmv('U', 'N', 'N', n, a, 4, x, 2)
  call begin_test('ztrmv_stride')
  call print_array('x', x_r, 6)
  call end_test()

  ! Test 9: lower, conj-trans, non-unit, N=3
  n = 3
  a = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 1.0d0)
  a(2,1) = (2.0d0, 1.0d0)
  a(3,1) = (3.0d0, 1.0d0)
  a(2,2) = (4.0d0, 2.0d0)
  a(3,2) = (5.0d0, 2.0d0)
  a(3,3) = (6.0d0, 3.0d0)
  x(1) = (1.0d0, 0.0d0)
  x(2) = (0.0d0, 1.0d0)
  x(3) = (1.0d0, 1.0d0)
  call ztrmv('L', 'C', 'N', n, a, 4, x, 1)
  call begin_test('ztrmv_lower_conjtrans')
  call print_array('x', x_r, 2*n)
  call end_test()

end program
