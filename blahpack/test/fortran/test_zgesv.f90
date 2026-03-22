program test_zgesv
  use test_utils
  implicit none
  double precision :: a_r(200), b_r(200), a_orig_r(200)
  complex*16 :: a(100), b(100), a_orig(100)
  equivalence (a, a_r)
  equivalence (b, b_r)
  equivalence (a_orig, a_orig_r)
  integer :: ipiv(10), info, i, j, n

  ! Test 1: 3x3 complex system, single RHS
  ! A = [(2+1i) (1+0.5i) (0.5+0.1i);
  !      (1-1i) (4+2i)   (1+0.3i);
  !      (0.5+0.2i) (1-0.5i) (3+1i)]
  ! x = [1+0i; 1+0i; 1+0i]
  ! b = A*x = sum of each row
  n = 3
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  a(1) = (2.0d0, 1.0d0);   a(2) = (1.0d0, -1.0d0);  a(3) = (0.5d0, 0.2d0)
  a(4) = (1.0d0, 0.5d0);   a(5) = (4.0d0, 2.0d0);   a(6) = (1.0d0, -0.5d0)
  a(7) = (0.5d0, 0.1d0);   a(8) = (1.0d0, 0.3d0);   a(9) = (3.0d0, 1.0d0)
  ! Compute b = A * [1;1;1]
  do i = 1, n
    b(i) = (0.0d0, 0.0d0)
    do j = 1, n
      b(i) = b(i) + a(i + (j-1)*n)
    end do
  end do
  a_orig(1:n*n) = a(1:n*n)
  call zgesv(n, 1, a, n, ipiv, b, n, info)
  call begin_test('solve_3x3')
  call print_array('x', b_r, 2*n)
  call print_int_array('ipiv', ipiv, n)
  call print_int('info', info)
  call end_test()

  ! Test 2: multiple RHS (NRHS=2), 2x2 system
  ! A = [(3+1i) (1-1i); (2+0.5i) (5+2i)]
  ! x1 = [1+0i; 0+0i], x2 = [0+1i; 1+0i]
  n = 2
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  a(1) = (3.0d0, 1.0d0);  a(2) = (2.0d0, 0.5d0)
  a(3) = (1.0d0, -1.0d0); a(4) = (5.0d0, 2.0d0)
  a_orig(1:n*n) = a(1:n*n)
  ! b1 = A*[1;0] = col1 = [(3+1i); (2+0.5i)]
  b(1) = (3.0d0, 1.0d0); b(2) = (2.0d0, 0.5d0)
  ! b2 = A*[i;1] = (3+1i)*i + (1-1i)*1, (2+0.5i)*i + (5+2i)*1
  !     = (-1+3i)+(1-1i), (-0.5+2i)+(5+2i) = (0+2i), (4.5+4i)
  b(3) = (0.0d0, 2.0d0); b(4) = (4.5d0, 4.0d0)
  call zgesv(n, 2, a, n, ipiv, b, n, info)
  call begin_test('multi_rhs')
  call print_array('x', b_r, 2*n*2)
  call print_int_array('ipiv', ipiv, n)
  call print_int('info', info)
  call end_test()

  ! Test 3: singular matrix (info > 0)
  n = 3
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  a(1) = (1.0d0, 0.0d0); a(2) = (2.0d0, 0.0d0); a(3) = (3.0d0, 0.0d0)
  a(4) = (2.0d0, 0.0d0); a(5) = (4.0d0, 0.0d0); a(6) = (6.0d0, 0.0d0)
  a(7) = (3.0d0, 0.0d0); a(8) = (6.0d0, 0.0d0); a(9) = (9.0d0, 0.0d0)
  b(1) = (1.0d0, 0.0d0); b(2) = (2.0d0, 0.0d0); b(3) = (3.0d0, 0.0d0)
  call zgesv(n, 1, a, n, ipiv, b, n, info)
  call begin_test('singular')
  call print_int('info', info)
  call end_test()

  ! Test 4: N=0 quick return
  call zgesv(0, 1, a, 1, ipiv, b, 1, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 5: NRHS=0 quick return
  a(1) = (5.0d0, 1.0d0)
  call zgesv(1, 0, a, 1, ipiv, b, 1, info)
  call begin_test('nrhs_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: 1x1 system: (5+2i)*x = (10+4i) => x = 2+0i
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  a(1) = (5.0d0, 2.0d0)
  b(1) = (10.0d0, 4.0d0)
  call zgesv(1, 1, a, 1, ipiv, b, 1, info)
  call begin_test('1x1')
  call print_array('x', b_r, 2)
  call print_int('info', info)
  call end_test()

  ! Test 7: 4x4 well-conditioned complex system
  ! A is diagonally dominant, x = [1+1i; 2-1i; -1+2i; 3+0i]
  n = 4
  a = (0.0d0, 0.0d0); b = (0.0d0, 0.0d0)
  a(1) = (10.0d0, 1.0d0); a(2) = (1.0d0, 2.0d0);  a(3) = (2.0d0, -1.0d0); a(4) = (3.0d0, 0.5d0)
  a(5) = (1.0d0, -1.0d0); a(6) = (12.0d0, 2.0d0);  a(7) = (1.0d0, 3.0d0);  a(8) = (2.0d0, -0.5d0)
  a(9) = (2.0d0, 0.5d0);  a(10) = (3.0d0, -1.0d0); a(11) = (15.0d0, 1.0d0); a(12) = (1.0d0, 2.0d0)
  a(13) = (1.0d0, 1.0d0); a(14) = (2.0d0, 0.5d0);  a(15) = (3.0d0, -2.0d0); a(16) = (20.0d0, 3.0d0)
  a_orig(1:n*n) = a(1:n*n)
  ! Compute b = A * x where x = [1+1i; 2-1i; -1+2i; 3+0i]
  b(1) = a(1)*(1.0d0,1.0d0) + a(5)*(2.0d0,-1.0d0) + a(9)*(-1.0d0,2.0d0) + a(13)*(3.0d0,0.0d0)
  b(2) = a(2)*(1.0d0,1.0d0) + a(6)*(2.0d0,-1.0d0) + a(10)*(-1.0d0,2.0d0) + a(14)*(3.0d0,0.0d0)
  b(3) = a(3)*(1.0d0,1.0d0) + a(7)*(2.0d0,-1.0d0) + a(11)*(-1.0d0,2.0d0) + a(15)*(3.0d0,0.0d0)
  b(4) = a(4)*(1.0d0,1.0d0) + a(8)*(2.0d0,-1.0d0) + a(12)*(-1.0d0,2.0d0) + a(16)*(3.0d0,0.0d0)
  call zgesv(n, 1, a, n, ipiv, b, n, info)
  call begin_test('4x4')
  call print_array('x', b_r, 2*n)
  call print_int_array('ipiv', ipiv, n)
  call print_int('info', info)
  call end_test()

end program
