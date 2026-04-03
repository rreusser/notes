program test_zspsv
  use test_utils
  implicit none
  complex*16 :: ap(100), b(100), ap_save(100)
  double precision :: ap_r(200), b_r(200)
  equivalence (ap, ap_r)
  equivalence (b, b_r)
  integer :: ipiv(10), info, i, j, n, nrhs

  ! Test 1: 3x3 complex symmetric, UPLO='U', NRHS=1
  ! A = [ (4,1)   (2,-1)   (1,2);
  !       (2,-1)  (5,0.5) (3,-1);
  !       (1,2)   (3,-1)   (6,1) ]
  ! Symmetric (NOT Hermitian): A(i,j) = A(j,i)
  ! Upper packed (column-major): A(1,1), A(1,2), A(2,2), A(1,3), A(2,3), A(3,3)
  ! x = [1+0i; 1+0i; 1+0i]
  ! b = A*x = sum of each row (using symmetry A(j,i)=A(i,j))
  n = 3
  ap = (0.0d0, 0.0d0)
  ap(1) = (4.0d0, 1.0d0)    ! A(1,1)
  ap(2) = (2.0d0, -1.0d0)   ! A(1,2)
  ap(3) = (5.0d0, 0.5d0)    ! A(2,2)
  ap(4) = (1.0d0, 2.0d0)    ! A(1,3)
  ap(5) = (3.0d0, -1.0d0)   ! A(2,3)
  ap(6) = (6.0d0, 1.0d0)    ! A(3,3)
  ! b = A*[1;1;1]:
  ! row 1: A(1,1)+A(1,2)+A(1,3) = (4,1)+(2,-1)+(1,2) = (7,2)
  ! row 2: A(2,1)+A(2,2)+A(2,3) = (2,-1)+(5,0.5)+(3,-1) = (10,-1.5)
  ! row 3: A(3,1)+A(3,2)+A(3,3) = (1,2)+(3,-1)+(6,1) = (10,2)
  b(1) = (7.0d0, 2.0d0)
  b(2) = (10.0d0, -1.5d0)
  b(3) = (10.0d0, 2.0d0)
  ipiv = 0
  call zspsv('U', n, 1, ap, ipiv, b, n, info)
  call begin_test('3x3_upper')
  call print_array('x', b_r, 2*n)
  call print_int_array('ipiv', ipiv, n)
  call print_int('info', info)
  call end_test()

  ! Test 2: 3x3 complex symmetric, UPLO='L', NRHS=1
  ! Same matrix, lower packed: A(1,1), A(2,1), A(3,1), A(2,2), A(3,2), A(3,3)
  n = 3
  ap = (0.0d0, 0.0d0)
  ap(1) = (4.0d0, 1.0d0)    ! A(1,1)
  ap(2) = (2.0d0, -1.0d0)   ! A(2,1)
  ap(3) = (1.0d0, 2.0d0)    ! A(3,1)
  ap(4) = (5.0d0, 0.5d0)    ! A(2,2)
  ap(5) = (3.0d0, -1.0d0)   ! A(3,2)
  ap(6) = (6.0d0, 1.0d0)    ! A(3,3)
  b(1) = (7.0d0, 2.0d0)
  b(2) = (10.0d0, -1.5d0)
  b(3) = (10.0d0, 2.0d0)
  ipiv = 0
  call zspsv('L', n, 1, ap, ipiv, b, n, info)
  call begin_test('3x3_lower')
  call print_array('x', b_r, 2*n)
  call print_int_array('ipiv', ipiv, n)
  call print_int('info', info)
  call end_test()

  ! Test 3: 3x3 complex symmetric, UPLO='U', NRHS=2
  ! Same matrix again, two RHS
  ! x1 = [1+0i; 1+0i; 1+0i], x2 = [0+1i; 1+0i; -1+0i]
  n = 3
  nrhs = 2
  ap = (0.0d0, 0.0d0)
  ap(1) = (4.0d0, 1.0d0)    ! A(1,1)
  ap(2) = (2.0d0, -1.0d0)   ! A(1,2)
  ap(3) = (5.0d0, 0.5d0)    ! A(2,2)
  ap(4) = (1.0d0, 2.0d0)    ! A(1,3)
  ap(5) = (3.0d0, -1.0d0)   ! A(2,3)
  ap(6) = (6.0d0, 1.0d0)    ! A(3,3)
  ! b1 = A*[1;1;1] (same as Test 1)
  b(1) = (7.0d0, 2.0d0)
  b(2) = (10.0d0, -1.5d0)
  b(3) = (10.0d0, 2.0d0)
  ! b2 = A*[i; 1; -1]:
  ! row 1: A(1,1)*i + A(1,2)*1 + A(1,3)*(-1) = (4,1)*i + (2,-1) - (1,2)
  !       = (-1,4) + (2,-1) + (-1,-2) = (0, 1)
  ! row 2: A(2,1)*i + A(2,2)*1 + A(2,3)*(-1) = (2,-1)*i + (5,0.5) - (3,-1)
  !       = (1,2) + (5,0.5) + (-3,1) = (3, 3.5)
  ! row 3: A(3,1)*i + A(3,2)*1 + A(3,3)*(-1) = (1,2)*i + (3,-1) - (6,1)
  !       = (-2,1) + (3,-1) + (-6,-1) = (-5, -1)
  b(4) = (0.0d0, 1.0d0)
  b(5) = (3.0d0, 3.5d0)
  b(6) = (-5.0d0, -1.0d0)
  ipiv = 0
  call zspsv('U', n, nrhs, ap, ipiv, b, n, info)
  call begin_test('multi_rhs')
  call print_array('x', b_r, 2*n*nrhs)
  call print_int_array('ipiv', ipiv, n)
  call print_int('info', info)
  call end_test()

  ! Test 4: Singular matrix (info > 0)
  ! A = [ (0,0) (0,0) (0,0);
  !       (0,0) (0,0) (0,0);
  !       (0,0) (0,0) (0,0) ]
  n = 3
  ap = (0.0d0, 0.0d0)
  b(1) = (1.0d0, 0.0d0)
  b(2) = (2.0d0, 0.0d0)
  b(3) = (3.0d0, 0.0d0)
  ipiv = 0
  call zspsv('U', n, 1, ap, ipiv, b, n, info)
  call begin_test('singular')
  call print_int('info', info)
  call end_test()

  ! Test 5: N=0 quick return
  call zspsv('U', 0, 1, ap, ipiv, b, 1, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: N=1 system: (5+2i)*x = (10+4i) => x = 2+0i
  n = 1
  ap(1) = (5.0d0, 2.0d0)
  b(1) = (10.0d0, 4.0d0)
  ipiv = 0
  call zspsv('U', n, 1, ap, ipiv, b, n, info)
  call begin_test('n_one')
  call print_array('x', b_r, 2)
  call print_int_array('ipiv', ipiv, 1)
  call print_int('info', info)
  call end_test()

  ! Test 7: 4x4 symmetric indefinite, UPLO='L', NRHS=1
  ! A = [ (2,1)    (1,-1)   (0.5,2)  (3,0.5);
  !       (1,-1)   (0,0)    (4,1)    (5,-2);
  !       (0.5,2)  (4,1)    (-1,0.5) (6,1);
  !       (3,0.5)  (5,-2)   (6,1)    (2,-1) ]
  ! Lower packed: A(1,1), A(2,1), A(3,1), A(4,1), A(2,2), A(3,2), A(4,2), A(3,3), A(4,3), A(4,4)
  ! x = [1+1i; 2-1i; -1+0i; 0+1i]
  n = 4
  ap = (0.0d0, 0.0d0)
  ap(1)  = (2.0d0, 1.0d0)     ! A(1,1)
  ap(2)  = (1.0d0, -1.0d0)    ! A(2,1)
  ap(3)  = (0.5d0, 2.0d0)     ! A(3,1)
  ap(4)  = (3.0d0, 0.5d0)     ! A(4,1)
  ap(5)  = (0.0d0, 0.0d0)     ! A(2,2)
  ap(6)  = (4.0d0, 1.0d0)     ! A(3,2)
  ap(7)  = (5.0d0, -2.0d0)    ! A(4,2)
  ap(8)  = (-1.0d0, 0.5d0)    ! A(3,3)
  ap(9)  = (6.0d0, 1.0d0)     ! A(4,3)
  ap(10) = (2.0d0, -1.0d0)    ! A(4,4)
  ! Compute b = A * x where x = [1+1i; 2-1i; -1+0i; 0+1i]
  ! row 1: A(1,1)*(1+i) + A(1,2)*(2-i) + A(1,3)*(-1) + A(1,4)*(i)
  !      = (2,1)*(1,1) + (1,-1)*(2,-1) + (0.5,2)*(-1,0) + (3,0.5)*(0,1)
  !      = (1,3) + (1,-3) + (-0.5,-2) + (-0.5,3)
  !      = (1.0, 1.0)
  b(1) = (2.0d0,1.0d0)*(1.0d0,1.0d0) + (1.0d0,-1.0d0)*(2.0d0,-1.0d0) &
       + (0.5d0,2.0d0)*(-1.0d0,0.0d0) + (3.0d0,0.5d0)*(0.0d0,1.0d0)
  b(2) = (1.0d0,-1.0d0)*(1.0d0,1.0d0) + (0.0d0,0.0d0)*(2.0d0,-1.0d0) &
       + (4.0d0,1.0d0)*(-1.0d0,0.0d0) + (5.0d0,-2.0d0)*(0.0d0,1.0d0)
  b(3) = (0.5d0,2.0d0)*(1.0d0,1.0d0) + (4.0d0,1.0d0)*(2.0d0,-1.0d0) &
       + (-1.0d0,0.5d0)*(-1.0d0,0.0d0) + (6.0d0,1.0d0)*(0.0d0,1.0d0)
  b(4) = (3.0d0,0.5d0)*(1.0d0,1.0d0) + (5.0d0,-2.0d0)*(2.0d0,-1.0d0) &
       + (6.0d0,1.0d0)*(-1.0d0,0.0d0) + (2.0d0,-1.0d0)*(0.0d0,1.0d0)
  ipiv = 0
  call zspsv('L', n, 1, ap, ipiv, b, n, info)
  call begin_test('4x4_lower')
  call print_array('x', b_r, 2*n)
  call print_int_array('ipiv', ipiv, n)
  call print_int('info', info)
  call end_test()

end program
