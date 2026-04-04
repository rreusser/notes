program test_dsprfs
  use test_utils
  implicit none
  double precision :: ap(100), afp(100), b(100), x(100), work(300)
  double precision :: ferr(10), berr(10)
  integer :: ipiv(10), iwork(10), info, i

  ! Test 1: 3x3 upper triangle, single RHS
  ! A = [4 2 1; 2 5 3; 1 3 6] (symmetric, stored upper packed)
  ! Upper packed: col-major upper triangle = (1,1),(1,2),(2,2),(1,3),(2,3),(3,3)
  ! = 4, 2, 5, 1, 3, 6
  ! b = [1; 2; 3]
  ap = 0.0d0; afp = 0.0d0; b = 0.0d0; x = 0.0d0
  ap(1) = 4.0d0; ap(2) = 2.0d0; ap(3) = 5.0d0
  ap(4) = 1.0d0; ap(5) = 3.0d0; ap(6) = 6.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0

  afp(1:6) = ap(1:6)
  call dsptrf('U', 3, afp, ipiv, info)

  x(1:3) = b(1:3)
  call dsptrs('U', 3, 1, afp, ipiv, x, 3, info)

  call dsprfs('U', 3, 1, ap, afp, ipiv, b, 3, x, 3, &
              ferr, berr, work, iwork, info)
  call begin_test('upper_3x3')
  call print_array('x', x, 3)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_int('info', info)
  call end_test()

  ! Test 2: 3x3 lower triangle, single RHS
  ! Same matrix stored as lower packed: (1,1),(2,1),(3,1),(2,2),(3,2),(3,3)
  ! = 4, 2, 1, 5, 3, 6
  ap = 0.0d0; afp = 0.0d0; b = 0.0d0; x = 0.0d0
  ap(1) = 4.0d0; ap(2) = 2.0d0; ap(3) = 1.0d0
  ap(4) = 5.0d0; ap(5) = 3.0d0; ap(6) = 6.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0

  afp(1:6) = ap(1:6)
  call dsptrf('L', 3, afp, ipiv, info)

  x(1:3) = b(1:3)
  call dsptrs('L', 3, 1, afp, ipiv, x, 3, info)

  call dsprfs('L', 3, 1, ap, afp, ipiv, b, 3, x, 3, &
              ferr, berr, work, iwork, info)
  call begin_test('lower_3x3')
  call print_array('x', x, 3)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_int('info', info)
  call end_test()

  ! Test 3: Multiple RHS (NRHS=2), upper
  ap = 0.0d0; afp = 0.0d0; b = 0.0d0; x = 0.0d0
  ap(1) = 4.0d0; ap(2) = 2.0d0; ap(3) = 5.0d0
  ap(4) = 1.0d0; ap(5) = 3.0d0; ap(6) = 6.0d0
  ! b1 = [1;0;0], b2 = [0;1;0]  (column-major, LDB=3)
  b(1) = 1.0d0; b(2) = 0.0d0; b(3) = 0.0d0
  b(4) = 0.0d0; b(5) = 1.0d0; b(6) = 0.0d0

  afp(1:6) = ap(1:6)
  call dsptrf('U', 3, afp, ipiv, info)

  x(1:6) = b(1:6)
  call dsptrs('U', 3, 2, afp, ipiv, x, 3, info)

  call dsprfs('U', 3, 2, ap, afp, ipiv, b, 3, x, 3, &
              ferr, berr, work, iwork, info)
  call begin_test('multi_rhs')
  call print_array('x', x, 6)
  call print_array('ferr', ferr, 2)
  call print_array('berr', berr, 2)
  call print_int('info', info)
  call end_test()

  ! Test 4: N=0 quick return
  call dsprfs('U', 0, 1, ap, afp, ipiv, b, 1, x, 1, &
              ferr, berr, work, iwork, info)
  call begin_test('n_zero')
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_int('info', info)
  call end_test()

  ! Test 5: NRHS=0 quick return
  call dsprfs('U', 3, 0, ap, afp, ipiv, b, 3, x, 3, &
              ferr, berr, work, iwork, info)
  call begin_test('nrhs_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: 1x1 system
  ap = 0.0d0; afp = 0.0d0; b = 0.0d0; x = 0.0d0
  ap(1) = 5.0d0
  b(1) = 10.0d0
  afp(1) = 5.0d0
  call dsptrf('U', 1, afp, ipiv, info)
  x(1) = b(1)
  call dsptrs('U', 1, 1, afp, ipiv, x, 1, info)

  call dsprfs('U', 1, 1, ap, afp, ipiv, b, 1, x, 1, &
              ferr, berr, work, iwork, info)
  call begin_test('one_by_one')
  call print_array('x', x, 1)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_int('info', info)
  call end_test()

  ! Test 7: Ill-conditioned symmetric matrix (Hilbert-like, upper packed)
  ! H(i,j) = 1/(i+j-1)
  ! Upper packed: H(1,1)=1, H(1,2)=0.5, H(2,2)=1/3, H(1,3)=1/3, H(2,3)=0.25, H(3,3)=0.2
  ap = 0.0d0; afp = 0.0d0; b = 0.0d0; x = 0.0d0
  ap(1) = 1.0d0; ap(2) = 0.5d0; ap(3) = 1.0d0/3.0d0
  ap(4) = 1.0d0/3.0d0; ap(5) = 0.25d0; ap(6) = 0.2d0
  b(1) = 1.0d0; b(2) = 1.0d0; b(3) = 1.0d0

  afp(1:6) = ap(1:6)
  call dsptrf('U', 3, afp, ipiv, info)

  x(1:3) = b(1:3)
  call dsptrs('U', 3, 1, afp, ipiv, x, 3, info)

  call dsprfs('U', 3, 1, ap, afp, ipiv, b, 3, x, 3, &
              ferr, berr, work, iwork, info)
  call begin_test('hilbert_upper')
  call print_array('x', x, 3)
  call print_array('ferr', ferr, 1)
  call print_array('berr', berr, 1)
  call print_int('info', info)
  call end_test()

end program
