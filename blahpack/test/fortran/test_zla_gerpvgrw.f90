program test_zla_gerpvgrw
  use test_utils
  implicit none
  complex*16 :: a(4, 4), af(4, 4)
  double precision :: result
  double precision :: zla_gerpvgrw
  external :: zla_gerpvgrw
  integer :: n, ncols, lda, ldaf

  lda = 4
  ldaf = 4

  ! Test 1: basic 3x3 complex matrix
  ! A = [ (6,1)  (2,0)  (1,1) ]
  !     [ (2,3)  (5,2)  (3,0) ]
  !     [ (1,0)  (3,1)  (4,2) ]
  ! CABS1(z) = |re| + |im|
  n = 3
  ncols = 3
  a = (0.0d0, 0.0d0)
  af = (0.0d0, 0.0d0)
  a(1,1) = (6.0d0, 1.0d0);  a(1,2) = (2.0d0, 0.0d0);  a(1,3) = (1.0d0, 1.0d0)
  a(2,1) = (2.0d0, 3.0d0);  a(2,2) = (5.0d0, 2.0d0);  a(2,3) = (3.0d0, 0.0d0)
  a(3,1) = (1.0d0, 0.0d0);  a(3,2) = (3.0d0, 1.0d0);  a(3,3) = (4.0d0, 2.0d0)
  ! AF upper triangular (hand-specified as if from LU)
  af(1,1) = (6.0d0, 1.0d0);  af(1,2) = (2.0d0, 0.0d0);  af(1,3) = (1.0d0, 1.0d0)
  af(2,2) = (4.0d0, 2.0d0);  af(2,3) = (2.5d0, 0.0d0)
  af(3,3) = (2.0d0, 1.0d0)
  ! Col 1: amax = max(cabs1) = max(7,5,1) = 7, umax = max(7) = 7, ratio = 1.0
  ! Col 2: amax = max(2,7,4) = 7, umax = max(2,6) = 6, ratio = 7/6
  ! Col 3: amax = max(2,3,6) = 6, umax = max(2,2.5,3) = 3, ratio = 2.0
  ! rpvgrw = min(1.0, 7/6, 2.0) = 1.0
  result = zla_gerpvgrw(n, ncols, a, lda, af, ldaf)
  call begin_test('basic_3x3')
  call print_scalar('result', result)
  call print_int('N', n)
  call print_int('ncols', ncols)
  call end_test()

  ! Test 2: ncols=0 should return 1.0 (no columns to process)
  n = 3
  ncols = 0
  result = zla_gerpvgrw(n, ncols, a, lda, af, ldaf)
  call begin_test('ncols_zero')
  call print_scalar('result', result)
  call end_test()

  ! Test 3: 1x1 complex matrix
  n = 1
  ncols = 1
  a = (0.0d0, 0.0d0)
  af = (0.0d0, 0.0d0)
  a(1,1) = (5.0d0, 3.0d0)
  af(1,1) = (5.0d0, 3.0d0)
  ! CABS1 = 8 for both => ratio = 1.0
  result = zla_gerpvgrw(n, ncols, a, lda, af, ldaf)
  call begin_test('one_by_one')
  call print_scalar('result', result)
  call end_test()

  ! Test 4: 1x1 with different A and AF magnitudes
  n = 1
  ncols = 1
  a(1,1) = (3.0d0, 1.0d0)
  af(1,1) = (6.0d0, 2.0d0)
  ! CABS1(a) = 4, CABS1(af) = 8 => ratio = 0.5
  result = zla_gerpvgrw(n, ncols, a, lda, af, ldaf)
  call begin_test('one_by_one_growth')
  call print_scalar('result', result)
  call end_test()

  ! Test 5: U has a zero column (UMAX = 0)
  n = 3
  ncols = 3
  a = (0.0d0, 0.0d0)
  af = (0.0d0, 0.0d0)
  a(1,1) = (4.0d0, 1.0d0);  a(1,2) = (0.0d0, 0.0d0);  a(1,3) = (3.0d0, 0.0d0)
  a(2,1) = (2.0d0, 0.0d0);  a(2,2) = (0.0d0, 0.0d0);  a(2,3) = (1.0d0, 1.0d0)
  a(3,1) = (1.0d0, 2.0d0);  a(3,2) = (0.0d0, 0.0d0);  a(3,3) = (2.0d0, 0.0d0)
  af(1,1) = (4.0d0, 1.0d0);  af(1,2) = (0.0d0, 0.0d0);  af(1,3) = (3.0d0, 0.0d0)
  af(2,2) = (0.0d0, 0.0d0);  af(2,3) = (1.0d0, 1.0d0)
  af(3,3) = (2.0d0, 0.0d0)
  ! Col 1: amax=max(5,2,3)=5, umax=5, ratio=1.0
  ! Col 2: amax=0, umax=0 => skip
  ! Col 3: amax=max(3,2,2)=3, umax=max(3,2,2)=3, ratio=1.0
  ! rpvgrw = 1.0
  result = zla_gerpvgrw(n, ncols, a, lda, af, ldaf)
  call begin_test('zero_u_column')
  call print_scalar('result', result)
  call end_test()

  ! Test 6: ratio < 1 case
  n = 2
  ncols = 2
  a = (0.0d0, 0.0d0)
  af = (0.0d0, 0.0d0)
  a(1,1) = (1.0d0, 1.0d0);  a(1,2) = (2.0d0, 0.0d0)
  a(2,1) = (3.0d0, 0.0d0);  a(2,2) = (1.0d0, 3.0d0)
  af(1,1) = (5.0d0, 3.0d0);  af(1,2) = (4.0d0, 4.0d0)
  af(2,2) = (6.0d0, 2.0d0)
  ! Col 1: amax=max(2,3)=3, umax=max(8)=8, ratio=3/8=0.375
  ! Col 2: amax=max(2,4)=4, umax=max(8,8)=8, ratio=4/8=0.5
  ! rpvgrw = min(0.375, 0.5) = 0.375
  result = zla_gerpvgrw(n, ncols, a, lda, af, ldaf)
  call begin_test('ratio_less_than_one')
  call print_scalar('result', result)
  call end_test()

  ! Test 7: ncols < N (only process first ncols columns)
  n = 3
  ncols = 2
  a = (0.0d0, 0.0d0)
  af = (0.0d0, 0.0d0)
  a(1,1) = (4.0d0, 0.0d0);  a(1,2) = (2.0d0, 1.0d0);  a(1,3) = (99.0d0, 0.0d0)
  a(2,1) = (1.0d0, 0.0d0);  a(2,2) = (5.0d0, 0.0d0);  a(2,3) = (99.0d0, 0.0d0)
  a(3,1) = (3.0d0, 0.0d0);  a(3,2) = (3.0d0, 3.0d0);  a(3,3) = (99.0d0, 0.0d0)
  af(1,1) = (4.0d0, 0.0d0);  af(1,2) = (2.0d0, 1.0d0);  af(1,3) = (99.0d0, 0.0d0)
  af(2,2) = (5.0d0, 0.0d0);  af(2,3) = (99.0d0, 0.0d0)
  af(3,3) = (99.0d0, 0.0d0)
  ! Col 1: amax=max(4,1,3)=4, umax=max(4)=4, ratio=1.0
  ! Col 2: amax=max(3,5,6)=6, umax=max(3,5)=5, ratio=1.2
  ! rpvgrw = 1.0
  result = zla_gerpvgrw(n, ncols, a, lda, af, ldaf)
  call begin_test('ncols_less_than_n')
  call print_scalar('result', result)
  call end_test()

  ! Test 8: purely imaginary elements
  n = 2
  ncols = 2
  a = (0.0d0, 0.0d0)
  af = (0.0d0, 0.0d0)
  a(1,1) = (0.0d0, 4.0d0);  a(1,2) = (0.0d0, 2.0d0)
  a(2,1) = (0.0d0, 6.0d0);  a(2,2) = (0.0d0, 5.0d0)
  af(1,1) = (0.0d0, 8.0d0);  af(1,2) = (0.0d0, 3.0d0)
  af(2,2) = (0.0d0, 10.0d0)
  ! CABS1 = |re| + |im|, so for purely imaginary (0,y), CABS1 = |y|
  ! Col 1: amax=max(4,6)=6, umax=max(8)=8, ratio=6/8=0.75
  ! Col 2: amax=max(2,5)=5, umax=max(3,10)=10, ratio=5/10=0.5
  ! rpvgrw = 0.5
  result = zla_gerpvgrw(n, ncols, a, lda, af, ldaf)
  call begin_test('purely_imaginary')
  call print_scalar('result', result)
  call end_test()

  ! Test 9: large values
  n = 2
  ncols = 2
  a = (0.0d0, 0.0d0)
  af = (0.0d0, 0.0d0)
  a(1,1) = (1.0d+100, 0.0d0);  a(1,2) = (2.0d+100, 0.0d0)
  a(2,1) = (3.0d+100, 0.0d0);  a(2,2) = (4.0d+100, 0.0d0)
  af(1,1) = (6.0d+100, 0.0d0);  af(1,2) = (5.0d+100, 0.0d0)
  af(2,2) = (8.0d+100, 0.0d0)
  ! Col 1: amax=3e100, umax=6e100, ratio=0.5
  ! Col 2: amax=4e100, umax=max(5e100,8e100)=8e100, ratio=0.5
  ! rpvgrw = 0.5
  result = zla_gerpvgrw(n, ncols, a, lda, af, ldaf)
  call begin_test('large_values')
  call print_scalar('result', result)
  call end_test()

end program
