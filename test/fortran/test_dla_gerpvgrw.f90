program test_dla_gerpvgrw
  use test_utils
  implicit none
  double precision :: a(4, 4), af(4, 4)
  double precision :: result
  double precision :: dla_gerpvgrw
  external :: dla_gerpvgrw
  integer :: n, ncols, lda, ldaf

  lda = 4
  ldaf = 4

  ! Test 1: basic 3x3 matrix
  ! A = [ 6  2  1 ]
  !     [ 2  5  3 ]
  !     [ 1  3  4 ]
  ! AF (LU factors) = [ 6.0   2.0         1.0       ]
  !                    [ 0.33  4.33        2.67      ]
  !                    [ 0.17  0.577       1.846     ]
  ! (approximations — actual values from dgetrf)
  n = 3
  ncols = 3
  a = 0.0d0
  af = 0.0d0
  a(1,1) = 6.0d0;  a(1,2) = 2.0d0;  a(1,3) = 1.0d0
  a(2,1) = 2.0d0;  a(2,2) = 5.0d0;  a(2,3) = 3.0d0
  a(3,1) = 1.0d0;  a(3,2) = 3.0d0;  a(3,3) = 4.0d0
  ! Hand-specified upper triangular AF (as if from LU factorization)
  af = 0.0d0
  af(1,1) = 6.0d0;  af(1,2) = 2.0d0;  af(1,3) = 1.0d0
  af(2,2) = 4.0d0;  af(2,3) = 2.5d0
  af(3,3) = 2.0d0
  ! Col 1: amax = max(6,2,1) = 6, umax = max(6) = 6, ratio = 1.0
  ! Col 2: amax = max(2,5,3) = 5, umax = max(2,4) = 4, ratio = 1.25
  ! Col 3: amax = max(1,3,4) = 4, umax = max(1,2.5,2) = 2.5, ratio = 1.6
  ! rpvgrw = min(1.0, 1.25, 1.6) = 1.0
  result = dla_gerpvgrw(n, ncols, a, lda, af, ldaf)
  call begin_test('basic_3x3')
  call print_scalar('result', result)
  call print_int('N', n)
  call print_int('ncols', ncols)
  call end_test()

  ! Test 2: ncols=0 should return 1.0 (no columns to process)
  n = 3
  ncols = 0
  result = dla_gerpvgrw(n, ncols, a, lda, af, ldaf)
  call begin_test('ncols_zero')
  call print_scalar('result', result)
  call end_test()

  ! Test 3: 1x1 matrix
  n = 1
  ncols = 1
  a = 0.0d0
  af = 0.0d0
  a(1,1) = 5.0d0
  af(1,1) = 5.0d0
  ! amax = 5, umax = 5, ratio = 1.0
  result = dla_gerpvgrw(n, ncols, a, lda, af, ldaf)
  call begin_test('one_by_one')
  call print_scalar('result', result)
  call end_test()

  ! Test 4: 1x1 with different A and AF magnitudes
  n = 1
  ncols = 1
  a(1,1) = 3.0d0
  af(1,1) = 6.0d0
  ! amax = 3, umax = 6, ratio = 0.5
  result = dla_gerpvgrw(n, ncols, a, lda, af, ldaf)
  call begin_test('one_by_one_growth')
  call print_scalar('result', result)
  call end_test()

  ! Test 5: U has a zero column (UMAX = 0)
  n = 3
  ncols = 3
  a = 0.0d0
  af = 0.0d0
  a(1,1) = 4.0d0;  a(1,2) = 0.0d0;  a(1,3) = 3.0d0
  a(2,1) = 2.0d0;  a(2,2) = 0.0d0;  a(2,3) = 1.0d0
  a(3,1) = 1.0d0;  a(3,2) = 0.0d0;  a(3,3) = 2.0d0
  af(1,1) = 4.0d0;  af(1,2) = 0.0d0;  af(1,3) = 3.0d0
  af(2,2) = 0.0d0;  af(2,3) = 1.0d0
  af(3,3) = 2.0d0
  ! Col 1: amax=4, umax=4, ratio=1.0
  ! Col 2: amax=0, umax=0 => skip (umax==0)
  ! Col 3: amax=3, umax=max(3,1,2)=3, ratio=1.0
  ! rpvgrw = 1.0
  result = dla_gerpvgrw(n, ncols, a, lda, af, ldaf)
  call begin_test('zero_u_column')
  call print_scalar('result', result)
  call end_test()

  ! Test 6: pivot growth < 1 (large A, small U diagonal)
  n = 2
  ncols = 2
  a = 0.0d0
  af = 0.0d0
  a(1,1) = 10.0d0;  a(1,2) = 8.0d0
  a(2,1) = 7.0d0;   a(2,2) = 9.0d0
  af(1,1) = 10.0d0;  af(1,2) = 8.0d0
  af(2,2) = 1.0d0
  ! Col 1: amax=10, umax=10, ratio=1.0
  ! Col 2: amax=9, umax=max(8,1)=8, ratio=9/8=1.125
  ! rpvgrw = min(1.0, 1.125) = 1.0
  result = dla_gerpvgrw(n, ncols, a, lda, af, ldaf)
  call begin_test('pivot_growth_2x2')
  call print_scalar('result', result)
  call end_test()

  ! Test 7: ratio < 1 case (A element < U element)
  n = 2
  ncols = 2
  a = 0.0d0
  af = 0.0d0
  a(1,1) = 1.0d0;  a(1,2) = 2.0d0
  a(2,1) = 3.0d0;  a(2,2) = 4.0d0
  af(1,1) = 5.0d0;  af(1,2) = 6.0d0
  af(2,2) = 8.0d0
  ! Col 1: amax=max(1,3)=3, umax=max(5)=5, ratio=0.6
  ! Col 2: amax=max(2,4)=4, umax=max(6,8)=8, ratio=0.5
  ! rpvgrw = min(0.6, 0.5) = 0.5
  result = dla_gerpvgrw(n, ncols, a, lda, af, ldaf)
  call begin_test('ratio_less_than_one')
  call print_scalar('result', result)
  call end_test()

  ! Test 8: ncols < N (only process first ncols columns)
  n = 3
  ncols = 2
  a = 0.0d0
  af = 0.0d0
  a(1,1) = 4.0d0;  a(1,2) = 2.0d0;  a(1,3) = 99.0d0
  a(2,1) = 1.0d0;  a(2,2) = 5.0d0;  a(2,3) = 99.0d0
  a(3,1) = 3.0d0;  a(3,2) = 6.0d0;  a(3,3) = 99.0d0
  af(1,1) = 4.0d0;  af(1,2) = 2.0d0;  af(1,3) = 99.0d0
  af(2,2) = 5.0d0;  af(2,3) = 99.0d0
  af(3,3) = 99.0d0
  ! Only columns 1-2 matter:
  ! Col 1: amax=max(4,1,3)=4, umax=max(4)=4, ratio=1.0
  ! Col 2: amax=max(2,5,6)=6, umax=max(2,5)=5, ratio=1.2
  ! rpvgrw = min(1.0, 1.2) = 1.0
  result = dla_gerpvgrw(n, ncols, a, lda, af, ldaf)
  call begin_test('ncols_less_than_n')
  call print_scalar('result', result)
  call end_test()

  ! Test 9: large values
  n = 2
  ncols = 2
  a = 0.0d0
  af = 0.0d0
  a(1,1) = 1.0d+100;  a(1,2) = 2.0d+100
  a(2,1) = 3.0d+100;  a(2,2) = 4.0d+100
  af(1,1) = 6.0d+100;  af(1,2) = 5.0d+100
  af(2,2) = 8.0d+100
  ! Col 1: amax=3e100, umax=6e100, ratio=0.5
  ! Col 2: amax=4e100, umax=max(5e100,8e100)=8e100, ratio=0.5
  ! rpvgrw = 0.5
  result = dla_gerpvgrw(n, ncols, a, lda, af, ldaf)
  call begin_test('large_values')
  call print_scalar('result', result)
  call end_test()

end program
