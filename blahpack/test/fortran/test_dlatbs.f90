program test_dlatbs
  use test_utils
  implicit none
  ! For banded storage: AB(LDAB, N) where LDAB >= KD+1
  ! We declare with exact LDAB = KD+1 for each test
  ! Upper: AB(KD+1-i+j, j) stores A(i,j) for max(1,j-KD)<=i<=j
  !   Main diagonal at row KD+1
  ! Lower: AB(1+i-j, j) stores A(i,j) for j<=i<=min(N,j+KD)
  !   Main diagonal at row 1
  double precision :: ab3(3,10), ab2(2,10), ab1(1,10)
  double precision :: x(10), cnorm(10), scale
  integer :: info, n, kd

  ! Test 1: Upper triangular banded, no transpose, non-unit, 4x4, KD=2
  ! Full matrix:
  !   [4  2  1  0]
  !   [0  3  1  2]
  !   [0  0  5  3]
  !   [0  0  0  6]
  ! Banded storage (KD=2, LDAB=3):
  !   col1  col2  col3  col4
  !   [ *    *     1     2  ]  row 1 (offset -2 from diag)
  !   [ *    2     1     3  ]  row 2 (offset -1 from diag)
  !   [ 4    3     5     6  ]  row 3 (main diagonal, KD+1)
  n = 4
  kd = 2
  ab3 = 0.0d0
  ! Column 1: main diag at row 3
  ab3(3,1) = 4.0d0
  ! Column 2: superdiag at row 2, main at row 3
  ab3(2,2) = 2.0d0; ab3(3,2) = 3.0d0
  ! Column 3: offset-2 at row 1, offset-1 at row 2, main at row 3
  ab3(1,3) = 1.0d0; ab3(2,3) = 1.0d0; ab3(3,3) = 5.0d0
  ! Column 4: offset-2 at row 1, offset-1 at row 2, main at row 3
  ab3(1,4) = 2.0d0; ab3(2,4) = 3.0d0; ab3(3,4) = 6.0d0

  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0; x(4) = 4.0d0
  scale = 0.0d0
  cnorm = 0.0d0
  call dlatbs('U', 'N', 'N', 'N', n, kd, ab3, kd+1, x, scale, cnorm, info)
  call begin_test('upper_N_nonunit')
  call print_array('x', x, n)
  call print_scalar('scale', scale)
  call print_array('cnorm', cnorm, n)
  call print_int('info', info)
  call end_test()

  ! Test 2: Lower triangular banded, no transpose, non-unit, 4x4, KD=2
  ! Full matrix:
  !   [4  0  0  0]
  !   [2  3  0  0]
  !   [1  1  5  0]
  !   [0  2  3  6]
  ! Banded storage (KD=2, LDAB=3):
  !   col1  col2  col3  col4
  !   [ 4    3     5     6  ]  row 1 (main diagonal)
  !   [ 2    1     3     *  ]  row 2 (subdiag 1)
  !   [ 1    2     *     *  ]  row 3 (subdiag 2)
  n = 4
  kd = 2
  ab3 = 0.0d0
  ab3(1,1) = 4.0d0; ab3(2,1) = 2.0d0; ab3(3,1) = 1.0d0
  ab3(1,2) = 3.0d0; ab3(2,2) = 1.0d0; ab3(3,2) = 2.0d0
  ab3(1,3) = 5.0d0; ab3(2,3) = 3.0d0
  ab3(1,4) = 6.0d0

  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0; x(4) = 4.0d0
  scale = 0.0d0
  cnorm = 0.0d0
  call dlatbs('L', 'N', 'N', 'N', n, kd, ab3, kd+1, x, scale, cnorm, info)
  call begin_test('lower_N_nonunit')
  call print_array('x', x, n)
  call print_scalar('scale', scale)
  call print_array('cnorm', cnorm, n)
  call print_int('info', info)
  call end_test()

  ! Test 3: Upper triangular banded, transpose, non-unit, 4x4, KD=2
  n = 4
  kd = 2
  ab3 = 0.0d0
  ab3(3,1) = 4.0d0
  ab3(2,2) = 2.0d0; ab3(3,2) = 3.0d0
  ab3(1,3) = 1.0d0; ab3(2,3) = 1.0d0; ab3(3,3) = 5.0d0
  ab3(1,4) = 2.0d0; ab3(2,4) = 3.0d0; ab3(3,4) = 6.0d0

  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0; x(4) = 4.0d0
  scale = 0.0d0
  cnorm = 0.0d0
  call dlatbs('U', 'T', 'N', 'N', n, kd, ab3, kd+1, x, scale, cnorm, info)
  call begin_test('upper_T_nonunit')
  call print_array('x', x, n)
  call print_scalar('scale', scale)
  call print_array('cnorm', cnorm, n)
  call print_int('info', info)
  call end_test()

  ! Test 4: Lower triangular banded, transpose, non-unit, 4x4, KD=2
  n = 4
  kd = 2
  ab3 = 0.0d0
  ab3(1,1) = 4.0d0; ab3(2,1) = 2.0d0; ab3(3,1) = 1.0d0
  ab3(1,2) = 3.0d0; ab3(2,2) = 1.0d0; ab3(3,2) = 2.0d0
  ab3(1,3) = 5.0d0; ab3(2,3) = 3.0d0
  ab3(1,4) = 6.0d0

  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0; x(4) = 4.0d0
  scale = 0.0d0
  cnorm = 0.0d0
  call dlatbs('L', 'T', 'N', 'N', n, kd, ab3, kd+1, x, scale, cnorm, info)
  call begin_test('lower_T_nonunit')
  call print_array('x', x, n)
  call print_scalar('scale', scale)
  call print_array('cnorm', cnorm, n)
  call print_int('info', info)
  call end_test()

  ! Test 5: Upper, unit diagonal, no transpose, 4x4, KD=2
  n = 4
  kd = 2
  ab3 = 0.0d0
  ab3(3,1) = 99.0d0  ! ignored (unit diagonal)
  ab3(2,2) = 2.0d0; ab3(3,2) = 99.0d0
  ab3(1,3) = 1.0d0; ab3(2,3) = 1.0d0; ab3(3,3) = 99.0d0
  ab3(1,4) = 2.0d0; ab3(2,4) = 3.0d0; ab3(3,4) = 99.0d0

  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0; x(4) = 4.0d0
  scale = 0.0d0
  cnorm = 0.0d0
  call dlatbs('U', 'N', 'U', 'N', n, kd, ab3, kd+1, x, scale, cnorm, info)
  call begin_test('upper_N_unit')
  call print_array('x', x, n)
  call print_scalar('scale', scale)
  call print_array('cnorm', cnorm, n)
  call print_int('info', info)
  call end_test()

  ! Test 6: Lower, unit diagonal, no transpose, 4x4, KD=2
  n = 4
  kd = 2
  ab3 = 0.0d0
  ab3(1,1) = 99.0d0; ab3(2,1) = 2.0d0; ab3(3,1) = 1.0d0
  ab3(1,2) = 99.0d0; ab3(2,2) = 1.0d0; ab3(3,2) = 2.0d0
  ab3(1,3) = 99.0d0; ab3(2,3) = 3.0d0
  ab3(1,4) = 99.0d0

  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0; x(4) = 4.0d0
  scale = 0.0d0
  cnorm = 0.0d0
  call dlatbs('L', 'N', 'U', 'N', n, kd, ab3, kd+1, x, scale, cnorm, info)
  call begin_test('lower_N_unit')
  call print_array('x', x, n)
  call print_scalar('scale', scale)
  call print_array('cnorm', cnorm, n)
  call print_int('info', info)
  call end_test()

  ! Test 7: N=0 (quick return)
  n = 0
  kd = 0
  scale = 0.0d0
  call dlatbs('U', 'N', 'N', 'N', n, kd, ab1, 1, x, scale, cnorm, info)
  call begin_test('n_zero')
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 8: N=1, upper, no transpose, non-unit
  n = 1
  kd = 0
  ab1(1,1) = 5.0d0
  x(1) = 10.0d0
  scale = 0.0d0
  cnorm = 0.0d0
  call dlatbs('U', 'N', 'N', 'N', n, kd, ab1, kd+1, x, scale, cnorm, info)
  call begin_test('n_one')
  call print_array('x', x, n)
  call print_scalar('scale', scale)
  call print_int('info', info)
  call end_test()

  ! Test 9: Pre-computed CNORM (normin='Y')
  n = 4
  kd = 2
  ab3 = 0.0d0
  ab3(3,1) = 4.0d0
  ab3(2,2) = 2.0d0; ab3(3,2) = 3.0d0
  ab3(1,3) = 1.0d0; ab3(2,3) = 1.0d0; ab3(3,3) = 5.0d0
  ab3(1,4) = 2.0d0; ab3(2,4) = 3.0d0; ab3(3,4) = 6.0d0

  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0; x(4) = 4.0d0
  scale = 0.0d0
  cnorm(1) = 0.0d0; cnorm(2) = 2.0d0; cnorm(3) = 2.0d0; cnorm(4) = 5.0d0
  call dlatbs('U', 'N', 'N', 'Y', n, kd, ab3, kd+1, x, scale, cnorm, info)
  call begin_test('normin_Y')
  call print_array('x', x, n)
  call print_scalar('scale', scale)
  call print_array('cnorm', cnorm, n)
  call print_int('info', info)
  call end_test()

  ! Test 10: KD=1 (tridiagonal band), upper
  n = 4
  kd = 1
  ab2 = 0.0d0
  ! Upper, KD=1, LDAB=2
  ! Full matrix:
  !   [3 1 0 0]
  !   [0 4 2 0]
  !   [0 0 5 1]
  !   [0 0 0 6]
  ! Banded: row 1 = superdiag, row 2 = main diag
  ab2(2,1) = 3.0d0
  ab2(1,2) = 1.0d0; ab2(2,2) = 4.0d0
  ab2(1,3) = 2.0d0; ab2(2,3) = 5.0d0
  ab2(1,4) = 1.0d0; ab2(2,4) = 6.0d0

  x(1) = 2.0d0; x(2) = 3.0d0; x(3) = 1.0d0; x(4) = 5.0d0
  scale = 0.0d0
  cnorm = 0.0d0
  call dlatbs('U', 'N', 'N', 'N', n, kd, ab2, kd+1, x, scale, cnorm, info)
  call begin_test('upper_kd1')
  call print_array('x', x, n)
  call print_scalar('scale', scale)
  call print_array('cnorm', cnorm, n)
  call print_int('info', info)
  call end_test()

  ! Test 11: Upper, transpose, unit diagonal, KD=2
  n = 4
  kd = 2
  ab3 = 0.0d0
  ab3(3,1) = 99.0d0
  ab3(2,2) = 2.0d0; ab3(3,2) = 99.0d0
  ab3(1,3) = 1.0d0; ab3(2,3) = 1.0d0; ab3(3,3) = 99.0d0
  ab3(1,4) = 2.0d0; ab3(2,4) = 3.0d0; ab3(3,4) = 99.0d0

  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0; x(4) = 4.0d0
  scale = 0.0d0
  cnorm = 0.0d0
  call dlatbs('U', 'T', 'U', 'N', n, kd, ab3, kd+1, x, scale, cnorm, info)
  call begin_test('upper_T_unit')
  call print_array('x', x, n)
  call print_scalar('scale', scale)
  call print_array('cnorm', cnorm, n)
  call print_int('info', info)
  call end_test()

  ! Test 12: Lower, transpose, unit diagonal, KD=2
  n = 4
  kd = 2
  ab3 = 0.0d0
  ab3(1,1) = 99.0d0; ab3(2,1) = 2.0d0; ab3(3,1) = 1.0d0
  ab3(1,2) = 99.0d0; ab3(2,2) = 1.0d0; ab3(3,2) = 2.0d0
  ab3(1,3) = 99.0d0; ab3(2,3) = 3.0d0
  ab3(1,4) = 99.0d0

  x(1) = 1.0d0; x(2) = 2.0d0; x(3) = 3.0d0; x(4) = 4.0d0
  scale = 0.0d0
  cnorm = 0.0d0
  call dlatbs('L', 'T', 'U', 'N', n, kd, ab3, kd+1, x, scale, cnorm, info)
  call begin_test('lower_T_unit')
  call print_array('x', x, n)
  call print_scalar('scale', scale)
  call print_array('cnorm', cnorm, n)
  call print_int('info', info)
  call end_test()

end program
