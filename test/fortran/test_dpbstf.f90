program test_dpbstf
  use test_utils
  implicit none

  ! Band storage: LDAB = KD+1 rows, N columns
  ! Upper: AB(KD+1+i-j, j) = A(i,j) for max(1,j-kd)<=i<=j
  ! Lower: AB(1+i-j, j) = A(i,j) for j<=i<=min(n,j+kd)

  double precision :: ab(10*10)
  integer :: info, ldab

  ! ===== Test 1: UPLO='U', N=5, KD=1 (tridiagonal) =====
  ! A = tridiag(-1, 2, -1): 5x5 SPD matrix
  ! Band storage (upper): LDAB=2, rows = [superdiag, diag]
  ldab = 2
  ab = 0.0d0
  ! Col 1
  ab(1) = 0.0d0   ! superdiag (unused for col 1)
  ab(2) = 2.0d0   ! diag
  ! Col 2
  ab(3) = -1.0d0  ! superdiag
  ab(4) = 2.0d0   ! diag
  ! Col 3
  ab(5) = -1.0d0
  ab(6) = 2.0d0
  ! Col 4
  ab(7) = -1.0d0
  ab(8) = 2.0d0
  ! Col 5
  ab(9) = -1.0d0
  ab(10) = 2.0d0

  call dpbstf('U', 5, 1, ab, ldab, info)
  call begin_test('upper_tridiag_5')
  call print_int('info', info)
  call print_array('ab', ab, 10)
  call end_test()

  ! ===== Test 2: UPLO='L', N=5, KD=1 (tridiagonal) =====
  ! Same matrix, lower band storage
  ldab = 2
  ab = 0.0d0
  ! Col 1
  ab(1) = 2.0d0   ! diag
  ab(2) = -1.0d0  ! subdiag
  ! Col 2
  ab(3) = 2.0d0
  ab(4) = -1.0d0
  ! Col 3
  ab(5) = 2.0d0
  ab(6) = -1.0d0
  ! Col 4
  ab(7) = 2.0d0
  ab(8) = -1.0d0
  ! Col 5
  ab(9) = 2.0d0
  ab(10) = 0.0d0

  call dpbstf('L', 5, 1, ab, ldab, info)
  call begin_test('lower_tridiag_5')
  call print_int('info', info)
  call print_array('ab', ab, 10)
  call end_test()

  ! ===== Test 3: UPLO='U', N=4, KD=2 (pentadiagonal-like) =====
  ! A = [4 -1 0.5 0; -1 4 -1 0.5; 0.5 -1 4 -1; 0 0.5 -1 4]
  ! Upper band: LDAB=3
  ldab = 3
  ab = 0.0d0
  ! Col 1
  ab(1) = 0.0d0   ! 2nd superdiag
  ab(2) = 0.0d0   ! 1st superdiag
  ab(3) = 4.0d0   ! diag
  ! Col 2
  ab(4) = 0.0d0
  ab(5) = -1.0d0  ! A(1,2)
  ab(6) = 4.0d0   ! diag
  ! Col 3
  ab(7) = 0.5d0   ! A(1,3)
  ab(8) = -1.0d0  ! A(2,3)
  ab(9) = 4.0d0   ! diag
  ! Col 4
  ab(10) = 0.5d0  ! A(2,4)
  ab(11) = -1.0d0 ! A(3,4)
  ab(12) = 4.0d0  ! diag

  call dpbstf('U', 4, 2, ab, ldab, info)
  call begin_test('upper_penta_4')
  call print_int('info', info)
  call print_array('ab', ab, 12)
  call end_test()

  ! ===== Test 4: UPLO='L', N=4, KD=2 (same matrix as test 3) =====
  ldab = 3
  ab = 0.0d0
  ! Col 1
  ab(1) = 4.0d0   ! diag
  ab(2) = -1.0d0  ! 1st subdiag = A(2,1)
  ab(3) = 0.5d0   ! 2nd subdiag = A(3,1)
  ! Col 2
  ab(4) = 4.0d0
  ab(5) = -1.0d0
  ab(6) = 0.5d0
  ! Col 3
  ab(7) = 4.0d0
  ab(8) = -1.0d0
  ab(9) = 0.0d0
  ! Col 4
  ab(10) = 4.0d0
  ab(11) = 0.0d0
  ab(12) = 0.0d0

  call dpbstf('L', 4, 2, ab, ldab, info)
  call begin_test('lower_penta_4')
  call print_int('info', info)
  call print_array('ab', ab, 12)
  call end_test()

  ! ===== Test 5: N=1 =====
  ab(1) = 9.0d0
  call dpbstf('U', 1, 0, ab, 1, info)
  call begin_test('n_one')
  call print_int('info', info)
  call print_array('ab', ab, 1)
  call end_test()

  ! ===== Test 6: N=0 (quick return) =====
  call dpbstf('L', 0, 0, ab, 1, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! ===== Test 7: Not positive definite =====
  ! 2x2 matrix with KD=1, UPLO='L'
  ! A = [1 2; 2 1] (not SPD)
  ldab = 2
  ab = 0.0d0
  ab(1) = 1.0d0   ! diag
  ab(2) = 2.0d0   ! subdiag
  ab(3) = 1.0d0   ! diag
  ab(4) = 0.0d0

  call dpbstf('L', 2, 1, ab, ldab, info)
  call begin_test('not_posdef')
  call print_int('info', info)
  call print_array('ab', ab, 4)
  call end_test()

  ! ===== Test 8: UPLO='U', N=7, KD=2 (from LAPACK doc example) =====
  ! Use a strongly diagonally dominant matrix for SPD guarantee
  ! A(i,i) = 10, A(i,i+1) = -1, A(i,i+2) = 0.5
  ldab = 3
  ab = 0.0d0
  ! Col 1
  ab(1) = 0.0d0;   ab(2) = 0.0d0;   ab(3) = 10.0d0
  ! Col 2
  ab(4) = 0.0d0;   ab(5) = -1.0d0;  ab(6) = 10.0d0
  ! Col 3
  ab(7) = 0.5d0;   ab(8) = -1.0d0;  ab(9) = 10.0d0
  ! Col 4
  ab(10) = 0.5d0;  ab(11) = -1.0d0; ab(12) = 10.0d0
  ! Col 5
  ab(13) = 0.5d0;  ab(14) = -1.0d0; ab(15) = 10.0d0
  ! Col 6
  ab(16) = 0.5d0;  ab(17) = -1.0d0; ab(18) = 10.0d0
  ! Col 7
  ab(19) = 0.5d0;  ab(20) = -1.0d0; ab(21) = 10.0d0

  call dpbstf('U', 7, 2, ab, ldab, info)
  call begin_test('upper_7x7_kd2')
  call print_int('info', info)
  call print_array('ab', ab, 21)
  call end_test()

  ! ===== Test 9: UPLO='L', N=7, KD=2 (same matrix, lower storage) =====
  ldab = 3
  ab = 0.0d0
  ! Col 1
  ab(1) = 10.0d0;  ab(2) = -1.0d0;  ab(3) = 0.5d0
  ! Col 2
  ab(4) = 10.0d0;  ab(5) = -1.0d0;  ab(6) = 0.5d0
  ! Col 3
  ab(7) = 10.0d0;  ab(8) = -1.0d0;  ab(9) = 0.5d0
  ! Col 4
  ab(10) = 10.0d0; ab(11) = -1.0d0; ab(12) = 0.5d0
  ! Col 5
  ab(13) = 10.0d0; ab(14) = -1.0d0; ab(15) = 0.5d0
  ! Col 6
  ab(16) = 10.0d0; ab(17) = -1.0d0; ab(18) = 0.0d0
  ! Col 7
  ab(19) = 10.0d0; ab(20) = 0.0d0;  ab(21) = 0.0d0

  call dpbstf('L', 7, 2, ab, ldab, info)
  call begin_test('lower_7x7_kd2')
  call print_int('info', info)
  call print_array('ab', ab, 21)
  call end_test()

  ! ===== Test 10: UPLO='U', N=3, KD=2 (full bandwidth) =====
  ! A = [4 2 1; 2 5 3; 1 3 6]
  ldab = 3
  ab = 0.0d0
  ! Col 1
  ab(1) = 0.0d0;  ab(2) = 0.0d0;  ab(3) = 4.0d0
  ! Col 2
  ab(4) = 0.0d0;  ab(5) = 2.0d0;  ab(6) = 5.0d0
  ! Col 3
  ab(7) = 1.0d0;  ab(8) = 3.0d0;  ab(9) = 6.0d0

  call dpbstf('U', 3, 2, ab, ldab, info)
  call begin_test('upper_full_3')
  call print_int('info', info)
  call print_array('ab', ab, 9)
  call end_test()

end program
