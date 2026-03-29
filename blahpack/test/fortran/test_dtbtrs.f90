program test_dtbtrs
  use test_utils
  implicit none
  double precision :: ab(100), b(100)
  integer :: info, ldab

  ! ---------------------------------------------------------------
  ! Test 1: 4x4 upper triangular banded solve, KD=2, no transpose
  ! Band storage (LDAB=3, KD=2):
  !   Row 0 (2nd superdiag): *, *, a13, a24
  !   Row 1 (1st superdiag): *, a12, a23, a34
  !   Row 2 (diagonal):      a11, a22, a33, a44
  !
  ! Dense A (upper tri, bandwidth 2):
  !   [3  1  2  0]
  !   [0  4  1  3]
  !   [0  0  5  2]
  !   [0  0  0  6]
  ! ---------------------------------------------------------------
  ab = 0.0d0; b = 0.0d0
  ldab = 3
  ! Col 1: ab(1,1)=*, ab(2,1)=*, ab(3,1)=3
  ab(1) = 0.0d0; ab(2) = 0.0d0; ab(3) = 3.0d0
  ! Col 2: ab(1,2)=*, ab(2,2)=1, ab(3,2)=4
  ab(4) = 0.0d0; ab(5) = 1.0d0; ab(6) = 4.0d0
  ! Col 3: ab(1,3)=2, ab(2,3)=1, ab(3,3)=5
  ab(7) = 2.0d0; ab(8) = 1.0d0; ab(9) = 5.0d0
  ! Col 4: ab(1,4)=0, ab(2,4)=3, ab(3,4)=6
  ab(10) = 0.0d0; ab(11) = 3.0d0; ab(12) = 6.0d0
  b(1) = 10.0d0; b(2) = 20.0d0; b(3) = 15.0d0; b(4) = 12.0d0
  call dtbtrs('U', 'N', 'N', 4, 2, 1, ab, ldab, b, 4, info)
  call begin_test('upper_no_trans')
  call print_array('x', b, 4)
  call print_int('info', info)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 2: 4x4 lower triangular banded solve, KD=2, no transpose
  ! Band storage (LDAB=3, KD=2):
  !   Row 0 (diagonal):      a11, a22, a33, a44
  !   Row 1 (1st subdiag):   a21, a32, a43, *
  !   Row 2 (2nd subdiag):   a31, a42, *, *
  !
  ! Dense A (lower tri, bandwidth 2):
  !   [3  0  0  0]
  !   [1  4  0  0]
  !   [2  1  5  0]
  !   [0  3  2  6]
  ! ---------------------------------------------------------------
  ab = 0.0d0; b = 0.0d0
  ldab = 3
  ! Col 1: ab(1,1)=3, ab(2,1)=1, ab(3,1)=2
  ab(1) = 3.0d0; ab(2) = 1.0d0; ab(3) = 2.0d0
  ! Col 2: ab(1,2)=4, ab(2,2)=1, ab(3,2)=3
  ab(4) = 4.0d0; ab(5) = 1.0d0; ab(6) = 3.0d0
  ! Col 3: ab(1,3)=5, ab(2,3)=2, ab(3,3)=*
  ab(7) = 5.0d0; ab(8) = 2.0d0; ab(9) = 0.0d0
  ! Col 4: ab(1,4)=6, ab(2,4)=*, ab(3,4)=*
  ab(10) = 6.0d0; ab(11) = 0.0d0; ab(12) = 0.0d0
  b(1) = 10.0d0; b(2) = 20.0d0; b(3) = 15.0d0; b(4) = 12.0d0
  call dtbtrs('L', 'N', 'N', 4, 2, 1, ab, ldab, b, 4, info)
  call begin_test('lower_no_trans')
  call print_array('x', b, 4)
  call print_int('info', info)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 3: upper triangular banded, transpose
  ! Same AB as Test 1
  ! ---------------------------------------------------------------
  ab = 0.0d0; b = 0.0d0
  ldab = 3
  ab(1) = 0.0d0; ab(2) = 0.0d0; ab(3) = 3.0d0
  ab(4) = 0.0d0; ab(5) = 1.0d0; ab(6) = 4.0d0
  ab(7) = 2.0d0; ab(8) = 1.0d0; ab(9) = 5.0d0
  ab(10) = 0.0d0; ab(11) = 3.0d0; ab(12) = 6.0d0
  b(1) = 10.0d0; b(2) = 20.0d0; b(3) = 15.0d0; b(4) = 12.0d0
  call dtbtrs('U', 'T', 'N', 4, 2, 1, ab, ldab, b, 4, info)
  call begin_test('upper_trans')
  call print_array('x', b, 4)
  call print_int('info', info)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 4: lower triangular banded, transpose
  ! Same AB as Test 2
  ! ---------------------------------------------------------------
  ab = 0.0d0; b = 0.0d0
  ldab = 3
  ab(1) = 3.0d0; ab(2) = 1.0d0; ab(3) = 2.0d0
  ab(4) = 4.0d0; ab(5) = 1.0d0; ab(6) = 3.0d0
  ab(7) = 5.0d0; ab(8) = 2.0d0; ab(9) = 0.0d0
  ab(10) = 6.0d0; ab(11) = 0.0d0; ab(12) = 0.0d0
  b(1) = 10.0d0; b(2) = 20.0d0; b(3) = 15.0d0; b(4) = 12.0d0
  call dtbtrs('L', 'T', 'N', 4, 2, 1, ab, ldab, b, 4, info)
  call begin_test('lower_trans')
  call print_array('x', b, 4)
  call print_int('info', info)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 5: unit diagonal, upper triangular
  ! Band storage (LDAB=2, KD=1):
  !   Row 0 (1st superdiag): *, a12, a23, a34
  !   Row 1 (diagonal):      1, 1, 1, 1
  !
  ! Dense A = [1  2  0  0]
  !           [0  1  3  0]
  !           [0  0  1  4]
  !           [0  0  0  1]
  ! ---------------------------------------------------------------
  ab = 0.0d0; b = 0.0d0
  ldab = 2
  ! Col 1: ab(1,1)=*, ab(2,1)=1
  ab(1) = 0.0d0; ab(2) = 1.0d0
  ! Col 2: ab(1,2)=2, ab(2,2)=1
  ab(3) = 2.0d0; ab(4) = 1.0d0
  ! Col 3: ab(1,3)=3, ab(2,3)=1
  ab(5) = 3.0d0; ab(6) = 1.0d0
  ! Col 4: ab(1,4)=4, ab(2,4)=1
  ab(7) = 4.0d0; ab(8) = 1.0d0
  b(1) = 10.0d0; b(2) = 6.0d0; b(3) = 3.0d0; b(4) = 1.0d0
  call dtbtrs('U', 'N', 'U', 4, 1, 1, ab, ldab, b, 4, info)
  call begin_test('upper_unit_diag')
  call print_array('x', b, 4)
  call print_int('info', info)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 6: N=0 quick return
  ! ---------------------------------------------------------------
  info = 999
  call dtbtrs('U', 'N', 'N', 0, 0, 1, ab, 1, b, 1, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 7: singular matrix (zero on diagonal) -> info > 0
  ! Upper band with KD=1:
  !   Row 0 (superdiag): *, 1, 1
  !   Row 1 (diagonal):  2, 0, 3   <- A(2,2)=0 singular
  ! ---------------------------------------------------------------
  ab = 0.0d0; b = 0.0d0
  ldab = 2
  ab(1) = 0.0d0; ab(2) = 2.0d0
  ab(3) = 1.0d0; ab(4) = 0.0d0
  ab(5) = 1.0d0; ab(6) = 3.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  call dtbtrs('U', 'N', 'N', 3, 1, 1, ab, ldab, b, 3, info)
  call begin_test('singular')
  call print_int('info', info)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 8: multiple RHS (NRHS=2), upper banded KD=1
  ! AB same as test 1 structure but simpler: KD=1
  !   Row 0 (superdiag): *, 1, 2
  !   Row 1 (diagonal):  3, 4, 5
  ! Dense: [3 1 0; 0 4 2; 0 0 5]
  ! ---------------------------------------------------------------
  ab = 0.0d0; b = 0.0d0
  ldab = 2
  ab(1) = 0.0d0; ab(2) = 3.0d0
  ab(3) = 1.0d0; ab(4) = 4.0d0
  ab(5) = 2.0d0; ab(6) = 5.0d0
  ! B is 3x2, col-major
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  b(4) = 7.0d0; b(5) = 8.0d0; b(6) = 9.0d0
  call dtbtrs('U', 'N', 'N', 3, 1, 2, ab, ldab, b, 3, info)
  call begin_test('multi_rhs')
  call print_array('x', b, 6)
  call print_int('info', info)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 9: N=1 edge case
  ! ---------------------------------------------------------------
  ab = 0.0d0; b = 0.0d0
  ldab = 1
  ab(1) = 5.0d0
  b(1) = 15.0d0
  call dtbtrs('U', 'N', 'N', 1, 0, 1, ab, ldab, b, 1, info)
  call begin_test('n_one')
  call print_array('x', b, 1)
  call print_int('info', info)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 10: lower triangular, singular at row 1
  ! KD=1:
  !   Row 0 (diagonal):  0, 4, 5  <- A(1,1)=0 singular
  !   Row 1 (subdiag):   1, 2, *
  ! ---------------------------------------------------------------
  ab = 0.0d0; b = 0.0d0
  ldab = 2
  ab(1) = 0.0d0; ab(2) = 1.0d0
  ab(3) = 4.0d0; ab(4) = 2.0d0
  ab(5) = 5.0d0; ab(6) = 0.0d0
  b(1) = 1.0d0; b(2) = 2.0d0; b(3) = 3.0d0
  call dtbtrs('L', 'N', 'N', 3, 1, 1, ab, ldab, b, 3, info)
  call begin_test('lower_singular')
  call print_int('info', info)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 11: conjugate-transpose (same as transpose for real)
  ! Same AB as test 1 (upper, KD=2)
  ! ---------------------------------------------------------------
  ab = 0.0d0; b = 0.0d0
  ldab = 3
  ab(1) = 0.0d0; ab(2) = 0.0d0; ab(3) = 3.0d0
  ab(4) = 0.0d0; ab(5) = 1.0d0; ab(6) = 4.0d0
  ab(7) = 2.0d0; ab(8) = 1.0d0; ab(9) = 5.0d0
  ab(10) = 0.0d0; ab(11) = 3.0d0; ab(12) = 6.0d0
  b(1) = 10.0d0; b(2) = 20.0d0; b(3) = 15.0d0; b(4) = 12.0d0
  call dtbtrs('U', 'C', 'N', 4, 2, 1, ab, ldab, b, 4, info)
  call begin_test('upper_conj_trans')
  call print_array('x', b, 4)
  call print_int('info', info)
  call end_test()

end program
