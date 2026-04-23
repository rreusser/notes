program test_dpbequ
  use test_utils
  implicit none

  ! Band storage: LDAB = KD+1 rows, N columns
  ! Upper: AB(KD+1+i-j, j) = A(i,j) for max(1,j-kd)<=i<=j
  !   => diagonal at row KD+1
  ! Lower: AB(1+i-j, j) = A(i,j) for j<=i<=min(n,j+kd)
  !   => diagonal at row 1

  double precision :: ab(50), s(10)
  double precision :: scond, amax
  integer :: info, ldab

  ! Test 1: UPLO='U', N=4, KD=2, SPD band matrix
  ! Diagonal: 4.0, 9.0, 16.0, 25.0
  ! Band storage (upper): LDAB=3 (KD+1=3), 4 columns
  ! Row 1 = 2nd superdiag, Row 2 = 1st superdiag, Row 3 = diagonal
  ldab = 3
  ab = 0.0d0
  ! Col 1: row3=diag
  ab(3) = 4.0d0
  ! Col 2: row2=superdiag, row3=diag
  ab(5) = 1.0d0
  ab(6) = 9.0d0
  ! Col 3: row1=2nd superdiag, row2=superdiag, row3=diag
  ab(7) = 0.5d0
  ab(8) = 2.0d0
  ab(9) = 16.0d0
  ! Col 4: row1=2nd superdiag, row2=superdiag, row3=diag
  ab(10) = 0.0d0
  ab(11) = 1.5d0
  ab(12) = 25.0d0

  call dpbequ('U', 4, 2, ab, ldab, s, scond, amax, info)
  call begin_test('upper_basic')
  call print_array('s', s, 4)
  call print_scalar('scond', scond)
  call print_scalar('amax', amax)
  call print_int('info', info)
  call end_test()

  ! Test 2: UPLO='L', N=4, KD=2, same diagonal
  ! Lower: LDAB=3, diagonal at row 1
  ldab = 3
  ab = 0.0d0
  ! Col 1: row1=diag, row2=subdiag, row3=2nd subdiag
  ab(1) = 4.0d0
  ab(2) = 1.0d0
  ab(3) = 0.5d0
  ! Col 2
  ab(4) = 9.0d0
  ab(5) = 2.0d0
  ab(6) = 0.0d0
  ! Col 3
  ab(7) = 16.0d0
  ab(8) = 1.5d0
  ab(9) = 0.0d0
  ! Col 4
  ab(10) = 25.0d0

  call dpbequ('L', 4, 2, ab, ldab, s, scond, amax, info)
  call begin_test('lower_basic')
  call print_array('s', s, 4)
  call print_scalar('scond', scond)
  call print_scalar('amax', amax)
  call print_int('info', info)
  call end_test()

  ! Test 3: N=0 (quick return)
  call dpbequ('U', 0, 0, ab, 1, s, scond, amax, info)
  call begin_test('n_zero')
  call print_scalar('scond', scond)
  call print_scalar('amax', amax)
  call print_int('info', info)
  call end_test()

  ! Test 4: N=1, KD=0 (single element)
  ldab = 1
  ab(1) = 49.0d0
  call dpbequ('U', 1, 0, ab, ldab, s, scond, amax, info)
  call begin_test('n_one')
  call print_array('s', s, 1)
  call print_scalar('scond', scond)
  call print_scalar('amax', amax)
  call print_int('info', info)
  call end_test()

  ! Test 5: Non-positive diagonal (upper), info > 0
  ! Diagonal: 4.0, -1.0, 9.0
  ldab = 2
  ab = 0.0d0
  ! Col 1: row2=diag
  ab(2) = 4.0d0
  ! Col 2: row1=superdiag, row2=diag
  ab(3) = 1.0d0
  ab(4) = -1.0d0
  ! Col 3: row1=superdiag, row2=diag
  ab(5) = 0.5d0
  ab(6) = 9.0d0

  call dpbequ('U', 3, 1, ab, ldab, s, scond, amax, info)
  call begin_test('non_positive_upper')
  call print_int('info', info)
  call end_test()

  ! Test 6: Zero diagonal (lower), info > 0
  ! Diagonal: 4.0, 0.0, 9.0
  ldab = 2
  ab = 0.0d0
  ! Col 1: row1=diag, row2=subdiag
  ab(1) = 4.0d0
  ab(2) = 1.0d0
  ! Col 2
  ab(3) = 0.0d0
  ab(4) = 0.5d0
  ! Col 3
  ab(5) = 9.0d0

  call dpbequ('L', 3, 1, ab, ldab, s, scond, amax, info)
  call begin_test('zero_diag_lower')
  call print_int('info', info)
  call end_test()

  ! Test 7: Identity band matrix (upper), N=3, KD=1
  ldab = 2
  ab = 0.0d0
  ab(2) = 1.0d0  ! Col 1 diag
  ab(4) = 1.0d0  ! Col 2 diag
  ab(6) = 1.0d0  ! Col 3 diag

  call dpbequ('U', 3, 1, ab, ldab, s, scond, amax, info)
  call begin_test('identity_upper')
  call print_array('s', s, 3)
  call print_scalar('scond', scond)
  call print_scalar('amax', amax)
  call print_int('info', info)
  call end_test()

  ! Test 8: Diagonal varied scales (lower), N=3, KD=0
  ! Only diagonal, no off-diag bands
  ldab = 1
  ab = 0.0d0
  ab(1) = 100.0d0  ! Col 1 diag
  ab(2) = 1.0d0    ! Col 2 diag
  ab(3) = 0.25d0   ! Col 3 diag

  call dpbequ('L', 3, 0, ab, ldab, s, scond, amax, info)
  call begin_test('diagonal_varied_lower')
  call print_array('s', s, 3)
  call print_scalar('scond', scond)
  call print_scalar('amax', amax)
  call print_int('info', info)
  call end_test()

  ! Test 9: Non-positive diagonal at first element (lower)
  ! Diagonal: -2.0, 4.0, 9.0
  ldab = 1
  ab = 0.0d0
  ab(1) = -2.0d0
  ab(2) = 4.0d0
  ab(3) = 9.0d0

  call dpbequ('L', 3, 0, ab, ldab, s, scond, amax, info)
  call begin_test('non_positive_first')
  call print_int('info', info)
  call end_test()

  ! Test 10: Non-positive at last element (upper)
  ! Diagonal: 4.0, 9.0, -3.0
  ldab = 1
  ab = 0.0d0
  ab(1) = 4.0d0
  ab(2) = 9.0d0
  ab(3) = -3.0d0

  call dpbequ('U', 3, 0, ab, ldab, s, scond, amax, info)
  call begin_test('non_positive_last')
  call print_int('info', info)
  call end_test()

end program
