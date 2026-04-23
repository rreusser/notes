program test_dppequ
  use test_utils
  implicit none

  ! Packed storage: AP has length N*(N+1)/2
  ! Upper: AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j
  !   Diag at positions: 1, 3, 6, 10, ...
  ! Lower: AP(i + (j-1)*(2n-j)/2) = A(i,j) for j<=i<=n
  !   Diag at positions: 1, N+1, 2N, 3N-2, ...

  double precision :: ap(50), s(10)
  double precision :: scond, amax
  integer :: info

  ! Test 1: UPLO='U', N=4, SPD packed matrix
  ! Matrix diagonal: 4.0, 9.0, 16.0, 25.0
  ! Upper packed: col 1 = [4], col 2 = [1, 9], col 3 = [0.5, 2, 16], col 4 = [0.3, 1.5, 3, 25]
  ! Positions: AP(1)=4, AP(2)=1, AP(3)=9, AP(4)=0.5, AP(5)=2, AP(6)=16, AP(7)=0.3, AP(8)=1.5, AP(9)=3, AP(10)=25
  ap = 0.0d0
  ap(1) = 4.0d0
  ap(2) = 1.0d0
  ap(3) = 9.0d0
  ap(4) = 0.5d0
  ap(5) = 2.0d0
  ap(6) = 16.0d0
  ap(7) = 0.3d0
  ap(8) = 1.5d0
  ap(9) = 3.0d0
  ap(10) = 25.0d0

  call dppequ('U', 4, ap, s, scond, amax, info)
  call begin_test('upper_basic')
  call print_array('s', s, 4)
  call print_scalar('scond', scond)
  call print_scalar('amax', amax)
  call print_int('info', info)
  call end_test()

  ! Test 2: UPLO='L', N=4, SPD packed matrix
  ! Same diagonal: 4.0, 9.0, 16.0, 25.0
  ! Lower packed: col 1 = [4, 1, 0.5, 0.3], col 2 = [9, 2, 1.5], col 3 = [16, 3], col 4 = [25]
  ! Positions: AP(1)=4, AP(2)=1, AP(3)=0.5, AP(4)=0.3, AP(5)=9, AP(6)=2, AP(7)=1.5, AP(8)=16, AP(9)=3, AP(10)=25
  ap = 0.0d0
  ap(1) = 4.0d0
  ap(2) = 1.0d0
  ap(3) = 0.5d0
  ap(4) = 0.3d0
  ap(5) = 9.0d0
  ap(6) = 2.0d0
  ap(7) = 1.5d0
  ap(8) = 16.0d0
  ap(9) = 3.0d0
  ap(10) = 25.0d0

  call dppequ('L', 4, ap, s, scond, amax, info)
  call begin_test('lower_basic')
  call print_array('s', s, 4)
  call print_scalar('scond', scond)
  call print_scalar('amax', amax)
  call print_int('info', info)
  call end_test()

  ! Test 3: N=0 (quick return)
  call dppequ('U', 0, ap, s, scond, amax, info)
  call begin_test('n_zero')
  call print_scalar('scond', scond)
  call print_scalar('amax', amax)
  call print_int('info', info)
  call end_test()

  ! Test 4: N=1 (single element)
  ap(1) = 49.0d0
  call dppequ('U', 1, ap, s, scond, amax, info)
  call begin_test('n_one')
  call print_array('s', s, 1)
  call print_scalar('scond', scond)
  call print_scalar('amax', amax)
  call print_int('info', info)
  call end_test()

  ! Test 5: Non-positive diagonal (upper), info > 0
  ! Diagonal at positions 1, 3, 6: values 4.0, -1.0, 9.0
  ap = 0.0d0
  ap(1) = 4.0d0
  ap(2) = 1.0d0
  ap(3) = -1.0d0
  ap(4) = 0.5d0
  ap(5) = 2.0d0
  ap(6) = 9.0d0

  call dppequ('U', 3, ap, s, scond, amax, info)
  call begin_test('non_positive_upper')
  call print_int('info', info)
  call end_test()

  ! Test 6: Zero diagonal (lower), info > 0
  ! N=3, Lower packed: diag at 1, 4, 6
  ! Diagonal: 4.0, 0.0, 9.0
  ap = 0.0d0
  ap(1) = 4.0d0
  ap(2) = 1.0d0
  ap(3) = 0.5d0
  ap(4) = 0.0d0
  ap(5) = 2.0d0
  ap(6) = 9.0d0

  call dppequ('L', 3, ap, s, scond, amax, info)
  call begin_test('zero_diag_lower')
  call print_int('info', info)
  call end_test()

  ! Test 7: Identity matrix (upper packed), N=3
  ! Diag at 1, 3, 6: all 1.0
  ap = 0.0d0
  ap(1) = 1.0d0
  ap(2) = 0.0d0
  ap(3) = 1.0d0
  ap(4) = 0.0d0
  ap(5) = 0.0d0
  ap(6) = 1.0d0

  call dppequ('U', 3, ap, s, scond, amax, info)
  call begin_test('identity_upper')
  call print_array('s', s, 3)
  call print_scalar('scond', scond)
  call print_scalar('amax', amax)
  call print_int('info', info)
  call end_test()

  ! Test 8: Diagonal varied scales (lower), N=3
  ! Diagonal: 100.0, 1.0, 0.25
  ! Lower packed positions: 1, 4, 6
  ap = 0.0d0
  ap(1) = 100.0d0
  ap(2) = 5.0d0
  ap(3) = 2.0d0
  ap(4) = 1.0d0
  ap(5) = 0.1d0
  ap(6) = 0.25d0

  call dppequ('L', 3, ap, s, scond, amax, info)
  call begin_test('diagonal_varied_lower')
  call print_array('s', s, 3)
  call print_scalar('scond', scond)
  call print_scalar('amax', amax)
  call print_int('info', info)
  call end_test()

  ! Test 9: Non-positive at first element (lower)
  ! Diagonal: -2.0, 4.0, 9.0
  ap = 0.0d0
  ap(1) = -2.0d0
  ap(2) = 1.0d0
  ap(3) = 0.5d0
  ap(4) = 4.0d0
  ap(5) = 2.0d0
  ap(6) = 9.0d0

  call dppequ('L', 3, ap, s, scond, amax, info)
  call begin_test('non_positive_first')
  call print_int('info', info)
  call end_test()

  ! Test 10: Non-positive at last element (upper)
  ! Diagonal at 1, 3, 6: values 4.0, 9.0, -3.0
  ap = 0.0d0
  ap(1) = 4.0d0
  ap(2) = 1.0d0
  ap(3) = 9.0d0
  ap(4) = 0.5d0
  ap(5) = 2.0d0
  ap(6) = -3.0d0

  call dppequ('U', 3, ap, s, scond, amax, info)
  call begin_test('non_positive_last')
  call print_int('info', info)
  call end_test()

end program
