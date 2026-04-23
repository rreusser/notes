program test_dlaqr1
  use test_utils
  implicit none

  double precision :: H(3,3), V(3)
  double precision :: sr1, si1, sr2, si2

  ! Test 1: 2x2, real shifts (si1=si2=0)
  H(1,1) = 4.0d0
  H(1,2) = 3.0d0
  H(2,1) = 2.0d0
  H(2,2) = 1.0d0
  sr1 = 1.0d0
  si1 = 0.0d0
  sr2 = 2.0d0
  si2 = 0.0d0
  call DLAQR1(2, H, 3, sr1, si1, sr2, si2, V)
  call begin_test('2x2_real_shifts')
  call print_array('v', V, 2)
  call end_test()

  ! Test 2: 2x2, complex conjugate shifts (sr1=sr2, si1=-si2)
  H(1,1) = 5.0d0
  H(1,2) = 2.0d0
  H(2,1) = 3.0d0
  H(2,2) = 4.0d0
  sr1 = 3.0d0
  si1 = 1.0d0
  sr2 = 3.0d0
  si2 = -1.0d0
  call DLAQR1(2, H, 3, sr1, si1, sr2, si2, V)
  call begin_test('2x2_complex_shifts')
  call print_array('v', V, 2)
  call end_test()

  ! Test 3: 3x3, real shifts
  H(1,1) = 6.0d0
  H(1,2) = 2.0d0
  H(1,3) = 1.0d0
  H(2,1) = 3.0d0
  H(2,2) = 5.0d0
  H(2,3) = 4.0d0
  H(3,1) = 1.0d0
  H(3,2) = 2.0d0
  H(3,3) = 3.0d0
  sr1 = 1.0d0
  si1 = 0.0d0
  sr2 = 2.0d0
  si2 = 0.0d0
  call DLAQR1(3, H, 3, sr1, si1, sr2, si2, V)
  call begin_test('3x3_real_shifts')
  call print_array('v', V, 3)
  call end_test()

  ! Test 4: 3x3, complex conjugate shifts
  H(1,1) = 6.0d0
  H(1,2) = 2.0d0
  H(1,3) = 1.0d0
  H(2,1) = 3.0d0
  H(2,2) = 5.0d0
  H(2,3) = 4.0d0
  H(3,1) = 1.0d0
  H(3,2) = 2.0d0
  H(3,3) = 3.0d0
  sr1 = 4.0d0
  si1 = 2.0d0
  sr2 = 4.0d0
  si2 = -2.0d0
  call DLAQR1(3, H, 3, sr1, si1, sr2, si2, V)
  call begin_test('3x3_complex_shifts')
  call print_array('v', V, 3)
  call end_test()

  ! Test 5: 2x2, S=0 (all zeros and shifts match diagonal)
  H(1,1) = 0.0d0
  H(1,2) = 0.0d0
  H(2,1) = 0.0d0
  H(2,2) = 0.0d0
  sr1 = 0.0d0
  si1 = 0.0d0
  sr2 = 0.0d0
  si2 = 0.0d0
  call DLAQR1(2, H, 3, sr1, si1, sr2, si2, V)
  call begin_test('2x2_zero_matrix')
  call print_array('v', V, 2)
  call end_test()

  ! Test 6: 3x3, S=0 (zero subdiag + shift matches H(1,1))
  H(1,1) = 0.0d0
  H(1,2) = 0.0d0
  H(1,3) = 0.0d0
  H(2,1) = 0.0d0
  H(2,2) = 0.0d0
  H(2,3) = 0.0d0
  H(3,1) = 0.0d0
  H(3,2) = 0.0d0
  H(3,3) = 0.0d0
  sr1 = 0.0d0
  si1 = 0.0d0
  sr2 = 0.0d0
  si2 = 0.0d0
  call DLAQR1(3, H, 3, sr1, si1, sr2, si2, V)
  call begin_test('3x3_zero_matrix')
  call print_array('v', V, 3)
  call end_test()

  ! Test 7: N=1 (quick return, should do nothing)
  V(1) = 99.0d0
  call DLAQR1(1, H, 3, sr1, si1, sr2, si2, V)
  call begin_test('n1_quick_return')
  call print_array('v', V, 1)
  call end_test()

  ! Test 8: 2x2 with large subdiagonal
  H(1,1) = 1.0d0
  H(1,2) = 1.0d0
  H(2,1) = 1.0d+100
  H(2,2) = 1.0d0
  sr1 = 1.0d0
  si1 = 0.0d0
  sr2 = 1.0d0
  si2 = 0.0d0
  call DLAQR1(2, H, 3, sr1, si1, sr2, si2, V)
  call begin_test('2x2_large_subdiag')
  call print_array('v', V, 2)
  call end_test()

end program
