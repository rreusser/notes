program test_dlaqz1
  use test_utils
  implicit none

  double precision :: A(3,3), B(3,3), V(3)
  double precision :: sr1, sr2, si, beta1, beta2

  ! Test 1: real shifts (si = 0), beta1 = beta2 = 1
  A(1,1) = 4.0d0; A(1,2) = 1.0d0; A(1,3) = 0.5d0
  A(2,1) = 2.0d0; A(2,2) = 5.0d0; A(2,3) = 1.0d0
  A(3,1) = 0.0d0; A(3,2) = 3.0d0; A(3,3) = 6.0d0
  B(1,1) = 2.0d0; B(1,2) = 0.5d0; B(1,3) = 0.1d0
  B(2,1) = 0.0d0; B(2,2) = 3.0d0; B(2,3) = 0.5d0
  B(3,1) = 0.0d0; B(3,2) = 0.0d0; B(3,3) = 4.0d0
  sr1 = 1.0d0
  sr2 = 2.0d0
  si = 0.0d0
  beta1 = 1.0d0
  beta2 = 1.0d0
  call DLAQZ1(A, 3, B, 3, sr1, sr2, si, beta1, beta2, V)
  call begin_test('real_shifts_beta1')
  call print_array('v', V, 3)
  call end_test()

  ! Test 2: real shifts (si = 0), nontrivial betas
  A(1,1) = 4.0d0; A(1,2) = 1.0d0; A(1,3) = 0.5d0
  A(2,1) = 2.0d0; A(2,2) = 5.0d0; A(2,3) = 1.0d0
  A(3,1) = 0.0d0; A(3,2) = 3.0d0; A(3,3) = 6.0d0
  B(1,1) = 2.0d0; B(1,2) = 0.5d0; B(1,3) = 0.1d0
  B(2,1) = 0.0d0; B(2,2) = 3.0d0; B(2,3) = 0.5d0
  B(3,1) = 0.0d0; B(3,2) = 0.0d0; B(3,3) = 4.0d0
  sr1 = 1.5d0
  sr2 = 2.5d0
  si = 0.0d0
  beta1 = 0.7d0
  beta2 = 1.3d0
  call DLAQZ1(A, 3, B, 3, sr1, sr2, si, beta1, beta2, V)
  call begin_test('real_shifts_betas')
  call print_array('v', V, 3)
  call end_test()

  ! Test 3: complex conjugate shifts (sr1 == sr2, nonzero si)
  A(1,1) = 3.0d0; A(1,2) = 1.0d0; A(1,3) = 0.5d0
  A(2,1) = 2.0d0; A(2,2) = 4.0d0; A(2,3) = 1.0d0
  A(3,1) = 0.0d0; A(3,2) = 2.0d0; A(3,3) = 5.0d0
  B(1,1) = 1.5d0; B(1,2) = 0.4d0; B(1,3) = 0.2d0
  B(2,1) = 0.0d0; B(2,2) = 2.5d0; B(2,3) = 0.6d0
  B(3,1) = 0.0d0; B(3,2) = 0.0d0; B(3,3) = 3.5d0
  sr1 = 2.0d0
  sr2 = 2.0d0
  si = 1.0d0
  beta1 = 1.0d0
  beta2 = 1.0d0
  call DLAQZ1(A, 3, B, 3, sr1, sr2, si, beta1, beta2, V)
  call begin_test('complex_conj_shifts')
  call print_array('v', V, 3)
  call end_test()

  ! Test 4: another complex conjugate case with betas
  A(1,1) = 5.0d0; A(1,2) = 2.0d0; A(1,3) = 1.0d0
  A(2,1) = 3.0d0; A(2,2) = 4.0d0; A(2,3) = 2.0d0
  A(3,1) = 0.0d0; A(3,2) = 1.0d0; A(3,3) = 6.0d0
  B(1,1) = 2.0d0; B(1,2) = 0.5d0; B(1,3) = 0.3d0
  B(2,1) = 0.0d0; B(2,2) = 3.0d0; B(2,3) = 0.7d0
  B(3,1) = 0.0d0; B(3,2) = 0.0d0; B(3,3) = 5.0d0
  sr1 = 1.5d0
  sr2 = 1.5d0
  si = 0.5d0
  beta1 = 0.8d0
  beta2 = 1.2d0
  call DLAQZ1(A, 3, B, 3, sr1, sr2, si, beta1, beta2, V)
  call begin_test('complex_conj_betas')
  call print_array('v', V, 3)
  call end_test()

  ! Test 5: identity-like B
  A(1,1) = 2.0d0; A(1,2) = 1.0d0; A(1,3) = 0.0d0
  A(2,1) = 1.0d0; A(2,2) = 3.0d0; A(2,3) = 1.0d0
  A(3,1) = 0.0d0; A(3,2) = 1.0d0; A(3,3) = 4.0d0
  B(1,1) = 1.0d0; B(1,2) = 0.0d0; B(1,3) = 0.0d0
  B(2,1) = 0.0d0; B(2,2) = 1.0d0; B(2,3) = 0.0d0
  B(3,1) = 0.0d0; B(3,2) = 0.0d0; B(3,3) = 1.0d0
  sr1 = 1.0d0
  sr2 = 2.0d0
  si = 0.0d0
  beta1 = 1.0d0
  beta2 = 1.0d0
  call DLAQZ1(A, 3, B, 3, sr1, sr2, si, beta1, beta2, V)
  call begin_test('identity_B')
  call print_array('v', V, 3)
  call end_test()

end program
