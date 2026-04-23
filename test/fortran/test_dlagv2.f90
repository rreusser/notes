program test_dlagv2
  use test_utils
  implicit none
  double precision :: A(2,2), B(2,2), ALPHAR(2), ALPHAI(2), BETA(2)
  double precision :: CSL, SNL, CSR, SNR

  ! Test 1: Already upper triangular (A(2,1) ~ 0)
  ! This hits the branch: ABS(A(2,1)) <= ULP
  A(1,1) = 4.0d0; A(1,2) = 2.0d0
  A(2,1) = 0.0d0; A(2,2) = 3.0d0
  B(1,1) = 1.0d0; B(1,2) = 0.5d0
  B(2,1) = 0.0d0; B(2,2) = 2.0d0
  call DLAGV2(A, 2, B, 2, ALPHAR, ALPHAI, BETA, CSL, SNL, CSR, SNR)
  call begin_test('already_upper')
  call print_array('A', A, 4)
  call print_array('B', B, 4)
  call print_array('ALPHAR', ALPHAR, 2)
  call print_array('ALPHAI', ALPHAI, 2)
  call print_array('BETA', BETA, 2)
  call print_scalar('CSL', CSL)
  call print_scalar('SNL', SNL)
  call print_scalar('CSR', CSR)
  call print_scalar('SNR', SNR)
  call end_test()

  ! Test 2: B(1,1) ~ 0, triggers DLARTG on A column 1
  A(1,1) = 3.0d0; A(1,2) = 1.0d0
  A(2,1) = 4.0d0; A(2,2) = 2.0d0
  B(1,1) = 0.0d0; B(1,2) = 1.0d0
  B(2,1) = 0.0d0; B(2,2) = 3.0d0
  call DLAGV2(A, 2, B, 2, ALPHAR, ALPHAI, BETA, CSL, SNL, CSR, SNR)
  call begin_test('b11_zero')
  call print_array('A', A, 4)
  call print_array('B', B, 4)
  call print_array('ALPHAR', ALPHAR, 2)
  call print_array('ALPHAI', ALPHAI, 2)
  call print_array('BETA', BETA, 2)
  call print_scalar('CSL', CSL)
  call print_scalar('SNL', SNL)
  call print_scalar('CSR', CSR)
  call print_scalar('SNR', SNR)
  call end_test()

  ! Test 3: B(2,2) ~ 0, triggers DLARTG on A row 2
  A(1,1) = 2.0d0; A(1,2) = 1.0d0
  A(2,1) = 3.0d0; A(2,2) = 5.0d0
  B(1,1) = 4.0d0; B(1,2) = 2.0d0
  B(2,1) = 0.0d0; B(2,2) = 0.0d0
  call DLAGV2(A, 2, B, 2, ALPHAR, ALPHAI, BETA, CSL, SNL, CSR, SNR)
  call begin_test('b22_zero')
  call print_array('A', A, 4)
  call print_array('B', B, 4)
  call print_array('ALPHAR', ALPHAR, 2)
  call print_array('ALPHAI', ALPHAI, 2)
  call print_array('BETA', BETA, 2)
  call print_scalar('CSL', CSL)
  call print_scalar('SNL', SNL)
  call print_scalar('CSR', CSR)
  call print_scalar('SNR', SNR)
  call end_test()

  ! Test 4: General case with real eigenvalues
  A(1,1) = 4.0d0; A(1,2) = 1.0d0
  A(2,1) = 2.0d0; A(2,2) = 3.0d0
  B(1,1) = 2.0d0; B(1,2) = 1.0d0
  B(2,1) = 0.0d0; B(2,2) = 1.0d0
  call DLAGV2(A, 2, B, 2, ALPHAR, ALPHAI, BETA, CSL, SNL, CSR, SNR)
  call begin_test('real_eigenvalues')
  call print_array('A', A, 4)
  call print_array('B', B, 4)
  call print_array('ALPHAR', ALPHAR, 2)
  call print_array('ALPHAI', ALPHAI, 2)
  call print_array('BETA', BETA, 2)
  call print_scalar('CSL', CSL)
  call print_scalar('SNL', SNL)
  call print_scalar('CSR', CSR)
  call print_scalar('SNR', SNR)
  call end_test()

  ! Test 5: Complex conjugate eigenvalues
  A(1,1) = 1.0d0; A(1,2) = -5.0d0
  A(2,1) = 3.0d0; A(2,2) = 1.0d0
  B(1,1) = 1.0d0; B(1,2) = 0.0d0
  B(2,1) = 0.0d0; B(2,2) = 1.0d0
  call DLAGV2(A, 2, B, 2, ALPHAR, ALPHAI, BETA, CSL, SNL, CSR, SNR)
  call begin_test('complex_eigenvalues')
  call print_array('A', A, 4)
  call print_array('B', B, 4)
  call print_array('ALPHAR', ALPHAR, 2)
  call print_array('ALPHAI', ALPHAI, 2)
  call print_array('BETA', BETA, 2)
  call print_scalar('CSL', CSL)
  call print_scalar('SNL', SNL)
  call print_scalar('CSR', CSR)
  call print_scalar('SNR', SNR)
  call end_test()

  ! Test 6: Identity B with general A
  A(1,1) = 5.0d0; A(1,2) = 2.0d0
  A(2,1) = 1.0d0; A(2,2) = 4.0d0
  B(1,1) = 1.0d0; B(1,2) = 0.0d0
  B(2,1) = 0.0d0; B(2,2) = 1.0d0
  call DLAGV2(A, 2, B, 2, ALPHAR, ALPHAI, BETA, CSL, SNL, CSR, SNR)
  call begin_test('identity_b')
  call print_array('A', A, 4)
  call print_array('B', B, 4)
  call print_array('ALPHAR', ALPHAR, 2)
  call print_array('ALPHAI', ALPHAI, 2)
  call print_array('BETA', BETA, 2)
  call print_scalar('CSL', CSL)
  call print_scalar('SNL', SNL)
  call print_scalar('CSR', CSR)
  call print_scalar('SNR', SNR)
  call end_test()

  ! Test 7: Non-trivial B, distinct real eigenvalues
  A(1,1) = 6.0d0; A(1,2) = 3.0d0
  A(2,1) = 2.0d0; A(2,2) = 7.0d0
  B(1,1) = 3.0d0; B(1,2) = 1.0d0
  B(2,1) = 0.0d0; B(2,2) = 2.0d0
  call DLAGV2(A, 2, B, 2, ALPHAR, ALPHAI, BETA, CSL, SNL, CSR, SNR)
  call begin_test('distinct_real')
  call print_array('A', A, 4)
  call print_array('B', B, 4)
  call print_array('ALPHAR', ALPHAR, 2)
  call print_array('ALPHAI', ALPHAI, 2)
  call print_array('BETA', BETA, 2)
  call print_scalar('CSL', CSL)
  call print_scalar('SNL', SNL)
  call print_scalar('CSR', CSR)
  call print_scalar('SNR', SNR)
  call end_test()

  ! Test 8: Negative elements, complex eigenvalue case
  A(1,1) = -2.0d0; A(1,2) = -4.0d0
  A(2,1) = 5.0d0; A(2,2) = -2.0d0
  B(1,1) = 2.0d0; B(1,2) = 1.0d0
  B(2,1) = 0.0d0; B(2,2) = 3.0d0
  call DLAGV2(A, 2, B, 2, ALPHAR, ALPHAI, BETA, CSL, SNL, CSR, SNR)
  call begin_test('negative_complex')
  call print_array('A', A, 4)
  call print_array('B', B, 4)
  call print_array('ALPHAR', ALPHAR, 2)
  call print_array('ALPHAI', ALPHAI, 2)
  call print_array('BETA', BETA, 2)
  call print_scalar('CSL', CSL)
  call print_scalar('SNL', SNL)
  call print_scalar('CSR', CSR)
  call print_scalar('SNR', SNR)
  call end_test()

  ! Test 9: Large values to test scaling
  A(1,1) = 1.0d10; A(1,2) = 5.0d9
  A(2,1) = 3.0d9; A(2,2) = 2.0d10
  B(1,1) = 1.0d10; B(1,2) = 4.0d9
  B(2,1) = 0.0d0; B(2,2) = 8.0d9
  call DLAGV2(A, 2, B, 2, ALPHAR, ALPHAI, BETA, CSL, SNL, CSR, SNR)
  call begin_test('large_values')
  call print_array('A', A, 4)
  call print_array('B', B, 4)
  call print_array('ALPHAR', ALPHAR, 2)
  call print_array('ALPHAI', ALPHAI, 2)
  call print_array('BETA', BETA, 2)
  call print_scalar('CSL', CSL)
  call print_scalar('SNL', SNL)
  call print_scalar('CSR', CSR)
  call print_scalar('SNR', SNR)
  call end_test()

  ! Test 10: Diagonal A with upper triangular B
  A(1,1) = 3.0d0; A(1,2) = 0.0d0
  A(2,1) = 0.0d0; A(2,2) = 7.0d0
  B(1,1) = 2.0d0; B(1,2) = 1.0d0
  B(2,1) = 0.0d0; B(2,2) = 5.0d0
  call DLAGV2(A, 2, B, 2, ALPHAR, ALPHAI, BETA, CSL, SNL, CSR, SNR)
  call begin_test('diagonal_a')
  call print_array('A', A, 4)
  call print_array('B', B, 4)
  call print_array('ALPHAR', ALPHAR, 2)
  call print_array('ALPHAI', ALPHAI, 2)
  call print_array('BETA', BETA, 2)
  call print_scalar('CSL', CSL)
  call print_scalar('SNL', SNL)
  call print_scalar('CSR', CSR)
  call print_scalar('SNR', SNR)
  call end_test()

  ! Test 11: Real eigenvalues with rr <= qq path
  ! Choose A so that scale1*A(1,1)-wr1*B(1,1) and scale1*A(1,2)-wr1*B(1,2)
  ! are small relative to scale1*A(2,1) and h3
  A(1,1) = 1.0d0; A(1,2) = 1.0d0
  A(2,1) = 10.0d0; A(2,2) = 2.0d0
  B(1,1) = 1.0d0; B(1,2) = 0.0d0
  B(2,1) = 0.0d0; B(2,2) = 1.0d0
  call DLAGV2(A, 2, B, 2, ALPHAR, ALPHAI, BETA, CSL, SNL, CSR, SNR)
  call begin_test('real_rr_le_qq')
  call print_array('A', A, 4)
  call print_array('B', B, 4)
  call print_array('ALPHAR', ALPHAR, 2)
  call print_array('ALPHAI', ALPHAI, 2)
  call print_array('BETA', BETA, 2)
  call print_scalar('CSL', CSL)
  call print_scalar('SNL', SNL)
  call print_scalar('CSR', CSR)
  call print_scalar('SNR', SNR)
  call end_test()

end program
