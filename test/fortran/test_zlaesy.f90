program test_zlaesy
  use test_utils
  implicit none

  complex*16 :: A, B, C, RT1, RT2, EVSCAL, CS1, SN1
  double precision :: rt1_r(2), rt2_r(2), evscal_r(2), cs1_r(2), sn1_r(2)
  equivalence (RT1, rt1_r)
  equivalence (RT2, rt2_r)
  equivalence (EVSCAL, evscal_r)
  equivalence (CS1, cs1_r)
  equivalence (SN1, sn1_r)

  ! Test 1: diagonal matrix (B=0)
  A = (2.0D0, 1.0D0)
  B = (0.0D0, 0.0D0)
  C = (1.0D0, -1.0D0)
  call ZLAESY(A, B, C, RT1, RT2, EVSCAL, CS1, SN1)
  call begin_test('diagonal')
  call print_array('rt1', rt1_r, 2)
  call print_array('rt2', rt2_r, 2)
  call print_array('evscal', evscal_r, 2)
  call print_array('cs1', cs1_r, 2)
  call print_array('sn1', sn1_r, 2)
  call end_test()

  ! Test 2: diagonal matrix (B=0) with |A| < |C| (swap)
  A = (0.5D0, 0.0D0)
  B = (0.0D0, 0.0D0)
  C = (3.0D0, 2.0D0)
  call ZLAESY(A, B, C, RT1, RT2, EVSCAL, CS1, SN1)
  call begin_test('diagonal_swap')
  call print_array('rt1', rt1_r, 2)
  call print_array('rt2', rt2_r, 2)
  call print_array('evscal', evscal_r, 2)
  call print_array('cs1', cs1_r, 2)
  call print_array('sn1', sn1_r, 2)
  call end_test()

  ! Test 3: basic non-diagonal real symmetric
  A = (4.0D0, 0.0D0)
  B = (1.0D0, 0.0D0)
  C = (2.0D0, 0.0D0)
  call ZLAESY(A, B, C, RT1, RT2, EVSCAL, CS1, SN1)
  call begin_test('real_symmetric')
  call print_array('rt1', rt1_r, 2)
  call print_array('rt2', rt2_r, 2)
  call print_array('evscal', evscal_r, 2)
  call print_array('cs1', cs1_r, 2)
  call print_array('sn1', sn1_r, 2)
  call end_test()

  ! Test 4: complex symmetric (non-Hermitian)
  A = (1.0D0, 2.0D0)
  B = (3.0D0, 1.0D0)
  C = (2.0D0, -1.0D0)
  call ZLAESY(A, B, C, RT1, RT2, EVSCAL, CS1, SN1)
  call begin_test('complex_symmetric')
  call print_array('rt1', rt1_r, 2)
  call print_array('rt2', rt2_r, 2)
  call print_array('evscal', evscal_r, 2)
  call print_array('cs1', cs1_r, 2)
  call print_array('sn1', sn1_r, 2)
  call end_test()

  ! Test 5: identity matrix
  A = (1.0D0, 0.0D0)
  B = (0.0D0, 0.0D0)
  C = (1.0D0, 0.0D0)
  call ZLAESY(A, B, C, RT1, RT2, EVSCAL, CS1, SN1)
  call begin_test('identity')
  call print_array('rt1', rt1_r, 2)
  call print_array('rt2', rt2_r, 2)
  call print_array('evscal', evscal_r, 2)
  call print_array('cs1', cs1_r, 2)
  call print_array('sn1', sn1_r, 2)
  call end_test()

  ! Test 6: zero matrix
  A = (0.0D0, 0.0D0)
  B = (0.0D0, 0.0D0)
  C = (0.0D0, 0.0D0)
  call ZLAESY(A, B, C, RT1, RT2, EVSCAL, CS1, SN1)
  call begin_test('zero_matrix')
  call print_array('rt1', rt1_r, 2)
  call print_array('rt2', rt2_r, 2)
  call print_array('evscal', evscal_r, 2)
  call print_array('cs1', cs1_r, 2)
  call print_array('sn1', sn1_r, 2)
  call end_test()

  ! Test 7: purely imaginary B
  A = (1.0D0, 0.0D0)
  B = (0.0D0, 2.0D0)
  C = (3.0D0, 0.0D0)
  call ZLAESY(A, B, C, RT1, RT2, EVSCAL, CS1, SN1)
  call begin_test('imaginary_b')
  call print_array('rt1', rt1_r, 2)
  call print_array('rt2', rt2_r, 2)
  call print_array('evscal', evscal_r, 2)
  call print_array('cs1', cs1_r, 2)
  call print_array('sn1', sn1_r, 2)
  call end_test()

  ! Test 8: A = C (equal diagonal) with non-zero B
  A = (2.0D0, 1.0D0)
  B = (1.0D0, 1.0D0)
  C = (2.0D0, 1.0D0)
  call ZLAESY(A, B, C, RT1, RT2, EVSCAL, CS1, SN1)
  call begin_test('equal_diagonal')
  call print_array('rt1', rt1_r, 2)
  call print_array('rt2', rt2_r, 2)
  call print_array('evscal', evscal_r, 2)
  call print_array('cs1', cs1_r, 2)
  call print_array('sn1', sn1_r, 2)
  call end_test()

  ! Test 9: large values (overflow-safe sqrt branch)
  A = (1.0D100, 0.0D0)
  B = (1.0D100, 0.0D0)
  C = (-1.0D100, 0.0D0)
  call ZLAESY(A, B, C, RT1, RT2, EVSCAL, CS1, SN1)
  call begin_test('large_values')
  call print_array('rt1', rt1_r, 2)
  call print_array('rt2', rt2_r, 2)
  call print_array('evscal', evscal_r, 2)
  call print_array('cs1', cs1_r, 2)
  call print_array('sn1', sn1_r, 2)
  call end_test()

  ! Test 10: eigenvalue swap case (|RT1| < |RT2| initially)
  A = (0.1D0, 0.0D0)
  B = (5.0D0, 0.0D0)
  C = (0.2D0, 0.0D0)
  call ZLAESY(A, B, C, RT1, RT2, EVSCAL, CS1, SN1)
  call begin_test('eigenvalue_swap')
  call print_array('rt1', rt1_r, 2)
  call print_array('rt2', rt2_r, 2)
  call print_array('evscal', evscal_r, 2)
  call print_array('cs1', cs1_r, 2)
  call print_array('sn1', sn1_r, 2)
  call end_test()

  ! Test 11: TABS > ONE branch for eigenvector normalization
  A = (0.0D0, 0.0D0)
  B = (0.01D0, 0.0D0)
  C = (100.0D0, 0.0D0)
  call ZLAESY(A, B, C, RT1, RT2, EVSCAL, CS1, SN1)
  call begin_test('large_sn1')
  call print_array('rt1', rt1_r, 2)
  call print_array('rt2', rt2_r, 2)
  call print_array('evscal', evscal_r, 2)
  call print_array('cs1', cs1_r, 2)
  call print_array('sn1', sn1_r, 2)
  call end_test()

  ! Test 12: eigenvalue swap in non-diagonal case (|S+T| < |S-T|)
  A = (-10.0D0, 0.0D0)
  B = (0.1D0, 0.0D0)
  C = (-8.0D0, 0.0D0)
  call ZLAESY(A, B, C, RT1, RT2, EVSCAL, CS1, SN1)
  call begin_test('nondiag_swap')
  call print_array('rt1', rt1_r, 2)
  call print_array('rt2', rt2_r, 2)
  call print_array('evscal', evscal_r, 2)
  call print_array('cs1', cs1_r, 2)
  call print_array('sn1', sn1_r, 2)
  call end_test()

  ! Test 13: csqrt with negative real and nonzero imag part
  ! A = 1+4i, B = 1+i, C = 1
  ! T_init = 2i, Z=2, (T/Z)^2 = -1, (B/Z)^2 = 0.5i
  ! csqrt arg = -1 + 0.5i (re<0, im!=0)
  A = (1.0D0, 4.0D0)
  B = (1.0D0, 1.0D0)
  C = (1.0D0, 0.0D0)
  call ZLAESY(A, B, C, RT1, RT2, EVSCAL, CS1, SN1)
  call begin_test('csqrt_neg_real')
  call print_array('rt1', rt1_r, 2)
  call print_array('rt2', rt2_r, 2)
  call print_array('evscal', evscal_r, 2)
  call print_array('cs1', cs1_r, 2)
  call print_array('sn1', sn1_r, 2)
  call end_test()

  ! Test 14: csqrt with zero re, zero im (z=0 branch)
  ! When A=C and B is small enough that T_init = 0 and Z = |B|
  ! Actually A = 5+0i, B = 0.001+0i, C = 5+0i => T_init = 0, Z = 0.001
  ! csqrt arg = (0)^2 + (1)^2 = 1 -- not zero
  ! Better: A = C, B very small complex => T = B
  ! For csqrt(0,0): need (T/Z)^2 + (B/Z)^2 = 0
  ! With real T,B: impossible since both squares are real and non-negative
  ! Skip: this branch can only be hit if T and B are both exactly zero,
  ! which is already handled by B=0 diagonal case.

  ! Test 15: evnorm < THRESH branch
  ! SN1 needs to be approximately ±i so 1+SN1^2 ≈ 0
  ! A = (0, 1), B = (1, 0), C = (0, -1)
  ! S = 0, T_init = (0,1), BABS = 1, TABS = 1, Z = 1
  ! sqrt((0+i)^2 + 1^2) = sqrt(-1 + 1) = sqrt(0) = 0
  ! T = 0, RT1 = 0, RT2 = 0, SN1 = (0 - (0+i)) / 1 = -i
  ! 1 + SN1^2 = 1 + (-i)^2 = 1 + (-1) = 0
  ! T = sqrt(0) = 0, evnorm = 0 < THRESH => evscal = 0
  A = (0.0D0, 1.0D0)
  B = (1.0D0, 0.0D0)
  C = (0.0D0, -1.0D0)
  call ZLAESY(A, B, C, RT1, RT2, EVSCAL, CS1, SN1)
  call begin_test('evscal_zero')
  call print_array('rt1', rt1_r, 2)
  call print_array('rt2', rt2_r, 2)
  call print_array('evscal', evscal_r, 2)
  call print_array('cs1', cs1_r, 2)
  call print_array('sn1', sn1_r, 2)
  call end_test()

end program
