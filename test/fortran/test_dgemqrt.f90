program test_dgemqrt
  use test_utils
  implicit none

  ! Compact WY QR factors produced by DGEQRT, then applied by DGEMQRT.
  ! Use container sizes that match the trimmed dimensions to keep
  ! EQUIVALENCE-style printing trivial.

  ! Small case: M=4, N=3, NB=2 (block-pass exercises both single-block
  ! and partial-block iterations).
  double precision :: A(4, 3), T(2, 3)
  double precision :: C44(4, 4), CR34(3, 4), CR44(4, 4)
  double precision :: WORK(4*32)

  ! Larger case: M=10, N=10, K=8, NB=3 (multi-block, K not divisible by NB).
  double precision :: AL(10, 8), TL(3, 8)
  double precision :: CLL(10, 10), CLR(10, 10)

  ! Single-block case: NB == K (no inner-loop iterations beyond one).
  double precision :: AS(5, 4), TS(4, 4)
  double precision :: CSS(5, 5)

  integer :: info, i, j

  ! ===== Build a 4x3 compact-WY QR factorization =====
  A(1,1) = 1.0d0;  A(2,1) = 2.0d0;  A(3,1) = 3.0d0;  A(4,1) = 4.0d0
  A(1,2) = 5.0d0;  A(2,2) = 6.0d0;  A(3,2) = 7.0d0;  A(4,2) = 8.0d0
  A(1,3) = 9.0d0;  A(2,3) = 10.0d0; A(3,3) = 11.0d0; A(4,3) = 12.0d0
  T = 0.0d0
  call dgeqrt(4, 3, 2, A, 4, T, 2, WORK, info)

  call begin_test('qr_factors_small')
  call print_int('info', info)
  call print_array('v', A, 12)
  call print_array('t', T, 6)
  call end_test()

  ! Test 1: SIDE='L', TRANS='N' -> C := Q * I_4
  C44 = 0.0d0
  do i = 1, 4
    C44(i, i) = 1.0d0
  end do
  call dgemqrt('L', 'N', 4, 4, 3, 2, A, 4, T, 2, C44, 4, WORK, info)
  call begin_test('left_notrans')
  call print_int('info', info)
  call print_array('c', C44, 16)
  call end_test()

  ! Test 2: SIDE='L', TRANS='T' -> C := Q^T * I_4
  C44 = 0.0d0
  do i = 1, 4
    C44(i, i) = 1.0d0
  end do
  call dgemqrt('L', 'T', 4, 4, 3, 2, A, 4, T, 2, C44, 4, WORK, info)
  call begin_test('left_trans')
  call print_int('info', info)
  call print_array('c', C44, 16)
  call end_test()

  ! Test 3: SIDE='R', TRANS='N' with rectangular C(3,4) -> C := C * Q
  ! Q is order 4 here? No: Right-side requires Q of order N. So we need
  ! a QR of an Nxr matrix where N matches the column count. Use a
  ! separate factorization: 4x3 means Q is 4x4. To apply on the right
  ! to a 3x4 C, the Q (order 4) acts on columns. Use the existing A.
  CR34(1,1) = 1.0d0;  CR34(2,1) = 0.0d0;  CR34(3,1) = 2.0d0
  CR34(1,2) = 2.0d0;  CR34(2,2) = 1.0d0;  CR34(3,2) = -1.0d0
  CR34(1,3) = -1.0d0; CR34(2,3) = 3.0d0;  CR34(3,3) = 0.0d0
  CR34(1,4) = 4.0d0;  CR34(2,4) = -2.0d0; CR34(3,4) = 1.0d0
  call dgemqrt('R', 'N', 3, 4, 3, 2, A, 4, T, 2, CR34, 3, WORK, info)
  call begin_test('right_notrans_rect')
  call print_int('info', info)
  call print_array('c', CR34, 12)
  call end_test()

  ! Test 4: SIDE='R', TRANS='T' on rectangular C(3,4) -> C := C * Q^T
  CR34(1,1) = 1.0d0;  CR34(2,1) = 0.0d0;  CR34(3,1) = 2.0d0
  CR34(1,2) = 2.0d0;  CR34(2,2) = 1.0d0;  CR34(3,2) = -1.0d0
  CR34(1,3) = -1.0d0; CR34(2,3) = 3.0d0;  CR34(3,3) = 0.0d0
  CR34(1,4) = 4.0d0;  CR34(2,4) = -2.0d0; CR34(3,4) = 1.0d0
  call dgemqrt('R', 'T', 3, 4, 3, 2, A, 4, T, 2, CR34, 3, WORK, info)
  call begin_test('right_trans_rect')
  call print_int('info', info)
  call print_array('c', CR34, 12)
  call end_test()

  ! Test 5: Left, no-trans on 4x4 non-identity (exercises blocked)
  CR44(1,1) = 1.0d0;  CR44(2,1) = 2.0d0;  CR44(3,1) = -1.0d0; CR44(4,1) = 3.0d0
  CR44(1,2) = -2.0d0; CR44(2,2) = 1.0d0;  CR44(3,2) = 4.0d0;  CR44(4,2) = 0.0d0
  CR44(1,3) = 3.0d0;  CR44(2,3) = -1.0d0; CR44(3,3) = 2.0d0;  CR44(4,3) = 1.0d0
  CR44(1,4) = 0.0d0;  CR44(2,4) = 5.0d0;  CR44(3,4) = -2.0d0; CR44(4,4) = 4.0d0
  call dgemqrt('L', 'N', 4, 4, 3, 2, A, 4, T, 2, CR44, 4, WORK, info)
  call begin_test('left_notrans_dense')
  call print_int('info', info)
  call print_array('c', CR44, 16)
  call end_test()

  ! Test 6: Left, trans on the same 4x4 dense matrix (round-trip companion)
  CR44(1,1) = 1.0d0;  CR44(2,1) = 2.0d0;  CR44(3,1) = -1.0d0; CR44(4,1) = 3.0d0
  CR44(1,2) = -2.0d0; CR44(2,2) = 1.0d0;  CR44(3,2) = 4.0d0;  CR44(4,2) = 0.0d0
  CR44(1,3) = 3.0d0;  CR44(2,3) = -1.0d0; CR44(3,3) = 2.0d0;  CR44(4,3) = 1.0d0
  CR44(1,4) = 0.0d0;  CR44(2,4) = 5.0d0;  CR44(3,4) = -2.0d0; CR44(4,4) = 4.0d0
  call dgemqrt('L', 'T', 4, 4, 3, 2, A, 4, T, 2, CR44, 4, WORK, info)
  call begin_test('left_trans_dense')
  call print_int('info', info)
  call print_array('c', CR44, 16)
  call end_test()

  ! Quick-return tests: M=0, N=0, K=0
  call dgemqrt('L', 'N', 0, 4, 0, 2, A, 4, T, 2, C44, 4, WORK, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call end_test()

  call dgemqrt('L', 'N', 4, 0, 0, 2, A, 4, T, 2, C44, 4, WORK, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  call dgemqrt('L', 'N', 4, 4, 0, 2, A, 4, T, 2, C44, 4, WORK, info)
  call begin_test('k_zero')
  call print_int('info', info)
  call end_test()

  ! ===== Multi-block case: M=10, N=10, K=8, NB=3 (K%NB != 0) =====
  do j = 1, 8
    do i = 1, 10
      AL(i, j) = sin(dble(i + 3*j)) + 0.1d0 * dble(i)
    end do
  end do
  TL = 0.0d0
  call dgeqrt(10, 8, 3, AL, 10, TL, 3, WORK, info)

  call begin_test('qr_factors_block')
  call print_int('info', info)
  call print_array('v', AL, 80)
  call print_array('t', TL, 24)
  call end_test()

  ! Left, no-trans: applies Q to a non-identity 10x10 dense matrix
  do j = 1, 10
    do i = 1, 10
      CLL(i, j) = cos(dble(i) + dble(j)) + 0.5d0
    end do
  end do
  call dgemqrt('L', 'N', 10, 10, 8, 3, AL, 10, TL, 3, CLL, 10, WORK, info)
  call begin_test('left_notrans_block')
  call print_int('info', info)
  call print_array('c', CLL, 100)
  call end_test()

  ! Left, trans
  do j = 1, 10
    do i = 1, 10
      CLL(i, j) = cos(dble(i) + dble(j)) + 0.5d0
    end do
  end do
  call dgemqrt('L', 'T', 10, 10, 8, 3, AL, 10, TL, 3, CLL, 10, WORK, info)
  call begin_test('left_trans_block')
  call print_int('info', info)
  call print_array('c', CLL, 100)
  call end_test()

  ! Right, no-trans
  do j = 1, 10
    do i = 1, 10
      CLR(i, j) = cos(dble(i) + dble(j)) + 0.5d0
    end do
  end do
  call dgemqrt('R', 'N', 10, 10, 8, 3, AL, 10, TL, 3, CLR, 10, WORK, info)
  call begin_test('right_notrans_block')
  call print_int('info', info)
  call print_array('c', CLR, 100)
  call end_test()

  ! Right, trans
  do j = 1, 10
    do i = 1, 10
      CLR(i, j) = cos(dble(i) + dble(j)) + 0.5d0
    end do
  end do
  call dgemqrt('R', 'T', 10, 10, 8, 3, AL, 10, TL, 3, CLR, 10, WORK, info)
  call begin_test('right_trans_block')
  call print_int('info', info)
  call print_array('c', CLR, 100)
  call end_test()

  ! ===== Single-block case: NB == K (one inner iteration) =====
  do j = 1, 4
    do i = 1, 5
      AS(i, j) = sin(dble(i*2 + j*3)) + 0.05d0 * dble(i)
    end do
  end do
  TS = 0.0d0
  call dgeqrt(5, 4, 4, AS, 5, TS, 4, WORK, info)

  call begin_test('qr_factors_single')
  call print_int('info', info)
  call print_array('v', AS, 20)
  call print_array('t', TS, 16)
  call end_test()

  ! Left, no-trans on a 5x5 dense matrix with NB == K
  do j = 1, 5
    do i = 1, 5
      CSS(i, j) = sin(dble(i + 2*j))
    end do
  end do
  call dgemqrt('L', 'N', 5, 5, 4, 4, AS, 5, TS, 4, CSS, 5, WORK, info)
  call begin_test('left_notrans_single')
  call print_int('info', info)
  call print_array('c', CSS, 25)
  call end_test()

  ! Left, trans on the same 5x5 matrix
  do j = 1, 5
    do i = 1, 5
      CSS(i, j) = sin(dble(i + 2*j))
    end do
  end do
  call dgemqrt('L', 'T', 5, 5, 4, 4, AS, 5, TS, 4, CSS, 5, WORK, info)
  call begin_test('left_trans_single')
  call print_int('info', info)
  call print_array('c', CSS, 25)
  call end_test()

end program
