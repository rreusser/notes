program test_dgemlqt
  use test_utils
  implicit none

  ! Compact WY LQ factors produced by DGELQT, then applied by DGEMQLT.
  ! In LQ: dgelqt(K, Q, mb, A, ...) produces V (rows of A) and T such
  ! that the product Q_orth = H(K) * H(K-1) * ... * H(1) is order Q.
  ! For dgemlqt with SIDE='L', Q_orth must be order M, so we factor
  ! a K-by-M matrix; for SIDE='R', Q_orth is order N, so we factor
  ! a K-by-N matrix.

  ! Small case: K=3, Q=4, MB=2 (block-pass exercises both single-block
  ! and partial-block iterations).
  double precision :: A(3, 4), T(2, 3)
  double precision :: C44(4, 4), CR43(4, 3)
  double precision :: WORK(4*32)

  ! Larger case: K=8, Q=10, MB=3 (multi-block, K not divisible by MB).
  double precision :: AL(8, 10), TL(3, 8)
  double precision :: CLL(10, 10)

  ! Single-block case: MB == K (one inner-loop iteration).
  double precision :: AS(4, 5), TS(4, 4)
  double precision :: CSS(5, 5)

  integer :: info, i, j

  ! ===== Build a 3x4 compact-WY LQ factorization =====
  A(1,1) = 1.0d0;  A(1,2) = 5.0d0;  A(1,3) = 9.0d0;  A(1,4) = 13.0d0
  A(2,1) = 2.0d0;  A(2,2) = 6.0d0;  A(2,3) = 10.0d0; A(2,4) = 14.0d0
  A(3,1) = 3.0d0;  A(3,2) = 7.0d0;  A(3,3) = 11.0d0; A(3,4) = 15.0d0
  T = 0.0d0
  call dgelqt(3, 4, 2, A, 3, T, 2, WORK, info)

  call begin_test('lq_factors_small')
  call print_int('info', info)
  call print_array('v', A, 12)
  call print_array('t', T, 6)
  call end_test()

  ! Test 1: SIDE='L', TRANS='N' -> C := Q * I_4 (Q is order 4, K=3)
  C44 = 0.0d0
  do i = 1, 4
    C44(i, i) = 1.0d0
  end do
  call dgemlqt('L', 'N', 4, 4, 3, 2, A, 3, T, 2, C44, 4, WORK, info)
  call begin_test('left_notrans')
  call print_int('info', info)
  call print_array('c', C44, 16)
  call end_test()

  ! Test 2: SIDE='L', TRANS='T' -> C := Q^T * I_4
  C44 = 0.0d0
  do i = 1, 4
    C44(i, i) = 1.0d0
  end do
  call dgemlqt('L', 'T', 4, 4, 3, 2, A, 3, T, 2, C44, 4, WORK, info)
  call begin_test('left_trans')
  call print_int('info', info)
  call print_array('c', C44, 16)
  call end_test()

  ! Test 3: SIDE='R', TRANS='N' with rectangular C(4,3) -> C := C * Q
  ! For SIDE='R', Q must be order N=3. Build a separate factorization.
  ! Use a 3x3 sub-matrix: factor K=3 reflectors over Q=3 columns.
  ! Reuse AS scratch with K=3, Q=3. For brevity reuse small with N=3.
  ! Instead, we'll apply the existing K=3, Q=4 Q to a C(4,3) where C
  ! is operated on the right? No — for SIDE='R', Q is order N (the
  ! number of columns of C). So we need a different factorization.
  !
  ! We'll set up a fresh K=3, Q=3 factorization:
  block
    double precision :: AR(3, 3), TR(2, 3), VR(3, 3)
    integer :: ii
    AR(1,1) = 2.0d0; AR(1,2) = -1.0d0; AR(1,3) = 0.5d0
    AR(2,1) = 1.0d0; AR(2,2) = 3.0d0;  AR(2,3) = -2.0d0
    AR(3,1) = -0.5d0; AR(3,2) = 1.5d0; AR(3,3) = 4.0d0
    TR = 0.0d0
    call dgelqt(3, 3, 2, AR, 3, TR, 2, WORK, info)
    do ii = 1, 9
      VR(mod(ii-1,3)+1, (ii-1)/3+1) = AR(mod(ii-1,3)+1, (ii-1)/3+1)
    end do

    call begin_test('lq_factors_rsmall')
    call print_int('info', info)
    call print_array('v', AR, 9)
    call print_array('t', TR, 6)
    call end_test()

    ! Test 3: SIDE='R', TRANS='N' on 4x3 dense
    CR43(1,1) = 1.0d0;  CR43(2,1) = 0.0d0;  CR43(3,1) = 2.0d0;  CR43(4,1) = -1.0d0
    CR43(1,2) = 2.0d0;  CR43(2,2) = 1.0d0;  CR43(3,2) = -1.0d0; CR43(4,2) = 3.0d0
    CR43(1,3) = -1.0d0; CR43(2,3) = 3.0d0;  CR43(3,3) = 0.0d0;  CR43(4,3) = -2.0d0
    call dgemlqt('R', 'N', 4, 3, 3, 2, AR, 3, TR, 2, CR43, 4, WORK, info)
    call begin_test('right_notrans_rect')
    call print_int('info', info)
    call print_array('c', CR43, 12)
    call end_test()

    ! Test 4: SIDE='R', TRANS='T' on 4x3 dense
    CR43(1,1) = 1.0d0;  CR43(2,1) = 0.0d0;  CR43(3,1) = 2.0d0;  CR43(4,1) = -1.0d0
    CR43(1,2) = 2.0d0;  CR43(2,2) = 1.0d0;  CR43(3,2) = -1.0d0; CR43(4,2) = 3.0d0
    CR43(1,3) = -1.0d0; CR43(2,3) = 3.0d0;  CR43(3,3) = 0.0d0;  CR43(4,3) = -2.0d0
    call dgemlqt('R', 'T', 4, 3, 3, 2, AR, 3, TR, 2, CR43, 4, WORK, info)
    call begin_test('right_trans_rect')
    call print_int('info', info)
    call print_array('c', CR43, 12)
    call end_test()
  end block

  ! Test 5: Left, no-trans on 4x4 non-identity (exercises blocked)
  C44(1,1) = 1.0d0;  C44(2,1) = 2.0d0;  C44(3,1) = -1.0d0; C44(4,1) = 3.0d0
  C44(1,2) = -2.0d0; C44(2,2) = 1.0d0;  C44(3,2) = 4.0d0;  C44(4,2) = 0.0d0
  C44(1,3) = 3.0d0;  C44(2,3) = -1.0d0; C44(3,3) = 2.0d0;  C44(4,3) = 1.0d0
  C44(1,4) = 0.0d0;  C44(2,4) = 5.0d0;  C44(3,4) = -2.0d0; C44(4,4) = 4.0d0
  call dgemlqt('L', 'N', 4, 4, 3, 2, A, 3, T, 2, C44, 4, WORK, info)
  call begin_test('left_notrans_dense')
  call print_int('info', info)
  call print_array('c', C44, 16)
  call end_test()

  ! Test 6: Left, trans on the same 4x4 dense matrix (round-trip companion)
  C44(1,1) = 1.0d0;  C44(2,1) = 2.0d0;  C44(3,1) = -1.0d0; C44(4,1) = 3.0d0
  C44(1,2) = -2.0d0; C44(2,2) = 1.0d0;  C44(3,2) = 4.0d0;  C44(4,2) = 0.0d0
  C44(1,3) = 3.0d0;  C44(2,3) = -1.0d0; C44(3,3) = 2.0d0;  C44(4,3) = 1.0d0
  C44(1,4) = 0.0d0;  C44(2,4) = 5.0d0;  C44(3,4) = -2.0d0; C44(4,4) = 4.0d0
  call dgemlqt('L', 'T', 4, 4, 3, 2, A, 3, T, 2, C44, 4, WORK, info)
  call begin_test('left_trans_dense')
  call print_int('info', info)
  call print_array('c', C44, 16)
  call end_test()

  ! Quick-return tests: M=0, N=0, K=0
  call dgemlqt('L', 'N', 0, 4, 0, 2, A, 3, T, 2, C44, 4, WORK, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call end_test()

  call dgemlqt('L', 'N', 4, 0, 0, 2, A, 3, T, 2, C44, 4, WORK, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  call dgemlqt('L', 'N', 4, 4, 0, 2, A, 3, T, 2, C44, 4, WORK, info)
  call begin_test('k_zero')
  call print_int('info', info)
  call end_test()

  ! ===== Multi-block case: K=8, Q=10, MB=3 (K%MB != 0) =====
  do j = 1, 10
    do i = 1, 8
      AL(i, j) = sin(dble(i + 3*j)) + 0.1d0 * dble(i)
    end do
  end do
  TL = 0.0d0
  call dgelqt(8, 10, 3, AL, 8, TL, 3, WORK, info)

  call begin_test('lq_factors_block')
  call print_int('info', info)
  call print_array('v', AL, 80)
  call print_array('t', TL, 24)
  call end_test()

  ! Left, no-trans on a 10x10 dense matrix
  do j = 1, 10
    do i = 1, 10
      CLL(i, j) = cos(dble(i) + dble(j)) + 0.5d0
    end do
  end do
  call dgemlqt('L', 'N', 10, 10, 8, 3, AL, 8, TL, 3, CLL, 10, WORK, info)
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
  call dgemlqt('L', 'T', 10, 10, 8, 3, AL, 8, TL, 3, CLL, 10, WORK, info)
  call begin_test('left_trans_block')
  call print_int('info', info)
  call print_array('c', CLL, 100)
  call end_test()

  ! Right, no-trans
  do j = 1, 10
    do i = 1, 10
      CLL(i, j) = cos(dble(i) + dble(j)) + 0.5d0
    end do
  end do
  call dgemlqt('R', 'N', 10, 10, 8, 3, AL, 8, TL, 3, CLL, 10, WORK, info)
  call begin_test('right_notrans_block')
  call print_int('info', info)
  call print_array('c', CLL, 100)
  call end_test()

  ! Right, trans
  do j = 1, 10
    do i = 1, 10
      CLL(i, j) = cos(dble(i) + dble(j)) + 0.5d0
    end do
  end do
  call dgemlqt('R', 'T', 10, 10, 8, 3, AL, 8, TL, 3, CLL, 10, WORK, info)
  call begin_test('right_trans_block')
  call print_int('info', info)
  call print_array('c', CLL, 100)
  call end_test()

  ! ===== Single-block case: MB == K (one inner iteration) =====
  do j = 1, 5
    do i = 1, 4
      AS(i, j) = sin(dble(i*2 + j*3)) + 0.05d0 * dble(i)
    end do
  end do
  TS = 0.0d0
  call dgelqt(4, 5, 4, AS, 4, TS, 4, WORK, info)

  call begin_test('lq_factors_single')
  call print_int('info', info)
  call print_array('v', AS, 20)
  call print_array('t', TS, 16)
  call end_test()

  ! Left, no-trans on a 5x5 dense matrix with MB == K
  do j = 1, 5
    do i = 1, 5
      CSS(i, j) = sin(dble(i + 2*j))
    end do
  end do
  call dgemlqt('L', 'N', 5, 5, 4, 4, AS, 4, TS, 4, CSS, 5, WORK, info)
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
  call dgemlqt('L', 'T', 5, 5, 4, 4, AS, 4, TS, 4, CSS, 5, WORK, info)
  call begin_test('left_trans_single')
  call print_int('info', info)
  call print_array('c', CSS, 25)
  call end_test()

end program
