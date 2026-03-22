program test_dormlq
  use test_utils
  implicit none

  double precision :: A(6, 6), C(6, 6), TAU(6), WORK(200)
  integer :: info, i, j, LWORK

  ! Large arrays for blocked path test (K=35)
  integer, parameter :: NBIG = 50, KBIG = 35
  double precision :: ABIG(NBIG, NBIG), CBIG(NBIG, NBIG)
  double precision :: TAUBIG(NBIG), WORKBIG(10000)
  integer :: LWORKBIG

  LWORK = 200
  LWORKBIG = 10000

  ! Test 1: Left, No transpose: Q * I where Q from 3x5 LQ
  A = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0; A(1,3) = 0.5d0; A(1,4) = 1.0d0; A(1,5) = 3.0d0
  A(2,1) = 0.5d0; A(2,2) = 1.0d0; A(2,3) = 3.0d0; A(2,4) = 2.0d0; A(2,5) = 1.0d0
  A(3,1) = 3.0d0; A(3,2) = 0.5d0; A(3,3) = 1.0d0; A(3,4) = 2.0d0; A(3,5) = 0.5d0
  TAU = 0.0d0
  call dgelqf(3, 5, A, 6, TAU, WORK, LWORK, info)

  ! C = 5x5 identity
  C = 0.0d0
  do i = 1, 5
    C(i,i) = 1.0d0
  end do
  call dormlq('L', 'N', 5, 5, 3, A, 6, TAU, C, 6, WORK, LWORK, info)
  call begin_test('left_notrans_5x5')
  call print_int('info', info)
  call print_array('c', C, 25)
  call end_test()

  ! Test 2: Left, Transpose: Q^T * I
  A = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0; A(1,3) = 0.5d0; A(1,4) = 1.0d0; A(1,5) = 3.0d0
  A(2,1) = 0.5d0; A(2,2) = 1.0d0; A(2,3) = 3.0d0; A(2,4) = 2.0d0; A(2,5) = 1.0d0
  A(3,1) = 3.0d0; A(3,2) = 0.5d0; A(3,3) = 1.0d0; A(3,4) = 2.0d0; A(3,5) = 0.5d0
  TAU = 0.0d0
  call dgelqf(3, 5, A, 6, TAU, WORK, LWORK, info)

  C = 0.0d0
  do i = 1, 5
    C(i,i) = 1.0d0
  end do
  call dormlq('L', 'T', 5, 5, 3, A, 6, TAU, C, 6, WORK, LWORK, info)
  call begin_test('left_trans_5x5')
  call print_int('info', info)
  call print_array('c', C, 25)
  call end_test()

  ! Test 3: Right, No transpose: I * Q
  A = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0; A(1,3) = 0.5d0; A(1,4) = 1.0d0; A(1,5) = 3.0d0
  A(2,1) = 0.5d0; A(2,2) = 1.0d0; A(2,3) = 3.0d0; A(2,4) = 2.0d0; A(2,5) = 1.0d0
  A(3,1) = 3.0d0; A(3,2) = 0.5d0; A(3,3) = 1.0d0; A(3,4) = 2.0d0; A(3,5) = 0.5d0
  TAU = 0.0d0
  call dgelqf(3, 5, A, 6, TAU, WORK, LWORK, info)

  C = 0.0d0
  do i = 1, 5
    C(i,i) = 1.0d0
  end do
  call dormlq('R', 'N', 5, 5, 3, A, 6, TAU, C, 6, WORK, LWORK, info)
  call begin_test('right_notrans_5x5')
  call print_int('info', info)
  call print_array('c', C, 25)
  call end_test()

  ! Test 4: Right, Transpose: I * Q^T
  A = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0; A(1,3) = 0.5d0; A(1,4) = 1.0d0; A(1,5) = 3.0d0
  A(2,1) = 0.5d0; A(2,2) = 1.0d0; A(2,3) = 3.0d0; A(2,4) = 2.0d0; A(2,5) = 1.0d0
  A(3,1) = 3.0d0; A(3,2) = 0.5d0; A(3,3) = 1.0d0; A(3,4) = 2.0d0; A(3,5) = 0.5d0
  TAU = 0.0d0
  call dgelqf(3, 5, A, 6, TAU, WORK, LWORK, info)

  C = 0.0d0
  do i = 1, 5
    C(i,i) = 1.0d0
  end do
  call dormlq('R', 'T', 5, 5, 3, A, 6, TAU, C, 6, WORK, LWORK, info)
  call begin_test('right_trans_5x5')
  call print_int('info', info)
  call print_array('c', C, 25)
  call end_test()

  ! Test 5: M=0 quick return
  call dormlq('L', 'N', 0, 5, 0, A, 6, TAU, C, 6, WORK, LWORK, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: N=0 quick return
  call dormlq('L', 'N', 5, 0, 0, A, 6, TAU, C, 6, WORK, LWORK, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 7: K=0 quick return
  call dormlq('L', 'N', 5, 5, 0, A, 6, TAU, C, 6, WORK, LWORK, info)
  call begin_test('k_zero')
  call print_int('info', info)
  call end_test()

  ! Test 8: Apply Q to non-identity matrix (right, trans)
  ! Rectangular C: 3x5, Q from 3x5 LQ, right multiply => C * Q^T => 3x5
  A = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0; A(1,3) = 0.5d0; A(1,4) = 1.0d0; A(1,5) = 3.0d0
  A(2,1) = 0.5d0; A(2,2) = 1.0d0; A(2,3) = 3.0d0; A(2,4) = 2.0d0; A(2,5) = 1.0d0
  A(3,1) = 3.0d0; A(3,2) = 0.5d0; A(3,3) = 1.0d0; A(3,4) = 2.0d0; A(3,5) = 0.5d0
  TAU = 0.0d0
  call dgelqf(3, 5, A, 6, TAU, WORK, LWORK, info)

  C = 0.0d0
  C(1,1) = 1.0d0; C(2,1) = 3.0d0; C(3,1) = -1.0d0
  C(1,2) = 0.5d0; C(2,2) = 1.0d0; C(3,2) = 4.0d0
  C(1,3) = 2.0d0; C(2,3) = 0.5d0; C(3,3) = 1.0d0
  C(1,4) = 1.0d0; C(2,4) = 2.0d0; C(3,4) = 0.5d0
  C(1,5) = 0.5d0; C(2,5) = 1.0d0; C(3,5) = 2.0d0
  call dormlq('R', 'T', 3, 5, 3, A, 6, TAU, C, 6, WORK, LWORK, info)
  call begin_test('right_trans_rect')
  call print_int('info', info)
  call print_array('c', C, 18)
  call end_test()

  ! Test 9: Apply Q to non-identity matrix (left, notrans) with rectangular C
  ! C is 5x3, Q from 3x5 LQ, left multiply Q*C => Q is 5x5, C is 5x3 => 5x3
  A = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0; A(1,3) = 0.5d0; A(1,4) = 1.0d0; A(1,5) = 3.0d0
  A(2,1) = 0.5d0; A(2,2) = 1.0d0; A(2,3) = 3.0d0; A(2,4) = 2.0d0; A(2,5) = 1.0d0
  A(3,1) = 3.0d0; A(3,2) = 0.5d0; A(3,3) = 1.0d0; A(3,4) = 2.0d0; A(3,5) = 0.5d0
  TAU = 0.0d0
  call dgelqf(3, 5, A, 6, TAU, WORK, LWORK, info)

  C = 0.0d0
  C(1,1) = 1.0d0; C(2,1) = 3.0d0; C(3,1) = -1.0d0; C(4,1) = 2.0d0; C(5,1) = 0.5d0
  C(1,2) = 0.5d0; C(2,2) = 1.0d0; C(3,2) = 4.0d0; C(4,2) = -0.5d0; C(5,2) = 1.0d0
  C(1,3) = 2.0d0; C(2,3) = 0.5d0; C(3,3) = 1.0d0; C(4,3) = 3.0d0; C(5,3) = -1.0d0
  call dormlq('L', 'N', 5, 3, 3, A, 6, TAU, C, 6, WORK, LWORK, info)
  call begin_test('left_notrans_rect')
  call print_int('info', info)
  call print_array('c', C, 18)
  call end_test()

  ! Test 10: Large matrix test to exercise blocked path (K=35 > NB=32)
  ! Use a deterministic matrix: A(i,j) = sin(i * 0.7 + j * 1.3) for variety
  ABIG = 0.0d0
  do i = 1, KBIG
    do j = 1, NBIG
      ABIG(i,j) = sin(dble(i) * 0.7d0 + dble(j) * 1.3d0)
    end do
  end do
  TAUBIG = 0.0d0
  call dgelqf(KBIG, NBIG, ABIG, NBIG, TAUBIG, WORKBIG, LWORKBIG, info)

  ! Output the factored A and TAU so JS test can use them directly
  call begin_test('lq_factored')
  call print_array('a', ABIG, NBIG*NBIG)
  call print_array('tau', TAUBIG, KBIG)
  call end_test()

  ! C = NBIG x NBIG identity (left multiply: Q * C where Q is NBIG x NBIG, C is NBIG x NBIG)
  CBIG = 0.0d0
  do i = 1, NBIG
    CBIG(i,i) = 1.0d0
  end do
  call dormlq('L', 'N', NBIG, NBIG, KBIG, ABIG, NBIG, TAUBIG, CBIG, NBIG, WORKBIG, LWORKBIG, info)
  call begin_test('left_notrans_blocked')
  call print_int('info', info)
  call print_array('c', CBIG, NBIG*NBIG)
  call end_test()

  ! Test 11: Large matrix test blocked path, left transpose
  ! (reuse same factored ABIG/TAUBIG from above)
  CBIG = 0.0d0
  do i = 1, NBIG
    CBIG(i,i) = 1.0d0
  end do
  call dormlq('L', 'T', NBIG, NBIG, KBIG, ABIG, NBIG, TAUBIG, CBIG, NBIG, WORKBIG, LWORKBIG, info)
  call begin_test('left_trans_blocked')
  call print_int('info', info)
  call print_array('c', CBIG, NBIG*NBIG)
  call end_test()

  ! Test 12: Large matrix test blocked path, right notrans
  CBIG = 0.0d0
  do i = 1, NBIG
    CBIG(i,i) = 1.0d0
  end do
  call dormlq('R', 'N', NBIG, NBIG, KBIG, ABIG, NBIG, TAUBIG, CBIG, NBIG, WORKBIG, LWORKBIG, info)
  call begin_test('right_notrans_blocked')
  call print_int('info', info)
  call print_array('c', CBIG, NBIG*NBIG)
  call end_test()

  ! Test 13: Large matrix test blocked path, right transpose
  CBIG = 0.0d0
  do i = 1, NBIG
    CBIG(i,i) = 1.0d0
  end do
  call dormlq('R', 'T', NBIG, NBIG, KBIG, ABIG, NBIG, TAUBIG, CBIG, NBIG, WORKBIG, LWORKBIG, info)
  call begin_test('right_trans_blocked')
  call print_int('info', info)
  call print_array('c', CBIG, NBIG*NBIG)
  call end_test()

end program
