program test_dorml2
  use test_utils
  implicit none

  double precision :: A(6, 6), C(6, 6), TAU(6), WORK(200)
  integer :: info, i, j

  ! Test 1: Left, No transpose: Q * I where Q from 3x5 LQ
  A = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0; A(1,3) = 0.5d0; A(1,4) = 1.0d0; A(1,5) = 3.0d0
  A(2,1) = 0.5d0; A(2,2) = 1.0d0; A(2,3) = 3.0d0; A(2,4) = 2.0d0; A(2,5) = 1.0d0
  A(3,1) = 3.0d0; A(3,2) = 0.5d0; A(3,3) = 1.0d0; A(3,4) = 2.0d0; A(3,5) = 0.5d0
  TAU = 0.0d0
  call dgelq2(3, 5, A, 6, TAU, WORK, info)

  ! C = 5x5 identity
  C = 0.0d0
  do i = 1, 5
    C(i,i) = 1.0d0
  end do
  call dorml2('L', 'N', 5, 5, 3, A, 6, TAU, C, 6, WORK, info)
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
  call dgelq2(3, 5, A, 6, TAU, WORK, info)

  C = 0.0d0
  do i = 1, 5
    C(i,i) = 1.0d0
  end do
  call dorml2('L', 'T', 5, 5, 3, A, 6, TAU, C, 6, WORK, info)
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
  call dgelq2(3, 5, A, 6, TAU, WORK, info)

  C = 0.0d0
  do i = 1, 5
    C(i,i) = 1.0d0
  end do
  call dorml2('R', 'N', 5, 5, 3, A, 6, TAU, C, 6, WORK, info)
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
  call dgelq2(3, 5, A, 6, TAU, WORK, info)

  C = 0.0d0
  do i = 1, 5
    C(i,i) = 1.0d0
  end do
  call dorml2('R', 'T', 5, 5, 3, A, 6, TAU, C, 6, WORK, info)
  call begin_test('right_trans_5x5')
  call print_int('info', info)
  call print_array('c', C, 25)
  call end_test()

  ! Test 5: M=0 quick return
  call dorml2('L', 'N', 0, 5, 0, A, 6, TAU, C, 6, WORK, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: N=0 quick return
  call dorml2('L', 'N', 5, 0, 0, A, 6, TAU, C, 6, WORK, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 7: K=0 quick return
  call dorml2('L', 'N', 5, 5, 0, A, 6, TAU, C, 6, WORK, info)
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
  call dgelq2(3, 5, A, 6, TAU, WORK, info)

  C = 0.0d0
  C(1,1) = 1.0d0; C(2,1) = 3.0d0; C(3,1) = -1.0d0
  C(1,2) = 0.5d0; C(2,2) = 1.0d0; C(3,2) = 4.0d0
  C(1,3) = 2.0d0; C(2,3) = 0.5d0; C(3,3) = 1.0d0
  C(1,4) = 1.0d0; C(2,4) = 2.0d0; C(3,4) = 0.5d0
  C(1,5) = 0.5d0; C(2,5) = 1.0d0; C(3,5) = 2.0d0
  call dorml2('R', 'T', 3, 5, 3, A, 6, TAU, C, 6, WORK, info)
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
  call dgelq2(3, 5, A, 6, TAU, WORK, info)

  C = 0.0d0
  C(1,1) = 1.0d0; C(2,1) = 3.0d0; C(3,1) = -1.0d0; C(4,1) = 2.0d0; C(5,1) = 0.5d0
  C(1,2) = 0.5d0; C(2,2) = 1.0d0; C(3,2) = 4.0d0; C(4,2) = -0.5d0; C(5,2) = 1.0d0
  C(1,3) = 2.0d0; C(2,3) = 0.5d0; C(3,3) = 1.0d0; C(4,3) = 3.0d0; C(5,3) = -1.0d0
  call dorml2('L', 'N', 5, 3, 3, A, 6, TAU, C, 6, WORK, info)
  call begin_test('left_notrans_rect')
  call print_int('info', info)
  call print_array('c', C, 18)
  call end_test()

end program
