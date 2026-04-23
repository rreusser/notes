program test_zunmqr
  use test_utils
  implicit none

  complex*16 :: A(6, 6), C(6, 6), TAU(6), WORK(200)
  double precision :: A_r(72), C_r(72), TAU_r(12)
  equivalence (A, A_r)
  equivalence (C, C_r)
  equivalence (TAU, TAU_r)
  integer :: info, i, j, LWORK

  LWORK = 200

  ! Test 1: Left, No transpose: Q * I where Q from 4x3 QR
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.0d0); A(2,1) = (2.0d0, 1.0d0); A(3,1) = (0.0d0, 0.0d0); A(4,1) = (1.0d0, 1.0d0)
  A(1,2) = (0.0d0, 2.0d0); A(2,2) = (1.0d0, 0.0d0); A(3,2) = (3.0d0, 1.0d0); A(4,2) = (2.0d0, 0.0d0)
  A(1,3) = (3.0d0, 1.0d0); A(2,3) = (0.0d0, 0.0d0); A(3,3) = (1.0d0, 0.0d0); A(4,3) = (2.0d0, 1.0d0)
  TAU = (0.0d0, 0.0d0)
  call zgeqr2(4, 3, A, 6, TAU, WORK, info)

  ! C = 4x4 identity
  C = (0.0d0, 0.0d0)
  do i = 1, 4
    C(i,i) = (1.0d0, 0.0d0)
  end do
  call zunmqr('L', 'N', 4, 4, 3, A, 6, TAU, C, 6, WORK, LWORK, info)
  call begin_test('left_notrans_4x4')
  call print_int('info', info)
  call print_array('c', C_r, 32)
  call end_test()

  ! Test 2: Left, Conjugate transpose: Q^H * I
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.0d0); A(2,1) = (2.0d0, 1.0d0); A(3,1) = (0.0d0, 0.0d0); A(4,1) = (1.0d0, 1.0d0)
  A(1,2) = (0.0d0, 2.0d0); A(2,2) = (1.0d0, 0.0d0); A(3,2) = (3.0d0, 1.0d0); A(4,2) = (2.0d0, 0.0d0)
  A(1,3) = (3.0d0, 1.0d0); A(2,3) = (0.0d0, 0.0d0); A(3,3) = (1.0d0, 0.0d0); A(4,3) = (2.0d0, 1.0d0)
  TAU = (0.0d0, 0.0d0)
  call zgeqr2(4, 3, A, 6, TAU, WORK, info)

  C = (0.0d0, 0.0d0)
  do i = 1, 4
    C(i,i) = (1.0d0, 0.0d0)
  end do
  call zunmqr('L', 'C', 4, 4, 3, A, 6, TAU, C, 6, WORK, LWORK, info)
  call begin_test('left_conjtrans_4x4')
  call print_int('info', info)
  call print_array('c', C_r, 32)
  call end_test()

  ! Test 3: Right, No transpose: I * Q
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.0d0); A(2,1) = (2.0d0, 1.0d0); A(3,1) = (0.0d0, 0.0d0); A(4,1) = (1.0d0, 1.0d0)
  A(1,2) = (0.0d0, 2.0d0); A(2,2) = (1.0d0, 0.0d0); A(3,2) = (3.0d0, 1.0d0); A(4,2) = (2.0d0, 0.0d0)
  A(1,3) = (3.0d0, 1.0d0); A(2,3) = (0.0d0, 0.0d0); A(3,3) = (1.0d0, 0.0d0); A(4,3) = (2.0d0, 1.0d0)
  TAU = (0.0d0, 0.0d0)
  call zgeqr2(4, 3, A, 6, TAU, WORK, info)

  C = (0.0d0, 0.0d0)
  do i = 1, 4
    C(i,i) = (1.0d0, 0.0d0)
  end do
  call zunmqr('R', 'N', 4, 4, 3, A, 6, TAU, C, 6, WORK, LWORK, info)
  call begin_test('right_notrans_4x4')
  call print_int('info', info)
  call print_array('c', C_r, 32)
  call end_test()

  ! Test 4: M=0 quick return
  call zunmqr('L', 'N', 0, 4, 0, A, 6, TAU, C, 6, WORK, LWORK, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call end_test()

  ! Test 5: N=0 quick return
  call zunmqr('L', 'N', 4, 0, 0, A, 6, TAU, C, 6, WORK, LWORK, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: K=0 quick return
  call zunmqr('L', 'N', 4, 4, 0, A, 6, TAU, C, 6, WORK, LWORK, info)
  call begin_test('k_zero')
  call print_int('info', info)
  call end_test()

  ! Test 7: Apply Q to non-identity matrix
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.0d0); A(2,1) = (2.0d0, 1.0d0); A(3,1) = (0.0d0, 0.0d0); A(4,1) = (1.0d0, 1.0d0)
  A(1,2) = (0.0d0, 2.0d0); A(2,2) = (1.0d0, 0.0d0); A(3,2) = (3.0d0, 1.0d0); A(4,2) = (2.0d0, 0.0d0)
  A(1,3) = (3.0d0, 1.0d0); A(2,3) = (0.0d0, 0.0d0); A(3,3) = (1.0d0, 0.0d0); A(4,3) = (2.0d0, 1.0d0)
  TAU = (0.0d0, 0.0d0)
  call zgeqr2(4, 3, A, 6, TAU, WORK, info)

  C = (0.0d0, 0.0d0)
  C(1,1) = (1.0d0, 1.0d0); C(2,1) = (3.0d0, 0.0d0); C(3,1) = (-1.0d0, 1.0d0); C(4,1) = (2.0d0, 0.0d0)
  C(1,2) = (0.0d0, 2.0d0); C(2,2) = (1.0d0, -1.0d0); C(3,2) = (4.0d0, 0.0d0); C(4,2) = (0.0d0, 3.0d0)
  call zunmqr('L', 'N', 4, 2, 3, A, 6, TAU, C, 6, WORK, LWORK, info)
  call begin_test('left_notrans_rect')
  call print_int('info', info)
  call print_array('c', C_r, 16)
  call end_test()

end program
