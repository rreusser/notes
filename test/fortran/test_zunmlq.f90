program test_zunmlq
  use test_utils
  implicit none

  complex*16 :: A(6, 6), C(6, 6), TAU(6), WORK(200)
  double precision :: A_r(72), C_r(72), TAU_r(12)
  equivalence (A, A_r)
  equivalence (C, C_r)
  equivalence (TAU, TAU_r)
  integer :: info, i, j, LWORK

  LWORK = 200

  ! Test 1: Left, No transpose: Q * I where Q from 3x5 LQ
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.0d0); A(1,2) = (2.0d0, 1.0d0); A(1,3) = (0.0d0, 0.0d0); A(1,4) = (1.0d0, 1.0d0); A(1,5) = (3.0d0, 0.0d0)
  A(2,1) = (0.0d0, 2.0d0); A(2,2) = (1.0d0, 0.0d0); A(2,3) = (3.0d0, 1.0d0); A(2,4) = (2.0d0, 0.0d0); A(2,5) = (1.0d0, 1.0d0)
  A(3,1) = (3.0d0, 1.0d0); A(3,2) = (0.0d0, 0.0d0); A(3,3) = (1.0d0, 0.0d0); A(3,4) = (2.0d0, 1.0d0); A(3,5) = (0.0d0, 2.0d0)
  TAU = (0.0d0, 0.0d0)
  call zgelq2(3, 5, A, 6, TAU, WORK, info)

  ! C = 5x5 identity
  C = (0.0d0, 0.0d0)
  do i = 1, 5
    C(i,i) = (1.0d0, 0.0d0)
  end do
  call zunmlq('L', 'N', 5, 5, 3, A, 6, TAU, C, 6, WORK, LWORK, info)
  call begin_test('left_notrans_5x5')
  call print_int('info', info)
  call print_array('c', C_r, 50)
  call end_test()

  ! Test 2: Left, Conjugate transpose: Q^H * I
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.0d0); A(1,2) = (2.0d0, 1.0d0); A(1,3) = (0.0d0, 0.0d0); A(1,4) = (1.0d0, 1.0d0); A(1,5) = (3.0d0, 0.0d0)
  A(2,1) = (0.0d0, 2.0d0); A(2,2) = (1.0d0, 0.0d0); A(2,3) = (3.0d0, 1.0d0); A(2,4) = (2.0d0, 0.0d0); A(2,5) = (1.0d0, 1.0d0)
  A(3,1) = (3.0d0, 1.0d0); A(3,2) = (0.0d0, 0.0d0); A(3,3) = (1.0d0, 0.0d0); A(3,4) = (2.0d0, 1.0d0); A(3,5) = (0.0d0, 2.0d0)
  TAU = (0.0d0, 0.0d0)
  call zgelq2(3, 5, A, 6, TAU, WORK, info)

  C = (0.0d0, 0.0d0)
  do i = 1, 5
    C(i,i) = (1.0d0, 0.0d0)
  end do
  call zunmlq('L', 'C', 5, 5, 3, A, 6, TAU, C, 6, WORK, LWORK, info)
  call begin_test('left_conjtrans_5x5')
  call print_int('info', info)
  call print_array('c', C_r, 50)
  call end_test()

  ! Test 3: Right, No transpose: I * Q
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.0d0); A(1,2) = (2.0d0, 1.0d0); A(1,3) = (0.0d0, 0.0d0); A(1,4) = (1.0d0, 1.0d0); A(1,5) = (3.0d0, 0.0d0)
  A(2,1) = (0.0d0, 2.0d0); A(2,2) = (1.0d0, 0.0d0); A(2,3) = (3.0d0, 1.0d0); A(2,4) = (2.0d0, 0.0d0); A(2,5) = (1.0d0, 1.0d0)
  A(3,1) = (3.0d0, 1.0d0); A(3,2) = (0.0d0, 0.0d0); A(3,3) = (1.0d0, 0.0d0); A(3,4) = (2.0d0, 1.0d0); A(3,5) = (0.0d0, 2.0d0)
  TAU = (0.0d0, 0.0d0)
  call zgelq2(3, 5, A, 6, TAU, WORK, info)

  C = (0.0d0, 0.0d0)
  do i = 1, 5
    C(i,i) = (1.0d0, 0.0d0)
  end do
  call zunmlq('R', 'N', 5, 5, 3, A, 6, TAU, C, 6, WORK, LWORK, info)
  call begin_test('right_notrans_5x5')
  call print_int('info', info)
  call print_array('c', C_r, 50)
  call end_test()

  ! Test 4: M=0 quick return
  call zunmlq('L', 'N', 0, 5, 0, A, 6, TAU, C, 6, WORK, LWORK, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call end_test()

  ! Test 5: N=0 quick return
  call zunmlq('L', 'N', 5, 0, 0, A, 6, TAU, C, 6, WORK, LWORK, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: K=0 quick return
  call zunmlq('L', 'N', 5, 5, 0, A, 6, TAU, C, 6, WORK, LWORK, info)
  call begin_test('k_zero')
  call print_int('info', info)
  call end_test()

  ! Test 7: Apply Q to non-identity, right, conj trans
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.0d0); A(1,2) = (2.0d0, 1.0d0); A(1,3) = (0.0d0, 0.0d0); A(1,4) = (1.0d0, 1.0d0); A(1,5) = (3.0d0, 0.0d0)
  A(2,1) = (0.0d0, 2.0d0); A(2,2) = (1.0d0, 0.0d0); A(2,3) = (3.0d0, 1.0d0); A(2,4) = (2.0d0, 0.0d0); A(2,5) = (1.0d0, 1.0d0)
  A(3,1) = (3.0d0, 1.0d0); A(3,2) = (0.0d0, 0.0d0); A(3,3) = (1.0d0, 0.0d0); A(3,4) = (2.0d0, 1.0d0); A(3,5) = (0.0d0, 2.0d0)
  TAU = (0.0d0, 0.0d0)
  call zgelq2(3, 5, A, 6, TAU, WORK, info)

  C = (0.0d0, 0.0d0)
  C(1,1) = (1.0d0, 1.0d0); C(2,1) = (3.0d0, 0.0d0); C(3,1) = (-1.0d0, 1.0d0)
  C(1,2) = (0.0d0, 2.0d0); C(2,2) = (1.0d0, -1.0d0); C(3,2) = (4.0d0, 0.0d0)
  C(1,3) = (2.0d0, 0.0d0); C(2,3) = (0.0d0, 1.0d0); C(3,3) = (1.0d0, 1.0d0)
  C(1,4) = (1.0d0, 0.0d0); C(2,4) = (2.0d0, 0.0d0); C(3,4) = (0.0d0, 3.0d0)
  C(1,5) = (0.0d0, 1.0d0); C(2,5) = (1.0d0, 1.0d0); C(3,5) = (2.0d0, 0.0d0)
  call zunmlq('R', 'C', 3, 5, 3, A, 6, TAU, C, 6, WORK, LWORK, info)
  call begin_test('right_conjtrans_rect')
  call print_int('info', info)
  call print_array('c', C_r, 30)
  call end_test()

end program
