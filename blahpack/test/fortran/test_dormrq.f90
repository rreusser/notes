program test_dormrq
  use test_utils
  implicit none

  ! RQ factorization of a 3x4 matrix A, then apply Q or Q^T to various C matrices
  double precision :: A(4, 4), ASAVE(4, 4), C(4, 4), TAU(4), TAUSAVE(4), WORK(100)
  double precision :: CR(2, 4)
  integer :: info, lwork

  lwork = 100

  ! Compute RQ of a 3x4 matrix
  ! A = [1 2 3 4; 5 6 7 8; 9 10 11 12]
  A = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0; A(1,3) = 3.0d0; A(1,4) = 4.0d0
  A(2,1) = 5.0d0; A(2,2) = 6.0d0; A(2,3) = 7.0d0; A(2,4) = 8.0d0
  A(3,1) = 9.0d0; A(3,2) = 10.0d0; A(3,3) = 11.0d0; A(3,4) = 12.0d0
  TAU = 0.0d0
  call dgerqf(3, 4, A, 4, TAU, WORK, lwork, info)

  ASAVE = A
  TAUSAVE = TAU

  ! Print the RQ factorization for reference
  call begin_test('rq_factor')
  call print_array('A', A, 12)
  call print_array('TAU', TAU, 3)
  call end_test()

  ! Test 1: Left, No transpose: C := Q * C where C = I_4
  C = 0.0d0
  C(1,1) = 1.0d0; C(2,2) = 1.0d0; C(3,3) = 1.0d0; C(4,4) = 1.0d0
  A = ASAVE; TAU = TAUSAVE
  call dormrq('L', 'N', 4, 4, 3, A, 4, TAU, C, 4, WORK, lwork, info)
  call begin_test('left_notrans')
  call print_int('info', info)
  call print_array('c', C, 16)
  call end_test()

  ! Test 2: Left, Transpose: C := Q^T * C where C = I_4
  C = 0.0d0
  C(1,1) = 1.0d0; C(2,2) = 1.0d0; C(3,3) = 1.0d0; C(4,4) = 1.0d0
  A = ASAVE; TAU = TAUSAVE
  call dormrq('L', 'T', 4, 4, 3, A, 4, TAU, C, 4, WORK, lwork, info)
  call begin_test('left_trans')
  call print_int('info', info)
  call print_array('c', C, 16)
  call end_test()

  ! Test 3: Right, No transpose: C := C * Q where C = I_4
  C = 0.0d0
  C(1,1) = 1.0d0; C(2,2) = 1.0d0; C(3,3) = 1.0d0; C(4,4) = 1.0d0
  A = ASAVE; TAU = TAUSAVE
  call dormrq('R', 'N', 4, 4, 3, A, 4, TAU, C, 4, WORK, lwork, info)
  call begin_test('right_notrans')
  call print_int('info', info)
  call print_array('c', C, 16)
  call end_test()

  ! Test 4: Right, Transpose: C := C * Q^T where C = I_4
  C = 0.0d0
  C(1,1) = 1.0d0; C(2,2) = 1.0d0; C(3,3) = 1.0d0; C(4,4) = 1.0d0
  A = ASAVE; TAU = TAUSAVE
  call dormrq('R', 'T', 4, 4, 3, A, 4, TAU, C, 4, WORK, lwork, info)
  call begin_test('right_trans')
  call print_int('info', info)
  call print_array('c', C, 16)
  call end_test()

  ! Test 5: M=0 quick return
  call dormrq('L', 'N', 0, 4, 0, A, 1, TAU, C, 1, WORK, lwork, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: N=0 quick return
  call dormrq('L', 'N', 4, 0, 0, A, 4, TAU, C, 4, WORK, lwork, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 7: K=0 quick return
  call dormrq('L', 'N', 4, 4, 0, A, 4, TAU, C, 4, WORK, lwork, info)
  call begin_test('k_zero')
  call print_int('info', info)
  call end_test()

  ! Test 8: Left, notrans with rectangular C (4x2)
  C = 0.0d0
  C(1,1) = 1.0d0; C(2,1) = 3.0d0; C(3,1) = -1.0d0; C(4,1) = 2.0d0
  C(1,2) = 2.0d0; C(2,2) = 0.0d0; C(3,2) = 4.0d0; C(4,2) = -1.0d0
  A = ASAVE; TAU = TAUSAVE
  call dormrq('L', 'N', 4, 2, 3, A, 4, TAU, C, 4, WORK, lwork, info)
  call begin_test('left_notrans_rect')
  call print_int('info', info)
  call print_array('c', C, 8)
  call end_test()

  ! Test 9: Right, notrans with rectangular C (2x4)
  CR = 0.0d0
  CR(1,1) = 1.0d0; CR(2,1) = 0.0d0
  CR(1,2) = 2.0d0; CR(2,2) = 1.0d0
  CR(1,3) = -1.0d0; CR(2,3) = 3.0d0
  CR(1,4) = 4.0d0; CR(2,4) = -2.0d0
  A = ASAVE; TAU = TAUSAVE
  call dormrq('R', 'N', 2, 4, 3, A, 4, TAU, CR, 2, WORK, lwork, info)
  call begin_test('right_notrans_rect')
  call print_int('info', info)
  call print_array('c', CR, 8)
  call end_test()

end program
