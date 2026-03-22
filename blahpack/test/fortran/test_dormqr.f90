program test_dormqr
  use test_utils
  implicit none

  ! Small tests: QR of a 4x3 matrix, then apply Q via dormqr
  double precision :: A(6, 6), C(6, 6), TAU(6), WORK(1000)
  ! Rectangular C for right-side tests
  double precision :: CR(3, 6)
  integer :: info, i, j, LWORK

  ! Large test: 40x35 matrix to exercise blocked path (NB=32)
  double precision :: AL(40, 40), CL(40, 40), TAUL(40), WORKL(10000)

  LWORK = 1000

  ! ===== Compute QR of a 4x3 matrix =====
  ! A = [1 5 9; 2 6 10; 3 7 11; 4 8 12] stored in 6x6 container (LDA=6)
  A = 0.0d0
  A(1,1) = 1.0d0; A(2,1) = 2.0d0; A(3,1) = 3.0d0; A(4,1) = 4.0d0
  A(1,2) = 5.0d0; A(2,2) = 6.0d0; A(3,2) = 7.0d0; A(4,2) = 8.0d0
  A(1,3) = 9.0d0; A(2,3) = 10.0d0; A(3,3) = 11.0d0; A(4,3) = 12.0d0
  TAU = 0.0d0
  call dgeqrf(4, 3, A, 6, TAU, WORK, LWORK, info)

  ! Print QR factors for JS test reuse
  call begin_test('qr_factors_small')
  call print_array('a', A, 36)
  call print_array('tau', TAU, 3)
  call end_test()

  ! Test 1: Left, No transpose: C := Q * C where C = I_4
  C = 0.0d0
  do i = 1, 4
    C(i,i) = 1.0d0
  end do
  call dormqr('L', 'N', 4, 4, 3, A, 6, TAU, C, 6, WORK, LWORK, info)
  call begin_test('left_notrans')
  call print_int('info', info)
  call print_array('c', C, 24)
  call end_test()

  ! Test 2: Left, Transpose: C := Q^T * C where C = I_4
  C = 0.0d0
  do i = 1, 4
    C(i,i) = 1.0d0
  end do
  call dormqr('L', 'T', 4, 4, 3, A, 6, TAU, C, 6, WORK, LWORK, info)
  call begin_test('left_trans')
  call print_int('info', info)
  call print_array('c', C, 24)
  call end_test()

  ! Test 3: Right, No transpose: C := C * Q where C = I_4
  C = 0.0d0
  do i = 1, 4
    C(i,i) = 1.0d0
  end do
  call dormqr('R', 'N', 4, 4, 3, A, 6, TAU, C, 6, WORK, LWORK, info)
  call begin_test('right_notrans')
  call print_int('info', info)
  call print_array('c', C, 24)
  call end_test()

  ! Test 4: Right, Transpose: C := C * Q^T where C = I_4
  C = 0.0d0
  do i = 1, 4
    C(i,i) = 1.0d0
  end do
  call dormqr('R', 'T', 4, 4, 3, A, 6, TAU, C, 6, WORK, LWORK, info)
  call begin_test('right_trans')
  call print_int('info', info)
  call print_array('c', C, 24)
  call end_test()

  ! Test 5: M=0 quick return
  call dormqr('L', 'N', 0, 4, 0, A, 6, TAU, C, 6, WORK, LWORK, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: N=0 quick return
  call dormqr('L', 'N', 4, 0, 0, A, 6, TAU, C, 6, WORK, LWORK, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 7: K=0 quick return
  call dormqr('L', 'N', 4, 4, 0, A, 6, TAU, C, 6, WORK, LWORK, info)
  call begin_test('k_zero')
  call print_int('info', info)
  call end_test()

  ! Test 8: Left, notrans with rectangular C (4x2, non-identity)
  C = 0.0d0
  C(1,1) = 1.0d0; C(2,1) = 3.0d0; C(3,1) = -1.0d0; C(4,1) = 2.0d0
  C(1,2) = 2.0d0; C(2,2) = 0.0d0; C(3,2) = 4.0d0; C(4,2) = -1.0d0
  call dormqr('L', 'N', 4, 2, 3, A, 6, TAU, C, 6, WORK, LWORK, info)
  call begin_test('left_notrans_rect')
  call print_int('info', info)
  call print_array('c', C, 12)
  call end_test()

  ! Test 9: Left, trans with rectangular C (4x2, non-identity)
  C = 0.0d0
  C(1,1) = 1.0d0; C(2,1) = 3.0d0; C(3,1) = -1.0d0; C(4,1) = 2.0d0
  C(1,2) = 2.0d0; C(2,2) = 0.0d0; C(3,2) = 4.0d0; C(4,2) = -1.0d0
  call dormqr('L', 'T', 4, 2, 3, A, 6, TAU, C, 6, WORK, LWORK, info)
  call begin_test('left_trans_rect')
  call print_int('info', info)
  call print_array('c', C, 12)
  call end_test()

  ! Test 10: Right, notrans with rectangular C (3x4)
  CR = 0.0d0
  CR(1,1) = 1.0d0; CR(2,1) = 0.0d0; CR(3,1) = 2.0d0
  CR(1,2) = 2.0d0; CR(2,2) = 1.0d0; CR(3,2) = -1.0d0
  CR(1,3) = -1.0d0; CR(2,3) = 3.0d0; CR(3,3) = 0.0d0
  CR(1,4) = 4.0d0; CR(2,4) = -2.0d0; CR(3,4) = 1.0d0
  call dormqr('R', 'N', 3, 4, 3, A, 6, TAU, CR, 3, WORK, LWORK, info)
  call begin_test('right_notrans_rect')
  call print_int('info', info)
  call print_array('c', CR, 12)
  call end_test()

  ! Test 11: Right, trans with rectangular C (3x4)
  CR = 0.0d0
  CR(1,1) = 1.0d0; CR(2,1) = 0.0d0; CR(3,1) = 2.0d0
  CR(1,2) = 2.0d0; CR(2,2) = 1.0d0; CR(3,2) = -1.0d0
  CR(1,3) = -1.0d0; CR(2,3) = 3.0d0; CR(3,3) = 0.0d0
  CR(1,4) = 4.0d0; CR(2,4) = -2.0d0; CR(3,4) = 1.0d0
  call dormqr('R', 'T', 3, 4, 3, A, 6, TAU, CR, 3, WORK, LWORK, info)
  call begin_test('right_trans_rect')
  call print_int('info', info)
  call print_array('c', CR, 12)
  call end_test()

  ! ===== Test 12: Large matrix to exercise blocked path (K=35 > NB=32) =====
  ! Compute QR of a 40x35 diagonally-dominant matrix
  AL = 0.0d0
  do i = 1, 40
    do j = 1, 35
      AL(i, j) = dble(i + j) * 0.1d0
    end do
    if (i <= 35) then
      AL(i, i) = AL(i, i) + 10.0d0
    end if
  end do
  TAUL = 0.0d0
  call dgeqrf(40, 35, AL, 40, TAUL, WORKL, 10000, info)

  ! Print QR factors for JS test reuse
  call begin_test('qr_factors_large')
  call print_array('a', AL, 1600)
  call print_array('tau', TAUL, 35)
  call end_test()

  ! Left, notrans: Q * I_40
  CL = 0.0d0
  do i = 1, 40
    CL(i,i) = 1.0d0
  end do
  call dormqr('L', 'N', 40, 40, 35, AL, 40, TAUL, CL, 40, WORKL, 10000, info)
  call begin_test('left_notrans_blocked')
  call print_int('info', info)
  call print_array('c', CL, 1600)
  call end_test()

  ! Test 13: Left, trans, blocked: Q^T * I_40
  CL = 0.0d0
  do i = 1, 40
    CL(i,i) = 1.0d0
  end do
  call dormqr('L', 'T', 40, 40, 35, AL, 40, TAUL, CL, 40, WORKL, 10000, info)
  call begin_test('left_trans_blocked')
  call print_int('info', info)
  call print_array('c', CL, 1600)
  call end_test()

  ! Test 14: Right, notrans, blocked: I_40 * Q
  CL = 0.0d0
  do i = 1, 40
    CL(i,i) = 1.0d0
  end do
  call dormqr('R', 'N', 40, 40, 35, AL, 40, TAUL, CL, 40, WORKL, 10000, info)
  call begin_test('right_notrans_blocked')
  call print_int('info', info)
  call print_array('c', CL, 1600)
  call end_test()

  ! Test 15: Right, trans, blocked: I_40 * Q^T
  CL = 0.0d0
  do i = 1, 40
    CL(i,i) = 1.0d0
  end do
  call dormqr('R', 'T', 40, 40, 35, AL, 40, TAUL, CL, 40, WORKL, 10000, info)
  call begin_test('right_trans_blocked')
  call print_int('info', info)
  call print_array('c', CL, 1600)
  call end_test()

end program
