program test_dormhr
  use test_utils
  implicit none

  ! Small test: 5x5 matrix, dgehrd with ILO=1, IHI=5
  double precision :: A(6, 6), C(6, 6), TAU(6), WORK(1000)
  integer :: info, i, j, LWORK

  ! Partial Hessenberg test: ILO=2, IHI=4 on 5x5
  double precision :: A2(6, 6), C2(6, 6), TAU2(6), WORK2(1000)

  LWORK = 1000

  ! ===== Generate Hessenberg factorization of 5x5 matrix =====
  ! A = diagonally dominant matrix
  A = 0.0d0
  do i = 1, 5
    do j = 1, 5
      A(i, j) = dble(i + j) * 0.5d0
    end do
    A(i, i) = A(i, i) + 5.0d0
  end do
  TAU = 0.0d0
  call dgehrd(5, 1, 5, A, 6, TAU, WORK, LWORK, info)

  ! Print Hessenberg factors for JS test reuse
  call begin_test('hess_factors')
  call print_array('a', A, 30)
  call print_array('tau', TAU, 4)
  call end_test()

  ! Test 1: Left, No transpose: C := Q * I_5
  C = 0.0d0
  do i = 1, 5
    C(i,i) = 1.0d0
  end do
  call dormhr('L', 'N', 5, 5, 1, 5, A, 6, TAU, C, 6, WORK, LWORK, info)
  call begin_test('left_notrans')
  call print_int('info', info)
  call print_matrix('c', C, 6, 5, 5)
  call end_test()

  ! Test 2: Left, Transpose: C := Q^T * I_5
  C = 0.0d0
  do i = 1, 5
    C(i,i) = 1.0d0
  end do
  call dormhr('L', 'T', 5, 5, 1, 5, A, 6, TAU, C, 6, WORK, LWORK, info)
  call begin_test('left_trans')
  call print_int('info', info)
  call print_matrix('c', C, 6, 5, 5)
  call end_test()

  ! Test 3: Right, No transpose: C := I_5 * Q
  C = 0.0d0
  do i = 1, 5
    C(i,i) = 1.0d0
  end do
  call dormhr('R', 'N', 5, 5, 1, 5, A, 6, TAU, C, 6, WORK, LWORK, info)
  call begin_test('right_notrans')
  call print_int('info', info)
  call print_matrix('c', C, 6, 5, 5)
  call end_test()

  ! Test 4: Right, Transpose: C := I_5 * Q^T
  C = 0.0d0
  do i = 1, 5
    C(i,i) = 1.0d0
  end do
  call dormhr('R', 'T', 5, 5, 1, 5, A, 6, TAU, C, 6, WORK, LWORK, info)
  call begin_test('right_trans')
  call print_int('info', info)
  call print_matrix('c', C, 6, 5, 5)
  call end_test()

  ! Test 5: Left, notrans with non-identity rectangular C (5x3)
  C = 0.0d0
  C(1,1) = 1.0d0; C(2,1) = 3.0d0; C(3,1) = -1.0d0; C(4,1) = 2.0d0; C(5,1) = 0.5d0
  C(1,2) = 2.0d0; C(2,2) = 0.0d0; C(3,2) = 4.0d0; C(4,2) = -1.0d0; C(5,2) = 1.5d0
  C(1,3) = -0.5d0; C(2,3) = 1.0d0; C(3,3) = 2.0d0; C(4,3) = 3.0d0; C(5,3) = -2.0d0
  call dormhr('L', 'N', 5, 3, 1, 5, A, 6, TAU, C, 6, WORK, LWORK, info)
  call begin_test('left_notrans_rect')
  call print_int('info', info)
  call print_matrix('c', C, 6, 5, 3)
  call end_test()

  ! Test 6: Right, trans with non-identity rectangular C (3x5)
  C = 0.0d0
  C(1,1) = 1.0d0; C(2,1) = 0.0d0; C(3,1) = 2.0d0
  C(1,2) = 2.0d0; C(2,2) = 1.0d0; C(3,2) = -1.0d0
  C(1,3) = -1.0d0; C(2,3) = 3.0d0; C(3,3) = 0.0d0
  C(1,4) = 4.0d0; C(2,4) = -2.0d0; C(3,4) = 1.0d0
  C(1,5) = 0.5d0; C(2,5) = 1.5d0; C(3,5) = -0.5d0
  call dormhr('R', 'T', 3, 5, 1, 5, A, 6, TAU, C, 6, WORK, LWORK, info)
  call begin_test('right_trans_rect')
  call print_int('info', info)
  call print_matrix('c', C, 6, 3, 5)
  call end_test()

  ! Test 7: M=0 quick return
  call dormhr('L', 'N', 0, 5, 1, 0, A, 6, TAU, C, 6, WORK, LWORK, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call end_test()

  ! Test 8: N=0 quick return
  call dormhr('L', 'N', 5, 0, 1, 5, A, 6, TAU, C, 6, WORK, LWORK, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 9: NH=0 quick return (ILO=IHI)
  call dormhr('L', 'N', 5, 5, 3, 3, A, 6, TAU, C, 6, WORK, LWORK, info)
  call begin_test('nh_zero')
  call print_int('info', info)
  call end_test()

  ! ===== Test 10: Partial Hessenberg ILO=2, IHI=4 on 5x5 =====
  A2 = 0.0d0
  do i = 1, 5
    do j = 1, 5
      A2(i, j) = dble(i * 2 + j) * 0.3d0
    end do
    A2(i, i) = A2(i, i) + 8.0d0
  end do
  TAU2 = 0.0d0
  call dgehrd(5, 2, 4, A2, 6, TAU2, WORK2, LWORK, info)

  call begin_test('hess_factors_partial')
  call print_array('a2', A2, 30)
  call print_array('tau2', TAU2, 4)
  call end_test()

  ! Test 10: Left, notrans with partial Hessenberg
  C2 = 0.0d0
  do i = 1, 5
    C2(i,i) = 1.0d0
  end do
  call dormhr('L', 'N', 5, 5, 2, 4, A2, 6, TAU2, C2, 6, WORK2, LWORK, info)
  call begin_test('left_notrans_partial')
  call print_int('info', info)
  call print_matrix('c', C2, 6, 5, 5)
  call end_test()

  ! Test 11: Right, trans with partial Hessenberg
  C2 = 0.0d0
  do i = 1, 5
    C2(i,i) = 1.0d0
  end do
  call dormhr('R', 'T', 5, 5, 2, 4, A2, 6, TAU2, C2, 6, WORK2, LWORK, info)
  call begin_test('right_trans_partial')
  call print_int('info', info)
  call print_matrix('c', C2, 6, 5, 5)
  call end_test()

end program
