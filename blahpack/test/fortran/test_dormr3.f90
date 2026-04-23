program test_dormr3
  use test_utils
  implicit none

  ! RZ factorization of a 3x5 matrix A, then apply Q or Q^T via dormr3
  double precision :: A(3, 5), ASAVE(3, 5), TAU(3), TAUSAVE(3), WORK(100)
  double precision :: C(5, 5), CR(3, 5)
  integer :: info, i, j

  ! Build a 3x5 matrix A; dtzrzf requires M <= N. After RZ,
  ! the upper triangle of the first M columns holds R and the
  ! remaining N-M columns of each row hold the z-vectors.
  A = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0; A(1,3) = 3.0d0; A(1,4) = 4.0d0;  A(1,5) = 5.0d0
  A(2,1) = 6.0d0; A(2,2) = 7.0d0; A(2,3) = 8.0d0; A(2,4) = 9.0d0;  A(2,5) = 10.0d0
  A(3,1) = 11.0d0; A(3,2) = 12.0d0; A(3,3) = 13.0d0; A(3,4) = 14.0d0; A(3,5) = 15.0d0
  TAU = 0.0d0
  call dtzrzf(3, 5, A, 3, TAU, WORK, 100, info)

  ASAVE = A
  TAUSAVE = TAU

  call begin_test('rz_factor')
  call print_int('info', info)
  call print_array('A', A, 15)
  call print_array('TAU', TAU, 3)
  call end_test()

  ! For SIDE='L', Q is M-by-M = 5x5 (order of matrix Q is the length of v = N)
  ! Here K=3 reflectors, L=N-M=2 trailing entries per reflector.
  ! Test 1: Left, No transpose: C := Q * C, C = I_5
  C = 0.0d0
  do i = 1, 5
    C(i,i) = 1.0d0
  end do
  A = ASAVE; TAU = TAUSAVE
  call dormr3('L', 'N', 5, 5, 3, 2, A, 3, TAU, C, 5, WORK, info)
  call begin_test('left_notrans')
  call print_int('info', info)
  call print_array('c', C, 25)
  call end_test()

  ! Test 2: Left, Transpose
  C = 0.0d0
  do i = 1, 5
    C(i,i) = 1.0d0
  end do
  A = ASAVE; TAU = TAUSAVE
  call dormr3('L', 'T', 5, 5, 3, 2, A, 3, TAU, C, 5, WORK, info)
  call begin_test('left_trans')
  call print_int('info', info)
  call print_array('c', C, 25)
  call end_test()

  ! Test 3: Right, No transpose: C := C * Q, C is 5x5 identity
  C = 0.0d0
  do i = 1, 5
    C(i,i) = 1.0d0
  end do
  A = ASAVE; TAU = TAUSAVE
  call dormr3('R', 'N', 5, 5, 3, 2, A, 3, TAU, C, 5, WORK, info)
  call begin_test('right_notrans')
  call print_int('info', info)
  call print_array('c', C, 25)
  call end_test()

  ! Test 4: Right, Transpose
  C = 0.0d0
  do i = 1, 5
    C(i,i) = 1.0d0
  end do
  A = ASAVE; TAU = TAUSAVE
  call dormr3('R', 'T', 5, 5, 3, 2, A, 3, TAU, C, 5, WORK, info)
  call begin_test('right_trans')
  call print_int('info', info)
  call print_array('c', C, 25)
  call end_test()

  ! Test 5: Left, notrans with non-identity rectangular C (5x3)
  do j = 1, 3
    do i = 1, 5
      C(i,j) = dble(i) + 0.5d0*dble(j) - 1.0d0
    end do
  end do
  A = ASAVE; TAU = TAUSAVE
  call dormr3('L', 'N', 5, 3, 3, 2, A, 3, TAU, C, 5, WORK, info)
  call begin_test('left_notrans_rect')
  call print_int('info', info)
  call print_array('c', C, 15)
  call end_test()

  ! Test 6: Right, trans with non-identity rectangular C (3x5)
  do j = 1, 5
    do i = 1, 3
      CR(i,j) = dble(j) - 0.25d0*dble(i) + 1.0d0
    end do
  end do
  A = ASAVE; TAU = TAUSAVE
  call dormr3('R', 'T', 3, 5, 3, 2, A, 3, TAU, CR, 3, WORK, info)
  call begin_test('right_trans_rect')
  call print_int('info', info)
  call print_array('c', CR, 15)
  call end_test()

  ! Test 7: M=0 quick return
  call dormr3('L', 'N', 0, 5, 0, 0, A, 3, TAU, C, 1, WORK, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call end_test()

  ! Test 8: N=0 quick return
  call dormr3('L', 'N', 5, 0, 0, 0, A, 3, TAU, C, 5, WORK, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 9: K=0 quick return
  call dormr3('L', 'N', 5, 5, 0, 0, A, 3, TAU, C, 5, WORK, info)
  call begin_test('k_zero')
  call print_int('info', info)
  call end_test()

  ! Test 10: L=0 (reflectors with no trailing z, effectively identity on tail)
  C = 0.0d0
  do i = 1, 5
    C(i,i) = 1.0d0
  end do
  A = ASAVE; TAU = 0.0d0
  call dormr3('L', 'N', 5, 5, 3, 0, A, 3, TAU, C, 5, WORK, info)
  call begin_test('l_zero')
  call print_int('info', info)
  call print_array('c', C, 25)
  call end_test()

end program
