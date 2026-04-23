program test_dormtr
  use test_utils
  implicit none

  integer, parameter :: NMAX = 4
  double precision :: A(NMAX, NMAX), Aorig(NMAX, NMAX)
  double precision :: C(NMAX, NMAX), Csave(NMAX, NMAX)
  double precision :: TAU(NMAX), D(NMAX), E(NMAX), WORK(256)
  double precision :: Cflat(NMAX*NMAX)
  integer :: INFO, I, J, LWORK

  LWORK = 256

  ! ============================================================
  ! Setup: 4x4 symmetric matrix, reduce with upper
  ! ============================================================
  A(1,1) =  4.0d0; A(1,2) =  1.0d0; A(1,3) = -2.0d0; A(1,4) =  2.0d0
  A(2,1) =  1.0d0; A(2,2) =  2.0d0; A(2,3) =  0.0d0; A(2,4) =  1.0d0
  A(3,1) = -2.0d0; A(3,2) =  0.0d0; A(3,3) =  3.0d0; A(3,4) = -2.0d0
  A(4,1) =  2.0d0; A(4,2) =  1.0d0; A(4,3) = -2.0d0; A(4,4) = -1.0d0
  Aorig = A

  call DSYTRD('U', 4, A, NMAX, D, E, TAU, WORK, LWORK, INFO)

  ! ============================================================
  ! Test 1: Left, No-transpose, Upper, C = I_4
  ! ============================================================
  C = 0.0d0
  C(1,1) = 1.0d0; C(2,2) = 1.0d0; C(3,3) = 1.0d0; C(4,4) = 1.0d0
  call DORMTR('L', 'U', 'N', 4, 4, A, NMAX, TAU, C, NMAX, WORK, LWORK, INFO)
  call begin_test('left_notrans_upper')
  call print_int('info', INFO)
  do J = 1, 4
    do I = 1, 4
      Cflat((J-1)*4 + I) = C(I,J)
    end do
  end do
  call print_array('C', Cflat, 16)
  call end_test()

  ! ============================================================
  ! Test 2: Left, Transpose, Upper, C = I_4
  ! ============================================================
  C = 0.0d0
  C(1,1) = 1.0d0; C(2,2) = 1.0d0; C(3,3) = 1.0d0; C(4,4) = 1.0d0
  call DORMTR('L', 'U', 'T', 4, 4, A, NMAX, TAU, C, NMAX, WORK, LWORK, INFO)
  call begin_test('left_trans_upper')
  call print_int('info', INFO)
  do J = 1, 4
    do I = 1, 4
      Cflat((J-1)*4 + I) = C(I,J)
    end do
  end do
  call print_array('C', Cflat, 16)
  call end_test()

  ! ============================================================
  ! Test 3: Right, No-transpose, Upper, C = I_4
  ! ============================================================
  C = 0.0d0
  C(1,1) = 1.0d0; C(2,2) = 1.0d0; C(3,3) = 1.0d0; C(4,4) = 1.0d0
  call DORMTR('R', 'U', 'N', 4, 4, A, NMAX, TAU, C, NMAX, WORK, LWORK, INFO)
  call begin_test('right_notrans_upper')
  call print_int('info', INFO)
  do J = 1, 4
    do I = 1, 4
      Cflat((J-1)*4 + I) = C(I,J)
    end do
  end do
  call print_array('C', Cflat, 16)
  call end_test()

  ! ============================================================
  ! Test 4: Right, Transpose, Upper, C = I_4
  ! ============================================================
  C = 0.0d0
  C(1,1) = 1.0d0; C(2,2) = 1.0d0; C(3,3) = 1.0d0; C(4,4) = 1.0d0
  call DORMTR('R', 'U', 'T', 4, 4, A, NMAX, TAU, C, NMAX, WORK, LWORK, INFO)
  call begin_test('right_trans_upper')
  call print_int('info', INFO)
  do J = 1, 4
    do I = 1, 4
      Cflat((J-1)*4 + I) = C(I,J)
    end do
  end do
  call print_array('C', Cflat, 16)
  call end_test()

  ! ============================================================
  ! Setup: same matrix, reduce with lower
  ! ============================================================
  A = Aorig
  call DSYTRD('L', 4, A, NMAX, D, E, TAU, WORK, LWORK, INFO)

  ! ============================================================
  ! Test 5: Left, No-transpose, Lower, C = I_4
  ! ============================================================
  C = 0.0d0
  C(1,1) = 1.0d0; C(2,2) = 1.0d0; C(3,3) = 1.0d0; C(4,4) = 1.0d0
  call DORMTR('L', 'L', 'N', 4, 4, A, NMAX, TAU, C, NMAX, WORK, LWORK, INFO)
  call begin_test('left_notrans_lower')
  call print_int('info', INFO)
  do J = 1, 4
    do I = 1, 4
      Cflat((J-1)*4 + I) = C(I,J)
    end do
  end do
  call print_array('C', Cflat, 16)
  call end_test()

  ! ============================================================
  ! Test 6: Left, Transpose, Lower, C = I_4
  ! ============================================================
  C = 0.0d0
  C(1,1) = 1.0d0; C(2,2) = 1.0d0; C(3,3) = 1.0d0; C(4,4) = 1.0d0
  call DORMTR('L', 'L', 'T', 4, 4, A, NMAX, TAU, C, NMAX, WORK, LWORK, INFO)
  call begin_test('left_trans_lower')
  call print_int('info', INFO)
  do J = 1, 4
    do I = 1, 4
      Cflat((J-1)*4 + I) = C(I,J)
    end do
  end do
  call print_array('C', Cflat, 16)
  call end_test()

  ! ============================================================
  ! Test 7: Right, No-transpose, Lower, C = I_4
  ! ============================================================
  C = 0.0d0
  C(1,1) = 1.0d0; C(2,2) = 1.0d0; C(3,3) = 1.0d0; C(4,4) = 1.0d0
  call DORMTR('R', 'L', 'N', 4, 4, A, NMAX, TAU, C, NMAX, WORK, LWORK, INFO)
  call begin_test('right_notrans_lower')
  call print_int('info', INFO)
  do J = 1, 4
    do I = 1, 4
      Cflat((J-1)*4 + I) = C(I,J)
    end do
  end do
  call print_array('C', Cflat, 16)
  call end_test()

  ! ============================================================
  ! Test 8: Right, Transpose, Lower, C = I_4
  ! ============================================================
  C = 0.0d0
  C(1,1) = 1.0d0; C(2,2) = 1.0d0; C(3,3) = 1.0d0; C(4,4) = 1.0d0
  call DORMTR('R', 'L', 'T', 4, 4, A, NMAX, TAU, C, NMAX, WORK, LWORK, INFO)
  call begin_test('right_trans_lower')
  call print_int('info', INFO)
  do J = 1, 4
    do I = 1, 4
      Cflat((J-1)*4 + I) = C(I,J)
    end do
  end do
  call print_array('C', Cflat, 16)
  call end_test()

  ! ============================================================
  ! Test 9: M=0 quick return
  ! ============================================================
  call DORMTR('L', 'U', 'N', 0, 4, A, 1, TAU, C, 1, WORK, LWORK, INFO)
  call begin_test('m_zero')
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 10: N=0 quick return
  ! ============================================================
  call DORMTR('L', 'U', 'N', 4, 0, A, NMAX, TAU, C, NMAX, WORK, LWORK, INFO)
  call begin_test('n_zero')
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 11: NQ=1 (left with M=1)
  ! ============================================================
  C(1,1) = 7.0d0; C(1,2) = 3.0d0; C(1,3) = -1.0d0; C(1,4) = 2.0d0
  call DORMTR('L', 'U', 'N', 1, 4, A, NMAX, TAU, C, NMAX, WORK, LWORK, INFO)
  call begin_test('nq_one_left')
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 12: Left, No-transpose, Upper, non-identity C (4x2)
  ! ============================================================
  A = Aorig
  call DSYTRD('U', 4, A, NMAX, D, E, TAU, WORK, LWORK, INFO)

  C = 0.0d0
  C(1,1) = 1.0d0; C(2,1) = 2.0d0; C(3,1) = 3.0d0; C(4,1) = 4.0d0
  C(1,2) = 5.0d0; C(2,2) = 6.0d0; C(3,2) = 7.0d0; C(4,2) = 8.0d0
  call DORMTR('L', 'U', 'N', 4, 2, A, NMAX, TAU, C, NMAX, WORK, LWORK, INFO)
  call begin_test('left_notrans_upper_rect')
  call print_int('info', INFO)
  do J = 1, 2
    do I = 1, 4
      Cflat((J-1)*4 + I) = C(I,J)
    end do
  end do
  call print_array('C', Cflat, 8)
  call end_test()

  ! ============================================================
  ! Test 13: Right, No-transpose, Lower, non-identity C (2x4)
  ! ============================================================
  A = Aorig
  call DSYTRD('L', 4, A, NMAX, D, E, TAU, WORK, LWORK, INFO)

  C = 0.0d0
  C(1,1) = 1.0d0; C(2,1) = 2.0d0
  C(1,2) = 3.0d0; C(2,2) = 4.0d0
  C(1,3) = 5.0d0; C(2,3) = 6.0d0
  C(1,4) = 7.0d0; C(2,4) = 8.0d0
  call DORMTR('R', 'L', 'N', 2, 4, A, NMAX, TAU, C, NMAX, WORK, LWORK, INFO)
  call begin_test('right_notrans_lower_rect')
  call print_int('info', INFO)
  do J = 1, 4
    do I = 1, 2
      Cflat((J-1)*2 + I) = C(I,J)
    end do
  end do
  call print_array('C', Cflat, 8)
  call end_test()

end program
