program test_dopmtr
  use test_utils
  implicit none

  integer, parameter :: NMAX = 4
  double precision :: AP(NMAX*(NMAX+1)/2), APsave(NMAX*(NMAX+1)/2)
  double precision :: C(NMAX, NMAX)
  double precision :: TAU(NMAX), D(NMAX), E(NMAX), WORK(256)
  double precision :: Cflat(NMAX*NMAX)
  double precision :: APflat(NMAX*(NMAX+1)/2)
  integer :: INFO, I, J

  ! ============================================================
  ! Setup: 4x4 symmetric matrix in upper-packed storage
  ! Original full matrix:
  !   4  1 -2  2
  !   1  2  0  1
  !  -2  0  3 -2
  !   2  1 -2 -1
  !
  ! Upper packed (column-major): col1=[4], col2=[1,2], col3=[-2,0,3], col4=[2,1,-2,-1]
  ! AP = [4, 1, 2, -2, 0, 3, 2, 1, -2, -1]
  ! ============================================================
  AP(1) = 4.0d0
  AP(2) = 1.0d0
  AP(3) = 2.0d0
  AP(4) = -2.0d0
  AP(5) = 0.0d0
  AP(6) = 3.0d0
  AP(7) = 2.0d0
  AP(8) = 1.0d0
  AP(9) = -2.0d0
  AP(10) = -1.0d0
  APsave = AP

  call DSPTRD('U', 4, AP, D, E, TAU, INFO)

  ! Output AP and TAU after upper reduction
  call begin_test('setup_upper')
  call print_array('AP', AP, 10)
  call print_array('TAU', TAU, 3)
  call end_test()

  ! Save AP after reduction for reuse
  APsave = AP

  ! ============================================================
  ! Test 1: Left, No-transpose, Upper, C = I_4
  ! ============================================================
  AP = APsave
  C = 0.0d0
  C(1,1) = 1.0d0; C(2,2) = 1.0d0; C(3,3) = 1.0d0; C(4,4) = 1.0d0
  call DOPMTR('L', 'U', 'N', 4, 4, AP, TAU, C, NMAX, WORK, INFO)
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
  AP = APsave
  C = 0.0d0
  C(1,1) = 1.0d0; C(2,2) = 1.0d0; C(3,3) = 1.0d0; C(4,4) = 1.0d0
  call DOPMTR('L', 'U', 'T', 4, 4, AP, TAU, C, NMAX, WORK, INFO)
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
  AP = APsave
  C = 0.0d0
  C(1,1) = 1.0d0; C(2,2) = 1.0d0; C(3,3) = 1.0d0; C(4,4) = 1.0d0
  call DOPMTR('R', 'U', 'N', 4, 4, AP, TAU, C, NMAX, WORK, INFO)
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
  AP = APsave
  C = 0.0d0
  C(1,1) = 1.0d0; C(2,2) = 1.0d0; C(3,3) = 1.0d0; C(4,4) = 1.0d0
  call DOPMTR('R', 'U', 'T', 4, 4, AP, TAU, C, NMAX, WORK, INFO)
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
  ! Setup: same matrix, reduce with lower-packed storage
  ! Lower packed (column-major): col1=[4,1,-2,2], col2=[2,0,1], col3=[3,-2], col4=[-1]
  ! AP = [4, 1, -2, 2, 2, 0, 1, 3, -2, -1]
  ! ============================================================
  AP(1) = 4.0d0
  AP(2) = 1.0d0
  AP(3) = -2.0d0
  AP(4) = 2.0d0
  AP(5) = 2.0d0
  AP(6) = 0.0d0
  AP(7) = 1.0d0
  AP(8) = 3.0d0
  AP(9) = -2.0d0
  AP(10) = -1.0d0

  call DSPTRD('L', 4, AP, D, E, TAU, INFO)

  ! Output AP and TAU after lower reduction
  call begin_test('setup_lower')
  call print_array('AP', AP, 10)
  call print_array('TAU', TAU, 3)
  call end_test()

  APsave = AP

  ! ============================================================
  ! Test 5: Left, No-transpose, Lower, C = I_4
  ! ============================================================
  AP = APsave
  C = 0.0d0
  C(1,1) = 1.0d0; C(2,2) = 1.0d0; C(3,3) = 1.0d0; C(4,4) = 1.0d0
  call DOPMTR('L', 'L', 'N', 4, 4, AP, TAU, C, NMAX, WORK, INFO)
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
  AP = APsave
  C = 0.0d0
  C(1,1) = 1.0d0; C(2,2) = 1.0d0; C(3,3) = 1.0d0; C(4,4) = 1.0d0
  call DOPMTR('L', 'L', 'T', 4, 4, AP, TAU, C, NMAX, WORK, INFO)
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
  AP = APsave
  C = 0.0d0
  C(1,1) = 1.0d0; C(2,2) = 1.0d0; C(3,3) = 1.0d0; C(4,4) = 1.0d0
  call DOPMTR('R', 'L', 'N', 4, 4, AP, TAU, C, NMAX, WORK, INFO)
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
  AP = APsave
  C = 0.0d0
  C(1,1) = 1.0d0; C(2,2) = 1.0d0; C(3,3) = 1.0d0; C(4,4) = 1.0d0
  call DOPMTR('R', 'L', 'T', 4, 4, AP, TAU, C, NMAX, WORK, INFO)
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
  call DOPMTR('L', 'U', 'N', 0, 4, AP, TAU, C, 1, WORK, INFO)
  call begin_test('m_zero')
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 10: N=0 quick return
  ! ============================================================
  call DOPMTR('L', 'U', 'N', 4, 0, AP, TAU, C, NMAX, WORK, INFO)
  call begin_test('n_zero')
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 11: Left, No-transpose, Upper, non-identity C (4x2)
  ! ============================================================
  ! Re-reduce with upper
  AP(1) = 4.0d0
  AP(2) = 1.0d0
  AP(3) = 2.0d0
  AP(4) = -2.0d0
  AP(5) = 0.0d0
  AP(6) = 3.0d0
  AP(7) = 2.0d0
  AP(8) = 1.0d0
  AP(9) = -2.0d0
  AP(10) = -1.0d0

  call DSPTRD('U', 4, AP, D, E, TAU, INFO)
  APsave = AP

  AP = APsave
  C = 0.0d0
  C(1,1) = 1.0d0; C(2,1) = 2.0d0; C(3,1) = 3.0d0; C(4,1) = 4.0d0
  C(1,2) = 5.0d0; C(2,2) = 6.0d0; C(3,2) = 7.0d0; C(4,2) = 8.0d0
  call DOPMTR('L', 'U', 'N', 4, 2, AP, TAU, C, NMAX, WORK, INFO)
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
  ! Test 12: Right, No-transpose, Lower, non-identity C (2x4)
  ! ============================================================
  AP(1) = 4.0d0
  AP(2) = 1.0d0
  AP(3) = -2.0d0
  AP(4) = 2.0d0
  AP(5) = 2.0d0
  AP(6) = 0.0d0
  AP(7) = 1.0d0
  AP(8) = 3.0d0
  AP(9) = -2.0d0
  AP(10) = -1.0d0

  call DSPTRD('L', 4, AP, D, E, TAU, INFO)
  APsave = AP

  AP = APsave
  C = 0.0d0
  C(1,1) = 1.0d0; C(2,1) = 2.0d0
  C(1,2) = 3.0d0; C(2,2) = 4.0d0
  C(1,3) = 5.0d0; C(2,3) = 6.0d0
  C(1,4) = 7.0d0; C(2,4) = 8.0d0
  call DOPMTR('R', 'L', 'N', 2, 4, AP, TAU, C, NMAX, WORK, INFO)
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
