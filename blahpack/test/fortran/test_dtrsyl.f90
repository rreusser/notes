program test_dtrsyl
  use test_utils
  implicit none

  double precision :: A(4,4), B(4,4), C(4,4), SCALE
  integer :: INFO

  ! Test 1: NN, M=2, N=2, scalar blocks
  A = 0.0d0; B = 0.0d0; C = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0
  A(2,2) = 3.0d0
  B(1,1) = 2.0d0; B(1,2) = 1.0d0
  B(2,2) = 4.0d0
  C(1,1) = 5.0d0; C(1,2) = 6.0d0
  C(2,1) = 7.0d0; C(2,2) = 8.0d0
  call DTRSYL('N', 'N', 1, 2, 2, A, 4, B, 4, C, 4, SCALE, INFO)
  call begin_test('NN basic 2x2')
  call print_array('C', C, 16)
  call print_scalar('scale', SCALE)
  call print_int('info', INFO)
  call end_test()

  ! Test 2: NN with isgn=-1
  A = 0.0d0; B = 0.0d0; C = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0
  A(2,2) = 3.0d0
  B(1,1) = 2.0d0; B(1,2) = 1.0d0
  B(2,2) = 4.0d0
  C(1,1) = 5.0d0; C(1,2) = 6.0d0
  C(2,1) = 7.0d0; C(2,2) = 8.0d0
  call DTRSYL('N', 'N', -1, 2, 2, A, 4, B, 4, C, 4, SCALE, INFO)
  call begin_test('NN isgn=-1')
  call print_array('C', C, 16)
  call print_scalar('scale', SCALE)
  call print_int('info', INFO)
  call end_test()

  ! Test 3: TN (transpose A)
  A = 0.0d0; B = 0.0d0; C = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0
  A(2,2) = 3.0d0
  B(1,1) = 2.0d0; B(1,2) = 1.0d0
  B(2,2) = 4.0d0
  C(1,1) = 5.0d0; C(1,2) = 6.0d0
  C(2,1) = 7.0d0; C(2,2) = 8.0d0
  call DTRSYL('T', 'N', 1, 2, 2, A, 4, B, 4, C, 4, SCALE, INFO)
  call begin_test('TN basic')
  call print_array('C', C, 16)
  call print_scalar('scale', SCALE)
  call print_int('info', INFO)
  call end_test()

  ! Test 4: TT (transpose both)
  A = 0.0d0; B = 0.0d0; C = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0
  A(2,2) = 3.0d0
  B(1,1) = 2.0d0; B(1,2) = 1.0d0
  B(2,2) = 4.0d0
  C(1,1) = 5.0d0; C(1,2) = 6.0d0
  C(2,1) = 7.0d0; C(2,2) = 8.0d0
  call DTRSYL('T', 'T', 1, 2, 2, A, 4, B, 4, C, 4, SCALE, INFO)
  call begin_test('TT basic')
  call print_array('C', C, 16)
  call print_scalar('scale', SCALE)
  call print_int('info', INFO)
  call end_test()

  ! Test 5: NT (transpose B)
  A = 0.0d0; B = 0.0d0; C = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0
  A(2,2) = 3.0d0
  B(1,1) = 2.0d0; B(1,2) = 1.0d0
  B(2,2) = 4.0d0
  C(1,1) = 5.0d0; C(1,2) = 6.0d0
  C(2,1) = 7.0d0; C(2,2) = 8.0d0
  call DTRSYL('N', 'T', 1, 2, 2, A, 4, B, 4, C, 4, SCALE, INFO)
  call begin_test('NT basic')
  call print_array('C', C, 16)
  call print_scalar('scale', SCALE)
  call print_int('info', INFO)
  call end_test()

  ! Test 6: M=0 quick return
  call DTRSYL('N', 'N', 1, 0, 2, A, 4, B, 4, C, 4, SCALE, INFO)
  call begin_test('M=0')
  call print_scalar('scale', SCALE)
  call print_int('info', INFO)
  call end_test()

  ! Test 7: N=0 quick return
  call DTRSYL('N', 'N', 1, 2, 0, A, 4, B, 4, C, 4, SCALE, INFO)
  call begin_test('N=0')
  call print_scalar('scale', SCALE)
  call print_int('info', INFO)
  call end_test()

  ! Test 8: 3x3 with 2x2 block (quasi-triangular A)
  A = 0.0d0; B = 0.0d0; C = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 0.5d0; A(1,3) = 0.3d0
  A(2,2) = 2.0d0; A(2,3) = 0.4d0
  A(3,2) = -0.5d0; A(3,3) = 2.0d0
  B(1,1) = 3.0d0; B(1,2) = 0.2d0; B(1,3) = 0.1d0
  B(2,2) = 4.0d0; B(2,3) = 0.3d0
  B(3,2) = -0.3d0; B(3,3) = 4.0d0
  C(1,1) = 1.0d0; C(1,2) = 2.0d0; C(1,3) = 3.0d0
  C(2,1) = 4.0d0; C(2,2) = 5.0d0; C(2,3) = 6.0d0
  C(3,1) = 7.0d0; C(3,2) = 8.0d0; C(3,3) = 9.0d0
  call DTRSYL('N', 'N', 1, 3, 3, A, 4, B, 4, C, 4, SCALE, INFO)
  call begin_test('NN 3x3 quasi-tri')
  call print_array('C', C, 16)
  call print_scalar('scale', SCALE)
  call print_int('info', INFO)
  call end_test()

  ! Test 9: M=1, N=1
  A = 0.0d0; B = 0.0d0; C = 0.0d0
  A(1,1) = 2.0d0
  B(1,1) = 3.0d0
  C(1,1) = 10.0d0
  call DTRSYL('N', 'N', 1, 1, 1, A, 4, B, 4, C, 4, SCALE, INFO)
  call begin_test('M=1 N=1')
  call print_array('C', C, 16)
  call print_scalar('scale', SCALE)
  call print_int('info', INFO)
  call end_test()

  ! Test 10: TN 3x3 quasi-triangular
  A = 0.0d0; B = 0.0d0; C = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 0.5d0; A(1,3) = 0.3d0
  A(2,2) = 2.0d0; A(2,3) = 0.4d0
  A(3,2) = -0.5d0; A(3,3) = 2.0d0
  B(1,1) = 3.0d0; B(1,2) = 0.2d0; B(1,3) = 0.1d0
  B(2,2) = 4.0d0; B(2,3) = 0.3d0
  B(3,2) = -0.3d0; B(3,3) = 4.0d0
  C(1,1) = 1.0d0; C(1,2) = 2.0d0; C(1,3) = 3.0d0
  C(2,1) = 4.0d0; C(2,2) = 5.0d0; C(2,3) = 6.0d0
  C(3,1) = 7.0d0; C(3,2) = 8.0d0; C(3,3) = 9.0d0
  call DTRSYL('T', 'N', 1, 3, 3, A, 4, B, 4, C, 4, SCALE, INFO)
  call begin_test('TN 3x3 quasi-tri')
  call print_array('C', C, 16)
  call print_scalar('scale', SCALE)
  call print_int('info', INFO)
  call end_test()

  ! Test 11: TT 3x3 quasi-triangular
  A = 0.0d0; B = 0.0d0; C = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 0.5d0; A(1,3) = 0.3d0
  A(2,2) = 2.0d0; A(2,3) = 0.4d0
  A(3,2) = -0.5d0; A(3,3) = 2.0d0
  B(1,1) = 3.0d0; B(1,2) = 0.2d0; B(1,3) = 0.1d0
  B(2,2) = 4.0d0; B(2,3) = 0.3d0
  B(3,2) = -0.3d0; B(3,3) = 4.0d0
  C(1,1) = 1.0d0; C(1,2) = 2.0d0; C(1,3) = 3.0d0
  C(2,1) = 4.0d0; C(2,2) = 5.0d0; C(2,3) = 6.0d0
  C(3,1) = 7.0d0; C(3,2) = 8.0d0; C(3,3) = 9.0d0
  call DTRSYL('T', 'T', 1, 3, 3, A, 4, B, 4, C, 4, SCALE, INFO)
  call begin_test('TT 3x3 quasi-tri')
  call print_array('C', C, 16)
  call print_scalar('scale', SCALE)
  call print_int('info', INFO)
  call end_test()

  ! Test 12: NT 3x3 quasi-triangular
  A = 0.0d0; B = 0.0d0; C = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 0.5d0; A(1,3) = 0.3d0
  A(2,2) = 2.0d0; A(2,3) = 0.4d0
  A(3,2) = -0.5d0; A(3,3) = 2.0d0
  B(1,1) = 3.0d0; B(1,2) = 0.2d0; B(1,3) = 0.1d0
  B(2,2) = 4.0d0; B(2,3) = 0.3d0
  B(3,2) = -0.3d0; B(3,3) = 4.0d0
  C(1,1) = 1.0d0; C(1,2) = 2.0d0; C(1,3) = 3.0d0
  C(2,1) = 4.0d0; C(2,2) = 5.0d0; C(2,3) = 6.0d0
  C(3,1) = 7.0d0; C(3,2) = 8.0d0; C(3,3) = 9.0d0
  call DTRSYL('N', 'T', 1, 3, 3, A, 4, B, 4, C, 4, SCALE, INFO)
  call begin_test('NT 3x3 quasi-tri')
  call print_array('C', C, 16)
  call print_scalar('scale', SCALE)
  call print_int('info', INFO)
  call end_test()

end program
