program test_dspgv
  use test_utils
  implicit none
  ! N=3: packed size = 6, Z is 3x3=9
  double precision :: ap(6), bp(6), w(4), z(3,3), work(100)
  integer :: info

  ! A = [4 2 1; 2 5 3; 1 3 6], B = [4 2 0; 2 5 1; 0 1 3]

  ! Test 1: ITYPE=1, JOBZ='V', UPLO='U', N=3
  ! Upper packed: col-major upper triangle
  ! AP = [A(1,1), A(1,2), A(2,2), A(1,3), A(2,3), A(3,3)]
  ap = (/ 4.0d0, 2.0d0, 5.0d0, 1.0d0, 3.0d0, 6.0d0 /)
  bp = (/ 4.0d0, 2.0d0, 5.0d0, 0.0d0, 1.0d0, 3.0d0 /)
  w = 0.0d0; z = 0.0d0
  call dspgv(1, 'V', 'U', 3, ap, bp, w, z, 3, work, info)
  call begin_test('itype1_v_upper')
  call print_int('info', info)
  call print_array('w', w, 3)
  call print_array('AP', ap, 6)
  call print_array('BP', bp, 6)
  call print_matrix('Z', z, 3, 3, 3)
  call end_test()

  ! Test 2: ITYPE=1, JOBZ='V', UPLO='L', N=3
  ! Lower packed: col-major lower triangle
  ! AP = [A(1,1), A(2,1), A(3,1), A(2,2), A(3,2), A(3,3)]
  ap = (/ 4.0d0, 2.0d0, 1.0d0, 5.0d0, 3.0d0, 6.0d0 /)
  bp = (/ 4.0d0, 2.0d0, 0.0d0, 5.0d0, 1.0d0, 3.0d0 /)
  w = 0.0d0; z = 0.0d0
  call dspgv(1, 'V', 'L', 3, ap, bp, w, z, 3, work, info)
  call begin_test('itype1_v_lower')
  call print_int('info', info)
  call print_array('w', w, 3)
  call print_array('AP', ap, 6)
  call print_array('BP', bp, 6)
  call print_matrix('Z', z, 3, 3, 3)
  call end_test()

  ! Test 3: ITYPE=1, JOBZ='N', UPLO='L', N=3 (eigenvalues only)
  ap = (/ 4.0d0, 2.0d0, 1.0d0, 5.0d0, 3.0d0, 6.0d0 /)
  bp = (/ 4.0d0, 2.0d0, 0.0d0, 5.0d0, 1.0d0, 3.0d0 /)
  w = 0.0d0; z = 0.0d0
  call dspgv(1, 'N', 'L', 3, ap, bp, w, z, 1, work, info)
  call begin_test('itype1_n_lower')
  call print_int('info', info)
  call print_array('w', w, 3)
  call end_test()

  ! Test 4: ITYPE=2, JOBZ='V', UPLO='U', N=3
  ap = (/ 4.0d0, 2.0d0, 5.0d0, 1.0d0, 3.0d0, 6.0d0 /)
  bp = (/ 4.0d0, 2.0d0, 5.0d0, 0.0d0, 1.0d0, 3.0d0 /)
  w = 0.0d0; z = 0.0d0
  call dspgv(2, 'V', 'U', 3, ap, bp, w, z, 3, work, info)
  call begin_test('itype2_v_upper')
  call print_int('info', info)
  call print_array('w', w, 3)
  call print_matrix('Z', z, 3, 3, 3)
  call end_test()

  ! Test 5: ITYPE=3, JOBZ='V', UPLO='L', N=3
  ap = (/ 4.0d0, 2.0d0, 1.0d0, 5.0d0, 3.0d0, 6.0d0 /)
  bp = (/ 4.0d0, 2.0d0, 0.0d0, 5.0d0, 1.0d0, 3.0d0 /)
  w = 0.0d0; z = 0.0d0
  call dspgv(3, 'V', 'L', 3, ap, bp, w, z, 3, work, info)
  call begin_test('itype3_v_lower')
  call print_int('info', info)
  call print_array('w', w, 3)
  call print_matrix('Z', z, 3, 3, 3)
  call end_test()

  ! Test 6: N=0 quick return
  call dspgv(1, 'V', 'U', 0, ap, bp, w, z, 1, work, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 7: N=1
  ap(1) = 6.0d0; bp(1) = 2.0d0; w(1) = 0.0d0; z = 0.0d0
  call dspgv(1, 'V', 'U', 1, ap, bp, w, z, 1, work, info)
  call begin_test('n_one')
  call print_int('info', info)
  call print_scalar('w1', w(1))
  call print_scalar('Z1', z(1,1))
  call end_test()

  ! Test 8: Non-positive definite B (should return info = N + k)
  ap(1) = 1.0d0; ap(2) = 0.0d0; ap(3) = 1.0d0
  bp(1) = -1.0d0; bp(2) = 0.0d0; bp(3) = 1.0d0
  call dspgv(1, 'V', 'L', 2, ap, bp, w, z, 2, work, info)
  call begin_test('not_posdef')
  call print_int('info', info)
  call end_test()

  ! Test 9: ITYPE=3, JOBZ='V', UPLO='U', N=3
  ap = (/ 4.0d0, 2.0d0, 5.0d0, 1.0d0, 3.0d0, 6.0d0 /)
  bp = (/ 4.0d0, 2.0d0, 5.0d0, 0.0d0, 1.0d0, 3.0d0 /)
  w = 0.0d0; z = 0.0d0
  call dspgv(3, 'V', 'U', 3, ap, bp, w, z, 3, work, info)
  call begin_test('itype3_v_upper')
  call print_int('info', info)
  call print_array('w', w, 3)
  call print_matrix('Z', z, 3, 3, 3)
  call end_test()

  ! Test 10: ITYPE=2, JOBZ='V', UPLO='L', N=3
  ap = (/ 4.0d0, 2.0d0, 1.0d0, 5.0d0, 3.0d0, 6.0d0 /)
  bp = (/ 4.0d0, 2.0d0, 0.0d0, 5.0d0, 1.0d0, 3.0d0 /)
  w = 0.0d0; z = 0.0d0
  call dspgv(2, 'V', 'L', 3, ap, bp, w, z, 3, work, info)
  call begin_test('itype2_v_lower')
  call print_int('info', info)
  call print_array('w', w, 3)
  call print_matrix('Z', z, 3, 3, 3)
  call end_test()

  ! Test 11: ITYPE=1, JOBZ='N', UPLO='U', N=3 (eigenvalues only, upper)
  ap = (/ 4.0d0, 2.0d0, 5.0d0, 1.0d0, 3.0d0, 6.0d0 /)
  bp = (/ 4.0d0, 2.0d0, 5.0d0, 0.0d0, 1.0d0, 3.0d0 /)
  w = 0.0d0
  call dspgv(1, 'N', 'U', 3, ap, bp, w, z, 1, work, info)
  call begin_test('itype1_n_upper')
  call print_int('info', info)
  call print_array('w', w, 3)
  call end_test()

end program
