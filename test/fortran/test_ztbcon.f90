program test_ztbcon
  use test_utils
  implicit none

  ! Arrays sized for different LDAB values
  complex*16 :: ab3(3, 6), ab2(2, 6), ab1(1, 6)
  complex*16 :: work(12)
  double precision :: ab3_r(2*3*6), ab2_r(2*2*6), ab1_r(2*1*6)
  double precision :: work_r(24)
  equivalence (ab3, ab3_r)
  equivalence (ab2, ab2_r)
  equivalence (ab1, ab1_r)
  equivalence (work, work_r)
  double precision :: rwork(6), rcond
  integer :: info

  ! ---------------------------------------------------------------
  ! Test 1: 4x4 upper triangular band, KD=2, non-unit, 1-norm
  !
  ! Full matrix A:
  !   A = [ (4+i)   (1+i)   (0.5+0i)  0     ]
  !       [   0     (3+0i)  (1-i)    (0.5+i) ]
  !       [   0       0     (2+i)    (1+0i)  ]
  !       [   0       0       0      (3-i)   ]
  !
  ! Band storage (upper, LDAB=3, KD=2):
  !   Row 1 (2nd superdiag): *        *       (0.5+0i) (0.5+i)
  !   Row 2 (1st superdiag): *       (1+i)   (1-i)    (1+0i)
  !   Row 3 (diagonal):      (4+i)   (3+0i)  (2+i)    (3-i)
  ! ---------------------------------------------------------------
  ab3 = (0.0d0, 0.0d0)

  ! Column 1: A(1,1) -> AB(3,1)
  ab3(3,1) = (4.0d0, 1.0d0)

  ! Column 2: A(1,2) -> AB(2,2), A(2,2) -> AB(3,2)
  ab3(2,2) = (1.0d0, 1.0d0)
  ab3(3,2) = (3.0d0, 0.0d0)

  ! Column 3: A(1,3) -> AB(1,3), A(2,3) -> AB(2,3), A(3,3) -> AB(3,3)
  ab3(1,3) = (0.5d0, 0.0d0)
  ab3(2,3) = (1.0d0, -1.0d0)
  ab3(3,3) = (2.0d0, 1.0d0)

  ! Column 4: A(2,4) -> AB(1,4), A(3,4) -> AB(2,4), A(4,4) -> AB(3,4)
  ab3(1,4) = (0.5d0, 1.0d0)
  ab3(2,4) = (1.0d0, 0.0d0)
  ab3(3,4) = (3.0d0, -1.0d0)

  call ztbcon('1', 'U', 'N', 4, 2, ab3, 3, rcond, work, rwork, info)
  call begin_test('upper_nonunit_1norm_k2')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 2: same matrix, inf-norm
  ! ---------------------------------------------------------------
  call ztbcon('I', 'U', 'N', 4, 2, ab3, 3, rcond, work, rwork, info)
  call begin_test('upper_nonunit_Inorm_k2')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 3: 4x4 lower triangular band, KD=2, non-unit, 1-norm
  !
  ! Full matrix A:
  !   A = [ (3+i)      0         0       0     ]
  !       [ (1+0i)   (4-i)       0       0     ]
  !       [ (0.5+i)  (1-i)     (2+0i)    0     ]
  !       [   0      (0.5+0i)  (1+i)   (3+i)   ]
  !
  ! Band storage (lower, LDAB=3, KD=2):
  !   Row 1 (diag):       (3+i)    (4-i)     (2+0i)   (3+i)
  !   Row 2 (1st subdiag): (1+0i)   (1-i)     (1+i)      *
  !   Row 3 (2nd subdiag): (0.5+i)  (0.5+0i)    *        *
  ! ---------------------------------------------------------------
  ab3 = (0.0d0, 0.0d0)

  ! Column 1
  ab3(1,1) = (3.0d0, 1.0d0)
  ab3(2,1) = (1.0d0, 0.0d0)
  ab3(3,1) = (0.5d0, 1.0d0)

  ! Column 2
  ab3(1,2) = (4.0d0, -1.0d0)
  ab3(2,2) = (1.0d0, -1.0d0)
  ab3(3,2) = (0.5d0, 0.0d0)

  ! Column 3
  ab3(1,3) = (2.0d0, 0.0d0)
  ab3(2,3) = (1.0d0, 1.0d0)

  ! Column 4
  ab3(1,4) = (3.0d0, 1.0d0)

  call ztbcon('1', 'L', 'N', 4, 2, ab3, 3, rcond, work, rwork, info)
  call begin_test('lower_nonunit_1norm_k2')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 4: same lower matrix, inf-norm
  ! ---------------------------------------------------------------
  call ztbcon('I', 'L', 'N', 4, 2, ab3, 3, rcond, work, rwork, info)
  call begin_test('lower_nonunit_Inorm_k2')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 5: 3x3 upper triangular band, KD=1, unit diagonal, 1-norm
  !
  ! Full matrix A (unit diag):
  !   A = [ 1       (1+i)    0    ]
  !       [ 0        1      (1-i) ]
  !       [ 0        0       1    ]
  !
  ! Band storage (upper, LDAB=2, KD=1):
  !   Row 1 (superdiag): *       (1+i)   (1-i)
  !   Row 2 (diagonal):  diag    diag    diag
  ! ---------------------------------------------------------------
  ab2 = (0.0d0, 0.0d0)
  ab2(1,2) = (1.0d0, 1.0d0)
  ab2(1,3) = (1.0d0, -1.0d0)
  ! diagonal not referenced for unit
  ab2(2,1) = (99.0d0, 99.0d0)
  ab2(2,2) = (99.0d0, 99.0d0)
  ab2(2,3) = (99.0d0, 99.0d0)

  call ztbcon('1', 'U', 'U', 3, 1, ab2, 2, rcond, work, rwork, info)
  call begin_test('upper_unit_1norm_k1')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 6: 3x3 lower triangular band, KD=1, unit diagonal, inf-norm
  !
  ! Full matrix A (unit diag):
  !   A = [ 1        0       0   ]
  !       [ (0.5+0.5i) 1     0   ]
  !       [ 0       (0.5-0.5i) 1 ]
  !
  ! Band storage (lower, LDAB=2, KD=1):
  !   Row 1 (diag):       diag         diag         diag
  !   Row 2 (subdiag):    (0.5+0.5i)   (0.5-0.5i)   *
  ! ---------------------------------------------------------------
  ab2 = (0.0d0, 0.0d0)
  ab2(1,1) = (99.0d0, 99.0d0)
  ab2(2,1) = (0.5d0, 0.5d0)
  ab2(1,2) = (99.0d0, 99.0d0)
  ab2(2,2) = (0.5d0, -0.5d0)
  ab2(1,3) = (99.0d0, 99.0d0)

  call ztbcon('I', 'L', 'U', 3, 1, ab2, 2, rcond, work, rwork, info)
  call begin_test('lower_unit_Inorm_k1')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 7: N=0 quick return (rcond = 1)
  ! ---------------------------------------------------------------
  call ztbcon('1', 'U', 'N', 0, 0, ab1, 1, rcond, work, rwork, info)
  call begin_test('n_zero')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 8: 3x3 identity band matrix (KD=0, diag only), rcond=1
  !
  ! Band storage (LDAB=1):
  !   Row 1 (diagonal): (1,0) (1,0) (1,0)
  ! ---------------------------------------------------------------
  ab1 = (0.0d0, 0.0d0)
  ab1(1,1) = (1.0d0, 0.0d0)
  ab1(1,2) = (1.0d0, 0.0d0)
  ab1(1,3) = (1.0d0, 0.0d0)

  call ztbcon('1', 'U', 'N', 3, 0, ab1, 1, rcond, work, rwork, info)
  call begin_test('identity_k0')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 9: 4x4 upper band, KD=1, non-unit, 1-norm
  !
  ! Full matrix A:
  !   A = [ (2+i)  (1+0i)    0       0     ]
  !       [   0    (3-i)   (0.5+i)   0     ]
  !       [   0      0     (1+2i)  (1-i)   ]
  !       [   0      0       0     (4+0i)  ]
  !
  ! Band storage (upper, LDAB=2, KD=1):
  !   Row 1 (superdiag): *       (1+0i)  (0.5+i) (1-i)
  !   Row 2 (diagonal):  (2+i)   (3-i)   (1+2i)  (4+0i)
  ! ---------------------------------------------------------------
  ab2 = (0.0d0, 0.0d0)
  ab2(2,1) = (2.0d0, 1.0d0)
  ab2(1,2) = (1.0d0, 0.0d0)
  ab2(2,2) = (3.0d0, -1.0d0)
  ab2(1,3) = (0.5d0, 1.0d0)
  ab2(2,3) = (1.0d0, 2.0d0)
  ab2(1,4) = (1.0d0, -1.0d0)
  ab2(2,4) = (4.0d0, 0.0d0)

  call ztbcon('1', 'U', 'N', 4, 1, ab2, 2, rcond, work, rwork, info)
  call begin_test('upper_nonunit_k1_1norm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 10: same KD=1 upper, inf-norm
  ! ---------------------------------------------------------------
  call ztbcon('I', 'U', 'N', 4, 1, ab2, 2, rcond, work, rwork, info)
  call begin_test('upper_nonunit_k1_Inorm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 11: 5x5 lower band, KD=1, non-unit, 1-norm
  !
  ! Full matrix A:
  !   A = [ (2+i)     0        0        0        0     ]
  !       [ (1+i)   (3+0i)     0        0        0     ]
  !       [   0     (0.5-i)  (4+i)      0        0     ]
  !       [   0       0      (1+0i)   (2-i)      0     ]
  !       [   0       0        0      (0.5+i)  (5+0i)  ]
  !
  ! Band storage (lower, LDAB=2, KD=1):
  !   Row 1 (diag):    (2+i)   (3+0i)  (4+i)   (2-i)   (5+0i)
  !   Row 2 (subdiag): (1+i)   (0.5-i) (1+0i)  (0.5+i) *
  ! ---------------------------------------------------------------
  ab2 = (0.0d0, 0.0d0)
  ab2(1,1) = (2.0d0, 1.0d0)
  ab2(2,1) = (1.0d0, 1.0d0)
  ab2(1,2) = (3.0d0, 0.0d0)
  ab2(2,2) = (0.5d0, -1.0d0)
  ab2(1,3) = (4.0d0, 1.0d0)
  ab2(2,3) = (1.0d0, 0.0d0)
  ab2(1,4) = (2.0d0, -1.0d0)
  ab2(2,4) = (0.5d0, 1.0d0)
  ab2(1,5) = (5.0d0, 0.0d0)

  call ztbcon('1', 'L', 'N', 5, 1, ab2, 2, rcond, work, rwork, info)
  call begin_test('lower_nonunit_k1_1norm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

  ! ---------------------------------------------------------------
  ! Test 12: same 5x5 lower, inf-norm
  ! ---------------------------------------------------------------
  call ztbcon('I', 'L', 'N', 5, 1, ab2, 2, rcond, work, rwork, info)
  call begin_test('lower_nonunit_k1_Inorm')
  call print_scalar('rcond', rcond)
  call print_int('info', info)
  call end_test()

end program
