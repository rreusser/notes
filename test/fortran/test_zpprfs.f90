program test_zpprfs
  use test_utils
  implicit none

  ! 3x3 arrays
  complex*16 :: AP(6), AFP(6), B(3,2), X(3,2), WORK(6)
  double precision :: AP_r(12), AFP_r(12), B_r(12), X_r(12), WORK_r(12)
  equivalence (AP, AP_r)
  equivalence (AFP, AFP_r)
  equivalence (B, B_r)
  equivalence (X, X_r)
  equivalence (WORK, WORK_r)
  double precision :: FERR(2), BERR(2), RWORK(3)
  integer :: info

  ! N=1 arrays
  complex*16 :: AP1(1), AFP1(1), B1(1,1), X1(1,1), WORK1(2)
  double precision :: AP1_r(2), AFP1_r(2), B1_r(2), X1_r(2), WORK1_r(4)
  equivalence (AP1, AP1_r)
  equivalence (AFP1, AFP1_r)
  equivalence (B1, B1_r)
  equivalence (X1, X1_r)
  equivalence (WORK1, WORK1_r)
  double precision :: FERR1(1), BERR1(1), RWORK1(1)

  ! ============================================================
  ! Test 1: basic 3x3 upper, single RHS
  ! ============================================================
  ! Hermitian positive definite:
  !   A = [10    3-i   1+2i ]
  !       [3+i   8     2-i  ]
  !       [1-2i  2+i   6    ]
  ! Upper packed (col-major upper triangle):
  !   col1: A(1,1)=10
  !   col2: A(1,2)=3-i, A(2,2)=8
  !   col3: A(1,3)=1+2i, A(2,3)=2-i, A(3,3)=6
  AP(1) = (10.0d0, 0.0d0)
  AP(2) = (3.0d0, -1.0d0); AP(3) = (8.0d0, 0.0d0)
  AP(4) = (1.0d0, 2.0d0); AP(5) = (2.0d0, -1.0d0); AP(6) = (6.0d0, 0.0d0)

  B(1,1) = (1.0d0, 1.0d0); B(2,1) = (2.0d0, -1.0d0); B(3,1) = (3.0d0, 0.5d0)

  ! Copy AP to AFP, factorize
  AFP = AP
  call ZPPTRF('U', 3, AFP, info)
  if (info /= 0) stop 'zpptrf failed'

  ! Solve for initial X
  X(:,1) = B(:,1)
  call ZPPTRS('U', 3, 1, AFP, X, 3, info)
  if (info /= 0) stop 'zpptrs failed'

  ! Refine
  call ZPPRFS('U', 3, 1, AP, AFP, B, 3, X, 3, FERR, BERR, WORK, &
              RWORK, info)

  call begin_test('basic_upper_3x3')
  call print_int('info', info)
  call print_array('AP', AP_r, 12)
  call print_array('AFP', AFP_r, 12)
  call print_array('B', B_r, 6)
  call print_array('x', X_r, 6)
  call print_array('ferr', FERR, 1)
  call print_array('berr', BERR, 1)
  call end_test()

  ! ============================================================
  ! Test 2: basic 3x3 lower, single RHS
  ! ============================================================
  ! Lower packed (col-major lower triangle):
  !   col1: A(1,1)=10, A(2,1)=3+i, A(3,1)=1-2i
  !   col2: A(2,2)=8, A(3,2)=2+i
  !   col3: A(3,3)=6
  AP(1) = (10.0d0, 0.0d0); AP(2) = (3.0d0, 1.0d0); AP(3) = (1.0d0, -2.0d0)
  AP(4) = (8.0d0, 0.0d0); AP(5) = (2.0d0, 1.0d0)
  AP(6) = (6.0d0, 0.0d0)

  B(1,1) = (1.0d0, 1.0d0); B(2,1) = (2.0d0, -1.0d0); B(3,1) = (3.0d0, 0.5d0)

  AFP = AP
  call ZPPTRF('L', 3, AFP, info)
  if (info /= 0) stop 'zpptrf failed'

  X(:,1) = B(:,1)
  call ZPPTRS('L', 3, 1, AFP, X, 3, info)
  if (info /= 0) stop 'zpptrs failed'

  call ZPPRFS('L', 3, 1, AP, AFP, B, 3, X, 3, FERR, BERR, WORK, &
              RWORK, info)

  call begin_test('basic_lower_3x3')
  call print_int('info', info)
  call print_array('AP', AP_r, 12)
  call print_array('AFP', AFP_r, 12)
  call print_array('B', B_r, 6)
  call print_array('x', X_r, 6)
  call print_array('ferr', FERR, 1)
  call print_array('berr', BERR, 1)
  call end_test()

  ! ============================================================
  ! Test 3: multiple RHS (3x3, 2 columns), upper
  ! ============================================================
  AP(1) = (10.0d0, 0.0d0)
  AP(2) = (3.0d0, -1.0d0); AP(3) = (8.0d0, 0.0d0)
  AP(4) = (1.0d0, 2.0d0); AP(5) = (2.0d0, -1.0d0); AP(6) = (6.0d0, 0.0d0)

  B(1,1) = (1.0d0, 2.0d0); B(2,1) = (3.0d0, -1.0d0); B(3,1) = (2.0d0, 1.0d0)
  B(1,2) = (4.0d0, 0.0d0); B(2,2) = (5.0d0, -2.0d0); B(3,2) = (6.0d0, 3.0d0)

  AFP = AP
  call ZPPTRF('U', 3, AFP, info)
  if (info /= 0) stop 'zpptrf failed'

  X = B
  call ZPPTRS('U', 3, 2, AFP, X, 3, info)
  if (info /= 0) stop 'zpptrs failed'

  call ZPPRFS('U', 3, 2, AP, AFP, B, 3, X, 3, FERR, BERR, WORK, &
              RWORK, info)

  call begin_test('multi_rhs_3x3')
  call print_int('info', info)
  call print_array('AP', AP_r, 12)
  call print_array('AFP', AFP_r, 12)
  call print_array('B', B_r, 12)
  call print_array('x', X_r, 12)
  call print_array('ferr', FERR, 2)
  call print_array('berr', BERR, 2)
  call end_test()

  ! ============================================================
  ! Test 4: N=0 quick return
  ! ============================================================
  FERR(1) = -1.0d0; BERR(1) = -1.0d0
  call ZPPRFS('U', 0, 1, AP, AFP, B, 1, X, 1, FERR, BERR, WORK, &
              RWORK, info)

  call begin_test('n_zero')
  call print_int('info', info)
  call print_array('ferr', FERR, 1)
  call print_array('berr', BERR, 1)
  call end_test()

  ! ============================================================
  ! Test 5: NRHS=0 quick return
  ! ============================================================
  call ZPPRFS('U', 3, 0, AP, AFP, B, 3, X, 3, FERR, BERR, WORK, &
              RWORK, info)

  call begin_test('nrhs_zero')
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 6: N=1 scalar case
  ! ============================================================
  AP1(1) = (4.0d0, 0.0d0)
  AFP1 = AP1
  call ZPPTRF('U', 1, AFP1, info)
  if (info /= 0) stop 'zpptrf failed for 1x1'

  B1(1,1) = (2.0d0, 3.0d0)
  X1(1,1) = B1(1,1)
  call ZPPTRS('U', 1, 1, AFP1, X1, 1, info)
  if (info /= 0) stop 'zpptrs failed for 1x1'

  call ZPPRFS('U', 1, 1, AP1, AFP1, B1, 1, X1, 1, FERR1, BERR1, &
              WORK1, RWORK1, info)

  call begin_test('n_one')
  call print_int('info', info)
  call print_array('AP', AP1_r, 2)
  call print_array('AFP', AFP1_r, 2)
  call print_array('B', B1_r, 2)
  call print_array('x', X1_r, 2)
  call print_array('ferr', FERR1, 1)
  call print_array('berr', BERR1, 1)
  call end_test()

end program
