program test_dpprfs
  use test_utils
  implicit none

  ! Shared variables for 3x3
  integer :: info, i
  double precision :: AP(6), AFP(6), B(3,2), X(3,2)
  double precision :: FERR(2), BERR(2), WORK(9)
  integer :: IWORK(3)

  ! Large test 5x5
  integer, parameter :: NL = 5
  double precision :: APL(15), AFPL(15), BL(NL,1), XL(NL,1)
  double precision :: FERRL(1), BERRL(1), WORKL(3*NL)
  integer :: IWORKL(NL)

  ! N=1 test
  double precision :: AP1(1), AFP1(1), B1(1,1), X1(1,1)
  double precision :: FERR1(1), BERR1(1), WORK1(3)
  integer :: IWORK1(1)

  ! ============================================================
  ! Test 1: basic 3x3 upper, single RHS
  ! ============================================================
  ! A = symmetric positive definite:
  !   [ 4  2  1 ]
  !   [ 2  5  3 ]
  !   [ 1  3  6 ]
  ! Upper packed: A(1,1), A(1,2), A(2,2), A(1,3), A(2,3), A(3,3)
  AP(1) = 4.0d0; AP(2) = 2.0d0; AP(3) = 5.0d0
  AP(4) = 1.0d0; AP(5) = 3.0d0; AP(6) = 6.0d0

  B(1,1) = 1.0d0; B(2,1) = 1.0d0; B(3,1) = 1.0d0

  ! Copy AP to AFP, factorize
  AFP = AP
  call DPPTRF('U', 3, AFP, info)
  if (info /= 0) stop 'dpptrf failed'

  ! Solve for initial X
  X(:,1) = B(:,1)
  call DPPTRS('U', 3, 1, AFP, X, 3, info)
  if (info /= 0) stop 'dpptrs failed'

  ! Refine
  call DPPRFS('U', 3, 1, AP, AFP, B, 3, X, 3, FERR, BERR, WORK, &
              IWORK, info)

  call begin_test('basic_upper_3x3')
  call print_int('info', info)
  call print_array('x', X(1:3,1), 3)
  call print_array('ferr', FERR, 1)
  call print_array('berr', BERR, 1)
  call end_test()

  ! ============================================================
  ! Test 2: basic 3x3 lower, single RHS
  ! ============================================================
  ! Lower packed: A(1,1), A(2,1), A(3,1), A(2,2), A(3,2), A(3,3)
  AP(1) = 4.0d0; AP(2) = 2.0d0; AP(3) = 1.0d0
  AP(4) = 5.0d0; AP(5) = 3.0d0; AP(6) = 6.0d0

  B(1,1) = 1.0d0; B(2,1) = 1.0d0; B(3,1) = 1.0d0

  AFP = AP
  call DPPTRF('L', 3, AFP, info)
  if (info /= 0) stop 'dpptrf failed'

  X(:,1) = B(:,1)
  call DPPTRS('L', 3, 1, AFP, X, 3, info)
  if (info /= 0) stop 'dpptrs failed'

  call DPPRFS('L', 3, 1, AP, AFP, B, 3, X, 3, FERR, BERR, WORK, &
              IWORK, info)

  call begin_test('basic_lower_3x3')
  call print_int('info', info)
  call print_array('x', X(1:3,1), 3)
  call print_array('ferr', FERR, 1)
  call print_array('berr', BERR, 1)
  call end_test()

  ! ============================================================
  ! Test 3: multiple RHS (3x3, 2 columns), upper
  ! ============================================================
  AP(1) = 4.0d0; AP(2) = 2.0d0; AP(3) = 5.0d0
  AP(4) = 1.0d0; AP(5) = 3.0d0; AP(6) = 6.0d0

  B(1,1) = 1.0d0; B(2,1) = 2.0d0; B(3,1) = 3.0d0
  B(1,2) = 4.0d0; B(2,2) = 5.0d0; B(3,2) = 6.0d0

  AFP = AP
  call DPPTRF('U', 3, AFP, info)
  if (info /= 0) stop 'dpptrf failed'

  X = B
  call DPPTRS('U', 3, 2, AFP, X, 3, info)
  if (info /= 0) stop 'dpptrs failed'

  call DPPRFS('U', 3, 2, AP, AFP, B, 3, X, 3, FERR, BERR, WORK, &
              IWORK, info)

  call begin_test('multi_rhs_3x3')
  call print_int('info', info)
  call print_array('x', X, 6)
  call print_array('ferr', FERR, 2)
  call print_array('berr', BERR, 2)
  call end_test()

  ! ============================================================
  ! Test 4: N=0 quick return
  ! ============================================================
  call DPPRFS('U', 0, 1, AP, AFP, B, 1, X, 1, FERR, BERR, WORK, &
              IWORK, info)

  call begin_test('n_zero')
  call print_int('info', info)
  call print_array('ferr', FERR, 1)
  call print_array('berr', BERR, 1)
  call end_test()

  ! ============================================================
  ! Test 5: NRHS=0 quick return
  ! ============================================================
  call DPPRFS('U', 3, 0, AP, AFP, B, 3, X, 3, FERR, BERR, WORK, &
              IWORK, info)

  call begin_test('nrhs_zero')
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 6: N=1 scalar case
  ! ============================================================
  AP1(1) = 4.0d0
  AFP1 = AP1
  call DPPTRF('U', 1, AFP1, info)
  if (info /= 0) stop 'dpptrf failed for 1x1'

  B1(1,1) = 2.0d0
  X1(1,1) = B1(1,1)
  call DPPTRS('U', 1, 1, AFP1, X1, 1, info)
  if (info /= 0) stop 'dpptrs failed for 1x1'

  call DPPRFS('U', 1, 1, AP1, AFP1, B1, 1, X1, 1, FERR1, BERR1, &
              WORK1, IWORK1, info)

  call begin_test('n_one')
  call print_int('info', info)
  call print_array('x', X1(1:1,1), 1)
  call print_array('ferr', FERR1, 1)
  call print_array('berr', BERR1, 1)
  call end_test()

  ! ============================================================
  ! Test 7: 5x5 upper, ill-conditioned
  ! ============================================================
  ! Upper packed storage for 5x5
  ! Row indices: (1,1),(1,2),(2,2),(1,3),(2,3),(3,3),(1,4),(2,4),(3,4),(4,4),(1,5),(2,5),(3,5),(4,5),(5,5)
  APL(1)  = 100.0d0  ! (1,1)
  APL(2)  = 10.0d0   ! (1,2)
  APL(3)  = 100.0d0  ! (2,2)
  APL(4)  = 1.0d0    ! (1,3)
  APL(5)  = 10.0d0   ! (2,3)
  APL(6)  = 100.0d0  ! (3,3)
  APL(7)  = 0.1d0    ! (1,4)
  APL(8)  = 1.0d0    ! (2,4)
  APL(9)  = 10.0d0   ! (3,4)
  APL(10) = 100.0d0  ! (4,4)
  APL(11) = 0.01d0   ! (1,5)
  APL(12) = 0.1d0    ! (2,5)
  APL(13) = 1.0d0    ! (3,5)
  APL(14) = 10.0d0   ! (4,5)
  APL(15) = 100.0d0  ! (5,5)

  BL(1,1) = 1.0d0; BL(2,1) = 2.0d0; BL(3,1) = 3.0d0; BL(4,1) = 4.0d0; BL(5,1) = 5.0d0

  AFPL = APL
  call DPPTRF('U', NL, AFPL, info)
  if (info /= 0) stop 'dpptrf failed for 5x5'

  XL(:,1) = BL(:,1)
  call DPPTRS('U', NL, 1, AFPL, XL, NL, info)
  if (info /= 0) stop 'dpptrs failed for 5x5'

  call DPPRFS('U', NL, 1, APL, AFPL, BL, NL, XL, NL, FERRL, BERRL, &
              WORKL, IWORKL, info)

  call begin_test('illcond_5x5')
  call print_int('info', info)
  call print_array('x', XL(1:NL,1), NL)
  call print_array('ferr', FERRL, 1)
  call print_array('berr', BERRL, 1)
  call end_test()

end program
