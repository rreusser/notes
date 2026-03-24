program test_dporfs
  use test_utils
  implicit none

  ! Shared variables
  integer :: info, i
  double precision :: A(3,3), AF(3,3), B(3,2), X(3,2)
  double precision :: FERR(2), BERR(2), WORK(9)
  integer :: IWORK(3)

  ! Large test
  integer, parameter :: NL = 5
  double precision :: AL(NL,NL), AFL(NL,NL), BL(NL,1), XL(NL,1)
  double precision :: FERRL(1), BERRL(1), WORKL(3*NL)
  integer :: IWORKL(NL)

  ! ============================================================
  ! Test 1: basic 3x3 upper, single RHS
  ! ============================================================
  ! A = symmetric positive definite (column-major):
  !   [ 4  2  1 ]
  !   [ 2  5  3 ]
  !   [ 1  3  6 ]
  A(1,1) = 4.0d0; A(2,1) = 2.0d0; A(3,1) = 1.0d0
  A(1,2) = 2.0d0; A(2,2) = 5.0d0; A(3,2) = 3.0d0
  A(1,3) = 1.0d0; A(2,3) = 3.0d0; A(3,3) = 6.0d0

  ! B = [1; 1; 1]
  B(1,1) = 1.0d0; B(2,1) = 1.0d0; B(3,1) = 1.0d0

  ! Copy A to AF, factorize
  AF = A
  call DPOTRF('U', 3, AF, 3, info)
  if (info /= 0) stop 'dpotrf failed'

  ! Solve for initial X
  X(:,1) = B(:,1)
  call DPOTRS('U', 3, 1, AF, 3, X, 3, info)
  if (info /= 0) stop 'dpotrs failed'

  ! Refine
  call DPORFS('U', 3, 1, A, 3, AF, 3, B, 3, X, 3, FERR, BERR, WORK, &
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
  A(1,1) = 4.0d0; A(2,1) = 2.0d0; A(3,1) = 1.0d0
  A(1,2) = 2.0d0; A(2,2) = 5.0d0; A(3,2) = 3.0d0
  A(1,3) = 1.0d0; A(2,3) = 3.0d0; A(3,3) = 6.0d0

  B(1,1) = 1.0d0; B(2,1) = 1.0d0; B(3,1) = 1.0d0

  AF = A
  call DPOTRF('L', 3, AF, 3, info)
  if (info /= 0) stop 'dpotrf failed'

  X(:,1) = B(:,1)
  call DPOTRS('L', 3, 1, AF, 3, X, 3, info)
  if (info /= 0) stop 'dpotrs failed'

  call DPORFS('L', 3, 1, A, 3, AF, 3, B, 3, X, 3, FERR, BERR, WORK, &
              IWORK, info)

  call begin_test('basic_lower_3x3')
  call print_int('info', info)
  call print_array('x', X(1:3,1), 3)
  call print_array('ferr', FERR, 1)
  call print_array('berr', BERR, 1)
  call end_test()

  ! ============================================================
  ! Test 3: multiple RHS (3x3, 2 columns)
  ! ============================================================
  A(1,1) = 4.0d0; A(2,1) = 2.0d0; A(3,1) = 1.0d0
  A(1,2) = 2.0d0; A(2,2) = 5.0d0; A(3,2) = 3.0d0
  A(1,3) = 1.0d0; A(2,3) = 3.0d0; A(3,3) = 6.0d0

  B(1,1) = 1.0d0; B(2,1) = 2.0d0; B(3,1) = 3.0d0
  B(1,2) = 4.0d0; B(2,2) = 5.0d0; B(3,2) = 6.0d0

  AF = A
  call DPOTRF('U', 3, AF, 3, info)
  if (info /= 0) stop 'dpotrf failed'

  X = B
  call DPOTRS('U', 3, 2, AF, 3, X, 3, info)
  if (info /= 0) stop 'dpotrs failed'

  call DPORFS('U', 3, 2, A, 3, AF, 3, B, 3, X, 3, FERR, BERR, WORK, &
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
  call DPORFS('U', 0, 1, A, 1, AF, 1, B, 1, X, 1, FERR, BERR, WORK, &
              IWORK, info)

  call begin_test('n_zero')
  call print_int('info', info)
  call print_array('ferr', FERR, 1)
  call print_array('berr', BERR, 1)
  call end_test()

  ! ============================================================
  ! Test 5: NRHS=0 quick return
  ! ============================================================
  call DPORFS('U', 3, 0, A, 3, AF, 3, B, 3, X, 3, FERR, BERR, WORK, &
              IWORK, info)

  call begin_test('nrhs_zero')
  call print_int('info', info)
  call end_test()

  ! ============================================================
  ! Test 6: 5x5 ill-conditioned upper
  ! ============================================================
  ! Diagonally dominant but with large off-diag to create some
  ! conditioning issues
  AL = 0.0d0
  AL(1,1) = 100.0d0; AL(1,2) = 10.0d0; AL(1,3) = 1.0d0; AL(1,4) = 0.1d0; AL(1,5) = 0.01d0
  AL(2,1) = 10.0d0;  AL(2,2) = 100.0d0; AL(2,3) = 10.0d0; AL(2,4) = 1.0d0; AL(2,5) = 0.1d0
  AL(3,1) = 1.0d0;   AL(3,2) = 10.0d0; AL(3,3) = 100.0d0; AL(3,4) = 10.0d0; AL(3,5) = 1.0d0
  AL(4,1) = 0.1d0;   AL(4,2) = 1.0d0;  AL(4,3) = 10.0d0; AL(4,4) = 100.0d0; AL(4,5) = 10.0d0
  AL(5,1) = 0.01d0;  AL(5,2) = 0.1d0;  AL(5,3) = 1.0d0;  AL(5,4) = 10.0d0; AL(5,5) = 100.0d0

  BL(1,1) = 1.0d0; BL(2,1) = 2.0d0; BL(3,1) = 3.0d0; BL(4,1) = 4.0d0; BL(5,1) = 5.0d0

  AFL = AL
  call DPOTRF('U', NL, AFL, NL, info)
  if (info /= 0) stop 'dpotrf failed for 5x5'

  XL(:,1) = BL(:,1)
  call DPOTRS('U', NL, 1, AFL, NL, XL, NL, info)
  if (info /= 0) stop 'dpotrs failed for 5x5'

  call DPORFS('U', NL, 1, AL, NL, AFL, NL, BL, NL, XL, NL, FERRL, BERRL, &
              WORKL, IWORKL, info)

  call begin_test('illcond_5x5')
  call print_int('info', info)
  call print_array('x', XL(1:NL,1), NL)
  call print_array('ferr', FERRL, 1)
  call print_array('berr', BERRL, 1)
  call end_test()

end program
