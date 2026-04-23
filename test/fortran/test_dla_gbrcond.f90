program test_dla_gbrcond
  use test_utils
  implicit none

  ! Parameters
  integer, parameter :: NMAX = 3
  integer, parameter :: KLMAX = 1, KUMAX = 1
  integer, parameter :: LDAB = KLMAX + KUMAX + 1
  integer, parameter :: LDAFB = 2*KLMAX + KUMAX + 1

  ! Variables
  double precision :: AB(LDAB, NMAX), AFB(LDAFB, NMAX)
  integer :: IPIV(NMAX)
  double precision :: C(NMAX), WORK(5*NMAX), result
  integer :: IWORK(NMAX), INFO
  double precision :: DLA_GBRCOND
  external :: DLA_GBRCOND, DGBTRF

  ! ============================================================
  ! Test 1: trans='N', cmode=1, well-conditioned 3x3 tridiagonal
  ! Band matrix (KL=1,KU=1):
  !   Row 0 (superdiag):  0   3   6
  !   Row 1 (diagonal):   2   5   8
  !   Row 2 (subdiag):    1   4   0
  ! Full matrix:
  !   [ 2  3  0 ]
  !   [ 1  5  6 ]
  !   [ 0  4  8 ]
  ! ============================================================
  AB(1, 1) = 0.0d0
  AB(1, 2) = 3.0d0
  AB(1, 3) = 6.0d0
  AB(2, 1) = 2.0d0
  AB(2, 2) = 5.0d0
  AB(2, 3) = 8.0d0
  AB(3, 1) = 1.0d0
  AB(3, 2) = 4.0d0
  AB(3, 3) = 0.0d0

  ! Copy into AFB for factoring
  AFB(1, :) = 0.0d0
  AFB(2, 1) = 0.0d0
  AFB(2, 2) = AB(1, 2)
  AFB(2, 3) = AB(1, 3)
  AFB(3, 1) = AB(2, 1)
  AFB(3, 2) = AB(2, 2)
  AFB(3, 3) = AB(2, 3)
  AFB(4, 1) = AB(3, 1)
  AFB(4, 2) = AB(3, 2)
  AFB(4, 3) = 0.0d0

  call DGBTRF(NMAX, NMAX, KLMAX, KUMAX, AFB, LDAFB, IPIV, INFO)

  C(1) = 1.0d0
  C(2) = 2.0d0
  C(3) = 3.0d0

  result = DLA_GBRCOND('N', NMAX, KLMAX, KUMAX, AB, LDAB, AFB, LDAFB, &
       IPIV, 1, C, INFO, WORK, IWORK)

  call begin_test('trans_N_cmode1_wellcond')
  call print_scalar('result', result)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 2: trans='T', cmode=1, same matrix
  ! ============================================================
  result = DLA_GBRCOND('T', NMAX, KLMAX, KUMAX, AB, LDAB, AFB, LDAFB, &
       IPIV, 1, C, INFO, WORK, IWORK)

  call begin_test('trans_T_cmode1_wellcond')
  call print_scalar('result', result)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 3: trans='N', cmode=0
  ! ============================================================
  result = DLA_GBRCOND('N', NMAX, KLMAX, KUMAX, AB, LDAB, AFB, LDAFB, &
       IPIV, 0, C, INFO, WORK, IWORK)

  call begin_test('trans_N_cmode0')
  call print_scalar('result', result)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 4: trans='N', cmode=-1
  ! ============================================================
  result = DLA_GBRCOND('N', NMAX, KLMAX, KUMAX, AB, LDAB, AFB, LDAFB, &
       IPIV, -1, C, INFO, WORK, IWORK)

  call begin_test('trans_N_cmode_neg1')
  call print_scalar('result', result)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 5: trans='T', cmode=0
  ! ============================================================
  result = DLA_GBRCOND('T', NMAX, KLMAX, KUMAX, AB, LDAB, AFB, LDAFB, &
       IPIV, 0, C, INFO, WORK, IWORK)

  call begin_test('trans_T_cmode0')
  call print_scalar('result', result)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 6: trans='T', cmode=-1
  ! ============================================================
  result = DLA_GBRCOND('T', NMAX, KLMAX, KUMAX, AB, LDAB, AFB, LDAFB, &
       IPIV, -1, C, INFO, WORK, IWORK)

  call begin_test('trans_T_cmode_neg1')
  call print_scalar('result', result)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 7: N=1 edge case (KL=0, KU=0)
  ! ============================================================
  block
    integer, parameter :: N1 = 1, KL1 = 0, KU1 = 0
    integer, parameter :: LDAB1 = 1, LDAFB1 = 1
    double precision :: AB1(1, 1), AFB1(1, 1)
    double precision :: C1(1), WORK1(5), res1
    integer :: IPIV1(1), IWORK1(1), INFO1

    AB1(1, 1) = 5.0d0
    AFB1(1, 1) = 5.0d0
    call DGBTRF(1, 1, KL1, KU1, AFB1, LDAFB1, IPIV1, INFO1)
    C1(1) = 2.0d0
    res1 = DLA_GBRCOND('N', N1, KL1, KU1, AB1, LDAB1, AFB1, LDAFB1, &
         IPIV1, 1, C1, INFO1, WORK1, IWORK1)

    call begin_test('n1_edge')
    call print_scalar('result', res1)
    call print_int('info', INFO1)
    call end_test()
  end block

end program
