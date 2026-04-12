program test_zla_gbrcond_x
  use test_utils
  implicit none

  ! Parameters
  integer, parameter :: NMAX = 3
  integer, parameter :: KLMAX = 1, KUMAX = 1
  integer, parameter :: LDAB = KLMAX + KUMAX + 1
  integer, parameter :: LDAFB = 2*KLMAX + KUMAX + 1

  ! Variables
  complex*16 :: AB(LDAB, NMAX), AFB(LDAFB, NMAX)
  complex*16 :: X(NMAX), WORK(2*NMAX)
  double precision :: RWORK(NMAX), result
  integer :: IPIV(NMAX), INFO
  double precision :: ZLA_GBRCOND_X
  external :: ZLA_GBRCOND_X, ZGBTRF

  ! ============================================================
  ! Test 1: trans='N', well-conditioned 3x3 complex tridiagonal
  ! Full matrix:
  !   [ 2+1i  3-1i   0      ]
  !   [ 1+0i  5+2i   6-1i   ]
  !   [ 0      4+1i  8+0i   ]
  ! Band storage (KL=1, KU=1), LDAB=3:
  !   Row 1 (superdiag): 0        3-1i    6-1i
  !   Row 2 (diagonal):  2+1i     5+2i    8+0i
  !   Row 3 (subdiag):   1+0i     4+1i    0
  ! ============================================================
  AB(1, 1) = ( 0.0d0, 0.0d0 )
  AB(1, 2) = ( 3.0d0, -1.0d0 )
  AB(1, 3) = ( 6.0d0, -1.0d0 )
  AB(2, 1) = ( 2.0d0, 1.0d0 )
  AB(2, 2) = ( 5.0d0, 2.0d0 )
  AB(2, 3) = ( 8.0d0, 0.0d0 )
  AB(3, 1) = ( 1.0d0, 0.0d0 )
  AB(3, 2) = ( 4.0d0, 1.0d0 )
  AB(3, 3) = ( 0.0d0, 0.0d0 )

  ! Copy into AFB for factoring (row KL+1..2*KL+KU+1 hold AB, top KL rows are workspace)
  AFB(1, :) = ( 0.0d0, 0.0d0 )
  AFB(2, 1) = ( 0.0d0, 0.0d0 )
  AFB(2, 2) = AB(1, 2)
  AFB(2, 3) = AB(1, 3)
  AFB(3, 1) = AB(2, 1)
  AFB(3, 2) = AB(2, 2)
  AFB(3, 3) = AB(2, 3)
  AFB(4, 1) = AB(3, 1)
  AFB(4, 2) = AB(3, 2)
  AFB(4, 3) = ( 0.0d0, 0.0d0 )

  call ZGBTRF(NMAX, NMAX, KLMAX, KUMAX, AFB, LDAFB, IPIV, INFO)

  X(1) = ( 1.0d0, 0.5d0 )
  X(2) = ( 2.0d0, -0.5d0 )
  X(3) = ( 3.0d0, 1.0d0 )

  result = ZLA_GBRCOND_X('N', NMAX, KLMAX, KUMAX, AB, LDAB, AFB, LDAFB, &
       IPIV, X, INFO, WORK, RWORK)

  call begin_test('trans_N_wellcond')
  call print_scalar('result', result)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 2: trans='C' (conjugate transpose), same matrix
  ! ============================================================
  result = ZLA_GBRCOND_X('C', NMAX, KLMAX, KUMAX, AB, LDAB, AFB, LDAFB, &
       IPIV, X, INFO, WORK, RWORK)

  call begin_test('trans_C_wellcond')
  call print_scalar('result', result)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 3: trans='N' with different X vector
  ! ============================================================
  X(1) = ( 2.0d0, 0.0d0 )
  X(2) = ( 1.0d0, 1.0d0 )
  X(3) = ( 0.5d0, -0.5d0 )

  result = ZLA_GBRCOND_X('N', NMAX, KLMAX, KUMAX, AB, LDAB, AFB, LDAFB, &
       IPIV, X, INFO, WORK, RWORK)

  call begin_test('trans_N_altx')
  call print_scalar('result', result)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 4: trans='C' with alt X
  ! ============================================================
  result = ZLA_GBRCOND_X('C', NMAX, KLMAX, KUMAX, AB, LDAB, AFB, LDAFB, &
       IPIV, X, INFO, WORK, RWORK)

  call begin_test('trans_C_altx')
  call print_scalar('result', result)
  call print_int('info', INFO)
  call end_test()

  ! ============================================================
  ! Test 5: N=1 edge case
  ! ============================================================
  block
    integer, parameter :: N1 = 1, KL1 = 0, KU1 = 0
    integer, parameter :: LDAB1 = 1, LDAFB1 = 1
    complex*16 :: AB1(1, 1), AFB1(1, 1), X1(1), WORK1(2)
    double precision :: RWORK1(1), res1
    integer :: IPIV1(1), INFO1

    AB1(1, 1) = ( 5.0d0, 2.0d0 )
    AFB1(1, 1) = ( 5.0d0, 2.0d0 )
    call ZGBTRF(1, 1, KL1, KU1, AFB1, LDAFB1, IPIV1, INFO1)
    X1(1) = ( 2.0d0, 1.0d0 )
    res1 = ZLA_GBRCOND_X('N', N1, KL1, KU1, AB1, LDAB1, AFB1, LDAFB1, &
         IPIV1, X1, INFO1, WORK1, RWORK1)

    call begin_test('n1_edge')
    call print_scalar('result', res1)
    call print_int('info', INFO1)
    call end_test()
  end block

end program
