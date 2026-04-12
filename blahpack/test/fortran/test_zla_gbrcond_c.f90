program test_zla_gbrcond_c
  use test_utils
  implicit none

  double precision :: ZLA_GBRCOND_C
  external :: ZLA_GBRCOND_C, ZGBTRF

  call test_trans_n_capply_true()
  call test_trans_c_capply_true()
  call test_trans_n_capply_false()
  call test_trans_c_capply_false()
  call test_n1_edge()

contains

  subroutine test_trans_n_capply_true()
    integer, parameter :: N = 3, KL = 1, KU = 1
    integer, parameter :: LDAB = KL + KU + 1
    integer, parameter :: LDAFB = 2*KL + KU + 1
    complex*16 :: AB(LDAB, N), AFB(LDAFB, N)
    complex*16 :: WORK(2*N)
    double precision :: C(N), RWORK(N), result
    integer :: IPIV(N), INFO

    AB = (0.0d0, 0.0d0)
    ! Matrix:
    !   [ (2,1) (3,-1)   0   ]
    !   [ (1,2) (5,0)  (6,1) ]
    !   [   0   (4,-2) (8,3) ]
    ! Col 1 superdiag (none), diag AB(2,1), sub AB(3,1)
    AB(2, 1) = (2.0d0, 1.0d0)
    AB(3, 1) = (1.0d0, 2.0d0)
    ! Col 2: super AB(1,2), diag AB(2,2), sub AB(3,2)
    AB(1, 2) = (3.0d0, -1.0d0)
    AB(2, 2) = (5.0d0, 0.0d0)
    AB(3, 2) = (4.0d0, -2.0d0)
    ! Col 3: super AB(1,3), diag AB(2,3)
    AB(1, 3) = (6.0d0, 1.0d0)
    AB(2, 3) = (8.0d0, 3.0d0)

    ! Build AFB (rows KL+1..2*KL+KU+1 hold the band; dgbtrf needs KL extra rows above)
    AFB = (0.0d0, 0.0d0)
    AFB(KL+1, 1) = (0.0d0, 0.0d0)        ! superdiag of col 1 = 0
    AFB(KL+2, 1) = AB(2, 1)              ! diag
    AFB(KL+3, 1) = AB(3, 1)              ! sub
    AFB(KL+1, 2) = AB(1, 2)
    AFB(KL+2, 2) = AB(2, 2)
    AFB(KL+3, 2) = AB(3, 2)
    AFB(KL+1, 3) = AB(1, 3)
    AFB(KL+2, 3) = AB(2, 3)

    call ZGBTRF(N, N, KL, KU, AFB, LDAFB, IPIV, INFO)

    C(1) = 1.5d0
    C(2) = 2.0d0
    C(3) = 3.5d0

    result = ZLA_GBRCOND_C('N', N, KL, KU, AB, LDAB, AFB, LDAFB, &
         IPIV, C, .true., INFO, WORK, RWORK)

    call begin_test('trans_N_capply_true')
    call print_scalar('result', result)
    call print_int('info', INFO)
    call end_test()
  end subroutine

  subroutine test_trans_c_capply_true()
    integer, parameter :: N = 3, KL = 1, KU = 1
    integer, parameter :: LDAB = KL + KU + 1
    integer, parameter :: LDAFB = 2*KL + KU + 1
    complex*16 :: AB(LDAB, N), AFB(LDAFB, N)
    complex*16 :: WORK(2*N)
    double precision :: C(N), RWORK(N), result
    integer :: IPIV(N), INFO

    AB = (0.0d0, 0.0d0)
    AB(2, 1) = (2.0d0, 1.0d0)
    AB(3, 1) = (1.0d0, 2.0d0)
    AB(1, 2) = (3.0d0, -1.0d0)
    AB(2, 2) = (5.0d0, 0.0d0)
    AB(3, 2) = (4.0d0, -2.0d0)
    AB(1, 3) = (6.0d0, 1.0d0)
    AB(2, 3) = (8.0d0, 3.0d0)

    AFB = (0.0d0, 0.0d0)
    AFB(KL+2, 1) = AB(2, 1)
    AFB(KL+3, 1) = AB(3, 1)
    AFB(KL+1, 2) = AB(1, 2)
    AFB(KL+2, 2) = AB(2, 2)
    AFB(KL+3, 2) = AB(3, 2)
    AFB(KL+1, 3) = AB(1, 3)
    AFB(KL+2, 3) = AB(2, 3)

    call ZGBTRF(N, N, KL, KU, AFB, LDAFB, IPIV, INFO)

    C(1) = 1.5d0
    C(2) = 2.0d0
    C(3) = 3.5d0

    result = ZLA_GBRCOND_C('C', N, KL, KU, AB, LDAB, AFB, LDAFB, &
         IPIV, C, .true., INFO, WORK, RWORK)

    call begin_test('trans_C_capply_true')
    call print_scalar('result', result)
    call print_int('info', INFO)
    call end_test()
  end subroutine

  subroutine test_trans_n_capply_false()
    integer, parameter :: N = 3, KL = 1, KU = 1
    integer, parameter :: LDAB = KL + KU + 1
    integer, parameter :: LDAFB = 2*KL + KU + 1
    complex*16 :: AB(LDAB, N), AFB(LDAFB, N)
    complex*16 :: WORK(2*N)
    double precision :: C(N), RWORK(N), result
    integer :: IPIV(N), INFO

    AB = (0.0d0, 0.0d0)
    AB(2, 1) = (2.0d0, 1.0d0)
    AB(3, 1) = (1.0d0, 2.0d0)
    AB(1, 2) = (3.0d0, -1.0d0)
    AB(2, 2) = (5.0d0, 0.0d0)
    AB(3, 2) = (4.0d0, -2.0d0)
    AB(1, 3) = (6.0d0, 1.0d0)
    AB(2, 3) = (8.0d0, 3.0d0)

    AFB = (0.0d0, 0.0d0)
    AFB(KL+2, 1) = AB(2, 1)
    AFB(KL+3, 1) = AB(3, 1)
    AFB(KL+1, 2) = AB(1, 2)
    AFB(KL+2, 2) = AB(2, 2)
    AFB(KL+3, 2) = AB(3, 2)
    AFB(KL+1, 3) = AB(1, 3)
    AFB(KL+2, 3) = AB(2, 3)

    call ZGBTRF(N, N, KL, KU, AFB, LDAFB, IPIV, INFO)

    C(1) = 1.5d0
    C(2) = 2.0d0
    C(3) = 3.5d0

    result = ZLA_GBRCOND_C('N', N, KL, KU, AB, LDAB, AFB, LDAFB, &
         IPIV, C, .false., INFO, WORK, RWORK)

    call begin_test('trans_N_capply_false')
    call print_scalar('result', result)
    call print_int('info', INFO)
    call end_test()
  end subroutine

  subroutine test_trans_c_capply_false()
    integer, parameter :: N = 3, KL = 1, KU = 1
    integer, parameter :: LDAB = KL + KU + 1
    integer, parameter :: LDAFB = 2*KL + KU + 1
    complex*16 :: AB(LDAB, N), AFB(LDAFB, N)
    complex*16 :: WORK(2*N)
    double precision :: C(N), RWORK(N), result
    integer :: IPIV(N), INFO

    AB = (0.0d0, 0.0d0)
    AB(2, 1) = (2.0d0, 1.0d0)
    AB(3, 1) = (1.0d0, 2.0d0)
    AB(1, 2) = (3.0d0, -1.0d0)
    AB(2, 2) = (5.0d0, 0.0d0)
    AB(3, 2) = (4.0d0, -2.0d0)
    AB(1, 3) = (6.0d0, 1.0d0)
    AB(2, 3) = (8.0d0, 3.0d0)

    AFB = (0.0d0, 0.0d0)
    AFB(KL+2, 1) = AB(2, 1)
    AFB(KL+3, 1) = AB(3, 1)
    AFB(KL+1, 2) = AB(1, 2)
    AFB(KL+2, 2) = AB(2, 2)
    AFB(KL+3, 2) = AB(3, 2)
    AFB(KL+1, 3) = AB(1, 3)
    AFB(KL+2, 3) = AB(2, 3)

    call ZGBTRF(N, N, KL, KU, AFB, LDAFB, IPIV, INFO)

    C(1) = 1.5d0
    C(2) = 2.0d0
    C(3) = 3.5d0

    result = ZLA_GBRCOND_C('C', N, KL, KU, AB, LDAB, AFB, LDAFB, &
         IPIV, C, .false., INFO, WORK, RWORK)

    call begin_test('trans_C_capply_false')
    call print_scalar('result', result)
    call print_int('info', INFO)
    call end_test()
  end subroutine

  subroutine test_n1_edge()
    integer, parameter :: N = 1, KL = 0, KU = 0
    integer, parameter :: LDAB = 1, LDAFB = 1
    complex*16 :: AB(LDAB, N), AFB(LDAFB, N)
    complex*16 :: WORK(2*N)
    double precision :: C(N), RWORK(N), result
    integer :: IPIV(N), INFO

    AB(1, 1) = (5.0d0, 2.0d0)
    AFB(1, 1) = (5.0d0, 2.0d0)

    call ZGBTRF(N, N, KL, KU, AFB, LDAFB, IPIV, INFO)

    C(1) = 2.0d0

    result = ZLA_GBRCOND_C('N', N, KL, KU, AB, LDAB, AFB, LDAFB, &
         IPIV, C, .true., INFO, WORK, RWORK)

    call begin_test('n1_edge')
    call print_scalar('result', result)
    call print_int('info', INFO)
    call end_test()
  end subroutine

end program
