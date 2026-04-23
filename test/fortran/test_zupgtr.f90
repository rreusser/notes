program test_zupgtr
  use test_utils
  implicit none

  integer, parameter :: NMAX = 4
  complex*16 :: AP(NMAX*(NMAX+1)/2), APsave(NMAX*(NMAX+1)/2)
  complex*16 :: TAU(NMAX), Q(NMAX, NMAX), WORK(256)
  double precision :: D(NMAX), E(NMAX)
  integer :: INFO, IINFO, I, J

  ! EQUIVALENCE for printing interleaved re/im
  double precision :: Q_r(2*NMAX*NMAX)
  equivalence (Q, Q_r)
  double precision :: Qflat_r(2*NMAX*NMAX)
  complex*16 :: Qflat(NMAX*NMAX)
  equivalence (Qflat, Qflat_r)

  ! ============================================================
  ! Setup: 4x4 Hermitian matrix in upper-packed storage
  ! ============================================================
  AP(1) = (4.0d0, 0.0d0)
  AP(2) = (1.0d0, -1.0d0)
  AP(3) = (2.0d0, 0.0d0)
  AP(4) = (-2.0d0, 1.0d0)
  AP(5) = (0.0d0, 0.0d0)
  AP(6) = (3.0d0, 0.0d0)
  AP(7) = (2.0d0, 0.0d0)
  AP(8) = (1.0d0, -1.0d0)
  AP(9) = (-2.0d0, -1.0d0)
  AP(10) = (-1.0d0, 0.0d0)

  call ZHPTRD('U', 4, AP, D, E, TAU, IINFO)
  APsave = AP

  ! ============================================================
  ! Test 1: Upper, N=4
  ! ============================================================
  AP = APsave
  call ZUPGTR('U', 4, AP, TAU, Q, NMAX, WORK, INFO)

  call begin_test('zupgtr_4x4_upper')
  call print_int('info', INFO)
  do J = 1, 4
    do I = 1, 4
      Qflat((J-1)*4 + I) = Q(I,J)
    end do
  end do
  call print_array('Q', Qflat_r, 32)
  call end_test()

  ! ============================================================
  ! Setup: lower packed
  ! ============================================================
  AP(1) = (4.0d0, 0.0d0)
  AP(2) = (1.0d0, 1.0d0)
  AP(3) = (-2.0d0, -1.0d0)
  AP(4) = (2.0d0, 0.0d0)
  AP(5) = (2.0d0, 0.0d0)
  AP(6) = (0.0d0, 0.0d0)
  AP(7) = (1.0d0, -1.0d0)
  AP(8) = (3.0d0, 0.0d0)
  AP(9) = (-2.0d0, 1.0d0)
  AP(10) = (-1.0d0, 0.0d0)

  call ZHPTRD('L', 4, AP, D, E, TAU, IINFO)
  APsave = AP

  ! ============================================================
  ! Test 2: Lower, N=4
  ! ============================================================
  AP = APsave
  call ZUPGTR('L', 4, AP, TAU, Q, NMAX, WORK, INFO)

  call begin_test('zupgtr_4x4_lower')
  call print_int('info', INFO)
  do J = 1, 4
    do I = 1, 4
      Qflat((J-1)*4 + I) = Q(I,J)
    end do
  end do
  call print_array('Q', Qflat_r, 32)
  call end_test()

  ! ============================================================
  ! Test 3: N=1
  ! ============================================================
  AP(1) = (5.0d0, 0.0d0)
  call ZUPGTR('U', 1, AP, TAU, Q, NMAX, WORK, INFO)

  call begin_test('zupgtr_1x1')
  call print_int('info', INFO)
  Qflat(1) = Q(1,1)
  call print_array('Q', Qflat_r, 2)
  call end_test()

  ! ============================================================
  ! Test 4: N=0
  ! ============================================================
  call ZUPGTR('U', 0, AP, TAU, Q, NMAX, WORK, INFO)

  call begin_test('zupgtr_0x0')
  call print_int('info', INFO)
  call end_test()

end program test_zupgtr
