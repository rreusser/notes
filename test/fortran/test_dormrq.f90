program test_dormrq
  use test_utils
  implicit none

  double precision :: A(4, 4), ASAVE(4, 4), C(4, 4), TAU(4), TAUSAVE(4), WORK(100)
  double precision :: CR(2, 4)
  integer :: info, lwork

  integer, parameter :: BIG = 40
  double precision :: ABIG(BIG, BIG), TAUBIG(BIG), CBIG(BIG, BIG), WBIG(10000)
  integer :: i, j

  lwork = 100

  A = 0.0d0
  A(1,1) = 1.0d0; A(1,2) = 2.0d0; A(1,3) = 3.0d0; A(1,4) = 4.0d0
  A(2,1) = 5.0d0; A(2,2) = 6.0d0; A(2,3) = 7.0d0; A(2,4) = 8.0d0
  A(3,1) = 9.0d0; A(3,2) = 10.0d0; A(3,3) = 11.0d0; A(3,4) = 12.0d0
  TAU = 0.0d0
  call dgerqf(3, 4, A, 4, TAU, WORK, lwork, info)
  ASAVE = A
  TAUSAVE = TAU

  call begin_test('rq_factor')
  call print_array('A', A, 12)
  call print_array('TAU', TAU, 3)
  call end_test()

  C = 0.0d0
  C(1,1) = 1.0d0; C(2,2) = 1.0d0; C(3,3) = 1.0d0; C(4,4) = 1.0d0
  A = ASAVE; TAU = TAUSAVE
  call dormrq('L', 'N', 4, 4, 3, A, 4, TAU, C, 4, WORK, lwork, info)
  call begin_test('left_notrans')
  call print_int('info', info)
  call print_array('c', C, 16)
  call end_test()

  C = 0.0d0
  C(1,1) = 1.0d0; C(2,2) = 1.0d0; C(3,3) = 1.0d0; C(4,4) = 1.0d0
  A = ASAVE; TAU = TAUSAVE
  call dormrq('L', 'T', 4, 4, 3, A, 4, TAU, C, 4, WORK, lwork, info)
  call begin_test('left_trans')
  call print_int('info', info)
  call print_array('c', C, 16)
  call end_test()

  C = 0.0d0
  C(1,1) = 1.0d0; C(2,2) = 1.0d0; C(3,3) = 1.0d0; C(4,4) = 1.0d0
  A = ASAVE; TAU = TAUSAVE
  call dormrq('R', 'N', 4, 4, 3, A, 4, TAU, C, 4, WORK, lwork, info)
  call begin_test('right_notrans')
  call print_int('info', info)
  call print_array('c', C, 16)
  call end_test()

  C = 0.0d0
  C(1,1) = 1.0d0; C(2,2) = 1.0d0; C(3,3) = 1.0d0; C(4,4) = 1.0d0
  A = ASAVE; TAU = TAUSAVE
  call dormrq('R', 'T', 4, 4, 3, A, 4, TAU, C, 4, WORK, lwork, info)
  call begin_test('right_trans')
  call print_int('info', info)
  call print_array('c', C, 16)
  call end_test()

  call dormrq('L', 'N', 0, 4, 0, A, 1, TAU, C, 1, WORK, lwork, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call end_test()

  call dormrq('L', 'N', 4, 0, 0, A, 4, TAU, C, 4, WORK, lwork, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  call dormrq('L', 'N', 4, 4, 0, A, 4, TAU, C, 4, WORK, lwork, info)
  call begin_test('k_zero')
  call print_int('info', info)
  call end_test()

  C = 0.0d0
  C(1,1) = 1.0d0; C(2,1) = 3.0d0; C(3,1) = -1.0d0; C(4,1) = 2.0d0
  C(1,2) = 2.0d0; C(2,2) = 0.0d0; C(3,2) = 4.0d0; C(4,2) = -1.0d0
  A = ASAVE; TAU = TAUSAVE
  call dormrq('L', 'N', 4, 2, 3, A, 4, TAU, C, 4, WORK, lwork, info)
  call begin_test('left_notrans_rect')
  call print_int('info', info)
  call print_array('c', C, 8)
  call end_test()

  CR = 0.0d0
  CR(1,1) = 1.0d0; CR(2,1) = 0.0d0
  CR(1,2) = 2.0d0; CR(2,2) = 1.0d0
  CR(1,3) = -1.0d0; CR(2,3) = 3.0d0
  CR(1,4) = 4.0d0; CR(2,4) = -2.0d0
  A = ASAVE; TAU = TAUSAVE
  call dormrq('R', 'N', 2, 4, 3, A, 4, TAU, CR, 2, WORK, lwork, info)
  call begin_test('right_notrans_rect')
  call print_int('info', info)
  call print_array('c', CR, 8)
  call end_test()

  ! Large blocked path tests (K=40 > NB=32)
  do j = 1, BIG
    do i = 1, BIG
      ABIG(i, j) = dble(mod(i*7 + j*13, 97)) / 97.0d0
    end do
  end do
  call dgerqf(BIG, BIG, ABIG, BIG, TAUBIG, WBIG, 10000, info)

  call begin_test('big_rq_factor')
  call print_array('A', ABIG, BIG*BIG)
  call print_array('TAU', TAUBIG, BIG)
  call end_test()

  CBIG = 0.0d0
  do i = 1, BIG
    CBIG(i,i) = 1.0d0
  end do
  call dormrq('L', 'N', BIG, BIG, BIG, ABIG, BIG, TAUBIG, CBIG, BIG, WBIG, 10000, info)
  call begin_test('blocked_left_notrans')
  call print_int('info', info)
  call print_array('c', CBIG, BIG*BIG)
  call end_test()

  CBIG = 0.0d0
  do i = 1, BIG
    CBIG(i,i) = 1.0d0
  end do
  call dormrq('L', 'T', BIG, BIG, BIG, ABIG, BIG, TAUBIG, CBIG, BIG, WBIG, 10000, info)
  call begin_test('blocked_left_trans')
  call print_int('info', info)
  call print_array('c', CBIG, BIG*BIG)
  call end_test()

  CBIG = 0.0d0
  do i = 1, BIG
    CBIG(i,i) = 1.0d0
  end do
  call dormrq('R', 'N', BIG, BIG, BIG, ABIG, BIG, TAUBIG, CBIG, BIG, WBIG, 10000, info)
  call begin_test('blocked_right_notrans')
  call print_int('info', info)
  call print_array('c', CBIG, BIG*BIG)
  call end_test()

  CBIG = 0.0d0
  do i = 1, BIG
    CBIG(i,i) = 1.0d0
  end do
  call dormrq('R', 'T', BIG, BIG, BIG, ABIG, BIG, TAUBIG, CBIG, BIG, WBIG, 10000, info)
  call begin_test('blocked_right_trans')
  call print_int('info', info)
  call print_array('c', CBIG, BIG*BIG)
  call end_test()

end program
