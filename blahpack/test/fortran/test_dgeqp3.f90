program test_dgeqp3
  use test_utils
  implicit none

  integer, parameter :: MAXMN = 8
  integer, parameter :: BIGMN = 140
  double precision :: A(MAXMN, MAXMN), TAU(MAXMN), WORK(200)
  double precision :: AB(BIGMN, BIGMN), TAUB(BIGMN), WORKB(10000)
  integer :: JPVT(MAXMN), JPVTB(BIGMN), info, i, j, LWORK

  LWORK = 200

  ! Test 1: 4x3 matrix (tall)
  A = 0.0d0
  A(1,1) = 1.0d0; A(2,1) = 2.0d0; A(3,1) = 0.0d0; A(4,1) = 1.0d0
  A(1,2) = 0.0d0; A(2,2) = 1.0d0; A(3,2) = 3.0d0; A(4,2) = 2.0d0
  A(1,3) = 3.0d0; A(2,3) = 0.0d0; A(3,3) = 1.0d0; A(4,3) = 2.0d0
  JPVT = 0
  call dgeqp3(4, 3, A, MAXMN, JPVT, TAU, WORK, LWORK, info)
  call begin_test('rect_4x3')
  call print_int('info', info)
  call print_matrix('a', A, MAXMN, 4, 3)
  call print_array('tau', TAU, 3)
  call print_int_array('jpvt', JPVT, 3)
  call end_test()

  ! Test 2: 3x4 matrix (wide)
  A = 0.0d0
  A(1,1) = 1.0d0; A(2,1) = 0.0d0; A(3,1) = 2.0d0
  A(1,2) = 3.0d0; A(2,2) = 1.0d0; A(3,2) = 0.0d0
  A(1,3) = 0.0d0; A(2,3) = 2.0d0; A(3,3) = 1.0d0
  A(1,4) = 1.0d0; A(2,4) = 0.0d0; A(3,4) = 3.0d0
  JPVT = 0
  call dgeqp3(3, 4, A, MAXMN, JPVT, TAU, WORK, LWORK, info)
  call begin_test('rect_3x4')
  call print_int('info', info)
  call print_matrix('a', A, MAXMN, 3, 4)
  call print_array('tau', TAU, 3)
  call print_int_array('jpvt', JPVT, 4)
  call end_test()

  ! Test 3: Square 4x4
  A = 0.0d0
  A(1,1) = 2.0d0; A(2,1) = 1.0d0; A(3,1) = 0.0d0; A(4,1) = 1.0d0
  A(1,2) = 0.0d0; A(2,2) = 3.0d0; A(3,2) = 1.0d0; A(4,2) = 2.0d0
  A(1,3) = 1.0d0; A(2,3) = 0.0d0; A(3,3) = 4.0d0; A(4,3) = 1.0d0
  A(1,4) = 3.0d0; A(2,4) = 2.0d0; A(3,4) = 1.0d0; A(4,4) = 5.0d0
  JPVT = 0
  call dgeqp3(4, 4, A, MAXMN, JPVT, TAU, WORK, LWORK, info)
  call begin_test('square_4x4')
  call print_int('info', info)
  call print_matrix('a', A, MAXMN, 4, 4)
  call print_array('tau', TAU, 4)
  call print_int_array('jpvt', JPVT, 4)
  call end_test()

  ! Test 4: N=0
  JPVT = 0
  call dgeqp3(3, 0, A, MAXMN, JPVT, TAU, WORK, LWORK, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 5: M=0
  JPVT = 0
  call dgeqp3(0, 3, A, 1, JPVT, TAU, WORK, LWORK, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: 1x1 matrix
  A = 0.0d0
  A(1,1) = 5.0d0
  JPVT(1) = 0
  call dgeqp3(1, 1, A, MAXMN, JPVT, TAU, WORK, LWORK, info)
  call begin_test('one_by_one')
  call print_int('info', info)
  call print_matrix('a', A, MAXMN, 1, 1)
  call print_array('tau', TAU, 1)
  call print_int_array('jpvt', JPVT, 1)
  call end_test()

  ! Test 7: Pre-set JPVT - fix column 1
  A = 0.0d0
  A(1,1) = 1.0d0; A(2,1) = 0.0d0; A(3,1) = 0.0d0
  A(1,2) = 0.0d0; A(2,2) = 3.0d0; A(3,2) = 4.0d0
  A(1,3) = 0.0d0; A(2,3) = 1.0d0; A(3,3) = 2.0d0
  JPVT = 0
  JPVT(1) = 1
  call dgeqp3(3, 3, A, MAXMN, JPVT, TAU, WORK, LWORK, info)
  call begin_test('fixed_col1')
  call print_int('info', info)
  call print_matrix('a', A, MAXMN, 3, 3)
  call print_array('tau', TAU, 3)
  call print_int_array('jpvt', JPVT, 3)
  call end_test()

  ! Test 8: Fix column 3 (will be swapped to front)
  A = 0.0d0
  A(1,1) = 1.0d0; A(2,1) = 2.0d0; A(3,1) = 0.0d0; A(4,1) = 1.0d0
  A(1,2) = 3.0d0; A(2,2) = 0.0d0; A(3,2) = 2.0d0; A(4,2) = 1.0d0
  A(1,3) = 0.0d0; A(2,3) = 1.0d0; A(3,3) = 3.0d0; A(4,3) = 2.0d0
  JPVT = 0
  JPVT(3) = 1
  call dgeqp3(4, 3, A, MAXMN, JPVT, TAU, WORK, LWORK, info)
  call begin_test('fixed_col3_swap')
  call print_int('info', info)
  call print_matrix('a', A, MAXMN, 4, 3)
  call print_array('tau', TAU, 3)
  call print_int_array('jpvt', JPVT, 3)
  call end_test()

  ! Test 9: Fix columns 1 and 3
  A = 0.0d0
  A(1,1) = 1.0d0; A(2,1) = 2.0d0; A(3,1) = 0.0d0; A(4,1) = 1.0d0
  A(1,2) = 3.0d0; A(2,2) = 0.0d0; A(3,2) = 2.0d0; A(4,2) = 1.0d0
  A(1,3) = 0.0d0; A(2,3) = 1.0d0; A(3,3) = 3.0d0; A(4,3) = 2.0d0
  JPVT = 0
  JPVT(1) = 1
  JPVT(3) = 1
  call dgeqp3(4, 3, A, MAXMN, JPVT, TAU, WORK, LWORK, info)
  call begin_test('fixed_two_cols')
  call print_int('info', info)
  call print_matrix('a', A, MAXMN, 4, 3)
  call print_array('tau', TAU, 3)
  call print_int_array('jpvt', JPVT, 3)
  call end_test()

  ! Test 10: Wide matrix (N=36) unblocked (sminmn=8 < NB=32)
  AB = 0.0d0
  do j = 1, 36
    do i = 1, 8
      AB(i,j) = dble(mod(i*j + 3*i + 7, 11)) - 5.0d0
    end do
  end do
  JPVTB = 0
  call dgeqp3(8, 36, AB, BIGMN, JPVTB, TAUB, WORKB, 10000, info)
  call begin_test('wide_8x36')
  call print_int('info', info)
  call print_matrix('a', AB, BIGMN, 8, 36)
  call print_array('tau', TAUB, 8)
  call print_int_array('jpvt', JPVTB, 36)
  call end_test()

  ! Test 11: Large 140x130 to trigger blocked dlaqps path (sminmn=130 > NX=128)
  ! Use sin/cos to avoid tied column norms
  AB = 0.0d0
  do j = 1, 130
    do i = 1, 140
      AB(i,j) = sin(dble(i) * 0.7d0 + dble(j) * 1.3d0) + cos(dble(i*j) * 0.3d0)
    end do
  end do
  JPVTB = 0
  call dgeqp3(140, 130, AB, BIGMN, JPVTB, TAUB, WORKB, 10000, info)
  call begin_test('large_140x130_blocked')
  call print_int('info', info)
  call print_matrix('a', AB, BIGMN, 140, 130)
  call print_array('tau', TAUB, 130)
  call print_int_array('jpvt', JPVTB, 130)
  call end_test()

end program
