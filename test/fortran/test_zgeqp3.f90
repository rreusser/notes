program test_zgeqp3
  use test_utils
  implicit none

  integer, parameter :: MAXMN = 8
  integer, parameter :: BIGMN = 40
  complex*16 :: A(MAXMN, MAXMN), TAU(MAXMN), WORK(200)
  complex*16 :: AB(BIGMN, BIGMN), TAUB(BIGMN), WORKB(10000)
  double precision :: A_r(2*MAXMN*MAXMN), TAU_r(2*MAXMN), RWORK(2*MAXMN)
  double precision :: AB_r(2*BIGMN*BIGMN), TAUB_r(2*BIGMN), RWORKB(2*BIGMN)
  equivalence (A, A_r)
  equivalence (TAU, TAU_r)
  equivalence (AB, AB_r)
  equivalence (TAUB, TAUB_r)
  integer :: JPVT(MAXMN), JPVTB(BIGMN), info, i, j, LWORK

  LWORK = 200

  ! Test 1: 4x3 matrix
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.0d0); A(2,1) = (2.0d0, 1.0d0); A(3,1) = (0.0d0, 0.0d0); A(4,1) = (1.0d0, 1.0d0)
  A(1,2) = (0.0d0, 2.0d0); A(2,2) = (1.0d0, 0.0d0); A(3,2) = (3.0d0, 1.0d0); A(4,2) = (2.0d0, 0.0d0)
  A(1,3) = (3.0d0, 1.0d0); A(2,3) = (0.0d0, 0.0d0); A(3,3) = (1.0d0, 0.0d0); A(4,3) = (2.0d0, 1.0d0)
  JPVT = 0
  call zgeqp3(4, 3, A, MAXMN, JPVT, TAU, WORK, LWORK, RWORK, info)
  call begin_test('rect_4x3')
  call print_int('info', info)
  call print_array('a', A_r, 2*MAXMN*3)
  call print_array('tau', TAU_r, 2*3)
  call print_int_array('jpvt', JPVT, 3)
  call end_test()

  ! Test 2: 3x4 matrix (more cols than rows)
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.0d0); A(2,1) = (0.0d0, 1.0d0); A(3,1) = (2.0d0, 0.0d0)
  A(1,2) = (3.0d0, 1.0d0); A(2,2) = (1.0d0, 0.0d0); A(3,2) = (0.0d0, 1.0d0)
  A(1,3) = (0.0d0, 2.0d0); A(2,3) = (2.0d0, 1.0d0); A(3,3) = (1.0d0, 0.0d0)
  A(1,4) = (1.0d0, 1.0d0); A(2,4) = (0.0d0, 0.0d0); A(3,4) = (3.0d0, 0.0d0)
  JPVT = 0
  call zgeqp3(3, 4, A, MAXMN, JPVT, TAU, WORK, LWORK, RWORK, info)
  call begin_test('rect_3x4')
  call print_int('info', info)
  call print_array('a', A_r, 2*MAXMN*4)
  call print_array('tau', TAU_r, 2*3)
  call print_int_array('jpvt', JPVT, 4)
  call end_test()

  ! Test 3: rank-deficient 3x3 matrix (col 3 = col 1 + col 2)
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.0d0); A(2,1) = (2.0d0, 0.0d0); A(3,1) = (3.0d0, 0.0d0)
  A(1,2) = (0.0d0, 1.0d0); A(2,2) = (0.0d0, 2.0d0); A(3,2) = (0.0d0, 3.0d0)
  A(1,3) = (1.0d0, 1.0d0); A(2,3) = (2.0d0, 2.0d0); A(3,3) = (3.0d0, 3.0d0)
  JPVT = 0
  call zgeqp3(3, 3, A, MAXMN, JPVT, TAU, WORK, LWORK, RWORK, info)
  call begin_test('rank_deficient')
  call print_int('info', info)
  call print_array('a', A_r, 2*MAXMN*3)
  call print_array('tau', TAU_r, 2*3)
  call print_int_array('jpvt', JPVT, 3)
  call end_test()

  ! Test 4: N=0 (empty)
  JPVT = 0
  call zgeqp3(3, 0, A, MAXMN, JPVT, TAU, WORK, LWORK, RWORK, info)
  call begin_test('n_zero')
  call print_int('info', info)
  call end_test()

  ! Test 5: M=0 (empty)
  JPVT = 0
  call zgeqp3(0, 3, A, 1, JPVT, TAU, WORK, LWORK, RWORK, info)
  call begin_test('m_zero')
  call print_int('info', info)
  call end_test()

  ! Test 6: 1x1 matrix
  A = (0.0d0, 0.0d0)
  A(1,1) = (3.0d0, 4.0d0)
  JPVT(1) = 0
  call zgeqp3(1, 1, A, MAXMN, JPVT, TAU, WORK, LWORK, RWORK, info)
  call begin_test('one_by_one')
  call print_int('info', info)
  call print_array('a', A_r, 2)
  call print_array('tau', TAU_r, 2)
  call print_int_array('jpvt', JPVT, 1)
  call end_test()

  ! Test 7: with pre-pivoted columns (JPVT(1) != 0 means "fix column 1")
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.0d0); A(2,1) = (0.0d0, 0.0d0); A(3,1) = (0.0d0, 0.0d0)
  A(1,2) = (0.0d0, 0.0d0); A(2,2) = (3.0d0, 0.0d0); A(3,2) = (4.0d0, 0.0d0)
  A(1,3) = (0.0d0, 0.0d0); A(2,3) = (1.0d0, 1.0d0); A(3,3) = (2.0d0, 0.0d0)
  JPVT = 0
  JPVT(1) = 1  ! Fix column 1
  call zgeqp3(3, 3, A, MAXMN, JPVT, TAU, WORK, LWORK, RWORK, info)
  call begin_test('fixed_col')
  call print_int('info', info)
  call print_array('a', A_r, 2*MAXMN*3)
  call print_array('tau', TAU_r, 2*3)
  call print_int_array('jpvt', JPVT, 3)
  call end_test()

  ! Test 8: Fixed column at position 3 (swap needed: j=2 > nfxd=0)
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.0d0); A(2,1) = (2.0d0, 0.0d0); A(3,1) = (0.0d0, 0.0d0); A(4,1) = (1.0d0, 0.0d0)
  A(1,2) = (3.0d0, 0.0d0); A(2,2) = (0.0d0, 1.0d0); A(3,2) = (2.0d0, 0.0d0); A(4,2) = (1.0d0, 1.0d0)
  A(1,3) = (0.0d0, 1.0d0); A(2,3) = (1.0d0, 0.0d0); A(3,3) = (3.0d0, 0.0d0); A(4,3) = (2.0d0, 0.0d0)
  JPVT = 0
  JPVT(3) = 1  ! Fix column 3 (will be swapped to position 1)
  call zgeqp3(4, 3, A, MAXMN, JPVT, TAU, WORK, LWORK, RWORK, info)
  call begin_test('fixed_col_swap')
  call print_int('info', info)
  call print_array('a', A_r, 2*MAXMN*3)
  call print_array('tau', TAU_r, 2*3)
  call print_int_array('jpvt', JPVT, 3)
  call end_test()

  ! Test 9: Multiple fixed columns (fix col 1 and col 3)
  A = (0.0d0, 0.0d0)
  A(1,1) = (1.0d0, 0.0d0); A(2,1) = (2.0d0, 0.0d0); A(3,1) = (0.0d0, 0.0d0); A(4,1) = (1.0d0, 0.0d0)
  A(1,2) = (3.0d0, 0.0d0); A(2,2) = (0.0d0, 1.0d0); A(3,2) = (2.0d0, 0.0d0); A(4,2) = (1.0d0, 1.0d0)
  A(1,3) = (0.0d0, 1.0d0); A(2,3) = (1.0d0, 0.0d0); A(3,3) = (3.0d0, 0.0d0); A(4,3) = (2.0d0, 0.0d0)
  JPVT = 0
  JPVT(1) = 1
  JPVT(3) = 1  ! Fix columns 1 and 3
  call zgeqp3(4, 3, A, MAXMN, JPVT, TAU, WORK, LWORK, RWORK, info)
  call begin_test('fixed_two_cols')
  call print_int('info', info)
  call print_array('a', A_r, 2*MAXMN*3)
  call print_array('tau', TAU_r, 2*3)
  call print_int_array('jpvt', JPVT, 3)
  call end_test()

  ! Test 10: Wide matrix (N=36 > NB=32) to trigger blocked zlaqps path
  AB = (0.0d0, 0.0d0)
  do j = 1, 36
    do i = 1, 8
      AB(i,j) = dcmplx(dble(mod(i*j+3, 7)) - 3.0d0, dble(mod(i+j, 5)) - 2.0d0)
    end do
  end do
  JPVTB = 0
  call zgeqp3(8, 36, AB, BIGMN, JPVTB, TAUB, WORKB, 10000, RWORKB, info)
  call begin_test('wide_8x36_blocked')
  call print_int('info', info)
  call print_array('a', AB_r, 2*BIGMN*36)
  call print_array('tau', TAUB_r, 2*8)
  call print_int_array('jpvt', JPVTB, 36)
  call end_test()

end program
